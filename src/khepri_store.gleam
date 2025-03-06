// src/khepri_store.gleam
import gleam/dynamic
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type KhepriResult(a) =
  Result(a, KhepriError)

pub type KhepriError {
  NotFound
  InvalidPath
  AlreadyExists
  ClusterError(String)
  OtherError(String)
}

// Define path components for Khepri paths
pub type PathComponent {
  NameComponent(String)
  HereComponent
  AllComponent
}

pub type Path =
  List(PathComponent)

// External function declarations with updated syntax
@external(erlang, "khepri", "start")
fn do_khepri_start(store: atom.Atom) -> dynamic.Dynamic

@external(erlang, "khepri", "stop")
fn do_khepri_stop(store: atom.Atom) -> dynamic.Dynamic

@external(erlang, "khepri", "put")
fn do_khepri_put(
  store: atom.Atom,
  path: dynamic.Dynamic,
  value: dynamic.Dynamic,
) -> dynamic.Dynamic

@external(erlang, "khepri", "get")
fn do_khepri_get(store: atom.Atom, path: dynamic.Dynamic) -> dynamic.Dynamic

@external(erlang, "khepri", "delete")
fn do_khepri_delete(store: atom.Atom, path: dynamic.Dynamic) -> dynamic.Dynamic

@external(erlang, "khepri", "join")
fn do_khepri_join(store: atom.Atom, remote: atom.Atom) -> dynamic.Dynamic

// Convert a path list to Khepri's path format
fn to_khepri_path(path: Path) -> dynamic.Dynamic {
  let path_elements =
    list.map(path, fn(component) {
      case component {
        NameComponent(name) -> atom.create_from_string(name)
        HereComponent -> atom.create_from_string("?")
        AllComponent -> atom.create_from_string("*")
      }
    })
  dynamic.from(path_elements)
}

// A decoder for tuple2 with "ok"/"error" first element
fn result_tuple_decoder() -> decode.Decoder(#(String, dynamic.Dynamic)) {
  // Create a custom decoder using decode.then
  decode.then(decode.dynamic, fn(raw_tuple) {
    case dynamic.tuple2(dynamic.string, dynamic.dynamic)(raw_tuple) {
      Ok(value) -> decode.success(value)
      Error(_) -> decode.failure(#("", dynamic.from(Nil)), "Result Tuple")
    }
  })
}

// Check if a dynamic value is the atom with the given name
fn is_atom_with_name(dyn: dynamic.Dynamic, name: String) -> Bool {
  case dynamic.classify(dyn) {
    "Atom" -> {
      // Try to get the atom name
      let atom_str = case dyn {
        a -> {
          // Attempt to convert to atom
          case atom.from_dynamic(a) {
            Ok(a) -> atom.to_string(a)
            Error(_) -> ""
          }
        }
      }

      // Compare with expected name
      atom_str == name
    }
    _ -> False
  }
}

// Start a local Khepri node
pub fn start_local(store_name: String) -> KhepriResult(Nil) {
  let store_atom = atom.create_from_string(store_name)
  let _ = do_khepri_start(store_atom)

  // We'll assume it succeeded unless there's evidence otherwise
  Ok(Nil)
}

// Stop a Khepri node
pub fn stop(store_name: String) -> KhepriResult(Nil) {
  let store_atom = atom.create_from_string(store_name)
  let _ = do_khepri_stop(store_atom)

  // We'll assume it succeeded
  Ok(Nil)
}

// Join a Khepri cluster
pub fn join_cluster(
  local_store_name: String,
  remote_node_name: String,
) -> KhepriResult(Nil) {
  let store_atom = atom.create_from_string(local_store_name)
  let remote_atom = atom.create_from_string(remote_node_name)

  let result = do_khepri_join(store_atom, remote_atom)

  case decode.run(result, result_tuple_decoder()) {
    Ok(#("error", reason)) -> {
      let error_msg = dynamic.classify(reason)
      Error(ClusterError("Failed to join cluster: " <> error_msg))
    }
    _ -> Ok(Nil)
    // Assume success for any other pattern
  }
}

// Store a value at the given path
pub fn put(
  store_name: String,
  path: Path,
  value: dynamic.Dynamic,
) -> KhepriResult(Nil) {
  let store_atom = atom.create_from_string(store_name)
  let khepri_path = to_khepri_path(path)

  let result = do_khepri_put(store_atom, khepri_path, value)

  case decode.run(result, result_tuple_decoder()) {
    Ok(#("error", reason)) -> {
      let error_msg = dynamic.classify(reason)
      Error(OtherError("Failed to put: " <> error_msg))
    }
    _ -> Ok(Nil)
  }
}

pub fn get(
  store_name: String,
  path: Path,
) -> KhepriResult(Option(dynamic.Dynamic)) {
  let store_atom = atom.create_from_string(store_name)
  let khepri_path = to_khepri_path(path)

  // Attempt to get the result
  let result = do_khepri_get(store_atom, khepri_path)

  // Check if the result is a noproc error
  let noproc_check = case dynamic.classify(result) {
    "Tuple" -> {
      case dynamic.tuple2(dynamic.dynamic, dynamic.dynamic)(result) {
        Ok(#(first, _second)) -> {
          // Check if the first element is the atom 'noproc'
          case dynamic.classify(first) {
            "Atom" -> {
              let atom_result = atom.from_dynamic(first)
              case atom_result {
                Ok(atom_val) -> {
                  let atom_str = atom.to_string(atom_val)
                  case atom_str {
                    "noproc" ->
                      Error(OtherError("Process not running (noproc)"))
                    _ -> Ok(None)
                    // Continue with normal processing
                  }
                }
                _ -> Ok(None)
                // Continue with normal processing
              }
            }
            _ -> Ok(None)
            // Continue with normal processing
          }
        }
        _ -> Ok(None)
        // Continue with normal processing
      }
    }
    _ -> Ok(None)
    // Continue with normal processing
  }

  // If we detected a noproc error, return it immediately
  case noproc_check {
    Error(err) -> Error(err)
    Ok(None) -> {
      // Normal result handling
      case decode.run(result, result_tuple_decoder()) {
        Ok(#("ok", value)) -> Ok(Some(value))
        Ok(#("error", reason)) -> {
          // Check if it's a not_found error
          case is_atom_with_name(reason, "not_found") {
            True -> Ok(None)
            False -> {
              let error_msg = dynamic.classify(reason)
              Error(OtherError("Failed to get: " <> error_msg))
            }
          }
        }
        _ -> {
          // If we can't decode it as a tuple, just return the raw value
          Ok(Some(result))
        }
      }
    }
    _ -> noproc_check
    // This should never happen but it makes the type system happy
  }
}

// Delete a value from the given path
pub fn delete(store_name: String, path: Path) -> KhepriResult(Nil) {
  let store_atom = atom.create_from_string(store_name)
  let khepri_path = to_khepri_path(path)

  let result = do_khepri_delete(store_atom, khepri_path)

  case decode.run(result, result_tuple_decoder()) {
    Ok(#("ok", _)) -> Ok(Nil)
    Ok(#("error", reason)) -> {
      // Check if it's a not_found error
      case is_atom_with_name(reason, "not_found") {
        True -> Error(NotFound)
        False -> {
          let error_msg = dynamic.classify(reason)
          Error(OtherError("Failed to delete: " <> error_msg))
        }
      }
    }
    _ -> Ok(Nil)
    // If we can't decode it as a tuple, assume success
  }
}
