import gleam/dict
import gleam/dynamic
import gleam/list
import gleam/result

/// Represents an error returned by Khepri operations
pub type KhepriError =
  String

/// Starts the Khepri database
///
/// Returns Ok(Nil) on success or Error(message) on failure
@external(erlang, "gleam_khepri", "start")
pub fn start() -> Result(Nil, KhepriError)

/// Stores a value at the specified path
///
/// The path is a list of strings representing the hierarchical location.
/// Automatically updates metadata for efficient key listing.
///
/// ## Examples
///
/// ```
/// my_khepri.put(["users", "alice"], "alice@example.org")
/// ```
@external(erlang, "gleam_khepri", "put")
pub fn put(path: List(String), value: a) -> Result(Nil, KhepriError)

/// Retrieves a value from the specified path
///
/// Returns the raw dynamic value which must be decoded.
/// Consider using get_as for type safety.
///
/// ## Examples
///
/// ```
/// my_khepri.get(["users", "alice"])
/// ```
@external(erlang, "gleam_khepri", "get")
pub fn get(path: List(String)) -> Result(dynamic.Dynamic, KhepriError)

/// Retrieves multiple values matching a path pattern
///
/// Use a wildcard "_" to match all keys at a particular level.
///
/// ## Examples
///
/// ```
/// my_khepri.get_many(["users", "_"])  // Get all users
/// ```
@external(erlang, "gleam_khepri", "get_many")
pub fn get_many(
  path_pattern: List(String),
) -> Result(List(#(String, dynamic.Dynamic)), KhepriError)

/// Lists all keys under a specific path
///
/// ## Examples
///
/// ```
/// my_khepri.list_keys(["users"])  // List all user keys
/// ```
@external(erlang, "gleam_khepri", "list_keys")
pub fn list_keys(path: List(String)) -> Result(List(String), KhepriError)

/// Retrieves a value and decodes it to the specified type
///
/// This combines get with automatic decoding to provide type safety.
///
/// ## Examples
///
/// ```
/// my_khepri.get_as(["users", "alice"], dynamic.string)
/// ```
pub fn get_as(
  path: List(String),
  decoder: fn(dynamic.Dynamic) -> Result(a, dynamic.DecodeErrors),
) -> Result(a, KhepriError) {
  case get(path) {
    Ok(value) -> {
      case decoder(value) {
        Ok(decoded) -> Ok(decoded)
        Error(_) -> Error("Failed to decode value")
      }
    }
    Error(e) -> Error(e)
  }
}

/// Retrieves multiple values and decodes them to the specified type
///
/// Similar to get_many but with automatic decoding for type safety.
///
/// ## Examples
///
/// ```
/// my_khepri.get_many_as(["users", "_"], dynamic.string)
/// ```
pub fn get_many_as(
  path_pattern: List(String),
  decoder: fn(dynamic.Dynamic) -> Result(a, dynamic.DecodeErrors),
) -> Result(List(#(String, a)), KhepriError) {
  case get_many(path_pattern) {
    Ok(pairs) -> {
      let decoded =
        list.filter_map(pairs, fn(pair) {
          let #(key, value) = pair

          case decoder(value) {
            Ok(decoded_value) -> Ok(#(key, decoded_value))
            Error(_) -> Error(Nil)
          }
        })

      Ok(decoded)
    }
    Error(e) -> Error(e)
  }
}

/// Deletes a value at the specified path
///
/// Automatically updates metadata for consistency.
///
/// ## Examples
///
/// ```
/// my_khepri.delete(["users", "alice"])
/// ```
@external(erlang, "gleam_khepri", "delete")
pub fn delete(path: List(String)) -> Result(Nil, KhepriError)

/// Creates a new directory at the specified path
///
/// A convenience function to create an empty container.
///
/// ## Examples
///
/// ```
/// my_khepri.create_dir(["users"])
/// ```
pub fn create_dir(path: List(String)) -> Result(Nil, KhepriError) {
  put(path, "")
}

/// Checks if a path exists in the database
///
/// ## Examples
///
/// ```
/// my_khepri.exists(["users", "alice"])
/// ```
pub fn exists(path: List(String)) -> Bool {
  case get(path) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Gets all key-value pairs under a path as a dictionary
///
/// ## Examples
///
/// ```
/// my_khepri.get_dict(["users"], dynamic.string)
/// ```
pub fn get_dict(
  path: List(String),
  decoder: fn(dynamic.Dynamic) -> Result(a, dynamic.DecodeErrors),
) -> Result(dict.Dict(String, a), KhepriError) {
  // Fix: Use ++ for list concatenation instead of +
  case get_many_as(list.append(path, ["_"]), decoder) {
    Ok(pairs) -> Ok(dict.from_list(pairs))
    Error(e) -> Error(e)
  }
}

/// Updates a value at the specified path if it exists
///
/// ## Examples
///
/// ```
/// my_khepri.update(["users", "alice"], fn(email) { email <> ".verified" })
/// ```
pub fn update(
  path: List(String),
  updater: fn(dynamic.Dynamic) -> a,
) -> Result(Nil, KhepriError) {
  case get(path) {
    Ok(value) -> {
      let updated = updater(value)
      put(path, updated)
    }
    Error(e) -> Error(e)
  }
}

/// Ensures a directory path exists, creating it if necessary
///
/// ## Examples
///
/// ```
/// my_khepri.ensure_dir(["users", "profiles"])
/// ```
pub fn ensure_dir(path: List(String)) -> Result(Nil, KhepriError) {
  case exists(path) {
    True -> Ok(Nil)
    False -> create_dir(path)
  }
}
