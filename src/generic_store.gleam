import gleam/dict
import gleam/dynamic
import gleam/list
import gleam/result
import my_khepri

/// Gets multiple values and attempts to decode them
///
/// This function is likely the one with the error on line 57
pub fn get_many_as(
  path_pattern: List(String),
  decoder: fn(dynamic.Dynamic) -> Result(a, dynamic.DecodeErrors),
) -> Result(List(#(String, a)), String) {
  case my_khepri.get_many(path_pattern) {
    Ok(pairs) -> {
      // Here's the fixed section - proper pattern matching with case
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

/// Gets all items as a dictionary with appropriate decoding
pub fn get_dict(
  path: List(String),
  decoder: fn(dynamic.Dynamic) -> Result(a, dynamic.DecodeErrors),
) -> Result(dict.Dict(String, a), String) {
  case get_many_as(list.append(path, ["_"]), decoder) {
    Ok(pairs) -> Ok(dict.from_list(pairs))
    Error(e) -> Error(e)
  }
}

/// Example wrapper for creating a typed store
pub fn create_email_store() {
  case my_khepri.ensure_dir(["emails"]) {
    Ok(_) -> Nil
    Error(_) -> Nil
  }
}

/// Example function for getting all emails
pub fn get_all_emails() -> Result(List(#(String, String)), String) {
  get_many_as(["emails", "_"], dynamic.string)
}

/// Example function for adding an email
pub fn add_email(name: String, email: String) -> Result(Nil, String) {
  my_khepri.put(["emails", name], email)
}

/// Example function for getting a specific email
pub fn get_email(name: String) -> Result(String, String) {
  my_khepri.get_as(["emails", name], dynamic.string)
}

/// Example function for deleting an email
pub fn delete_email(name: String) -> Result(Nil, String) {
  my_khepri.delete(["emails", name])
}
