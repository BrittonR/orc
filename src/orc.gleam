// src/main.gleam
import gleam/dynamic
import gleam/io
import gleam/option
import khepri_store.{type Path, NameComponent}

pub fn main() {
  // Start a local Khepri store
  let store_name = "my_khepri_store"
  case khepri_store.start_local(store_name) {
    Ok(_) -> io.println("Successfully started Khepri store")
    Error(_) -> io.println("Failed to start Khepri store")
  }

  // Create a path
  let path: Path = [NameComponent("users"), NameComponent("user1")]

  // Store a user value
  let user = dynamic.from(#("John Doe", 30))
  case khepri_store.put(store_name, path, user) {
    Ok(_) -> io.println("Successfully stored user")
    Error(_) -> io.println("Failed to store user")
  }

  // Retrieve the user
  case khepri_store.get(store_name, path) {
    Ok(option.Some(val)) -> {
      io.println("Retrieved user:")
      io.debug(val)
      Nil
    }
    Ok(option.None) -> {
      io.println("User not found")
      Nil
    }
    Error(_) -> {
      io.println("Error retrieving user")
      Nil
    }
  }

  // Delete the user
  case khepri_store.delete(store_name, path) {
    Ok(_) -> io.println("Successfully deleted user")
    Error(_) -> io.println("Failed to delete user")
  }

  // Stop the Khepri store
  case khepri_store.stop(store_name) {
    Ok(_) -> io.println("Successfully stopped Khepri store")
    Error(_) -> io.println("Failed to stop Khepri store")
  }
}
