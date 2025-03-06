import gleam/dynamic
import gleam/erlang/process
import gleam/io
import gleam/option
import khepri_store.{type Path, NameComponent}

pub fn main() {
  // Start a local Khepri store
  let store_name = "my_khepri_store"

  // Function to ensure the store is started
  let ensure_store = fn() {
    case khepri_store.start_local(store_name) {
      Ok(_) -> {
        process.sleep(100)
        // Small delay to ensure it's ready
        Ok(Nil)
      }
      Error(err) -> Error(err)
    }
  }

  // Start the store initially
  let _ = case ensure_store() {
    Ok(_) -> {
      io.println("Successfully started Khepri store")

      // Create a path
      let path: Path = [NameComponent("users"), NameComponent("user1")]

      // Store a user value
      let user = dynamic.from(#("John Doe", 30))

      // Ensure store is running before put
      let _ = ensure_store()
      let _ = case khepri_store.put(store_name, path, user) {
        Ok(_) -> {
          io.println("Successfully stored user")

          // Ensure store is running before get
          let _ = ensure_store()
          let _ = case khepri_store.get(store_name, path) {
            Ok(option.Some(val)) -> {
              io.println("Retrieved user:")
              io.debug(val)
              Nil
            }
            Ok(option.None) -> {
              io.println("User not found")
              Nil
            }
            Error(err) -> {
              io.println("Error retrieving user: " <> debug_error(err))
              Nil
            }
          }

          // Ensure store is running before delete
          let _ = ensure_store()
          let _ = case khepri_store.delete(store_name, path) {
            Ok(_) -> {
              io.println("Successfully deleted user")
              Nil
            }
            Error(err) -> {
              io.println("Failed to delete user: " <> debug_error(err))
              Nil
            }
          }

          // Stop the Khepri store at the end
          let _ = case khepri_store.stop(store_name) {
            Ok(_) -> {
              io.println("Successfully stopped Khepri store")
              Nil
            }
            Error(err) -> {
              io.println("Failed to stop Khepri store: " <> debug_error(err))
              Nil
            }
          }

          Nil
        }
        Error(err) -> {
          io.println("Failed to store user: " <> debug_error(err))
          Nil
        }
      }

      Nil
    }
    Error(err) -> {
      io.println("Failed to start Khepri store: " <> debug_error(err))
      Nil
    }
  }

  Nil
}

// Helper function to convert errors to strings for debugging
fn debug_error(err: khepri_store.KhepriError) -> String {
  case err {
    khepri_store.NotFound -> "NotFound"
    khepri_store.InvalidPath -> "InvalidPath"
    khepri_store.AlreadyExists -> "AlreadyExists"
    khepri_store.ClusterError(msg) -> "ClusterError: " <> msg
    khepri_store.OtherError(msg) -> "OtherError: " <> msg
  }
}
