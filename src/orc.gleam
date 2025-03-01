import cluster
import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/erlang
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/set
import gleam/string
import my_khepri
import shellout

pub fn main() {
  // Start Khepri
  case my_khepri.start() {
    Ok(_) -> io.println("Khepri started successfully")
    Error(e) -> {
      io.println("Failed to start Khepri: " <> e)
      panic
    }
  }

  io.println("\n=== Email Example ===")
  email_example()

  io.println("\n=== User Profile Example ===")
  user_profile_example()

  io.println("\n=== VM Example ===")
  vm_example()

  io.println("\n=== Cluster Example ===")
  cluster_example()
  // io.println("\n=== Shellout Example ===")
  // file_watcher()
}

// --- Email Example ---
fn email_example() {
  my_khepri.ensure_dir(["emails"])

  let test_emails = [
    #("alice", "alice@example.org"),
    #("bob", "bob@example.org"),
    #("charlie", "charlie@example.org"),
  ]

  list.each(test_emails, fn(pair) {
    let #(name, email) = pair
    case my_khepri.put(["emails", name], email) {
      Ok(_) -> io.println("Added email for " <> name)
      Error(e) -> io.println("Failed to add email: " <> e)
    }
  })

  io.println("\nAll emails:")
  case my_khepri.get_many_as(["emails", "_"], dynamic.string) {
    Ok(emails) -> {
      list.each(emails, fn(pair) {
        let #(name, email) = pair
        io.println("  " <> name <> ": " <> email)
      })
    }
    Error(e) -> io.println("Error listing emails: " <> e)
  }

  io.println("\nEmails as dictionary:")
  case my_khepri.get_dict(["emails"], dynamic.string) {
    Ok(email_dict) -> {
      dict.fold(email_dict, "", fn(acc, name, email) {
        acc <> "\n  " <> name <> ": " <> email
      })
      |> io.println
    }
    Error(e) -> io.println("Error getting email dict: " <> e)
  }
}

// --- User Profile Example ---

// Define the UserProfile record.
type UserProfile {
  UserProfile(name: String, age: Int, interests: List(String))
}

// Convert a UserProfile to a dynamic dictionary.
fn user_profile_to_dynamic(profile: UserProfile) -> dynamic.Dynamic {
  let d =
    dict.from_list([
      #("name", dynamic.from(profile.name)),
      #("age", dynamic.from(profile.age)),
      #("interests", dynamic.from(list.map(profile.interests, dynamic.from))),
    ])
  dynamic.from(d)
}

// Decoder for UserProfile.
// We assume the dynamic data is a dictionary with keys "name", "age", and "interests".
fn decode_profile(
  data: dynamic.Dynamic,
) -> Result(UserProfile, List(decode.DecodeError)) {
  let decoder = {
    use name <- decode.field("name", decode.string)
    use age <- decode.field("age", decode.int)
    use interests <- decode.field("interests", decode.list(of: decode.string))
    decode.success(UserProfile(name: name, age: age, interests: interests))
  }
  decode.run(data, decoder)
}

fn user_profile_example() {
  my_khepri.ensure_dir(["users"])

  let alice =
    UserProfile(name: "Alice Smith", age: 28, interests: [
      "programming", "hiking", "photography",
    ])
  let bob =
    UserProfile(name: "Bob Johnson", age: 35, interests: [
      "cooking", "gaming", "travel",
    ])

  // Store profiles as dynamic dictionaries.
  my_khepri.put(["users", "alice"], user_profile_to_dynamic(alice))
  my_khepri.put(["users", "bob"], user_profile_to_dynamic(bob))

  io.println("\nAlice's profile:")
  case my_khepri.get(["users", "alice"]) {
    Ok(data) -> {
      case decode_profile(data) {
        Ok(profile) -> {
          io.println("  Name: " <> profile.name)
          io.println("  Age: " <> int.to_string(profile.age))
          io.println("  Interests: " <> string.join(profile.interests, ", "))
        }
        Error(errs) ->
          io.println(
            "Failed to decode profile: "
            <> string.join(list.map(errs, fn(err) { err.expected }), ", "),
          )
      }
    }
    Error(e) -> io.println("Error getting profile: " <> e)
  }

  io.println("\nAll users:")
  case my_khepri.list_keys(["users"]) {
    Ok(users) -> list.each(users, fn(user) { io.println("  " <> user) })
    Error(e) -> io.println("Error listing users: " <> e)
  }
}

// --- VM Example ---

// Make VmStatus a public type so it can be used in public interfaces.
pub type VmStatus {
  Running
  Stopped
  Paused
}

type VirtualMachine {
  VirtualMachine(
    name: String,
    status: VmStatus,
    cpu_cores: Int,
    memory_mb: Int,
    storage_gb: Int,
  )
}

// Convert a VirtualMachine to a dynamic dictionary.
fn virtual_machine_to_dynamic(vm: VirtualMachine) -> dynamic.Dynamic {
  let status_string = case vm.status {
    Running -> "Running"
    Stopped -> "Stopped"
    Paused -> "Paused"
  }
  let d =
    dict.from_list([
      #("name", dynamic.from(vm.name)),
      #("status", dynamic.from(status_string)),
      #("cpu_cores", dynamic.from(vm.cpu_cores)),
      #("memory_mb", dynamic.from(vm.memory_mb)),
      #("storage_gb", dynamic.from(vm.storage_gb)),
    ])
  dynamic.from(d)
}

// Public decoder for VmStatus.
pub fn decode_vm_status() -> decode.Decoder(VmStatus) {
  decode.then(decode.string, fn(s) {
    case s {
      "Running" -> decode.success(Running)
      "Stopped" -> decode.success(Stopped)
      "Paused" -> decode.success(Paused)
      _ -> decode.failure(Running, "VmStatus")
    }
  })
}

// Decoder for VirtualMachine.
// We assume the dynamic data is a dictionary with keys corresponding to the record fields.
fn decode_vm(
  data: dynamic.Dynamic,
) -> Result(VirtualMachine, List(decode.DecodeError)) {
  let decoder = {
    use name <- decode.field("name", decode.string)
    use status <- decode.field("status", decode_vm_status())
    use cpu_cores <- decode.field("cpu_cores", decode.int)
    use memory_mb <- decode.field("memory_mb", decode.int)
    use storage_gb <- decode.field("storage_gb", decode.int)
    decode.success(VirtualMachine(
      name: name,
      status: status,
      cpu_cores: cpu_cores,
      memory_mb: memory_mb,
      storage_gb: storage_gb,
    ))
  }
  decode.run(data, decoder)
}

fn vm_example() {
  my_khepri.ensure_dir(["vms"])

  let vm1 =
    VirtualMachine(
      name: "web-server",
      status: Running,
      cpu_cores: 2,
      memory_mb: 4096,
      storage_gb: 40,
    )
  let vm2 =
    VirtualMachine(
      name: "db-server",
      status: Running,
      cpu_cores: 4,
      memory_mb: 8192,
      storage_gb: 100,
    )
  let vm3 =
    VirtualMachine(
      name: "test-instance",
      status: Stopped,
      cpu_cores: 1,
      memory_mb: 2048,
      storage_gb: 20,
    )

  // Store VMs as dynamic dictionaries.
  my_khepri.put(["vms", "vm1"], virtual_machine_to_dynamic(vm1))
  my_khepri.put(["vms", "vm2"], virtual_machine_to_dynamic(vm2))
  my_khepri.put(["vms", "vm3"], virtual_machine_to_dynamic(vm3))

  io.println("\nAll VMs:")
  case my_khepri.list_keys(["vms"]) {
    Ok(vms) -> {
      list.each(vms, fn(vm_id) {
        case my_khepri.get(["vms", vm_id]) {
          Ok(data) -> {
            case decode_vm(data) {
              Ok(vm) -> {
                let status_str = case vm.status {
                  Running -> "Running"
                  Stopped -> "Stopped"
                  Paused -> "Paused"
                }
                io.println("VM " <> vm_id <> " details:")
                io.println("  Name: " <> vm.name)
                io.println("  Status: " <> status_str)
                io.println("  CPU Cores: " <> int.to_string(vm.cpu_cores))
                io.println("  Memory (MB): " <> int.to_string(vm.memory_mb))
                io.println("  Storage (GB): " <> int.to_string(vm.storage_gb))
              }
              Error(_) -> io.println("  " <> vm_id <> ": Failed to decode VM")
            }
          }
          Error(_) -> io.println("  " <> vm_id <> ": Error getting VM data")
        }
      })
    }
    Error(e) -> io.println("Error listing VMs: " <> e)
  }

  io.println("\nUpdating VM status:")
  case
    my_khepri.update(["vms", "vm3"], fn(vm_data) {
      case decode_vm(vm_data) {
        Ok(vm) ->
          virtual_machine_to_dynamic(VirtualMachine(..vm, status: Running))
        Error(_) -> panic("Failed to decode VM data")
      }
    })
  {
    Ok(_) -> io.println("  vm3 status updated to Running")
    Error(e) -> io.println("  Error updating VM: " <> e)
  }
  io.println("\nAll VMs:")
  case my_khepri.list_keys(["vms"]) {
    Ok(vms) -> {
      list.each(vms, fn(vm_id) {
        case my_khepri.get(["vms", vm_id]) {
          Ok(data) -> {
            case decode_vm(data) {
              Ok(vm) -> {
                let status_str = case vm.status {
                  Running -> "Running"
                  Stopped -> "Stopped"
                  Paused -> "Paused"
                }
                io.println("VM " <> vm_id <> " details:")
                io.println("  Name: " <> vm.name)
                io.println("  Status: " <> status_str)
                io.println("  CPU Cores: " <> int.to_string(vm.cpu_cores))
                io.println("  Memory (MB): " <> int.to_string(vm.memory_mb))
                io.println("  Storage (GB): " <> int.to_string(vm.storage_gb))
              }
              Error(_) -> io.println("  " <> vm_id <> ": Failed to decode VM")
            }
          }
          Error(_) -> io.println("  " <> vm_id <> ": Error getting VM data")
        }
      })
    }
    Error(e) -> io.println("Error listing VMs: " <> e)
  }
}

// --- Recursive Gleam File Watcher Example ---

// Public function that starts the file watcher
pub fn file_watcher() -> Nil {
  io.println("Starting Gleam file watcher (recursive implementation)...")
  io.println("Press Ctrl+C to stop the watcher")

  // Start recursive loop with initial empty set and iteration counter
  file_watcher_loop(set.new(), 1)
}

// Private recursive function with accumulators for previous files and iteration counter
fn file_watcher_loop(previous_files: set.Set(String), iteration: Int) -> Nil {
  // Clear screen (attempt to use clear command but fallback to printing newlines)
  case
    shellout.command(run: "clear", with: [], in: ".", opt: [
      shellout.LetBeStdout,
    ])
  {
    // If clear doesn't work, print some newlines to create visual separation
    Error(_) -> list.range(0, 10) |> list.each(fn(_) { io.println("") })
    Ok(_) -> Nil
  }

  // Get current files
  let current_files = case find_gleam_files() {
    Ok(files) -> files
    Error(error) -> {
      io.println("Error finding Gleam files: " <> error)
      set.new()
    }
  }

  // Display header with iteration count
  io.println("=== Gleam Files (Scan #" <> int.to_string(iteration) <> ") ===")

  // Calculate changes
  let new_files = set.difference(current_files, previous_files)
  let removed_files = set.difference(previous_files, current_files)

  // Print current files
  set.to_list(current_files)
  |> list.sort(string.compare)
  |> list.each(fn(file) {
    let prefix = case set.contains(new_files, file) {
      True -> "+ "
      // New file
      False -> "  "
      // Existing file
    }
    io.println(prefix <> file)
  })

  // Print removed files
  set.to_list(removed_files)
  |> list.sort(string.compare)
  |> list.each(fn(file) { io.println("- " <> file) })

  // Print summary
  io.println("")
  io.println("Total: " <> int.to_string(set.size(current_files)) <> " files")

  let changes = set.size(new_files) + set.size(removed_files)
  case changes {
    0 -> io.println("No changes detected")
    1 -> io.println("1 change detected")
    _ -> io.println(int.to_string(changes) <> " changes detected")
  }

  io.println("")
  io.println("Press Ctrl+C to exit. Next scan in 2 seconds...")

  // Sleep for 2 seconds before next scan
  process.sleep(2000)

  // Tail recursive call with updated accumulators
  file_watcher_loop(current_files, iteration + 1)
}

// Helper function to find all Gleam files
fn find_gleam_files() -> Result(set.Set(String), String) {
  case
    shellout.command(
      run: "find",
      with: [".", "-name", "*.gleam", "-type", "f"],
      in: ".",
      opt: [],
    )
  {
    Ok(output) -> {
      // Split output by lines and convert to a set
      string.split(output, on: "\n")
      |> list.filter(fn(line) { line != "" })
      |> set.from_list
      |> Ok
    }
    Error(#(code, message)) ->
      Error(
        "Command failed with code " <> int.to_string(code) <> ": " <> message,
      )
  }
}

fn cluster_example() {
  io.println("\n=== Cluster Example ===")

  // Check if we're already in distributed mode
  let current_node = cluster.get_node_name()
  let is_distributed = current_node != "nonode@nohost"

  io.println("Current node: " <> current_node)

  case is_distributed {
    True -> {
      io.println("Running in distributed mode")
    }
    False -> {
      io.println(
        "Not running in distributed mode. Attempting to start distributed system...",
      )

      // Prompt for node name
      io.print("Enter node name (e.g., node1): ")
      let node_name = case erlang.get_line("") {
        Ok(line) -> string.trim(line)
        Error(_) -> "node1"
      }

      // Prompt for cookie
      io.print("Enter cookie for authentication (e.g., mycookie): ")
      let cookie = case erlang.get_line("") {
        Ok(line) -> string.trim(line)
        Error(_) -> "mycookie"
      }

      // Start distributed system with cookie
      case cluster.start_distributed(node_name, cookie) {
        Ok(_) -> {
          io.println(
            "Successfully started distributed system as: "
            <> cluster.get_node_name(),
          )
          io.println("Cookie set to: " <> cookie)
        }
        Error(e) -> {
          io.println("Failed to start distributed system: " <> e)
        }
      }
    }
  }

  // Show current cluster status
  show_cluster_status()

  // Interactive loop for cluster management
  cluster_interactive_loop()
}

// Helper function to display current cluster status
fn show_cluster_status() {
  io.println("\n=== Cluster Status ===")
  io.println("Local node: " <> cluster.local_node())

  case cluster.is_clustered() {
    True -> {
      io.println("\nConnected nodes:")
      list.each(cluster.list_nodes(), fn(node) { io.println("  - " <> node) })

      io.println("\nKhepri cluster members:")
      case cluster.list_members() {
        Ok(members) -> {
          case list.length(members) {
            0 -> io.println("  No members (not part of a Khepri cluster)")
            _ -> {
              list.each(members, fn(member) { io.println("  - " <> member) })
            }
          }
        }
        Error(e) -> io.println("  Error getting members: " <> e)
      }
    }
    False -> io.println("\nThis node is not connected to any other nodes")
  }
}

fn cluster_interactive_loop() {
  io.println("\n=== Cluster Management (Press Ctrl+C to exit) ===")
  io.println("Available commands:")
  io.println("  1: Refresh status")
  io.println("  2: Connect to a node")
  io.println("  3: Join Khepri cluster")
  io.println("  4: Leave Khepri cluster")
  io.println("  5: Ping a node")
  io.println("  0: Exit")

  let input = case erlang.get_line("Enter command: ") {
    Ok(line) -> string.trim(line)
    Error(_) -> ""
  }

  case input {
    "1" -> {
      show_cluster_status()
      cluster_interactive_loop()
    }
    "2" -> {
      let node = case
        erlang.get_line("Enter node name (e.g., node2@localhost): ")
      {
        Ok(line) -> string.trim(line)
        Error(_) -> ""
      }

      case node {
        "" -> {
          io.println("Invalid node name")
          cluster_interactive_loop()
        }
        _ -> {
          case cluster.connect(node) {
            Ok(_) -> io.println("Successfully connected to " <> node)
            Error(e) -> io.println("Failed to connect: " <> e)
          }
          cluster_interactive_loop()
        }
      }
    }
    "3" -> {
      let node = case erlang.get_line("Enter node name to join cluster: ") {
        Ok(line) -> string.trim(line)
        Error(_) -> ""
      }

      case node {
        "" -> {
          io.println("Invalid node name")
          cluster_interactive_loop()
        }
        _ -> {
          case cluster.join_cluster(node) {
            Ok(_) ->
              io.println("Successfully joined Khepri cluster via " <> node)
            Error(e) -> io.println("Failed to join cluster: " <> e)
          }
          cluster_interactive_loop()
        }
      }
    }
    "4" -> {
      case cluster.leave_cluster() {
        Ok(_) -> io.println("Successfully left Khepri cluster")
        Error(e) -> io.println("Failed to leave cluster: " <> e)
      }
      cluster_interactive_loop()
    }
    "5" -> {
      let node = case erlang.get_line("Enter node name to ping: ") {
        Ok(line) -> string.trim(line)
        Error(_) -> ""
      }

      case node {
        "" -> {
          io.println("Invalid node name")
          cluster_interactive_loop()
        }
        _ -> {
          case cluster.ping_node(node) {
            Ok(_) -> io.println("Node " <> node <> " is reachable")
            Error(e) ->
              io.println("Node " <> node <> " is not reachable: " <> e)
          }
          cluster_interactive_loop()
        }
      }
    }
    "0" -> {
      io.println("Exiting cluster management")
      Nil
    }
    _ -> {
      io.println("Unknown command")
      cluster_interactive_loop()
    }
  }
}
