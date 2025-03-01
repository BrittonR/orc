import gleam/io
import gleam/list
import gleam/result
import gleam/string

/// Represents a cluster operation result
pub type ClusterResult =
  Result(Nil, String)

/// Connect to a remote Erlang node
/// 
/// Takes a node name in the format "name@host"
pub fn connect(remote_node: String) -> ClusterResult {
  case string.contains(remote_node, "@") {
    False ->
      Error(
        "Invalid node name: " <> remote_node <> " (expected format: name@host)",
      )
    True -> erlang_connect(remote_node)
  }
}

/// Set the Erlang cookie for distributed communication
///
/// All nodes in a cluster must share the same cookie value
// pub fn set_cookie(cookie: String) -> ClusterResult {
//   erlang_set_cookie(cookie)
// }

@external(erlang, "khepri_cluster_compat", "join")
fn join_khepri_cluster(remote_node: String) -> ClusterResult

@external(erlang, "khepri_cluster_compat", "leave")
fn leave_khepri_cluster() -> ClusterResult

/// Join a Khepri cluster by connecting to an existing member
pub fn join_cluster(remote_node: String) -> ClusterResult {
  // First connect to the node
  case connect(remote_node) {
    Error(e) -> Error(e)
    Ok(_) -> {
      // Call the Erlang function to join the cluster
      join_khepri_cluster(remote_node)
    }
  }
}

/// Leave the current Khepri cluster
pub fn leave_cluster() -> ClusterResult {
  leave_khepri_cluster()
}

/// List all nodes in the current cluster
// pub fn list_nodes() -> List(String) {
//   erlang_nodes()
// }

/// Get the members of the Khepri cluster (may differ from connected nodes)
pub fn list_members() -> Result(List(String), String) {
  khepri_members()
}

/// Get cluster status information
pub fn status() -> Result(String, String) {
  khepri_status()
}

/// Check if the local node is part of a cluster
pub fn is_clustered() -> Bool {
  list.length(erlang_nodes()) > 0
}

/// Get the name of the local node
pub fn local_node() -> String {
  erlang_node()
}

/// Check if a node is connected to the cluster
pub fn is_node_connected(node: String) -> Bool {
  erlang_is_connected(node)
}

/// Ping a node to check if it's reachable
pub fn ping_node(node: String) -> ClusterResult {
  erlang_ping(node)
}

/// Internal function to connect to an Erlang node
@external(erlang, "erlang_node", "connect")
fn erlang_connect(remote_node: String) -> ClusterResult

/// Internal function to set the Erlang cookie
@external(erlang, "erlang_node", "set_cookie")
fn erlang_set_cookie(cookie: String) -> ClusterResult

/// Internal function to get list of connected nodes
@external(erlang, "erlang_node", "nodes")
fn erlang_nodes() -> List(String)

/// Internal function to get local node name
@external(erlang, "erlang_node", "node")
fn erlang_node() -> String

/// Internal function to ping a node
@external(erlang, "erlang_node", "ping")
fn erlang_ping(node: String) -> ClusterResult

/// Internal function to check if a node is connected
@external(erlang, "erlang_node", "is_connected")
fn erlang_is_connected(node: String) -> Bool

/// Internal function to join a Khepri cluster
@external(erlang, "khepri_cluster", "join")
fn join_khepri_cluster(remote_node: String) -> ClusterResult

/// Internal function to leave a Khepri cluster
@external(erlang, "khepri_cluster", "leave")
fn leave_khepri_cluster() -> ClusterResult

/// Internal function to get Khepri cluster members
@external(erlang, "khepri_cluster", "members")
fn khepri_members() -> Result(List(String), String)

/// Internal function to get Khepri cluster status
@external(erlang, "khepri_cluster", "status")
fn khepri_status() -> Result(String, String)

/// Start the distributed Erlang system with a cookie
pub fn start_distributed(node_name: String, cookie: String) -> ClusterResult {
  orc_dist_start_distributed(node_name, cookie)
}

/// Get the current node name
pub fn get_node_name() -> String {
  orc_dist_get_node_name()
}

/// List connected nodes
pub fn list_nodes() -> List(String) {
  orc_dist_list_nodes()
}

@external(erlang, "orc_dist", "start_distributed")
fn orc_dist_start_distributed(
  node_name: String,
  cookie: String,
) -> ClusterResult

@external(erlang, "orc_dist", "get_node_name")
fn orc_dist_get_node_name() -> String

@external(erlang, "orc_dist", "list_nodes")
fn orc_dist_list_nodes() -> List(String)
