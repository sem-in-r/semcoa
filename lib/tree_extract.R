# All parents of a node back to the root
parents <- function(node_id) {
  if (node_id[1] != 1)
    c(Recall(if (node_id %% 2 == 0L) node_id / 2 else (node_id - 1) / 2), node_id)
  else node_id
}

leaf_paths <- function(leaf_ids) {
  all_paths <- lapply(as.numeric(leaf_ids), parents)
  names(all_paths) <- leaf_ids
  all_paths
}

# Reports which leaves include a node in its ancestry
# node_paths - list of all paths to consider (usually fro leaf_paths() function)
# node_id - id of node in question
leaves_from_node <- function(node_id, node_paths) {
  leaves <- which(sapply(node_paths, function(x) { node_id %in% x }))
  names(leaves)
}

# Reports which leaves belong to a list of nodes
# parent_ids - vector of node ids (character)
# node_paths - list of node paths (typically from leaf_paths() function)
leaves_from_nodes <- function(parent_ids, node_paths) {
  paths_list <- lapply(parent_ids, leaves_from_node, node_paths = node_paths)
  names(paths_list) <- parent_ids
  paths_list
}

# Returns index of data cases that match nodes in tree frame
# tree - rpart tree object
# logical_frame - logical vector of which nodes in tree frame we want (e.g., use a predicate tree function)
cases <- function(tree, logical_frame) {
  which(tree$where %in% which(logical_frame))
}

# OTHER UTILITIES
#
# Get splitting criteria of a node
# path.rpart(tree, 4)