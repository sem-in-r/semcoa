# Nodes to take from root to a node
path_to <- function(node_id) {
  if (node_id[1] != 1)
    c(Recall(if (node_id %% 2 == 0L) node_id / 2 else (node_id - 1) / 2), node_id)
  else node_id
}

leaf_paths <- function(leaf_ids) {
  all_paths <- lapply(as.numeric(leaf_ids), path_to)
  names(all_paths) <- leaf_ids
  all_paths
}

# Reports which leaves include a node in its ancestry
# node_paths - list of all paths to consider (usually from leaf_paths() function)
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

# Given a set of node ids, returns parent ids, ignoring any decendants
# e.x., parent_ids: "4"  "40"  "81"  "12"  "24" "119" "239"  "31" (81, 24, 239 are children of others)
#          returns: "4"  "40"  "12" "119"  "31" (parents only)
# e.x., parent_ids: "2"  "12"  "24" "204"  "26"  "29" "117" "469"  "15"  "31"  "62" "124"  "63"
#          returns: "2"  "12" "26" "29" "15"
main_ancestors <- function(parent_ids) {
  ids <- as.integer(parent_ids)
  
  ## Eliminates only immediate descendents
  # possible_parents <- floor(ids / 2)
  # ancestor_ids <- ids[!(possible_parents %in% ids)]
  
  ## Eliminates all descendents
  ancestor_ids <- unique(sapply(ids, function(id) { 
    min(ids[ids %in% path_to(id)])
  }))
  as.character(ancestor_ids)
}

# OTHER UTILITIES
#
# Get splitting criteria of a node
# path.rpart(tree, 4)