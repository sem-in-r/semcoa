#' Find segments from unpruned deviance tree
#' @param tree An `rpart` based deviance tree
#' @export
segments <- function(tree) {
  frame <- tree$frame
  names <- row.names(frame)
  
  is_leaf <- frame$var == "<leaf>"
  leaves <- frame[is_leaf, ]
  leaf_ids <- row.names(leaves)
  paths_to_leaves <- leaf_paths(leaf_ids)
  
  parents <- frame[!is_leaf, ]
  parent_ids <- row.names(parents)
  
  parent_cases <- sapply(parent_ids, cases_from_node, frame=frame, paths_to_leaves)
  # rerun model without each potential segment
}