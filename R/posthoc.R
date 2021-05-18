# Posthoc analysis of groups from dtree
# TODO: Write tests for all these functions

#' @export
group_scores <- function(group_id, coa) {
  coa$pls_model$construct_scores[unlist(coa$dtree$deviant_groups[as.character(group_id)]), ]
}

#' @export
group_rules <- function(group_id, dtree) {
  splits <- node_splits(group_id, dtree)
  data.frame(
    var       = splits$var, 
    sign      = ifelse(splits$ncat > 0, ">=", "< "),
    value     = splits$index
  )
}

is_leaf <- function(frame) {
  frame$var == "<leaf>"
}

odd <- function(num) { num %% 2 == 1 }

#' Reports all competing rules for a node split
#' Example: (given UTAUT deviant group nodes: 4, 40, 12, 119, 31)
#'   competes(40, utaut_overfit$dtree)
#' 
#' @export
competes <- function(node_id, dtree) {
  if (node_id == 1) stop("No splits before root (node 1) of tree")
  
  tree <- dtree$tree
  frame <- tree$frame
  
  search_node <- ifelse(odd(node_id), node_id - 1, node_id)
  frame_row <- match(search_node/2, row.names(frame))
  index <- cumsum(c(1, frame$ncompete + frame$nsurrogate + !is_leaf(frame)))
  
  start <- index[frame_row-1]
  end <- start + frame$ncompete[frame_row]
  splits <- as.data.frame(tree$splits[start:end, ])
  
  if(odd(node_id)) { splits$ncat <- splits$ncat * -1 }
  data.frame(
    criterion = row.names(splits), 
    sign      = ifelse(splits$ncat > 0, ">=", "< "),
    value     = splits$index, 
    improve   = splits$improve,
    row.names = NULL
  )
}

node_splits <- function(node_id, dtree) {
  node_path <- path_to(node_id)[-1]
  data.frame(
    do.call(rbind, lapply(node_path, split_criteria, tree=dtree$tree)),
    row.names=NULL)
}

split_criteria <- function(node_id, tree) {
  # TODO: move frame, index, all_splits out of function so that lappy
  #       using split_criteria doesn't recompute these every time
  frame <- tree$frame
  index <- cumsum(c(1, frame$ncompete + frame$nsurrogate + !is_leaf(frame)))
  all_splits <- cbind(var=rownames(tree$splits), 
                      data.frame(tree$splits[, c("ncat", "index")], row.names=NULL))

  search_node <- ifelse(odd(node_id), node_id - 1, node_id)
  frame_row <- match(search_node, row.names(frame))

  split_row <- index[frame_row-1]
  split <- all_splits[split_row, ]
  if(odd(node_id)) { split$ncat <- split$ncat * -1 }
  split
}
