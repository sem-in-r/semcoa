# Posthoc analysis of groups from dtree
# TODO: Write tests for all these functions

group_scores <- function(group_id, coa) {
  coa$pls_model$construct_scores[unlist(coa$dtree$deviant_groups[as.character(group_id)]), ]
}

group_rules <- function(group_id, dtree) {
  utils::capture.output(
    rules <- unlist(unname(
      rpart::path.rpart(dtree$tree, nodes = group_id)
    ))
  )
  
  return(rules)
}

is_leaf <- function(frame) {
  frame$var == "<leaf>"
}

odd <- function(num) { num %% 2 == 1 }

#' Reports all competing rules for a node split
#' Example: (given UTAUT deviant group nodes: 4, 10, 12, 119, 31)
#'   competes(40, utaut_overfit$dtree)
competes <- function(node_id, dtree) {
  if (node_id == 1) stop("No splits before root (node 1) of tree")
  
  tree <- dtree$tree
  frame <- tree$frame
  
  search_node <- ifelse(odd(node_id), node_id - 1, node_id)
  frame_row <- match(search_node, row.names(frame))
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
