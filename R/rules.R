# Identifying group split criteria from dtree
# TODO: Write tests for all these functions

#' @export
group_scores <- function(group_id, coa) {
  coa$pls_model$construct_scores[unlist(coa$dtree$deviant_groups[as.character(group_id)]), ]
}

#' @export
group_rules <- function(group_id, dtree) {
  group_node_id <- as.numeric(group_id)
  node_path <- path_to(group_node_id)[-1]
  splits <- data.frame(
    do.call(rbind, lapply(node_path, split_criteria, tree=dtree$tree)),
    row.names=NULL)
  
  splits_df <- data.frame(
    var       = splits$var, 
    sign      = ifelse(splits$ncat > 0, ">=", "< "),
    value     = splits$index
  )
  
  consolidate_all_rules(splits_df)
}

#' @export
print.group_criteria <- function(criteria) {
  # cat(paste(criteria, collapse="\n"))
  rows <- apply(criteria, 1, print_criteria_row)
  cat(paste(rows, collapse="\n"))
}

print_criteria_row <- function(criteria_row, digits=2) {
  gte <- ifelse(is.na(criteria_row$gte), "", 
                paste(fmt_value(criteria_row$gte, digits), " <= ", sep=""))
  lt <- ifelse(is.na(criteria_row$lt), "", 
               paste(" < ", fmt_value(criteria_row$lt, digits), sep=""))
  paste(gte, criteria_row$construct, lt, sep="")
}

fmt_value <- function(value, digits=2) {
  ifelse(is.na(value), NA,
         format(round(value, digits=digits), nsmall = digits))
}

consolidate_all_rules <- function(splits_df) {
  vars <- unique(splits_df$var)
  criteria <- as.data.frame(t(sapply(vars, consolidate_rule, splits_df)))
  
  rownames(criteria) <- NULL
  class(criteria) <- c("group_criteria", class(criteria))
  criteria
}

#' Consolidates rules of a single criterion
consolidate_rule <- function(construct, splits_df) {
  split_rows <- splits_df[splits_df$var == construct, ]
  
  gt_rule <- split_rows[split_rows$sign == ">=", ]
  if(nrow(gt_rule) > 1) gt_rule <- gt_rule[gt_rule$value == max(gt_rule$value), ]
  lt_rule <- split_rows[split_rows$sign == "< ", ]
  if(nrow(lt_rule) > 1) lt_rule <- lt_rule[lt_rule$value == min(lt_rule$value), ]

  data.frame(construct = construct,
             gte = ifelse(nrow(gt_rule) == 0, NA, gt_rule$value),
             lt  = ifelse(nrow(lt_rule) == 0, NA, lt_rule$value))
}

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

is_leaf <- function(frame) {
  frame$var == "<leaf>"
}

odd <- function(num) { num %% 2 == 1 }

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
