grow_deviance_tree <- function(dtree) {
  groups_path <- lapply(dtree$group_roots, semcoa:::path_to)
  
  groups_path_df <- data.frame(
    pathString = sapply(groups_path, paste, collapse="/"),
    groupName  = names(dtree$group_roots),
    cases      = sapply(dtree$deviant_groups, paste, collapse=", "),
    size       = sapply(dtree$deviant_groups, length)
  )
  
  deviance_tree <- data.tree::as.Node(groups_path_df)
  
  # print(deviance_tree, "groupName", "cases", "size")
  
  groups_rules <- dtree$dev_group_rules
  # group_rules <- lapply(dtree$dev_group_rules, function(x) {x[-1]})
  
  label_edges <- function(ids, rules, deviance_tree) {
    mapply(label_edge, ids, rules, MoreArgs = list(deviance_tree=deviance_tree))
  }
  
  label_edge <- function(id, rule, deviance_tree) {
    # cat(id, rule, "\n")
    node <- data.tree::FindNode(deviance_tree, id)
    
    # if root node of deviant group
    if (is.null(node$children)) {
      data.tree::SetNodeStyle(node, label=node$groupName, inherit=FALSE,
                              color="gray87",
                              fontcolor="black", fontname="helvetica bold", fontsize=20)
    } else {
      data.tree::SetNodeStyle(node, color="gray87", fontcolor="white", fontname="helvetica")
    }
    
    data.tree::SetEdgeStyle(node, taillabel=paste("   ", rule), color="gray87")
  }
  
  node_rules <- mapply(function(nodes, rules) label_edges(nodes, rules, deviance_tree),
                       groups_path, groups_rules)
  
  class(deviance_tree) <- c("deviance_tree", class(deviance_tree))
  deviance_tree
}

#' @export
plot.deviance_tree <- function(deviance_tree, ...) {
  data.tree::SetGraphStyle(deviance_tree, splines="false", rankdir="TB", nodesep="2.0", fontname="helvetica")
  data.tree:::plot.Node(deviance_tree, ...)
}



# deviance_tree <- grow_deviance_tree(utaut_overfit$dtree)
# plot_deviance_tree(deviance_tree)

# dgrmr_dtree <- data.tree::ToDiagrammeRGraph(deviance_tree)
# 
# plot()
# 
# dot_dtree <- DiagrammeR::generate_dot(dgrmr_dtree)
# fileConn<-file("dtree.dot")
# writeLines(dot_dtree, fileConn)
# close(fileConn)