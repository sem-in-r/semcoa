plot_pd <- function(coa_analysis) {
  group_names <- 1:length(coa_analysis$dtree$deviant_groups)
  n <- nrow(coa_analysis$pls_model$data)
  group_tally_matrix <- mapply(tally, coa_analysis$dtree$deviant_groups, group_names, n=n)
  group_tally <- rowSums(group_tally_matrix)
  group_tally[coa_analysis$dtree$unique_deviants] <- length(group_names)+1
  group_tally <- group_tally + 1
  pd_groups <- data.frame(pd=coa_analysis$pd$PD, group=group_tally)
  
  sorted_pd_groups <- pd_groups[order(pd_groups$pd),]
  plot(sorted_pd_groups$pd, pch=19, col=sorted_pd_groups$group)
}

tally <- function(devs, name, n) {
  groups <- rep(0, n)
  groups[devs] <- name
  groups
}