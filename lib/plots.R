plot_pd <- function(coa_analysis) {
  group_id <- 2:(length(coa_analysis$dtree$deviant_groups)+1)
  n <- nrow(coa_analysis$pls_model$data)
  group_tally_matrix <- mapply(tally, coa_analysis$dtree$deviant_groups, group_id, n=n)
  group_tally <- rowSums(group_tally_matrix)
  group_tally[coa_analysis$dtree$unique_deviants] <- 1 #length(coa_analysis$dtree$deviant_groups)
  # group_tally <- group_tally + 1
  pd_groups <- data.frame(pd=coa_analysis$pd$PD, group=group_tally)
  
  sorted_pd_groups <- pd_groups[order(pd_groups$pd),]
  sorted_pd_groups$order <- 1:nrow(sorted_pd_groups)
  
  pd_min <- min(coa_analysis$pd$PD)
  pd_max <- max(coa_analysis$pd$PD)
  non_deviants <- subset(sorted_pd_groups, group == 0)
  deviants <- subset(sorted_pd_groups, group != 0)
  deviants$name <- ""
  is_unique <- deviants$group == 1
  is_group <- !is_unique
  deviants[is_unique, ]$name <- "*"
  deviants[is_group, ]$name <- letters[deviants[is_group, ]$group - 1]
  
  dev_quantiles <- quantile(coa_analysis$pd$PD, probs = coa_analysis$deviance_bounds)
  
  library(maptools)
  old_palette <- palette()
  palette(palette.colors(palette="Classic Tableau"))
  
  plot(NULL, NULL, xlim=c(1,n), ylim=c(pd_min, pd_max), frame.plot=FALSE, xlab="Cases", ylab="Predictive Deviance")
  abline(h=dev_quantiles, col="darkgray")
  points(non_deviants$order, non_deviants$pd, pch=19, col="lightgray")
  
  # points(deviants$order, deviants$pd, pch=21, col=deviants$group)
  
  points(deviants$order, deviants$pd, pch=19, col=deviants$group)
  text(x=deviants[is_unique, ]$order, y=deviants[is_unique, ]$pd, label=deviants[is_unique, ]$name, col="white")
  pointLabel(x=deviants[is_group, ]$order, y=deviants[is_group, ]$pd, label=deviants[is_group, ]$name, col=deviants[is_group, ]$group, cex=0.5)
  
  palette(old_palette)
}

tally <- function(deviant_cases, name, n) {
  groups <- rep(0, n)
  groups[deviant_cases] <- name
  groups
}