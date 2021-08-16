#' @export
plot_pd <- function(coa_analysis) {
  group_id <- 2:(length(coa_analysis$dtree$deviant_groups)+1)
  n <- nrow(coa_analysis$pls_model$data)
  group_tally_matrix <- mapply(tally, coa_analysis$dtree$deviant_groups, group_id, n=n)
  group_tally <- rowSums(group_tally_matrix)
  group_tally[coa_analysis$dtree$unique_deviants] <- 1 #length(coa_analysis$dtree$deviant_groups)
  # group_tally <- group_tally + 1
  pd_groups <- data.frame(pd=coa_analysis$predictions$PD, group=group_tally)
  
  sorted_pd_groups <- pd_groups[order(pd_groups$pd),]
  sorted_pd_groups$order <- 1:nrow(sorted_pd_groups)
  
  pd_min <- min(coa_analysis$predictions$PD)
  pd_max <- max(coa_analysis$predictions$PD)
  non_deviants <- subset(sorted_pd_groups, group == 0)
  deviants <- subset(sorted_pd_groups, group != 0)
  deviants$name <- ""
  is_unique <- deviants$group == 1
  is_group <- !is_unique
  deviants[is_unique, ]$name <- "*"
  deviants[is_group, ]$name <- toupper(letters[deviants[is_group, ]$group - 1])
  
  dev_quantiles <- quantile(coa_analysis$predictions$PD, probs = coa_analysis$deviance_bounds)
  
  suppressMessages(library(maptools))
  old_palette <- palette()
  palette(palette.colors(palette="Classic Tableau"))
  
  plot(NULL, NULL, xlim=c(1,n), ylim=c(pd_min, pd_max), frame.plot=FALSE, xlab="Cases", ylab="Predictive Deviance")
  abline(h=dev_quantiles, col="darkgray")
  points(non_deviants$order, non_deviants$pd, pch=19, col="lightgray")
  
  # points(deviants$order, deviants$pd, pch=21, col=deviants$group)
  
  points(deviants$order, deviants$pd, pch=19, col=deviants$group)
  text(x=deviants[is_unique, ]$order, y=deviants[is_unique, ]$pd, label=deviants[is_unique, ]$name, col="white")
  pointLabel(x=deviants[is_group, ]$order, y=deviants[is_group, ]$pd, label=deviants[is_group, ]$name, col=deviants[is_group, ]$group, cex=0.8)
  
  palette(old_palette)
}

tally <- function(deviant_cases, name, n) {
  groups <- rep(0, n)
  groups[deviant_cases] <- name
  groups
}

#' @export
plot_group_scores <- function(coa, remove=NULL) {
  group_scores <- semcoa:::all_groups_avg_scores(coa)
  if (!is.null(remove)) {
    group_scores <- group_scores[!rownames(group_scores) %in% remove,]
  }
  num_groups <- nrow(group_scores)
  
  old_palette <- palette()
  palette(palette.colors(palette="Classic Tableau"))
  
  # Plot 50% zone
  matplot(group_scores, type = c("b"), pch=colnames(group_scores), ylim=c(-3.5,3.5), 
          lty="solid", xaxt="n", bty="n", ylab="Average Construct Score")
  polygon(x = c(0, num_groups+1, num_groups+1, 0), 
          y = c(1.15, 1.15, -1.15, -1.15), 
          col = "gray97", border=NA)
  polygon(x = c(0, num_groups+1, num_groups+1, 0), 
          y = c(0.67, 0.67, -0.67, -0.67), 
          col = "gray92", border=NA)
  
  # Plot group scores
  matlines(group_scores, type = c("b"), pch=colnames(group_scores), ylim=c(-3.5,3.5), 
          lty="solid", xaxt="n", bty="n", ylab="Average Construct Score")
  axis(1, at=seq(1,nrow(group_scores)), labels=rownames(group_scores))
  axis(2, at=seq(-3,3), labels=seq(-3,3))
  
  palette(old_palette)
}

# matplot(group_scores[-c(8,9,10),], type = c("b"),pch=colnames(group_scores),col = 1:4, ylim=c(-3.5,3.5), xaxt="n")
# axis(1, at=1:8, labels=rownames(group_scores[-c(8,9,10),]))
