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

  # Move outcome construct to end
  outcome <- which(rownames(group_scores) == coa$focal_construct)
  constructs <- rownames(group_scores)
  group_scores <- rbind(group_scores[-outcome, ], group_scores[outcome, ])
  rownames(group_scores) <- c(constructs[-outcome], constructs[outcome])
  
  num_constructs <- nrow(group_scores)
  
  old_palette <- palette()
  palette(palette.colors(palette="Classic Tableau"))
  
  # Plot 50% zone
  matplot(group_scores, type = c("b"), pch=colnames(group_scores), ylim=c(-3.5,3.5), 
          lty="solid", xaxt="n", bty="n", ylab="Average Construct Score")
  polygon(x = c(0, num_constructs+1, num_constructs+1, 0), 
          y = c(1.15, 1.15, -1.15, -1.15), 
          col = "gray97", border=NA)
  polygon(x = c(0, num_constructs+1, num_constructs+1, 0), 
          y = c(0.67, 0.67, -0.67, -0.67), 
          col = "gray92", border=NA)
  
  # plot separator from outcome
  abline(v=num_constructs - 0.5, col="white", lwd=5)
  
  # Plot group scores
  matlines(group_scores, type = c("b"), pch=colnames(group_scores), ylim=c(-3.5,3.5), 
          lty="solid", xaxt="n", bty="n", ylab="Average Construct Score")
  axis(1, at=seq(1,num_constructs-1), labels=rownames(group_scores)[-outcome])
  axis(1, at=num_constructs, labels=coa$focal_construct, font=2)
  axis(2, at=seq(-3,3), labels=seq(-3,3))
  
  palette(old_palette)
}

#' @export
plot.deviance_tree <- function(deviance_tree, ...) {
  data.tree::SetGraphStyle(deviance_tree, splines="false", rankdir="TB", nodesep="2.0", fontname="helvetica")
  data.tree:::plot.Node(deviance_tree, ...)
}

# matplot(group_scores[-c(8,9,10),], type = c("b"),pch=colnames(group_scores),col = 1:4, ylim=c(-3.5,3.5), xaxt="n")
# axis(1, at=1:8, labels=rownames(group_scores[-c(8,9,10),]))

#' @export
plot_errors <- function(coa) {
  y_out  <- coa$predictions$predicted_score
  y_in   <- coa$predictions$fitted_score
  y_star <- coa$pls_model$construct_scores[, "BI"]
  scores <- data.frame(y_star, y_in, y_out)
  scores$n <- 1:nrow(scores)
  deviant_n <- unlist(utaut_overfit$dtree$deviant_groups)
  
  sorted_ys <- scores[order(scores$y_star),]
  sorted_ys$order <- 1:nrow(sorted_ys)
  
  plot(NULL, NULL, xlim=c(1,nrow(scores)), ylim=c(-3, 3), frame.plot=FALSE, xlab="Cases", ylab="Scores")
  
  # Plot actual* points and in-sample errors
  segments(sorted_ys$order, sorted_ys$y_star, sorted_ys$order, sorted_ys$y_in, col="lightgray", lwd=3)  
  points(sorted_ys$order, sorted_ys$y_star, pch=20, cex=0.5, col="darkgray")
  
  # Plot the worst in-sample errors
  sorted_ie <- sorted_ys[order(-abs(sorted_ys$y_in - sorted_ys$y_star)),][1:length(deviant_n),]
  segments(sorted_ie$order, sorted_ie$y_star, sorted_ie$order, sorted_ie$y_in, col="darkgray", lwd=3)  
  
  # Plot the out-of-sample errors
  segments(sorted_ys$order, sorted_ys$y_in, sorted_ys$order, sorted_ys$y_out, col="pink", lwd=3)  
  
  # Plot and label deviants  
  deviants <- sorted_ys[sorted_ys$n %in% deviant_n,]
  deviants$label <- names(deviant_n[match(deviants$n, deviant_n)])
  y_offset <- ifelse(deviants$y_star - deviants$y_in < 0, 1, -1) * 0.2
  
  segments(deviants$order, deviants$y_in, deviants$order, deviants$y_out, col="coral3", lwd=3)
  text(x=deviants$order, y=deviants$y_out + y_offset, labels = deviants$label, cex=0.5)
}

barplot_errors <- function(coa) {
  y_out  <- coa$predictions$predicted_score
  y_in   <- coa$predictions$fitted_score
  y_star <- coa$pls_model$construct_scores[, "BI"]
  
  scores <- data.frame(y_star, y_in, y_out,
                       fit_error = y_star - y_in, 
                       pred_error = y_star - y_out,
                       pd = y_in - y_out)
  scores <- scores[order(scores$pd), ]
  
  scores_matrix <- t(as.matrix(scores[, c("fit_error", "pred_error")]))
  
  barplot(scores_matrix, beside = TRUE, col = c("darkgray", "pink"), border=NA)
}
