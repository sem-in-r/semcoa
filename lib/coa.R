source("tree_extract.R")
source("pls_predict.R")

coa <- function(pls_model, focal_construct, ...) {
  predictions <- prediction_metrics(pls_model, focal_construct, ...)
  deviants <- deviance_tree(predictions)
}

prediction_metrics <- function(pls_model, focal_construct) {
  # Run predict_pls
  cat("Running PLSpredict to get predicted scores\n")
  
  plspredict_model <- predict_pls2(pls_model,
                                   technique = predict_DA)
  
  fitted <- plspredict_model$composites$composite_in_sample[,focal_construct]
  predicted <- plspredict_model$composites$composite_out_of_sample[,focal_construct]
  actual_star <- pls_model$construct_scores[,focal_construct]
  IS_MSE <- mean((actual_star - fitted)^2)
  OOS_MSE <- mean((actual_star - predicted)^2)
  overfit_ratio <- (OOS_MSE - IS_MSE)/IS_MSE
  
  PD <- predicted - fitted 
  pd_data <- cbind(as.data.frame(pls_model$construct_scores),PD)
  predictions <- list(
    plspredict_model = plspredict_model,
    IS_MSE = IS_MSE,
    OOS_MSE = OOS_MSE,
    overfit_ratio = overfit_ratio,
    fitted_score = fitted,
    predicted_score = predicted,
    PD = PD,
    pd_data = pd_data
  )
  class(predictions) <- c("coa_predictions", class(predictions))
  predictions
}

deviance_tree <- function(predictions) {
  # Generate Deviance Tree
  cat("Generating Deviance Tree\n")
  
  tree <- rpart(
    PD ~ ., 
    data = predictions$pd_data,
    minsplit = 2,
    cp = 0.0000000001
  )

    # plot(tree)
  # fancyRpartPlot(predictions$tree, caption = NULL)
  
  dev_interval <- quantile(predictions$PD, probs = c(0.025, 0.975))
  #     2.5%    97.5% 
  #   -0.0906  0.0744
  
  # Tree frame predicates
  nodes <- row.names(tree$frame)
  is_leaf <- tree$frame$var == "<leaf>"
  is_left_deviant <- tree$frame$yval < dev_interval["2.5%"]
  is_right_deviant <- tree$frame$yval > dev_interval["97.5%"]
  is_deviant <- is_left_deviant | is_right_deviant
  is_deviant_leaf <- is_deviant & is_leaf
  is_deviant_parent <- is_deviant & !is_leaf
  
  # Extract deviant leaves from tree frame
  deviants <- cases(tree, is_deviant_leaf)
  
  leaves <- tree$frame[is_leaf, ]
  sorted_PD <- sort(leaves$yval, decreasing = TRUE)
  class(sorted_PD) <- c("coa_sortedPD", class(sorted_PD))
  leaf_ids <- row.names(leaves)
  all_paths <- leaf_paths(leaf_ids)
  # plot(sort(leaves$yval, decreasing = TRUE), pch=19, col="cornflowerblue")
  # abline(h=dev_interval)
  
  # 5% most deviant node and leaves
  dev_nodes <- tree$frame[is_deviant,]
  dev_parents <- tree$frame[is_deviant_parent, ]
  dev_parent_ids <- row.names(dev_parents)
  dev_parent_leaves <- leaves_from_nodes(dev_parent_ids, all_paths)
  
  deviant_groups <- lapply(dev_parent_leaves, function(group) { cases(tree, nodes %in% group) })
  unique_deviants <- setdiff(unlist(deviant_groups), deviants)
  
  deviants <- list(
    tree = tree,
    sorted_PD = sorted_PD,
    deviant_groups = deviant_groups,
    unique_deviants = unique_deviants
  )
  class(deviants) <- c("coa_deviance_tree", class(deviants))
  deviants
}
