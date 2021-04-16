library(seminr)
library(rpart)

source("tree_extract.R")
source("pls_predict.R")
source("unstable.R")
source("plots.R")
source("posthoc.R")

# COA Analysis
# Params:
#   pls_model - estimated seminr PLS model (seminr_model)
#   focal_construct - name of focal construct to analyze (character string)
#   deviance_bounds - two values of upper and lower deviance threshholds (numeric vector)
#   ... - other optional parameters for prediction(), dtree(), or unstable() steps
#         e.g., params for unstable()
coa <- function(pls_model, focal_construct, deviance_bounds = c(0.025, 0.975), ...) {
  predictions <- prediction_metrics(pls_model, focal_construct, ...)
  dtree <- deviance_tree(predictions, deviance_bounds)
  unstable <- unstable_params(pls_model, dtree, ...)
  
  analysis <- list(
    pls_model = pls_model,
    focal_construct = focal_construct,
    deviance_bounds = deviance_bounds,
    predictions = predictions,
    dtree = dtree,
    unstable = unstable
  )
  
  class(analysis) <- c("coa", class(analysis))
  analysis
}

prediction_metrics <- function(pls_model, focal_construct, ...) {
  # Run predict_pls
  cat("Computing predictive deviance\n")
  
  plspredict_model <- predict_pls2(pls_model,
                                   technique = predict_DA)
  
  fitted <- plspredict_model$composites$composite_in_sample[,focal_construct]
  predicted <- plspredict_model$composites$composite_out_of_sample[,focal_construct]
  actual_star <- pls_model$construct_scores[,focal_construct]
  IS_MSE <- mean((actual_star - fitted)^2)
  OOS_MSE <- mean((actual_star - predicted)^2)
  overfit_ratio <- (OOS_MSE - IS_MSE)/IS_MSE
  
  PD <- fitted - predicted 
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
  class(predictions) <- c("coa_deviance", class(predictions))
  predictions
}

deviance_tree <- function(predictions, deviance_bounds = c(0.025, 0.975), ...) {
  # Generate Deviance Tree
  cat("Generating Deviance Tree\n")
  
  tree <- rpart(
    PD ~ ., 
    data = predictions$pd_data,
    minsplit = 2,
    cp = 0
  )
  
  dev_interval <- quantile(predictions$PD, probs = deviance_bounds)

  nodes <- extract_nodes(tree$frame, dev_interval)

  # Sort the PD values for reporting
  sorted_PD <- sort(nodes$leaves$yval, decreasing = TRUE)
  class(sorted_PD) <- c("coa_sortedPD", class(sorted_PD))

  # Identify original cases from dataset
  deviants <- cases(tree, nodes$is_deviant_leaf)
  deviant_groups <- lapply(nodes$dev_parent_leaves, function(group) { cases(tree, nodes$names %in% group) })
  unique_deviants <- setdiff(deviants, unlist(deviant_groups))
  
  utils::capture.output(
    dev_group_rules <- path.rpart(tree, nodes$dev_ancestor_ids)
  )
  
  dtree <- list(
    tree = tree,
    sorted_PD = sorted_PD,
    deviant_groups = deviant_groups,
    unique_deviants = unique_deviants,
    deviant_nodes = nodes$deviants,
    dev_group_rules = dev_group_rules
  )
  class(dtree) <- c("coa_deviance_tree", class(dtree))
  dtree
}

#' Generates report of unstable parameters for a given pls_model and dtree, or a completed coa analysis
#' params:
#'   pls_model - an estimated seminr_model
#'   dtree - a deviance tree object
#'   params - parameter, or vector of parameters, to diff
#'
#' example: (given a seminr_model called utaut_model)
#'   predictions <- prediction_metrics(utaut_model, "BI")
#'   deviance_bounds = c(0.025, 0.975)
#'   dtree <- deviance_tree(pd, deviance_bounds)
#'   unstable <- unstable_params(
#'     pls_model, dtree, 
#'     params = c("path_coef", "outer_weights", "rSquared")
#'   )
#'   
unstable_params <- function(pls_model=NULL, dtree=NULL, analysis=NULL, params="path_coef", ...) {
  if (!is.null(analysis)) {
    pls_model <- analysis$pls_model
    dtree <- analysis$dtree
  }
  
  cat("Identifying Unstable Paths")
  group_param_diffs <- lapply(dtree$deviant_groups, param_diffs, pls_model = pls_model, params=params)
  unique_param_diffs <- lapply(dtree$unique_deviants, param_diffs, pls_model = pls_model, params=params)
  unstable <- list(
    group_diffs  = Map(list, group=dtree$deviant_groups, param_diffs=group_param_diffs),
    unique_diffs = Map(list, deviant=dtree$unique_deviants, param_diffs=unique_param_diffs)
  )
  cat("\n")
  class(unstable) <- c(class(unstable), "unstable_paths")
  return(unstable)
}
