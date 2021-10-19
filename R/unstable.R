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
#' @export
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

param_diffs <- function(remove_cases, pls_model, params="path_coef") {
  keep_cases <- seq(1, nrow(pls_model$data))[-remove_cases]
  subset <- estimate_subset(keep_cases, pls_model, params)
  
  diffs <- lapply(params, function(param) {
    subset[param][[1]] - pls_model[param][[1]]
  })
  names(diffs) <- params
  diffs
}

estimate_subset <- function(subset_cases, pls_model, params="path_coef") {
  dgroup_data <- pls_model$data[subset_cases,]
  suppressMessages(
    dgroup_model <- estimate_pls(
      data=dgroup_data, 
      measurement_model = pls_model$measurement_model, 
      structural_model = pls_model$smMatrix
    )
  )
  
  report_params <- lapply(params, function(param) {
    dgroup_model[param][[1]]
  })
  names(report_params) <- params
  report_params
}
