library(seminr)
library(rpart)

#' COA Analysis
#' Params:
#'   pls_model - estimated seminr PLS model (seminr_model)
#'   focal_construct - name of focal construct to analyze (character string)
#'   deviance_bounds - two values of upper and lower deviance threshholds (numeric vector)
#'   ... - other optional parameters for prediction(), dtree(), or unstable() steps
#'         e.g., params for unstable()
#' 
#' @export
coa <- function(pls_model, focal_construct, deviance_bounds = c(0.025, 0.975), ...) {
  predictions <- prediction_metrics(pls_model, focal_construct, ...)
  dtree <- deviance_tree(predictions, deviance_bounds)
  deviance_tree <- grow_deviance_tree(dtree, predictions)
  unstable <- unstable_params(pls_model, dtree, ...)
  
  analysis <- list(
    pls_model = pls_model,
    focal_construct = focal_construct,
    deviance_bounds = deviance_bounds,
    deviance_tree = deviance_tree,
    predictions = predictions,
    dtree = dtree,
    unstable = unstable
  )
  
  class(analysis) <- c("coa", class(analysis))
  analysis
}
