source("tree_extract.R")
source("pls_predict.R")

fit_rpart_tree_seminr <- function(pls_model, focal_construct, ...) {
  # Run predict_pls
  cat("Running PLSpredict to get predicted scores\n")
  plspredict_model <- predict_pls2(pls_model,
                                   technique = predict_DA)
  
  # Calculate prediction metrics
  cat("Calculating prediction metrics\n")
  fitted <- plspredict_model$composites$composite_in_sample[,focal_construct]
  predicted <- plspredict_model$composites$composite_out_of_sample[,focal_construct]
  actual_star <- pls_model$construct_scores[,focal_construct]
  IS_MSE <- mean((actual_star - fitted)^2)
  OOS_MSE <- mean((actual_star - predicted)^2)
  overfit_ratio <- (OOS_MSE - IS_MSE)/IS_MSE
  
  PD <- predicted - fitted 
  cs_data <- cbind(as.data.frame(pls_model$construct_scores),PD)
  
  # Generate Deviance Tree
  cat("Generating Deviance Tree\n")
  cstree <- rpart(
    PD ~ ., 
    data = cs_data, 
    minsplit = 2,
    ...
  )
  return(list(
    tree = cstree,
    plspredict_model = plspredict_model,
    IS_MSE = IS_MSE,
    OOS_MSE = OOS_MSE,
    overfit_ratio = overfit_ratio,
    fitted_score = fitted,
    predicted_score = predicted,
    PD = PD
  ))
}
