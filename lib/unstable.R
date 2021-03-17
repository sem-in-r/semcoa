path_diffs <- function(remove_cases, pls_model) {
  cat(".")
  no_dgroup_data <- pls_model$data[-remove_cases,]
  utils::capture.output(
    no_dgroup_model <- estimate_pls(
      data=no_dgroup_data, 
      measurement_model = pls_model$measurement_model, 
      structural_model = pls_model$smMatrix
    )
  )
  pls_model$path_coef - no_dgroup_model$path_coef
}
