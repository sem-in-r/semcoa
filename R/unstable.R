param_diffs <- function(remove_cases, pls_model, params="path_coef") {
  no_dgroup_data <- pls_model$data[-remove_cases,]
  suppressMessages(
    no_dgroup_model <- estimate_pls(
      data=no_dgroup_data, 
      measurement_model = pls_model$measurement_model, 
      structural_model = pls_model$smMatrix
    )
  )
  # pls_model[param][[1]] - no_dgroup_model[param][[1]]
  diffs <- lapply(params, function(param) {
    pls_model[param][[1]] - no_dgroup_model[param][[1]]
  })
  names(diffs) <- params
  diffs
}
