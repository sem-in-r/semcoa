#' Find segments from unpruned deviance tree
#' @param coa A completed `coa` analysis
#' @export
model_segments <- function(coa) {
  frame <- coa$dtree$tree$frame
  names <- row.names(frame)
  
  is_leaf <- frame$var == "<leaf>"
  leaves <- frame[is_leaf, ]
  leaf_ids <- row.names(leaves)
  paths_to_leaves <- leaf_paths(leaf_ids)
  
  parents <- frame[!is_leaf, ]
  parent_ids <- row.names(parents)
  node_cases <- sapply(parent_ids, cases_from_node, frame=frame, paths_to_leaves)
  
  pls_model <- coa$pls_model
  segmented_coef <- lapply(node_cases[-1], estimate_subset, pls_model, params="path_coef")
  segmented_coef[[1]] <- estimate_pls(pls_model$data, pls_model$measurement_model, pls_model$smMatrix)

  list(
    node_cases = node_cases,
    segmented_coef = segmented_coef
  )
}

#' Computes correlation of two matrices of same dimension (from Matlab)
#' https://stackoverflow.com/questions/27343283/explaining-corr2-function-in-matlab
corr2 <- function(A, B) {
  A_mc <- A - mean(A)
  B_mc <- B - mean(B)
  
  sum(A_mc * B_mc) / sqrt(sum(A_mc^2) * sum(B_mc^2))
}

# segmented_coef["2"]
# corr2(segmented_coef["2"][[1]], utaut_model$path_coef)  # 0.59
#
# tail(segmented_coef, n=1)
# corr2(tail(segmented_coef, n=1)[[1]], utaut_model$path_coef)  # 0.98

#' Simulates random splitting of cases and reports path coefficient correlation
#' @params
#'   original_model - composed or estimated seminr pls model
#'   cases - vector of all cases (rows) from data to consider in splitting
#'   split - vector of size of each split.. e.g, `c(143, 73)`
#' @returns vector of average correlations between each split coefficients and original model coefficients
random_split_corrs <- function(original_model, node_cases, node_id) {
  original_data <- original_model$data[node_cases[[node_id]], ]
  node_id <- as.character(node_id)
  cases <- node_cases[[node_id]]
  n <- length(cases)
  a_split_node_id <- as.character(as.integer(node_id) * 2)
  a_split_size <- length(node_cases[[a_split_node_id]])
  a_indices <- sample(cases, size = a_split_size)
  b_indices <- setdiff(cases, a_indices)
  
  a_model <- suppressMessages(
    estimate_pls(data = original_data[a_indices, ], measurement_model = original_model$measurement_model, structural_model = original_model$smMatrix))
  b_model <- suppressMessages(
    estimate_pls(data = original_data[b_indices, ], measurement_model = original_model$measurement_model, structural_model = original_model$smMatrix))
  
  c(a_corr = corr2(a_model$path_coef, original_model$path_coef),
    b_corr = corr2(b_model$path_coef, original_model$path_coef))
}

segments_corr2 <- function(segment_a, segment_b, segmented_coef) {
  corr2(segmented_coef[[segment_a]], segmented_coef[[segment_b]])
}

# set.seed(23432423)
# full_split <- replicate(1000, random_split_corrs(utaut_model, parent_cases, "1"))
# 
# plot(density(full_split[1,]), xlim=c(0,1))
# lines(density(full_split[2,]), lty="dashed")
# 
# quantile(full_split[1,], c(0.01))
# quantile(full_split[2,], c(0.01))

# NEXT STEPS
# - Compare split segments against each other instead of parent
# - Refactor code to allow arbitrary segment comparisons (coefs)
# - Remove PDs before analysis and try to find bigger segments
# - Try on a bigger datasets (300-400+)
