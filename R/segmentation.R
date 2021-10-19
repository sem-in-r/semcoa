#' Find segments from unpruned deviance tree
#' @param coa A completed `coa` analysis
#' @export
model_segments <- function(coa) {
  tree <- coa$dtree$tree
  frame <- tree$frame
  names <- row.names(frame)
  
  is_leaf <- frame$var == "<leaf>"
  leaves <- frame[is_leaf, ]
  leaf_ids <- row.names(leaves)
  paths_to_leaves <- leaf_paths(leaf_ids)
  
  parents <- frame[!is_leaf, ]
  parent_ids <- row.names(parents)
  
  node_cases <- sapply(parent_ids, cases_from_node, tree=tree, paths_to_leaves)
  
  pls_model <- coa$pls_model

  segmented_estimates <- lapply(
    node_cases, 
    function(cases){
     tryCatch({estimate_subset(cases, pls_model, params="path_coef")},
              error = function(err) { NA }
     )})
  
  list(
    node_cases = node_cases,
    segmented_estimates = segmented_estimates,
    viable = segment_viability(segmented_estimates)
  )
}

# coef_list <- segments$segmented_coef
# coef_list <- list("1"=T, "2"=T, "4"=T, "8"=T, "9"=NA, "5"=NA, "10"=NA, "11"=NA, "3"=T, "6"=T, "12"=NA, "13"=NA, "7"=NA, "14"=NA, "15"=NA)
# coef_list <- list("1"=T, "2"=T, "4"=T, "8"=T, "9"=NA, "5"=NA, "10"=NA, "11"=NA, "3"=T, "6"=T, "12"=NA, "13"=NA, "7"=T, "14"=NA, "15"=NA)
segment_viability <- function(coef_list, idL=1) {
  children_ids <- c(idL*2, idL*2 + 1)

  node <- coef_list[[as.character(idL)]]
  if(is.na(node) || is.null(node)) { return(NA) }
  
  left  <- Recall(coef_list, children_ids[1])
  if(any(is.na(left))) { return(idL) }
  
  right <- Recall(coef_list, children_ids[2])
  if(any(is.na(right))) { return(idL) }

  return(c(left, right))
  # potential <- as.character(c(left=left, right=right))
  # if(segments_corr2(potential['left'], potential['right']))
  # 
}

#' Computes correlation of two matrices of same dimension (from Matlab)
#' https://stackoverflow.com/questions/27343283/explaining-corr2-function-in-matlab
corr2 <- function(A, B) {
  A_mc <- A - mean(A)
  B_mc <- B - mean(B)
  
  sum(A_mc * B_mc) / sqrt(sum(A_mc^2) * sum(B_mc^2))
}

#' Simulates random splitting of cases and reports path coefficient correlation
#' @params
#'   original_model - composed or estimated seminr pls model
#'   node_cases - list of cases for each node in dtree
#'   node_id - character id of node to split
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
    b_corr = corr2(b_model$path_coef, original_model$path_coef),
    ab_corr = corr2(a_model$path_coef, b_model$path_coef))
}

segments_corr2 <- function(segment_a, segment_b, segmented_estimates) {
  corr2(segmented_estimates[[segment_a]]$path_coef, segmented_estimates[[segment_b]]$path_coef)
}


# NEXT STEPS
# - move utaut segmentation analysis to segmentation notebook
# - Compare split segments against each other instead of parent
# - Refactor code to allow arbitrary segment comparisons (coefs)
# - Remove PDs before analysis and try to find bigger segments
# - Try on a bigger datasets (300-400+)
