# Function to standardize a matrix by sd vector and mean vector
standardize_data <- function(data_matrix,means_vector,sd_vector) {
  return(t(t(sweep(data_matrix,2,means_vector)) / sd_vector))
}

# Function to un-standardize a matrix by sd vector and mean vector
unstandardize_data <- function(data_matrix,means_vector,sd_vector) {
  return(sweep((data_matrix %*% diag(sd_vector)),2,means_vector,"+"))
}

# Function to sum rows of a matrix
sum_rows <- function(x, matrix, noFolds, constructs) {
  return(rowSums(matrix[,(0:(noFolds-1)*length(constructs))+x]))
}

# Function to mean rows of a matrix
mean_rows <- function(x, matrix, noFolds, constructs) {
  return(rowSums(matrix[,(0:(noFolds-1)*length(constructs))+x])/(noFolds-1))
}