#' Reduce Correlated Variables in a Dataset
#'
#' Identifies groups of highly correlated variables (above a specified threshold)
#' and retains only the variable with the highest variance in each group.
#' Non-covariate columns are preserved and returned with the final dataset.
#'
#' @param dt A `data.table` containing the full dataset.
#' @param noncovariate_cols A character vector of column names to exclude from correlation analysis.
#' @param cor_threshold A numeric value between 0 and 1 specifying the correlation threshold (default is 0.98).
#' @param return_dropped Logical. If TRUE, returns a list including the dropped variables.
#'
#' @return A `data.table` with reduced multicollinearity, including non-covariate columns.
#' If `return_dropped = TRUE`, returns a list:
#' \describe{
#'   \item{data}{The filtered `data.table`.}
#'   \item{dropped}{Character vector of dropped covariate names.}
#' }
#' @export
#'
#' @examples
#' dt <- data.table::as.data.table(mtcars)
#' result <- reduce_correlated_variables(dt, cor_threshold = 0.9)
reduce_correlated_variables <- function(
  dt,
  noncovariate_cols = c("time", "AMA_SITE_CODE", "lon", "lat", "year"),
  cor_threshold = 0.89,
  return_dropped = FALSE
) {
  require(data.table)
  require(igraph)

  dt <- copy(dt) # avoid modifying by reference

  # Separate covariates and non-covariates
  cov_data <- dt[, setdiff(names(dt), noncovariate_cols), with = FALSE]
  noncov_data <- dt[, intersect(names(dt), noncovariate_cols), with = FALSE]

  # Filter: remove columns that are all NA, all 0s, or have <= 1 unique value
  keep_cols <- sapply(cov_data, function(col) {
    vals <- col[!is.na(col)]
    length(unique(vals)) > 1 && any(vals != 0)
  })
  cov_data <- cov_data[, names(keep_cols)[keep_cols], with = FALSE]

  # Catch: Not enough covariate columns
  if (ncol(cov_data) <= 1) {
    warning(
      "Not enough covariate columns for reduction. Returning original dataset unchanged."
    )
    reduced_dt <- cbind(noncov_data, cov_data)
    return(
      if (return_dropped) list(data = reduced_dt, dropped = character(0)) else
        reduced_dt
    )
  }

  # Compute correlation matrix
  cor_matrix <- cor(cov_data, use = "pairwise.complete.obs")

  # Identify highly correlated pairs (excluding self-correlation)
  high_cor_pairs <- which(
    abs(cor_matrix) > cor_threshold & abs(cor_matrix) < 1,
    arr.ind = TRUE
  )
  if (nrow(high_cor_pairs) == 0) {
    message("No highly correlated pairs found.")
    reduced_dt <- cbind(noncov_data, cov_data)
    return(
      if (return_dropped) list(data = reduced_dt, dropped = character(0)) else
        reduced_dt
    )
  }

  # Convert index matrix to variable names
  var_names <- colnames(cor_matrix)
  edge_list <- matrix(var_names[as.integer(high_cor_pairs)], ncol = 2)

  # Build graph using variable names
  cor_graph <- igraph::graph_from_edgelist(edge_list, directed = FALSE)

  # Get connected components (correlated groups)
  groups <- igraph::components(cor_graph)$membership
  grouped_vars <- split(names(groups), groups)

  # Keep variable with highest variance in each group
  vars_to_keep <- sapply(grouped_vars, function(group_vars) {
    group_dt <- cov_data[, group_vars, with = FALSE]
    var_vals <- sapply(group_dt, var, na.rm = TRUE)
    names(which.max(var_vals))
  })

  # Identify variables not in any correlated group
  all_grouped_vars <- unique(unlist(grouped_vars))
  vars_unrelated <- setdiff(names(cov_data), all_grouped_vars)
  final_vars <- c(vars_to_keep, vars_unrelated)

  # Subset covariates
  reduced_cov_data <- cov_data[, final_vars, with = FALSE]

  # Combine non-covariate and reduced covariate columns
  reduced_dt <- cbind(noncov_data, reduced_cov_data)

  if (return_dropped) {
    dropped_vars <- setdiff(names(cov_data), final_vars)
    return(list(data = reduced_dt, dropped = dropped_vars))
  } else {
    return(reduced_dt)
  }
}
