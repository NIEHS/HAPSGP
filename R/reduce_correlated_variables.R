#' Reduce Correlated Variables in a Dataset
#'
#' Identifies groups of highly correlated variables (above a specified threshold)
#' and retains only the variable with the highest variance in each group.
#'
#' @param dt A `data.table` containing the full dataset.
#' @param noncovariate_cols A character vector of column names to exclude from correlation analysis.
#' @param cor_threshold A numeric value between 0 and 1 specifying the correlation threshold (default is 0.98).
#' @param return_dropped Logical. If TRUE, returns a list including the dropped variables.
#'
#' @return A `data.table` with reduced multicollinearity. If `return_dropped = TRUE`, returns a list with:
#' \describe{
#'   \item{data}{The filtered `data.table`.}
#'   \item{dropped}{Character vector of dropped variable names.}
#' }
#' @export
#'
#' @examples
#' dt <- data.table::as.data.table(mtcars)
#' result <- reduce_correlated_variables(dt, cor_threshold = 0.9)
reduce_correlated_variables <- function(
  dt,
  noncovariate_cols = c("time", "AMA_SITE_CODE"),
  cor_threshold = 0.98,
  return_dropped = FALSE
) {
  require(data.table)
  require(igraph)

  dt <- copy(dt) # avoid modifying by reference

  # Select only covariate columns
  cov_data <- dt[, setdiff(names(dt), noncovariate_cols), with = FALSE]

  # Filter: remove columns that are all NA, all 0s, or have <= 1 unique value
  keep_cols <- sapply(cov_data, function(col) {
    vals <- col[!is.na(col)]
    length(unique(vals)) > 1 && any(vals != 0)
  })
  cov_data <- cov_data[, ..names(keep_cols)[keep_cols]]

  # If no variables left, return early
  if (ncol(cov_data) < 2) {
    warning("Not enough valid covariates after filtering.")
    return(
      if (return_dropped) list(data = cov_data, dropped = character(0)) else
        cov_data
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
    return(
      if (return_dropped) list(data = cov_data, dropped = character(0)) else
        cov_data
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
    group_dt <- cov_data[, ..group_vars]
    var_vals <- sapply(group_dt, var, na.rm = TRUE)
    names(which.max(var_vals))
  })

  # Identify variables not in any correlated group
  all_grouped_vars <- unique(unlist(grouped_vars))
  vars_unrelated <- setdiff(names(cov_data), all_grouped_vars)
  final_vars <- c(vars_to_keep, vars_unrelated)

  # Subset and return
  reduced_dt <- cov_data[, ..final_vars]

  if (return_dropped) {
    dropped_vars <- setdiff(names(cov_data), final_vars)
    return(list(data = reduced_dt, dropped = dropped_vars))
  } else {
    return(reduced_dt)
  }
}
