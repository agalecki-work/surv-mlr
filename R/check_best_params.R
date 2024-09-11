
# Function to check and extract the best parameters
check_best_params <- function(at) {
  # Ensure archives are available
  if (!is.null(at$archive)) {
    # Get the best parameters from the tuning archive
    best_params <- at$archive$best()
    if (!is.null(best_params)) {
      return(best_params)
    } else {
      warning("Tuning results are empty. Please check the setup.")
      return(NULL)
    }
  } else {
    stop("Tuning did not produce any results. Please check the setup.")
  }
}
