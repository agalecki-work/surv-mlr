custom_fg_metric = function(task, prediction) {
  # Extract true survival times and event status
  df <- data.frame(
    true_times = task$truth("time"),
    true_status = task$truth("event"),
    # ensure predictions correspond to the risk scores or equivalent
    predicted_crank = prediction$crank()
  )
  
  # Convert `true_status` to reflect competing risk simplification, focusing only on event type 1
  df_cr = df %>%
    mutate(fg_event = ifelse(true_status == 1, 1, 0)) # adjust based on the event of interest

  # Pairwise C-index calculation for the event of interest
  concordance_index = function(df) {
    # Implementation of calculation of concordance
    return(as.numeric(survConcordance(Surv(true_times, fg_event) ~ predicted_crank, data = df)$concordance))
  }
  
  # Calculate custom metric and return
  score = concordance_index(df_cr)
  
  return(score)
}
# mlr3 Custom Measure Example
MeasureFGConcordance = R6Class("MeasureFGConcordance",
  inherit = Measure,
  public = list(
    initialize = function() {
      super$initialize(
        id = "fg_cindex",
        range = c(0, 1), # C-Index range
        minimize = FALSE
      )
    }
  ),
  
  private = list(
    .score = function(prediction, task, ...) {
      custom_fg_metric(task, prediction)
    }
  )
)

# Instantiate the custom measure and integrate into your scoring
measure_fg_concordance = MeasureFGConcordance$new()