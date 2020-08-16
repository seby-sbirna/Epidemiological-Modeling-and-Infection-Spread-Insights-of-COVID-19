# Define project functions -----------------------------------------------------

# Linear model for the time series
mdl <- function(df) {
  lm(count ~ date_observation, data = df)
}
