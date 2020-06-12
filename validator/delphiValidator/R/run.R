#' Run the validation pipeline on one data source.
#'
#' See the README.md file in the source directory for more information about how to run
#' this function.
#'
#' @param source Data source name we want to report on.
#' @param parameter_path    path to the parameters file
#'
#' @return none
#' @export
run_validator <- function(source, date, parameter_path = "params.json")
{
  # read parameters file
  params <- read_params(parameter_path)

  reporter <- Reporter$new()

  stopifnot(source %in% params$sources)

  source_params <- params$sources[source]

  data <- NULL ## TODO Get the latest data somehow

  reporter$report(check_geo_ids(data, TODO))
  reporter$report(check_vals(data, source_params))
  reporter$report(check_sample_sizes(data, source_params))
}
