#' Return params file as an R list
#'
#' Reads a parameters file. If the file does not exist, the function will create a copy of
#' '"params.json.template" and read from that.
#'
#' @param path    path to the parameters file; if not present, will try to copy the file
#'                "params.json.template"
#'
#' @return a named list of parameters values
#'
#' @importFrom dplyr if_else
#' @importFrom jsonlite read_json
#' @importFrom lubridate ymd_hms
#' @export
read_params <- function(path = "params.json") {
  if (!file.exists(path)) file.copy("params.json.template", "params.json")
  params <- read_json(path, simplifyVector = TRUE)

  return(params)
}

#' Produce a named vector containing values for which cond is false.
#'
#' @param vec Vector of any type of value.
#' @param bad_cond Logical vector of the same length.
#' @return Named vector containing elements of vec for which the corresponding
#'     element of bad_cond is TRUE. Names are the indices of these values.
annotate_bad <- function(vec, bad_cond) {
    bad <- vec[bad_cond]
    names(bad) <- which(bad_cond)

    return(bad)
}
