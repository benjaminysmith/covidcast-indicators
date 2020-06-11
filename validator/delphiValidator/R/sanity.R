## Functions to ensure the generated data matches sanity-checking rules, such as
## reasonable value ranges and geo IDs.

#' Verify that all geo IDs match rules.
#'
#' @param df A covidalert data frame containing a geo_id column.
#' @param geo_type What geo_type is this? (county, msa, state, hrr, or dma)
#' @return Vector of invalid geo_ids, or length-0 vector if all are valid. The
#'   vector is named with the row numbers of the invalid entries.
#' @export
check_geo_ids <- function(df, geo_type = c("county", "msa", "state", "hrr", "dma")) {
  geo_type <- match.arg(geo_type)

  pattern <- paste0("^",
                    switch(geo_type,
                           "hrr" = "\\d{1,3}",
                           "msa" = "\\d{5}",
                           "county" = "\\d{5}",
                           "state" = "[a-z]{2}",
                           "dma" =  "\\d{3}"
                           ),
                    "$")

  bad_idxs <- !grepl(pattern, df$geo_id)
  return(annotate_bad(df$geo_id, bad_dxs))
}

#' Verify that all values match rules.
#'
#' @param df A covidalert data frame containing a val column.
#' @param params A params list containing entries `na_val_allowed`, `min_val`, and
#'   `max_val` specifying the acceptable range of the value.
#' @return Vector of invalid values, or length-0 vector if all are valid. The
#'   vector is named with the row numbers of the invalid entries.
#' @export
check_vals <- function(df, params) {
  bad <- c()

  if (!params$na_val_allowed) {
    na_idxs <- is.na(df$val)
    bad <- c(bad, annotate_bad(df$val, na_idxs))
  }

  out_of_range <- df$val > params$max_val | df$val < params$min_val
  bad <- c(bad, annotate_bad(df$val, out_of_range))

  return(bad)
}

#' Verify that all sample sizes are valid.
#'
#' @param df A covidalert data frame containing a sample_size column.
#' @param params A parameter list containing entries `report_sample_size`, `min_sample_size`.
#' @return Vector of invalid values, or length-0 vector if all are valid. The
#'   vector is named with the row numbers of the invalid entries.
#' @export
check_sample_sizes <- function(df, params) {
  if (!params$report_sample_size) {
    ## This data source is not required to report sample sizes, so they should
    ## all be NA.
    bad_idxs <- df$sample_size != "NA"

    return(annotate_bad(df$sample_size, bad_idxs))
  }

  if (any(is.na(df$sample_size))) {
      ## Data sources that *do* report sample sizes should never report NA. Exit
      ## early because with NAs, we won't be able to check the range easily.
      return(annotate_bad(df$sample_size, is.na(df$sample_size)))
  }

  ## Sample sizes should all be above the minimum.
  return(annotate_bad(df$sample_size, df$sample_size < params$min_sample_size))
}
