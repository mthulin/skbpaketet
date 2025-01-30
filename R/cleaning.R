#' Example data
#' @export
SFM_example <- structure(list(IDCODE = c("SFM0001", "SFM0001", "SFM0001", "SFM0001",
                                     "SFM0001", "SFM0001"), SECUP = c("3,95", "3,95", "3,95", "3,95",
                                                                      "3,95", "3,95"), SECLOW = c("4,95", "4,95", "4,95", "4,95", "4,95",
                                                                                                  "4,95"), DAY_TIMESTAMP = c("2002-09-20", "2002-09-21", "2002-09-22",
                                                                                                                             "2002-09-23", "2002-09-24", "2002-09-25"), DAY_MIN = c("-0,29",
                                                                                                                                                                                    "-0,21", "-0,01", "0,05", "0,42", "0,4"), DAY_MAX = c("-0,22",
                                                                                                                                                                                                                                          "-0,02", "0,05", "0,41", "0,43", "0,43"), DAY_AVG = c("-0,25",
                                                                                                                                                                                                                                                                                                "-0,11", "0,03", "0,24", "0,43", "0,41"), DAY_COUNT = c(8, 19,
                                                                                                                                                                                                                                                                                                                                                        12, 20, 12, 12)), row.names = c(NA, -6L), class = c("tbl_df",
                                                                                                                                                                                                                                                                                                                                                                                                            "tbl", "data.frame"))

#' Turn character values with decimal commas into numeric values with decimal points
#'
#' Turns "3,78" into 3.78.
#'
#' @param x A character vector.
#' @return A numeric vector.
#' @details Replaces all commas "," with points "." and converts the values to numeric.
#' @examples
#' library(dplyr)
#' SFM_example |> mutate(across(where(is.character), dc_to_numeric))
#' @export
dc_to_numeric <- function(x)
{
    if(length(grep(",", x)) > 0) { x <- as.numeric(gsub(",", ".", x)) }
    return(x)
}


#' Turn all columns with character values with decimal commas into numeric columns with decimal points
#'
#' Turns "3,78" into 3.78 for all columns containing numeric data with decimal commas stored as text.
#'
#' @param x A data frame
#' @return A data frame.
#' @details Replaces all commas "," with points "." in alla text columns and converts the values to numeric.
#' @examples
#' SFM_example |> all_dc_to_numeric()
#' @export
all_dc_to_numeric <- function(x)
{
    dplyr::mutate(x, dplyr::across(tidyselect::where(is.character), dc_to_numeric))
}

#' Compute hydrological year from a date
#'
#' The hydrological year is the same as the calendar year for January-September, and the calendar year +1 for October-December.
#'
#' @param x A Date vector, or a character vector containing dates using the YYYY-MM-DD format.
#' @return A numeric vector.
#' @examples
#' dates <- c("2024-03-15", "2024-08-13", "2024-11-29")
#' hydroyear(dates)
#' @export
hydroyear <- function(x)
{
    yr <- lubridate::year(x)
    mth <- lubridate::month(x)
    yr[mth >= 10] <- yr[mth >= 10]+1
    return(yr)
}
