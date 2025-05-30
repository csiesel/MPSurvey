#' Create Survey Design Object
#'
#' This function creates a survey design object using the `srvyr` package. It takes a data frame and specified columns for IDs, strata, weights, and variables, and returns a survey design object.
#'
#' @param df A data frame containing the survey data. Default is NULL.
#' @param ids A column name in the data frame representing the cluster or primary sampling unit (PSU) IDs.
#' @param strata A column name in the data frame representing the strata.
#' @param weight A column name in the data frame representing the survey weights.
#' @param vars A character vector of variable names to include in the survey design.
#'
#' @return A survey design object created using the `srvyr` package.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' survey_design <- mps_design(df = your_dataframe, ids = "psu_id", strata = "strata", weight = "weight", vars = c("var1", "var2", "var3"))
#' }
mps_design <- function(df=NULL, ids, strata, weight, vars){

  vars = vars[which(vars %in% names(df))]

  na_words <- c("skip", "refused")


  mps <- df |>
    dplyr::mutate(!!dplyr::enquo(weight) := as.numeric(!!dplyr::enquo(weight))) |>
    dplyr::mutate(dplyr::across(vars, ~ifelse(grepl(paste0(na_words,collapse = "|"), ., ignore.case = T), NA, .))) |>
    dplyr::select(!!dplyr::enquo(ids), !!dplyr::enquo(strata), !!dplyr::enquo(weight), vars) |>
    srvyr::as_survey_design(ids=!!dplyr::enquo(ids),
                            strata=!!dplyr::enquo(strata),
                            weight=!!dplyr::enquo(weight))
  return(mps)
}
