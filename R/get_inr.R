#' Calculate Item Non-Response (INR) Rates
#'
#' This function calculates the item non-response (INR) rates for specified variables in a survey design object. It returns a list containing the INR rates and the response distributions for each variable.
#'
#' @param x A data frame containing the final data for the survey analysis. Default is NULL.
#' @param vars A character vector of variable names for which to calculate INR rates. Default is NULL.
#' @param include_skips A logical value indicating whether to include skip patterns in the analysis. Default is FALSE.
#' @param skip_words A character vector of words indicating skip patterns (e.g., "Skip", "REFUSED"). Default is c("Skip", "SKIP", "skip", "REFUSED", "Refused", "refused").
#'
#' @return A list containing two elements: `inr` (a tibble with INR rates) and `responses` (a tibble with response distributions).
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' inr_results <- get_inr(mps = your_survey_design, vars = c("var1", "var2"), include_skips = FALSE)
#' inr <- inr_results$inr
#' responses <- inr_results$responses
#' }
get_inr <- function(x=NULL, vars=NULL, include_skips=FALSE, skip_words=c("Skip", "SKIP", "skip", "REFUSED", "Refused", "refused")){


  vars = vars[which(vars %in% names(x))]

  df = x |>
    dplyr::select(dplyr::all_of(vars))

  if(include_skips==FALSE){
    df = df |>
      dplyr::mutate_all(~as.character(.)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(. %in% skip_words, NA, .)))
  } else{
    df = df |>
      dplyr::mutate_all(~as.character(.))
  }


  df_group <- df |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    dplyr::group_by(name, value) |>
    dplyr::summarise(n=dplyr::n()) |>
    dplyr::group_by(name) |>
    mutate(total_resp = sum(n) - sum(n[which(value =="NOT APPLICABLE")]),
           perc = n/total_resp*100)

  inr <- df_group |>
    dplyr::group_by(name) |>
    dplyr::reframe(perc_non_response =  100 - (sum(perc[which(!is.na(value))]) - sum(perc[which(value=="NOT APPLICABLE")])),
                     n_resp = sum(n[which(!is.na(value))]) - sum(n[which(value=="NOT APPLICABLE")]),
                     total = total_resp) |>
    dplyr::distinct()

  # View(df_group)
  # View(inr)
  return(list("inr"=inr,
              "responses"=df_group))
}
