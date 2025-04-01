#' get_inr
#'
#' @param mps
#' @param vars
#' @param include_skips
#' @param skip_words
#'
#' @return
#' @export
#'
#' @examples
get_inr <- function(mps=NULL, vars=NULL, include_skips=FALSE, skip_words=c("Skip", "SKIP", "skip", "REFUSED", "Refused", "refused")){
  # Check if mps is provided and has the 'variables' attribute
  if (is.null(mps) || !("variables" %in% names(mps))) {
    stop("Invalid mps object: Please provide a valid survey design object.")
  }

  df = mps$variables |>
    dplyr::select(dplyr::all_of(vars))

  if(include_skips==FALSE){
    df = df |>
      dplyr::mutate_all(~as.character(.)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(. %in% skip_words, NA, .)))
  }


  df_group <- df |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    dplyr::group_by(name, value) |>
    dplyr::summarise(n=dplyr::n()) |>
    dplyr::group_by(name) |>
    mutate(total_resp = sum(n),
           perc = n/total_resp*100)

  inr <- df_group |>
    dplyr::group_by(name) |>
    dplyr::reframe(perc_non_response =  100 - sum(perc[which(!is.na(value))]),
                     n_resp = sum(n[which(!is.na(value))]),
                     total = total_resp) |>
    dplyr::distinct()

  View(df_group)
  View(inr)
}
