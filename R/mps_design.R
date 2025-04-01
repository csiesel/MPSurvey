#' mps_design
#'
#' @param df
#' @param ids
#' @param strata
#' @param weight
#' @param vars
#'
#' @return
#' @export
#'
#' @examples
mps_design <- function(df=NULL, ids, strata, weight, vars){
  mps <- df |>
    dplyr::mutate(!!dplyr::enquo(weight) := as.numeric(!!dplyr::enquo(weight))) |>
    dplyr::select(!!dplyr::enquo(ids), !!dplyr::enquo(strata), !!dplyr::enquo(weight), vars) |>
    srvyr::as_survey_design(ids=!!dplyr::enquo(ids),
                            strata=!!dplyr::enquo(strata),
                            weight=!!dplyr::enquo(weight))
  return(mps)
}
