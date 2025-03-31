mps_design <- function(df=NULL, ids, strata, weight){
  mps <- df |>
    dplyr::mutate(!!dplyr::enquo(weight) := as.numeric(!!dplyr::enquo(weight))) |>
    srvyr::as_survey_design(ids=!!dplyr::enquo(ids),
                            strata=!!dplyr::enquo(strata),
                            weight=!!dplyr::enquo(weight))
  return(mps)
}
