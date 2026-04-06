#' Analyze Survey Data
#'
#' This function analyzes survey data using the `gtsummary` package. It creates
#' a summary table of specified variables, stratified by sex/gender, and includes
#' overall statistics, sample sizes, and confidence intervals.
#'
#' @param mps A survey design object created using the `srvyr` package.
#' @param vars A character vector of variable names to include in the analysis.
#' @param numeric_vars An optional character vector of variable names from `vars`
#'   that should be interpreted as continuous/numeric in `tbl_svysummary()`.
#'   Default is NULL.
#'
#' @return A `gtsummary` object containing the summary table of the analyzed
#'   survey data.
#' @export
analyze_survey <- function(mps, vars, numeric_vars = NULL){

  mps$variables <- mps$variables %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ ifelse(. %in% "NOT APPLICABLE", NA, .)
      )
    )

  gtsummary::theme_gtsummary_compact()
  list("style_number-arg:big.mark" = "") %>%
    gtsummary::set_gtsummary_theme()

  if (is.null(numeric_vars)) {
    numeric_vars <- character(0)
  } else {
    numeric_vars <- intersect(as.character(numeric_vars), as.character(vars))
  }

  if (length(numeric_vars) > 0) {
    mps$variables <- mps$variables %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(numeric_vars),
          ~ suppressWarnings(as.numeric(as.character(.)))
        )
      )
  }

  type_list <- NULL
  if (length(numeric_vars) > 0) {
    type_list <- stats::setNames(
      rep("continuous", length(numeric_vars)),
      numeric_vars
    )
    type_list <- as.list(type_list)
  }

  if("sex" %in% names(mps$variables)){
    gtsummary_indicators <- mps %>%
      dplyr::select(dplyr::all_of(c(vars, names(mps$strata)))) %>%
      gtsummary::tbl_svysummary(
        by = sex,
        digits = list(gtsummary::all_categorical() ~ c(0, 0, 1, 1, 1)),
        type = type_list,
        missing = "no",
        statistic = list(
          gtsummary::all_continuous() ~ "{N_nonmiss_unweighted}/{N_obs_unweighted}\n{mean} ({sd})",
          gtsummary::all_categorical() ~ "{n_unweighted}/{N_unweighted}\n{p}%"
        )
      ) %>%
      gtsummary::add_overall() %>%
      gtsummary::add_ci(
        style_fun = list(
          gtsummary::all_continuous() ~ gtsummary::label_style_number(digits = 2),
          gtsummary::all_categorical() ~ gtsummary::label_style_percent(digits = 2)
        )
      ) %>%
      gtsummary::modify_header(
        label = "**Characteristic**",
        stat_0 = "**Overall**",
        ci_stat_0 = "**95% CI**",
        stat_1 = "**Female**",
        ci_stat_1 = "**95% CI**",
        stat_2 = "**Male**",
        ci_stat_2 = "**95% CI**"
      )

  } else if("gender" %in% names(mps$variables)){
    gtsummary_indicators <- mps %>%
      dplyr::select(dplyr::all_of(c(vars, names(mps$strata)))) %>%
      gtsummary::tbl_svysummary(
        by = gender,
        digits = list(gtsummary::all_categorical() ~ c(0, 0, 1, 1, 1)),
        type = type_list,
        missing = "no",
        statistic = list(
          gtsummary::all_continuous() ~ "{N_nonmiss_unweighted}/{N_obs_unweighted}\n{mean} ({sd})",
          gtsummary::all_categorical() ~ "{n_unweighted}/{N_unweighted}\n{p}%"
        )
      ) %>%
      gtsummary::add_overall() %>%
      gtsummary::add_ci(
        style_fun = list(
          gtsummary::all_continuous() ~ gtsummary::label_style_number(digits = 2),
          gtsummary::all_categorical() ~ gtsummary::label_style_percent(digits = 2)
        )
      ) %>%
      gtsummary::modify_header(
        label = "**Characteristic**",
        stat_0 = "**Overall**",
        ci_stat_0 = "**95% CI**",
        stat_1 = "**Female**",
        ci_stat_1 = "**95% CI**",
        stat_2 = "**Male**",
        ci_stat_2 = "**95% CI**"
      )

  } else {
    stop("Neither 'sex' nor 'gender' was found in mps$variables.")
  }

  return(gtsummary_indicators)
}
