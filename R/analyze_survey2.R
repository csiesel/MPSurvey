#' Analyze Survey Data (v2)
#'
#' This function analyzes survey data using the `gtsummary` package. It creates
#' summary tables of specified variables stratified by sex/gender and agecat,
#' and returns a merged table including overall statistics, sample sizes, and
#' confidence intervals.
#'
#' @param mps A survey design object created using the `srvyr` package.
#' @param vars A character vector of variable names to include in the analysis.
#' @param numeric_vars An optional character vector of variable names from `vars`
#'   that should be interpreted as continuous/numeric in `tbl_svysummary()`.
#'   Default is NULL.
#' @param numeric_exclude_values Numeric values that should be treated as missing
#'   for variables listed in `numeric_vars`. These are commonly refusal or
#'   don't-know codes such as 88, 99, or 999. Default is
#'   `c(88, 98, 99, 888, 998, 999)`.
#'
#' @return A `gtsummary` object containing merged summary tables.
#' @export
analyze_survey2 <- function(mps,
                            vars,
                            numeric_vars = NULL,
                            numeric_exclude_values = c(88, 98, 99, 888, 998, 999)) {

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
          ~ {
            x <- as.character(.)
            x <- trimws(x)

            x[toupper(x) %in% c(
              "NOT APPLICABLE",
              "REFUSED",
              "REFUSED/ DON'T KNOW",
              "REFUSED/DON'T KNOW",
              "DON'T KNOW",
              "DONT KNOW",
              "NA",
              ""
            )] <- NA_character_

            x_num <- suppressWarnings(as.numeric(x))
            x_num[x_num %in% numeric_exclude_values] <- NA_real_
            x_num
          }
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

  digits_list <- list(
    gtsummary::all_categorical() ~ c(0, 0, 1, 1, 1),
    gtsummary::all_continuous() ~ c(0, 1, 1, 1, 1)
  )

  statistic_list <- list(
    gtsummary::all_continuous() ~ "{N_nonmiss_unweighted}\n{mean} ({sd})",
    gtsummary::all_categorical() ~ "{n_unweighted}/{N_unweighted}\n{p}%"
  )

  ci_style_list <- list(
    gtsummary::all_continuous() ~ gtsummary::label_style_number(digits = 2),
    gtsummary::all_categorical() ~ gtsummary::label_style_percent(digits = 2)
  )

  if ("sex" %in% names(mps$variables)) {
    sex <- mps %>%
      dplyr::select(dplyr::all_of(c(vars, names(mps$strata)))) %>%
      gtsummary::tbl_svysummary(
        by = sex,
        digits = digits_list,
        type = type_list,
        missing = "no",
        statistic = statistic_list
      ) %>%
      gtsummary::add_overall() %>%
      gtsummary::add_ci(style_fun = ci_style_list)

    agecat <- mps %>%
      dplyr::select(dplyr::all_of(c(vars, names(mps$strata)))) %>%
      gtsummary::tbl_svysummary(
        by = agecat,
        digits = digits_list,
        type = type_list,
        missing = "no",
        statistic = statistic_list
      ) %>%
      gtsummary::add_ci(style_fun = ci_style_list)

    stratified <- mps %>%
      dplyr::select(dplyr::all_of(c(vars, names(mps$strata)))) %>%
      dplyr::mutate(strat = paste0(sex)) %>%
      gtsummary::tbl_strata2(
        strata = strat,
        .tbl_fun = ~ .x %>%
          gtsummary::tbl_svysummary(
            by = agecat,
            digits = digits_list,
            type = type_list,
            missing = "no",
            statistic = statistic_list
          ) %>%
          gtsummary::add_ci(style_fun = ci_style_list)
      )
  } else if ("gender" %in% names(mps$variables)) {
    sex <- mps %>%
      dplyr::select(dplyr::all_of(c(vars, names(mps$strata)))) %>%
      gtsummary::tbl_svysummary(
        by = gender,
        digits = digits_list,
        type = type_list,
        missing = "no",
        statistic = statistic_list
      ) %>%
      gtsummary::add_overall() %>%
      gtsummary::add_ci(style_fun = ci_style_list)

    agecat <- mps %>%
      dplyr::select(dplyr::all_of(c(vars, names(mps$strata)))) %>%
      gtsummary::tbl_svysummary(
        by = agecat,
        digits = digits_list,
        type = type_list,
        missing = "no",
        statistic = statistic_list
      ) %>%
      gtsummary::add_ci(style_fun = ci_style_list)

    stratified <- mps %>%
      dplyr::select(dplyr::all_of(c(vars, names(mps$strata)))) %>%
      dplyr::mutate(strat = paste0(gender)) %>%
      gtsummary::tbl_strata2(
        strata = strat,
        .tbl_fun = ~ .x %>%
          gtsummary::tbl_svysummary(
            by = agecat,
            digits = digits_list,
            type = type_list,
            missing = "no",
            statistic = statistic_list
          ) %>%
          gtsummary::add_ci(style_fun = ci_style_list)
      )
  } else {
    stop("Neither 'sex' nor 'gender' was found in mps$variables.")
  }

  tbl_final <- list(sex, agecat, stratified) %>%
    gtsummary::tbl_merge(tab_spanner = FALSE)

  return(tbl_final)
}
