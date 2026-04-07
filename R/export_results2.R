#' Export Results to Excel (v2)
#'
#' This function takes the results from a statistical analysis, processes the
#' card-level data from a `gtsummary` object, and exports the processed results
#' to an Excel workbook with separate sheets for overall/sex, age category,
#' and sex by age category results.
#'
#' @param results A list containing the results of a statistical analysis,
#'   typically returned by `analyze_survey2()`.
#' @param file A character string specifying the path to the output Excel file.
#'
#' @return None. The function writes the processed data to an Excel file.
#' @export
#'
#' @examples
#' \dontrun{
#' export_results2(results = your_survey_results, file = "path/to/export/file.xlsx")
#' }
export_results2 <- function(results, file){

  clean_numeric_stat <- function(x) {
    x <- as.character(x)
    x <- gsub("%", "", x)
    x <- gsub("<0.001", "0.001", x, fixed = TRUE)
    suppressWarnings(as.numeric(x))
  }

  get_first_stat <- function(stat_vec, stat_name_vec, targets) {
    idx <- which(as.character(stat_name_vec) %in% targets)
    if (length(idx) == 0) return(NA_real_)
    clean_numeric_stat(stat_vec[idx[1]])
  }

  prep_card_df <- function(df) {
    df %>%
      dplyr::mutate(
        dplyr::across(dplyr::everything(), as.character),
        stat = as.character(stat),
        stat_name = as.character(stat_name),
        variable = as.character(variable),
        variable_level = as.character(variable_level),
        group1 = as.character(group1),
        group1_level = as.character(group1_level)
      ) %>%
      dplyr::filter(is.na(stat) | stat != "logit")
  }

  summarise_export <- function(df, group_vars) {
    df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(
        n = dplyr::coalesce(
          get_first_stat(stat, stat_name, c("n_unweighted", "n", "N_nonmiss_unweighted")),
          NA_real_
        ),
        N = dplyr::coalesce(
          get_first_stat(stat, stat_name, c("N_unweighted", "N", "N_obs_unweighted")),
          NA_real_
        ),
        estimate = dplyr::coalesce(
          get_first_stat(stat, stat_name, c("estimate", "p", "mean")),
          NA_real_
        ),
        conf.low = get_first_stat(stat, stat_name, c("conf.low", "ci.low")),
        conf.high = get_first_stat(stat, stat_name, c("conf.high", "ci.high")),
        .groups = "drop"
      )
  }

  # Overall and sex/gender -------------------------------------------------
  temp <- results$tbls[[1]]

  overall_group_name <- unique(as.character(temp$cards$tbl_svysummary$group1))
  overall_group_name <- overall_group_name[!is.na(overall_group_name)]
  overall_group_name <- if (length(overall_group_name) > 0) overall_group_name[1] else "group"

  sex_sum <- temp$cards$tbl_svysummary %>%
    dplyr::filter(!is.na(group1), !is.na(group1_level))

  sex_overall <- temp$cards$add_overall %>%
    dplyr::mutate(
      group1 = overall_group_name,
      group1_level = "Overall",
      .before = 1
    )

  sex_ci <- temp$cards$add_ci %>%
    dplyr::mutate(
      group1 = overall_group_name,
      group1_level = ifelse(group1_level == "NULL" | is.na(group1_level), "Overall", group1_level)
    )

  overall_sex_df <- dplyr::bind_rows(
    prep_card_df(sex_sum),
    prep_card_df(sex_overall),
    prep_card_df(sex_ci)
  )

  overall_sex_results <- summarise_export(
    overall_sex_df,
    group_vars = c("group1", "group1_level", "variable", "variable_level")
  ) %>%
    dplyr::select(-group1) %>%
    dplyr::rename(group = group1_level)

  # Agecat -----------------------------------------------------------------
  temp2 <- results$tbls[[2]]

  agecat_sum <- temp2$cards$tbl_svysummary %>%
    dplyr::filter(!is.na(group1), !is.na(group1_level))

  agecat_ci <- temp2$cards$add_ci %>%
    dplyr::filter(!is.na(group1), !is.na(group1_level))

  agecat_df <- dplyr::bind_rows(
    prep_card_df(agecat_sum),
    prep_card_df(agecat_ci)
  )

  agecat_results <- summarise_export(
    agecat_df,
    group_vars = c("group1", "group1_level", "variable", "variable_level")
  ) %>%
    dplyr::select(-group1) %>%
    dplyr::rename(agecat = group1_level)

  # Sex/gender by agecat ---------------------------------------------------
  temp3 <- results$tbls[[3]]
  strata_tbls <- temp3$tbls

  strata_results <- lapply(seq_along(strata_tbls), function(i) {
    this_tbl <- strata_tbls[[i]]

    stratum_label <- names(strata_tbls)[i]
    if (is.null(stratum_label) || identical(stratum_label, "")) {
      stratum_label <- paste0("Stratum_", i)
    }

    this_sum <- this_tbl$cards$tbl_svysummary %>%
      dplyr::mutate(group = stratum_label, .before = 1) %>%
      dplyr::filter(!is.na(group1), !is.na(group1_level))

    this_ci <- this_tbl$cards$add_ci %>%
      dplyr::mutate(group = stratum_label, .before = 1) %>%
      dplyr::filter(!is.na(group1), !is.na(group1_level))

    this_df <- dplyr::bind_rows(
      prep_card_df(this_sum),
      prep_card_df(this_ci)
    )

    summarise_export(
      this_df,
      group_vars = c("group", "group1", "group1_level", "variable", "variable_level")
    )
  })

  agecat_group_results <- dplyr::bind_rows(strata_results) %>%
    dplyr::select(-group1) %>%
    dplyr::rename(agecat = group1_level)

  # Optional relabel of generic strata names
  if (nrow(agecat_group_results) > 0 && all(grepl("^Stratum_", agecat_group_results$group))) {
    uniq <- unique(agecat_group_results$group)
    if (length(uniq) == 2) {
      agecat_group_results$group <- dplyr::recode(
        agecat_group_results$group,
        !!!stats::setNames(c("Female", "Male"), uniq)
      )
    }
  }

  xlsx::write.xlsx(
    overall_sex_results %>% as.data.frame(),
    file = file,
    row.names = FALSE,
    sheetName = "Overall and Group",
    append = TRUE
  )

  xlsx::write.xlsx(
    agecat_results %>% as.data.frame(),
    file = file,
    row.names = FALSE,
    sheetName = "Agecat",
    append = TRUE
  )

  xlsx::write.xlsx(
    agecat_group_results %>% as.data.frame(),
    file = file,
    row.names = FALSE,
    sheetName = "Group and Agecat",
    append = TRUE
  )
}
