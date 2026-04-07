export_results2 <- function(results, file){

  get_first_stat <- function(stat_vec, stat_name_vec, targets) {
    idx <- which(stat_name_vec %in% targets)
    if (length(idx) == 0) return(NA_real_)
    suppressWarnings(as.numeric(stat_vec[idx[1]]))
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
          get_first_stat(stat, stat_name, c("n_unweighted", "N_nonmiss_unweighted")),
          NA_real_
        ),
        N = dplyr::coalesce(
          get_first_stat(stat, stat_name, c("N_unweighted", "N_obs_unweighted")),
          NA_real_
        ),
        estimate = get_first_stat(stat, stat_name, "estimate"),
        conf.low = get_first_stat(stat, stat_name, "conf.low"),
        conf.high = get_first_stat(stat, stat_name, "conf.high"),
        .groups = "drop"
      )
  }

  temp <- results$tbls[[1]]

  sex_sum <- temp$cards$tbl_svysummary %>%
    dplyr::filter(!is.na(group1), !is.na(group1_level))

  sex_overall <- temp$cards$add_overall %>%
    dplyr::mutate(
      group1 = "sex",
      group1_level = "Overall",
      .before = 1
    )

  sex_ci <- temp$cards$add_ci %>%
    dplyr::mutate(
      group1 = "sex",
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
    dplyr::rename(sex = group1_level)

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

  temp3 <- results$tbls[[3]]
  strata_tbls <- temp3$tbls

  strata_results <- lapply(seq_along(strata_tbls), function(i) {
    this_tbl <- strata_tbls[[i]]

    sex_label <- names(strata_tbls)[i]
    if (is.null(sex_label) || identical(sex_label, "")) {
      sex_label <- paste0("Stratum_", i)
    }

    this_sum <- this_tbl$cards$tbl_svysummary %>%
      dplyr::mutate(sex = sex_label, .before = 1) %>%
      dplyr::filter(!is.na(group1), !is.na(group1_level))

    this_ci <- this_tbl$cards$add_ci %>%
      dplyr::mutate(sex = sex_label, .before = 1) %>%
      dplyr::filter(!is.na(group1), !is.na(group1_level))

    this_df <- dplyr::bind_rows(
      prep_card_df(this_sum),
      prep_card_df(this_ci)
    )

    summarise_export(
      this_df,
      group_vars = c("sex", "group1", "group1_level", "variable", "variable_level")
    )
  })

  agecat_sex_results <- dplyr::bind_rows(strata_results) %>%
    dplyr::select(-group1) %>%
    dplyr::rename(agecat = group1_level)

  if (all(grepl("^Stratum_", agecat_sex_results$sex))) {
    uniq <- unique(agecat_sex_results$sex)
    if (length(uniq) == 2) {
      agecat_sex_results$sex <- dplyr::recode(
        agecat_sex_results$sex,
        !!!stats::setNames(c("Female", "Male"), uniq)
      )
    }
  }

  xlsx::write.xlsx(
    overall_sex_results %>% as.data.frame(),
    file = file,
    row.names = FALSE,
    sheetName = "Overall and Sex",
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
    agecat_sex_results %>% as.data.frame(),
    file = file,
    row.names = FALSE,
    sheetName = "Sex and Agecat",
    append = TRUE
  )
}
