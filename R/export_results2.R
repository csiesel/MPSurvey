#' Export Results to Excel (v2)
#'
#' This function takes the results from a statistical analysis, processes the data to extract relevant statistics, and exports the processed data to an Excel file.
#'
#' @param results A list containing the results of a statistical analysis. The list should have a component named `table_body` which is a data frame containing the raw data.
#' @param file A string specifying the path to the output Excel file.
#'
#' @return None. The function writes the processed data to an Excel file.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' export_results2(results = your_survey_results, file = "path/to/export/file.xlsx")
#' }
export_results2 <- function(results, file){
# Sex and Overall ---------------------------------------------------------
  temp <- results$tbls[[1]]

  a <- temp$cards$tbl_svysummary %>%
    filter(!is.na(group1) & !is.null(group1_level) & variable_level!="NULL")

  b <- temp$cards$add_overall %>%
    mutate(group1="sex",
           group1_level="Overall",
           .before=1)  %>%
    filter(!is.na(group1) & !is.null(group1_level) & variable_level!="NULL")

  c <- temp$cards$add_ci %>%
    mutate(group1="sex",
           group1_level = ifelse(group1_level=="NULL", "Overall", group1_level))


# Agecat ------------------------------------------------------------------
  temp2 <- results$tbls[[2]]

  a2 <- temp2$cards$tbl_svysummary %>%
    filter(!is.na(group1) & !is.null(group1_level) & variable_level!="NULL")

  c2 <- temp2$cards$add_ci


# Sex and Agecat ----------------------------------------------------------
  temp3f_sum <- results$tbls[[3]]$tbls[[1]]$cards$tbl_svysummary %>%
    mutate(sex="Female",
           .before=1) %>%
    filter(!is.na(group1) & !is.null(group1_level) & variable_level!="NULL")
  temp3f_ci <- results$tbls[[3]]$tbls[[1]]$cards$add_ci %>%
    mutate(sex="Female",
           .before=1) %>%
    filter(!is.na(group1) & !is.null(group1_level) & variable_level!="NULL")

  temp3m_sum <- results$tbls[[3]]$tbls[[2]]$cards$tbl_svysummary %>%
    mutate(sex="Male",
           .before=1) %>%
    filter(!is.na(group1) & !is.null(group1_level) & variable_level!="NULL")
  temp3m_ci <- results$tbls[[3]]$tbls[[2]]$cards$add_ci %>%
    mutate(sex="Male",
           .before=1) %>%
    filter(!is.na(group1) & !is.null(group1_level) & variable_level!="NULL")





# Combining! --------------------------------------------------------------

  overall_sex_df <- dplyr::bind_rows(a %>% mutate(across(everything(), as.character)) %>% filter(stat!="logit"),
                                b %>% mutate(across(everything(), as.character)) %>% filter(stat!="logit"),
                                c %>% mutate(across(everything(), as.character)) %>% filter(stat!="logit")) %>%
    readr::type_convert()

  agecat_df <- dplyr::bind_rows(a2 %>% mutate(across(everything(), as.character)) %>% filter(stat!="logit"),
                                     c2 %>% mutate(across(everything(), as.character)) %>% filter(stat!="logit")) %>%
    readr::type_convert()

  agecat_sex_df <- dplyr::bind_rows(temp3f_sum %>% mutate(across(everything(), as.character)) %>% filter(stat!="logit"),
                                    temp3f_ci %>% mutate(across(everything(), as.character)) %>% filter(stat!="logit"),
                                    temp3m_sum %>% mutate(across(everything(), as.character)) %>% filter(stat!="logit"),
                                    temp3m_ci %>% mutate(across(everything(), as.character)) %>% filter(stat!="logit")) %>%
    readr::type_convert()




# Pivoting! ---------------------------------------------------------------


  overall_sex_results <- overall_sex_df %>%
    group_by(group1, group1_level, variable, variable_level) %>%
    summarise(n=stat[which(stat_name=="n_unweighted")],
              N=stat[which(stat_name=="N_unweighted")],
              estimate=stat[which(stat_name=="estimate")],
              conf.low=stat[which(stat_name=="conf.low")],
              conf.high=stat[which(stat_name=="conf.high")]) %>%
    ungroup() %>%
    select(-group1) %>%
    rename("sex"=group1_level)

  agecat_results <- agecat_df %>%
    group_by(group1, group1_level, variable, variable_level) %>%
    summarise(n=stat[which(stat_name=="n_unweighted")],
              N=stat[which(stat_name=="N_unweighted")],
              estimate=stat[which(stat_name=="estimate")],
              conf.low=stat[which(stat_name=="conf.low")],
              conf.high=stat[which(stat_name=="conf.high")]) %>%
    ungroup() %>%
    select(-group1) %>%
    rename("agecat"=group1_level)

  agecat_sex_results <- agecat_sex_df %>%
    group_by(sex, group1, group1_level, variable, variable_level) %>%
    summarise(n=stat[which(stat_name=="n_unweighted")],
              N=stat[which(stat_name=="N_unweighted")],
              estimate=stat[which(stat_name=="estimate")],
              conf.low=stat[which(stat_name=="conf.low")],
              conf.high=stat[which(stat_name=="conf.high")]) %>%
    ungroup() %>%
    select(-group1) %>%
    rename("agecat"=group1_level)


  xlsx::write.xlsx(overall_sex_results %>% as.data.frame(), file=file, row.names = F, sheetName = "Overall and Sex",
                   append = TRUE)
  xlsx::write.xlsx(agecat_results %>% as.data.frame(), file=file, row.names = F, sheetName = "Agecat",
                   append = TRUE)
  xlsx::write.xlsx(agecat_sex_results %>% as.data.frame(), file=file, row.names = F, sheetName = "Sex and Agecat",
                   append = TRUE)

}
