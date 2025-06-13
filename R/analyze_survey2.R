#' Analyze Survey Data (v2)
#'
#' This function analyzes survey data using the `gtsummary` package. It creates a summary table of specified variables, stratified by sex, and includes overall statistics, sample sizes, and confidence intervals.
#'
#' @param mps A survey design object created using the `srvyr` package.
#' @param vars A character vector of variable names to include in the analysis.
#'
#' @return A `gtsummary` object containing the summary table of the analyzed survey data.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' summary_table <- analyze_survey(mps = your_survey_design, vars = c("var1", "var2", "var3"))
#' }
analyze_survey2 <- function(mps, vars, strata){


  mps$variables <- mps$variables %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(. %in% "NOT APPLICABLE", NA, .)))


  gtsummary::theme_gtsummary_compact()
  list("style_number-arg:big.mark" = "") %>%
    gtsummary::set_gtsummary_theme()


  if ("sex" %in% names(mps$variables)){
    sex <- mps %>%
      # Selecting all indicators
      select(dplyr::all_of(c(vars, names(mps$strata)))) %>%
      # Similar tbl_svysummary as above with a few differences
      gtsummary::tbl_svysummary(by=sex,
                                digits = list(gtsummary::all_categorical() ~ c(0,0,1,1,1)),
                                # Setting the varible type as continuous for the days indicators
                                # type = list(fruit_days ~ "continuous",
                                #             fruit_servings ~ "continuous",
                                #             veg_days ~ "continuous",
                                #             veg_servings ~ "continuous",
                                #             fruit_veg_servings ~ "continuous",
                                #             pa_days ~ "continuous"),
                                missing = "no",
                                statistic = list(
                                  # Formatting the continuous variables to be presented as mean and standard deviation
                                  gtsummary::all_continuous() ~ "{n_unweighted}/{N_unweighted}/n{mean} ({sd})",
                                  gtsummary::all_categorical() ~ "{n_unweighted}/{N_unweighted}\n{p}%")) %>%
      gtsummary::add_overall() %>%
      # gtsummary::add_n(statistic="{N_nonmiss_unweighted}", location="level") %>%
      gtsummary::add_ci(style_fun = list(
        gtsummary::all_continuous() ~ gtsummary::label_style_number(digits = 2),
        gtsummary::all_categorical() ~ gtsummary::label_style_percent(digits=2))) #%>%
    # gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ "***Overall***")


    agecat <- mps %>%
      # Selecting all indicators
      select(dplyr::all_of(c(vars, names(mps$strata)))) %>%
      # Similar tbl_svysummary as above with a few differences
      gtsummary::tbl_svysummary(by=agecat,
                                digits = list(gtsummary::all_categorical() ~ c(0,0,1,1,1)),
                                # Setting the varible type as continuous for the days indicators
                                # type = list(fruit_days ~ "continuous",
                                #             fruit_servings ~ "continuous",
                                #             veg_days ~ "continuous",
                                #             veg_servings ~ "continuous",
                                #             fruit_veg_servings ~ "continuous",
                                #             pa_days ~ "continuous"),
                                missing = "no",
                                statistic = list(
                                  # Formatting the continuous variables to be presented as mean and standard deviation
                                  gtsummary::all_continuous() ~ "{n_unweighted}/{N_unweighted}/n{mean} ({sd})",
                                  gtsummary::all_categorical() ~ "{n_unweighted}/{N_unweighted}\n{p}%")) %>%
      # gtsummary::add_overall() %>%
      # gtsummary::add_n(statistic="{N_nonmiss_unweighted}", location="level") %>%
      gtsummary::add_ci(style_fun = list(
        gtsummary::all_continuous() ~ gtsummary::label_style_number(digits = 2),
        gtsummary::all_categorical() ~ gtsummary::label_style_percent(digits=2))) #%>%
    # gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ "***Overall***")


    stratified <- mps %>%
      # Selecting all indicators
      select(dplyr::all_of(c(vars, names(mps$strata)))) %>%
      mutate(strat = paste0(sex)) %>%
      gtsummary::tbl_strata2(
        strata=strat,
        .tbl_fun =
          ~ .x %>%
          gtsummary::tbl_svysummary(by=agecat,
                                    digits = list(gtsummary::all_categorical() ~ c(0,0,1,1,1)),
                                    # Setting the varible type as continuous for the days indicators
                                    # type = list(fruit_days ~ "continuous",
                                    #             fruit_servings ~ "continuous",
                                    #             veg_days ~ "continuous",
                                    #             veg_servings ~ "continuous",
                                    #             fruit_veg_servings ~ "continuous",
                                    #             pa_days ~ "continuous"),
                                    missing = "no",
                                    statistic = list(
                                      # Formatting the continuous variables to be presented as mean and standard deviation
                                      gtsummary::all_continuous() ~ "{n_unweighted}/{N_unweighted}/n{mean} ({sd})",
                                      gtsummary::all_categorical() ~ "{n_unweighted}/{N_unweighted}\n{p}%")) %>%
          # gtsummary::add_overall() %>%
          # gtsummary::add_n(statistic="{N_nonmiss_unweighted}", location="level") %>%
          gtsummary::add_ci(style_fun = list(
            gtsummary::all_continuous() ~ gtsummary::label_style_number(digits = 2),
            gtsummary::all_categorical() ~ gtsummary::label_style_percent(digits=2))))
  }
  else if ("gender" %in% names(mps$variables)){
    sex <- mps %>%
      # Selecting all indicators
      select(dplyr::all_of(c(vars, names(mps$strata)))) %>%
      # Similar tbl_svysummary as above with a few differences
      gtsummary::tbl_svysummary(by=gender,
                                digits = list(gtsummary::all_categorical() ~ c(0,0,1,1,1)),
                                # Setting the varible type as continuous for the days indicators
                                # type = list(fruit_days ~ "continuous",
                                #             fruit_servings ~ "continuous",
                                #             veg_days ~ "continuous",
                                #             veg_servings ~ "continuous",
                                #             fruit_veg_servings ~ "continuous",
                                #             pa_days ~ "continuous"),
                                missing = "no",
                                statistic = list(
                                  # Formatting the continuous variables to be presented as mean and standard deviation
                                  gtsummary::all_continuous() ~ "{n_unweighted}/{N_unweighted}/n{mean} ({sd})",
                                  gtsummary::all_categorical() ~ "{n_unweighted}/{N_unweighted}\n{p}%")) %>%
      gtsummary::add_overall() %>%
      # gtsummary::add_n(statistic="{N_nonmiss_unweighted}", location="level") %>%
      gtsummary::add_ci(style_fun = list(
        gtsummary::all_continuous() ~ gtsummary::label_style_number(digits = 2),
        gtsummary::all_categorical() ~ gtsummary::label_style_percent(digits=2))) #%>%
    # gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ "***Overall***")


    agecat <- mps %>%
      # Selecting all indicators
      select(dplyr::all_of(c(vars, names(mps$strata)))) %>%
      # Similar tbl_svysummary as above with a few differences
      gtsummary::tbl_svysummary(by=agecat,
                                digits = list(gtsummary::all_categorical() ~ c(0,0,1,1,1)),
                                # Setting the varible type as continuous for the days indicators
                                # type = list(fruit_days ~ "continuous",
                                #             fruit_servings ~ "continuous",
                                #             veg_days ~ "continuous",
                                #             veg_servings ~ "continuous",
                                #             fruit_veg_servings ~ "continuous",
                                #             pa_days ~ "continuous"),
                                missing = "no",
                                statistic = list(
                                  # Formatting the continuous variables to be presented as mean and standard deviation
                                  gtsummary::all_continuous() ~ "{n_unweighted}/{N_unweighted}/n{mean} ({sd})",
                                  gtsummary::all_categorical() ~ "{n_unweighted}/{N_unweighted}\n{p}%")) %>%
      # gtsummary::add_overall() %>%
      # gtsummary::add_n(statistic="{N_nonmiss_unweighted}", location="level") %>%
      gtsummary::add_ci(style_fun = list(
        gtsummary::all_continuous() ~ gtsummary::label_style_number(digits = 2),
        gtsummary::all_categorical() ~ gtsummary::label_style_percent(digits=2))) #%>%
    # gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ "***Overall***")


    stratified <- mps %>%
      # Selecting all indicators
      select(dplyr::all_of(c(vars, names(mps$strata)))) %>%
      mutate(strat = paste0(gender)) %>%
      gtsummary::tbl_strata2(
        strata=strat,
        .tbl_fun =
          ~ .x %>%
          gtsummary::tbl_svysummary(by=agecat,
                                    digits = list(gtsummary::all_categorical() ~ c(0,0,1,1,1)),
                                    # Setting the varible type as continuous for the days indicators
                                    # type = list(fruit_days ~ "continuous",
                                    #             fruit_servings ~ "continuous",
                                    #             veg_days ~ "continuous",
                                    #             veg_servings ~ "continuous",
                                    #             fruit_veg_servings ~ "continuous",
                                    #             pa_days ~ "continuous"),
                                    missing = "no",
                                    statistic = list(
                                      # Formatting the continuous variables to be presented as mean and standard deviation
                                      gtsummary::all_continuous() ~ "{n_unweighted}/{N_unweighted}/n{mean} ({sd})",
                                      gtsummary::all_categorical() ~ "{n_unweighted}/{N_unweighted}\n{p}%")) %>%
          # gtsummary::add_overall() %>%
          # gtsummary::add_n(statistic="{N_nonmiss_unweighted}", location="level") %>%
          gtsummary::add_ci(style_fun = list(
            gtsummary::all_continuous() ~ gtsummary::label_style_number(digits = 2),
            gtsummary::all_categorical() ~ gtsummary::label_style_percent(digits=2))))
  }




  tbl_final <- list(sex, agecat, stratified) %>%
    gtsummary::tbl_merge(tab_spanner=FALSE)






  return(tbl_final)


}


