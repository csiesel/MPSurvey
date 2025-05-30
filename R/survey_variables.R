#' Select Survey Variables for Analysis
#'
#' This function allows the user to select which variables to analyze from a given description data frame. The function returns a vector of variable names that are selected for analysis.
#'
#' @param desc A data frame or tibble containing variable descriptions. Default is NULL.
#'
#' @return A character vector of variable names that are selected for analysis.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' selected_vars <- survey_variables(desc = your_description_dataframe)
#' }
survey_variables <- function(desc=NULL){
  survey_vars <- DataEditR::data_edit(desc,
                                      col_readonly = c("var", "desc"),
                                      col_bind = "Analyze?",
                                      col_options=list(`Analyze?`=c(TRUE, FALSE)),
                                      read_fun = NA, hide=TRUE)

  survey_variables <- survey_vars$var[which(survey_vars$`Analyze?`==TRUE)]
  return(survey_variables)
}
