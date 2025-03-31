#' survey_variables
#'
#' @param desc
#'
#' @return asdf
#' @export
#'
#' @examples asdf
survey_variables <- function(desc=NULL){
  survey_vars <- DataEditR::data_edit(desc,
                                      col_readonly = c("var", "desc"),
                                      col_bind = "Analyze?",
                                      col_options=list(`Analyze?`=c(TRUE, FALSE)),
                                      read_fun = NA, hide=TRUE)

  survey_variables <- survey_vars$var[which(survey_vars$`Analyze?`==TRUE)]
  return(survey_variables)
}
