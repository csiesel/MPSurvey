#' Set Descriptions for Variables
#'
#' This function reads a JSON file from the specified path to get existing descriptions, and then allows the user to set new descriptions for variables that are not already described. The function returns a tibble with variable names and their corresponding descriptions.
#'
#' @param path A character string specifying the path to the JSON file. Default is NULL.
#' @param x A data frame or tibble containing the variables for which descriptions need to be set. Default is NULL.
#'
#' @return A tibble with two columns: `var` (variable names) and `desc` (descriptions).
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' descriptions <- set_descriptions(path = "path/to/your/jsonfile.json", x = your_dataframe)
#' }
set_descriptions <- function(path=NULL,
                             x=NULL){
  `%!in%`= Negate(`%in%`)
  descr <- get_descriptions(path=path)
  new_desc <- data.frame(var=c(names(x)), desc=NA)
  new_desc <- new_desc |>
    dplyr::filter(var %!in% descr$var)
  descriptions <- dplyr::bind_rows(new_desc, descr)
  descriptions <- DataEditR::data_edit(descriptions,
                                       col_readonly = c("var"),
                                       read_fun = NA, hide=TRUE)
  return(descriptions)
}
