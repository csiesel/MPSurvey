#' Recode Variables in a Data Frame
#'
#' This function recodes specified variables in a data frame by grouping them, summarizing the counts, and allowing the user to manually input new values for the recoded variable. The function returns an updated data frame with the recoded variable.
#'
#' @param x A data frame or tibble containing the data to be recoded.
#' @param vars A character vector of variable names to be recoded.
#' @param new_var A character string specifying the name of the new recoded variable.
#'
#' @return A data frame with the recoded variable.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' updated_data <- recode_var(x = your_data, vars = c("var1", "var2"), new_var = "new_var")
#' }
recode_var <- function(x, vars, new_var){
  grouped_df <- x |>
    dplyr::group_by(across(all_of(vars))) |>
    dplyr::summarise(n=dplyr::n()) |>
    rename_with(.fn = ~ paste0(.x, "_original")) |>
    rename("n"="n_original") |>
    dplyr::arrange(-n) |>
    dplyr::mutate(!!dplyr::enquo(new_var) := NA)

  grouped_df <- DataEditR::data_edit(grouped_df,
                                     col_readonly = c(paste0(vars, "_original"), "n"),
                                     read_fun = NA, hide=TRUE) |>
    dplyr::select(-n)

  vars2 <- paste0(vars, "_original")
  updated_df <- x |>
    merge(grouped_df, by.x = vars, by.y=vars2)

  dup_vars <- names(updated_df[grepl("\\.y", names(updated_df))])

  if(any(!is.na(dup_vars) | !is.null(dup_vars) | !length(dup_vars)==0)){
  # if(!is.na(dup_vars) | !length(dup_vars)==0){
    original_var <- gsub(".y", "", dup_vars)

    updated_df <- updated_df |>
      select(-original_var) |>
      rename(!!original_var := sym(dup_vars))
  }


  updated_df <- updated_df |>
    mutate(across(everything(), ~na_if(., "")))


  return(updated_df)
}
