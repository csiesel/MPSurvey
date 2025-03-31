#' recode
#'
#' @param x
#' @param vars
#' @param new_var
#'
#' @return asdf
#' @export
#'
#' @examples asdf
recode <- function(x, vars, new_var){

  grouped_df <- x |>
    dplyr::group_by(across(all_of(vars))) |>
    dplyr::summarise(n=dplyr::n()) |>
    dplyr::arrange(-n) |>
    dplyr::mutate(!!dplyr::enquo(new_var) := NA)

  grouped_df <- DataEditR::data_edit(grouped_df,
                                     col_readonly = c(vars, "n"),
                                     read_fun = NA, hide=TRUE) |>
    dplyr::select(-n)

  updated_df <- x |>
    dplyr::left_join(grouped_df, by=vars)

  return(updated_df)
}
