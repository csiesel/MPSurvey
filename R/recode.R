recode <- function(x, vars, new_var){

  grouped_df <- x %>%
    dplyr::group_by(across(all_of(vars))) %>%
    dplyr::summarise(n=n()) %>%
    dplyr::select(-n) %>%
    dplyr::mutate(!!enquo(new_var) := NA)

  updated_df <- DataEditR::data_edit(grouped_df, read_fun = NA, code=TRUE)


}
recode(data, vars=c("yn", "geo"), asdf)
