#' Get Descriptions from JSON File
#'
#' This function reads a JSON file from the specified path, extracts descriptions, and returns a tibble with variable names and their corresponding descriptions.
#'
#' @param path A character string specifying the path to the JSON file. Default is NULL.
#'
#' @return A tibble with two columns: `var` (variable names) and `desc` (descriptions).
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' descriptions <- get_descriptions("path/to/your/jsonfile.json")
#' }
get_descriptions <- function(path = NULL){
  x <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)[[1]]
  x <- tibble::as_tibble(x)

  codes <- data.frame()
  for(i in 2:nrow(x %>% filter(!grepl("complet", x$title, ignore.case=T)))){
    temp_df <- x$steps[i][[1]] |>
      dplyr::select(title, store)
    codes <- dplyr::bind_rows(codes, temp_df)
  }
  codes <- codes |>
    dplyr::filter(!is.na(store)) |>
    dplyr::rename(var = "store",
                  desc = "title")
  return(codes)
}
