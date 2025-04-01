#' get_descriptions
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
get_descriptions <- function(path = NULL){
  x <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)[[1]]
  x <- tibble::as_tibble(x)

  codes <- data.frame()
  for(i in 2:nrow(x)){
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
