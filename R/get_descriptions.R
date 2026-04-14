#' Get Descriptions from JSON File
#'
#' This function reads a JSON file from the specified path, extracts
#' descriptions, and returns a tibble/data frame with variable names and their
#' corresponding descriptions.
#'
#' @param path A character string specifying the path to the JSON file.
#'   Default is NULL.
#' @param CATI Logical. Defaults to FALSE. If TRUE, parse CATI-compatible
#'   manifests while preserving the legacy output format.
#'
#' @return A tibble/data frame with two columns: `var` and `desc`.
#' @export
#'
#' @examples
#' \dontrun{
#' descriptions <- get_descriptions("path/to/your/jsonfile.json")
#' descriptions_cati <- get_descriptions("path/to/your/cati_manifest.json", CATI = TRUE)
#' }
get_descriptions <- function(path = NULL, CATI = FALSE){

  if (!CATI) {
    x <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)[[1]]
    x <- tibble::as_tibble(x)

    codes <- data.frame()
    for(i in 2:nrow(x %>% dplyr::filter(!grepl("complet", x$title, ignore.case = TRUE)))){
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

  raw <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE)

  if (is.null(raw$steps) || length(raw$steps) == 0) {
    return(data.frame(
      var = character(),
      desc = character(),
      stringsAsFactors = FALSE
    ))
  }

  .safe_chr <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    as.character(x[[1]])
  }

  .prompt_text <- function(step) {
    if (!is.null(step$prompt$en$sms) && length(step$prompt$en$sms) == 1) {
      return(as.character(step$prompt$en$sms))
    }
    if (!is.null(step$prompt$en$ivr$text) && length(step$prompt$en$ivr$text) == 1) {
      return(as.character(step$prompt$en$ivr$text))
    }
    if (!is.null(step$title) && length(step$title) == 1) {
      return(as.character(step$title))
    }
    NA_character_
  }

  out <- list()

  for (section in raw$steps) {
    if (is.null(section$steps) || length(section$steps) == 0) next

    for (step in section$steps) {
      if (is.null(step$store) || is.null(step$type)) next

      # analytic variables only
      if (!(step$type %in% c("multiple-choice", "numeric"))) next

      out[[length(out) + 1]] <- data.frame(
        var = .safe_chr(step$store),
        desc = .prompt_text(step),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(out) == 0) {
    return(data.frame(
      var = character(),
      desc = character(),
      stringsAsFactors = FALSE
    ))
  }

  codes <- dplyr::bind_rows(out) |>
    dplyr::filter(!is.na(var)) |>
    dplyr::distinct(var, .keep_all = TRUE)

  return(codes)
}
