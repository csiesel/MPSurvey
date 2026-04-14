#' label_data
#'
#' @param x a [data.frame()] object containing the raw survey data
#' @param codebook a [data.frame()] object containing columns `options`, `values`,
#'   `grp`, and optionally `orders`:
#'  - `options` : raw data values in the dataset
#'  - `values` : label values for each defined raw value in `options`
#'  - `grp` : the grouping value for the raw data and values. Often this will be
#'    at a question level but grouping may cut across multiple questions for
#'    common responses
#'  - `orders` : the order that the labels should appear as factor levels
#' @param CATI Logical. Defaults to FALSE. If TRUE, coerce inputs to improve
#'   matching for CATI-style explicit refusal codes while preserving the legacy
#'   output format.
#'
#' @return a labelled [data.frame()] object where the data values are replaced
#'   with the `values` column in `codebook` based on the data value defined by
#'   `options`.
#' @export
label_data <- function(x = NULL, codebook = NULL, CATI = FALSE){

  if (!CATI) {
    ldf <- matchmaker::match_df(x, codebook, from = "options", to = "values", by = "grp")

    ldf <- ldf %>%
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ gsub("^[0-9]+(?=[a-zA-Z])", "", ., perl = TRUE)
      ))

    return(ldf)
  }

  # CATI branch:
  # make matching robust when raw data are numeric but codebook options are character
  x_cati <- x
  cb_cati <- codebook

  # preserve original column names, only coerce values for matching
  x_cati[] <- lapply(x_cati, function(col) {
    if (is.factor(col)) col <- as.character(col)
    as.character(col)
  })

  if ("options" %in% names(cb_cati)) {
    cb_cati$options <- as.character(cb_cati$options)
  }

  if ("values" %in% names(cb_cati)) {
    cb_cati$values <- as.character(cb_cati$values)
  }

  if ("grp" %in% names(cb_cati)) {
    cb_cati$grp <- as.character(cb_cati$grp)
  }

  ldf <- matchmaker::match_df(
    x_cati,
    cb_cati,
    from = "options",
    to = "values",
    by = "grp"
  )

  # keep same post-processing behavior as legacy path
  ldf <- ldf %>%
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ gsub("^[0-9]+(?=[a-zA-Z])", "", ., perl = TRUE)
    ))

  return(ldf)
}
