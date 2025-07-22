#' label_data
#'
#' @param x a [data.frame()] object containing the raw survey data
#' @param codebook a [data.frame()] object containing columns `options`, `values`, `grp`, and `orders`:
#'  - `options` : raw data values in the dataset
#'  - `values` : label values for each defined raw value in `options`
#'  - `grp` : the grouping value for the raw data and values. Often this will be at a question level but grouping may cut across multiple questions for common responses (i.e. 'yn' for yes and no questions)
#'  - `orders` : the order that the labels should appear as factor levels
#'
#' @return a labelled [data.frame()] object where the data values are replaced with the `values` column in `codebook` based on the data value defined by `options`.
#' @export
#'
#' @examples
#' # Codebook data frame
#' cb = data.frame(
#'   options = c(1, 2, 1, 2, 3),
#'   values = c("Yes", "No", "Urban", "Rural", "Other"),
#'   grp = c("yn", "yn", "geo", "geo", "geo"),
#'   orders = c(1, 2, 1, 2, 3)
#' )
#'
#'# Raw, unlabeled dataset
#' data = data.frame(
#'   yn = c(1, 1, 2),
#'   geo = c(3, 1, 2)
#' )
#'
#'# Labeled output
#'label_data(data, cb)
#'

label_data <- function(x=NULL, codebook=NULL){

  ldf <- matchmaker::match_df(x, codebook, from = "options", to = "values", by = "grp")

  # Removing numbers before characters
  ldf <- ldf %>%
    mutate(across(everything(), ~ gsub("^[0-9]+(?=[a-zA-Z])", "", ., perl = TRUE)))


  return(ldf)

}

