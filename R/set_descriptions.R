#' set_descriptions
#'
#' @param path
#' @param x
#'
#' @return
#' @export
#'
#' @examples
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
