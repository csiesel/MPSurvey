#' Launch the recode_var Shiny app
#'
#' Opens an interactive Shiny app to build recode mappings and generate
#' copy/paste-ready dplyr code.
#'
#' @param data A data frame or tibble.
#'
#' @return Invisibly returns NULL.
#' @export
recode_var_app <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.")
  }

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for `recode_var_app()`. Please install it.")
  }

  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Package 'DT' is required for `recode_var_app()`. Please install it.")
  }

  data_name <- deparse(substitute(data))

  if (grepl("\\(", data_name)) {
    warning(
      "`data` was passed as an expression; generated code may not have a clean object name."
    )
  }

  app <- shiny::shinyApp(
    ui = recode_var_app_ui(),
    server = function(input, output, session) {
      recode_var_app_server(
        input = input,
        output = output,
        session = session,
        data = data,
        data_name = data_name
      )
    }
  )

  shiny::runApp(app)
  invisible(NULL)
}
