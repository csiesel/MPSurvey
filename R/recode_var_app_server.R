#' Server for the recode_var Shiny app
#'
#' @param input,output,session Standard Shiny server arguments.
#' @param data A data frame or tibble passed into the app.
#' @param data_name A character string naming the data object used in emitted code.
#'
#' @return Called for side effects.
#' @keywords internal
recode_var_app_server <- function(input, output, session, data, data_name) {

  dat <- shiny::reactive({
    data
  })

  shiny::observe({
    shiny::updateSelectInput(
      session = session,
      inputId = "vars",
      choices = names(dat())
    )
  })

  recode_map <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$build_map, {
    shiny::req(dat(), input$vars)

    map_df <- dat() %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(input$vars))) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(n)) %>%
      dplyr::mutate(new_value = NA_character_)

    recode_map(map_df)
  })

  output$map_table <- DT::renderDT({
    shiny::req(recode_map())

    df <- recode_map()
    editable_col <- which(names(df) == "new_value") - 1

    DT::datatable(
      df,
      editable = list(
        target = "cell",
        disable = list(
          columns = setdiff(seq_along(df) - 1, editable_col)
        )
      ),
      rownames = FALSE,
      class = "cell-border stripe nowrap",
      extensions = "Scroller",
      options = list(
        scrollX = TRUE,
        # scrollY = "60vh",
        autoWidth = TRUE,
        paging = FALSE,
        dom = "t"
      )
    )
  }, server = FALSE)

  shiny::observeEvent(input$map_table_cell_edit, {
    info <- input$map_table_cell_edit
    df <- recode_map()
    df[info$row, info$col + 1] <- info$value
    recode_map(df)
  })

  mapping_table <- shiny::reactive({
    shiny::req(recode_map(), input$vars)

    recode_map() %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(input$vars), as.character)) %>%
      dplyr::select(dplyr::all_of(input$vars), new_value)
  })

  generated_code <- shiny::reactive({
    shiny::req(input$new_var, input$vars, recode_map())

    map_tbl <- mapping_table() %>%
      dplyr::filter(!is.na(new_value), new_value != "")

    if (nrow(map_tbl) == 0) {
      return("# No recode values entered yet.")
    }

    vars <- input$vars

    if (length(vars) == 1) {
      src <- vars[[1]]

      old_vals <- paste0(
        "\"",
        gsub("\"", "\\\\\"", map_tbl[[src]]),
        "\""
      )

      new_vals <- paste0(
        "\"",
        gsub("\"", "\\\\\"", map_tbl$new_value),
        "\""
      )

      named_vec <- paste0(
        "c(\n  ",
        paste0(new_vals, " = ", old_vals, collapse = ",\n  "),
        "\n)"
      )

      keep_line <- if (isTRUE(input$keep_unmatched)) {
        paste0("      TRUE ~ as.character(.data[[\"", src, "\"]])")
      } else {
        "      TRUE ~ NA_character_"
      }

      case_lines <- paste0(
        "      as.character(.data[[\"",
        src,
        "\"]]) == ",
        old_vals,
        " ~ ",
        new_vals,
        collapse = ",\n"
      )

      return(
        paste0(
          "# Option 1: compact lookup vector\n",
          "recode_map <- ", named_vec, "\n\n",
          data_name, " <- ", data_name, " |>\n",
          "  dplyr::mutate(\n",
          "    ", input$new_var, " = dplyr::recode(\n",
          "      as.character(.data[[\"", src, "\"]]),\n",
          "      !!!recode_map",
          if (isTRUE(input$keep_unmatched)) {
            paste0(",\n      .default = as.character(.data[[\"", src, "\"]])\n")
          } else {
            ",\n      .default = NA_character_\n"
          },
          "    )\n",
          "  )\n\n",
          "# Option 2: explicit case_when\n",
          data_name, " <- ", data_name, " |>\n",
          "  dplyr::mutate(\n",
          "    ", input$new_var, " = dplyr::case_when(\n",
          case_lines, ",\n",
          keep_line, "\n",
          "    )\n",
          "  )\n"
        )
      )
    }

    tribble_header <- paste0("  ~", c(vars, "new_value"), collapse = ", ")

    tribble_rows <- apply(map_tbl, 1, function(row) {
      vals <- vapply(
        row,
        function(v) paste0("\"", gsub("\"", "\\\\\"", as.character(v)), "\""),
        character(1)
      )
      paste0("  ", paste(vals, collapse = ", "))
    })

    by_clause <- paste0(
      "c(",
      paste0("\"", vars, "\"", collapse = ", "),
      ")"
    )

    last_var <- vars[length(vars)]

    paste0(
      "# Creating variable ", input$new_var, " from ", paste0(vars, collapse=", "), "\n",
      "lookup <- tibble::tribble(\n",
      tribble_header, ",\n",
      paste(tribble_rows, collapse = ",\n"), "\n",
      ")\n\n",
      data_name, " <- ", data_name, " %>%\n",
      "  mutate(across(all_of(", by_clause, "), as.character)) %>%\n",
      "  left_join(lookup, by = ", by_clause, ") %>%\n",
      "  mutate(\"", input$new_var, "\" := new_value) %>%\n",
      "  select(-new_value) %>%\n",
      "  relocate( all_of(\"", input$new_var, "\"), .after = all_of(\"", last_var, "\"))\n"
    )
  })

  output$code_out <- shiny::renderText({
    generated_code()
  })

  shiny::observeEvent(input$copy_success, {
    shiny::showNotification(
      shiny::tagList("\u2714", " Code copied to clipboard"),
      type = "message",
      duration = 2
    )
  })
}

