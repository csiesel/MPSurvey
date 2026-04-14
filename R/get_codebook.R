#' Extract Codebook from JSON File
#'
#' This function reads a JSON file from the specified path, extracts the
#' codebook information, and returns a data frame with the variable names,
#' values, and response options.
#'
#' @param path A character string specifying the path to the JSON file
#'   containing the codebook information. Default is NULL.
#' @param CATI Logical. Defaults to FALSE. If TRUE, parse CATI-compatible
#'   manifests while preserving the legacy output format.
#'
#' @return A data frame containing the codebook information with columns:
#'   grp, values, options.
#' @export
get_codebook <- function(path = NULL, CATI = FALSE){

  .empty_codebook <- function() {
    data.frame(
      grp = character(),
      values = character(),
      options = character(),
      stringsAsFactors = FALSE
    )
  }

  if (!CATI) {
    x <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)[[1]]
    x <- tibble::as_tibble(x)

    codes <- data.frame()
    for(i in 2:nrow(x)){
      prompts <- x$steps[i][[1]]$store[which(x$steps[i][[1]]$type == "multiple-choice")]
      prompt_df <- data.frame()

      for(q in prompts){
        num_row <- nrow(x$steps[i][[1]]$choices[which(x$steps[i][[1]]$store == q)][[1]])
        temp_prompt_df <- data.frame("var" = rep(q, num_row))
        prompt_df <- dplyr::bind_rows(prompt_df, temp_prompt_df)
      }

      choices <- x$steps[i][[1]]$choices |> dplyr::bind_rows()

      if(!is.null(choices$responses$sms$en |> unlist())){
        responses <- choices$responses$sms$en |> unlist()
      }
      if(!is.null(choices$responses$ivr |> unlist())){
        responses <- choices$responses$ivr |> unlist()
      }

      if(nrow(prompt_df) > 0){
        responses_df <- data.frame("responses" = responses)
        choices_df <- choices |> dplyr::select(value)
        codes <- dplyr::bind_rows(codes, dplyr::bind_cols(prompt_df, choices_df, responses_df))
      }
    }

    codes <- codes |>
      dplyr::rename(values = "value",
                    options = "responses",
                    grp = "var") |>
      dplyr::select(grp, values, options) |>
      dplyr::mutate(
        values = gsub("^[0-9]+(?=[a-zA-Z])", "", values, perl = TRUE)
      )

    return(codes)
  }

  raw <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE)

  if (is.null(raw$steps) || length(raw$steps) == 0) {
    return(.empty_codebook())
  }

  out <- list()

  .append_row <- function(grp, values, options) {
    data.frame(
      grp = as.character(grp),
      values = as.character(values),
      options = as.character(options),
      stringsAsFactors = FALSE
    )
  }

  for (section in raw$steps) {

    if (is.null(section$steps) || length(section$steps) == 0) {
      next
    }

    for (step in section$steps) {

      if (is.null(step$type) || is.null(step$store)) {
        next
      }

      # only analytic question types
      if (!(step$type %in% c("multiple-choice", "numeric"))) {
        next
      }

      # multiple-choice questions
      if (identical(step$type, "multiple-choice")) {

        if (is.null(step$choices) || length(step$choices) == 0) {
          next
        }

        for (choice in step$choices) {

          if (is.null(choice$value)) {
            next
          }

          resp_code <- NULL

          # prefer sms/en
          if (!is.null(choice$responses$sms$en) && length(choice$responses$sms$en) > 0) {
            resp_code <- choice$responses$sms$en[[1]]
          }

          # fallback to ivr if present
          if (is.null(resp_code) && !is.null(choice$responses$ivr) && length(choice$responses$ivr) > 0) {
            resp_code <- choice$responses$ivr[[1]]
          }

          if (!is.null(resp_code) && !is.na(resp_code) && resp_code != "") {
            out[[length(out) + 1]] <- .append_row(
              grp = step$store,
              values = choice$value,
              options = resp_code
            )
          }
        }
      }

      # numeric questions: preserve legacy output shape by emitting refusal rows only
      if (identical(step$type, "numeric")) {

        numeric_rows <- list()

        if (!is.null(step$ranges) && length(step$ranges) > 0) {
          for (rg in step$ranges) {

            from_val <- if (!is.null(rg$from)) as.character(rg$from) else NA_character_
            value_val <- if (!is.null(rg$value)) as.character(rg$value) else NA_character_
            is_refusal <- isTRUE(rg$is_refusal)

            refusal_like <- FALSE

            if (is_refusal) {
              refusal_like <- TRUE
            }

            if (!is.na(value_val) && toupper(trimws(value_val)) %in% c("REFUSED", "REFUSAL")) {
              refusal_like <- TRUE
            }

            # CATI manifests often encode refusal as 88/99/999 in numeric ranges
            if (!is.na(from_val) && from_val %in% c("88", "99", "999")) {
              refusal_like <- TRUE
            }

            if (refusal_like && !is.na(from_val)) {
              numeric_rows[[length(numeric_rows) + 1]] <- .append_row(
                grp = step$store,
                values = "REFUSED",
                options = from_val
              )
            }
          }
        }

        # fallback: infer refusal code from prompt if ranges are messy or incomplete
        if (length(numeric_rows) == 0 && !is.null(step$prompt$en$sms)) {
          prompt_txt <- step$prompt$en$sms

          refusal_hits <- stringr::str_match_all(
            prompt_txt,
            "(\\b\\d{1,4})\\s*\\.\\s*(REFUSED|DON['’]?T KNOW|REFUSED/DON['’]?T KNOW)"
          )[[1]]

          if (!is.null(refusal_hits) && nrow(refusal_hits) > 0) {
            codes_found <- unique(refusal_hits[, 2])
            for (code in codes_found) {
              numeric_rows[[length(numeric_rows) + 1]] <- .append_row(
                grp = step$store,
                values = "REFUSED",
                options = code
              )
            }
          }
        }

        if (length(numeric_rows) > 0) {
          step_num_df <- dplyr::bind_rows(numeric_rows) |>
            dplyr::distinct()

          # latest CATI age item indicates 999 refusal; repeated 99 rows are manifest noise
          if (identical(step$store, "age") && any(step_num_df$options == "999")) {
            step_num_df <- step_num_df |>
              dplyr::filter(options == "999")
          }

          out[[length(out) + 1]] <- step_num_df
        }
      }
    }
  }

  if (length(out) == 0) {
    return(.empty_codebook())
  }

  codes <- dplyr::bind_rows(out) |>
    dplyr::distinct() |>
    dplyr::select(grp, values, options) |>
    dplyr::mutate(
      grp = as.character(grp),
      values = as.character(values),
      options = as.character(options),
      values = gsub("^[0-9]+(?=[a-zA-Z])", "", values, perl = TRUE)
    )

  return(codes)
}
