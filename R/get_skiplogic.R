#' Extract Skip Logic from JSON File
#'
#' This function reads a JSON file from the specified path, extracts skip logic
#' rules, and returns a data frame with the skip logic information.
#'
#' @param path A character string specifying the path to the JSON file containing
#'   skip logic rules. Default is NULL.
#' @param CATI Logical. Defaults to FALSE. If TRUE, parse CATI-compatible
#'   manifests while preserving the legacy output format.
#'
#' @return A data frame containing the skip logic information, including
#'   variables, question IDs, question text, response values, and skip logic
#'   conditions.
#' @export
#'
#' @examples
#' \dontrun{
#' skip_logic <- get_skiplogic(path = "path/to/your/skiplogic.json")
#' skip_logic_cati <- get_skiplogic(path = "path/to/your/cati_manifest.json", CATI = TRUE)
#' }
get_skiplogic <- function(path = NULL, CATI = FALSE){

  if (!CATI) {
    x <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)[[1]]
    x <- tibble::as_tibble(x)

    codes <- data.frame()
    for(i in 2:nrow(x %>% dplyr::filter(!grepl("complet", x$title, ignore.case = TRUE)))){
      prompts <- x$steps[i][[1]]$store[!is.na(x$steps[i][[1]]$store)]
      ids <- x$steps[i][[1]]$id[!is.na(x$steps[i][[1]]$store)]
      types <- x$steps[i][[1]]$type[!is.na(x$steps[i][[1]]$store)]
      refusals <- x$steps[i][[1]]$refusal$skip_logic[!is.na(x$steps[i][[1]]$store)]
      relevants <- x$steps[i][[1]]$relevant[!is.na(x$steps[i][[1]]$store)]

      if(all(!is.na((x$steps[i][[1]]$prompt$en$ivr$text[!is.na(x$steps[i][[1]]$store)])))){
        quex <- x$steps[i][[1]]$prompt$en$ivr$text[!is.na(x$steps[i][[1]]$store)]
      }
      if(all(!is.na(x$steps[i][[1]]$prompt$en$sms[!is.na(x$steps[i][[1]]$store)]))){
        quex <- x$steps[i][[1]]$prompt$en$sms[which(x$steps[i][[1]]$type %in% c("multiple-choice", "numeric"))]
      }

      if(is.null(refusals)){
        refusals <- rep(NA, length(prompts))
      }
      if(is.null(relevants)){
        relevants <- rep(NA, length(prompts))
      }

      quex_df <- data.frame(
        "var" = prompts,
        "id" = ids,
        "type" = types,
        "quex" = quex,
        "refusal" = refusals,
        "relevants" = relevants
      )

      skip_df <- data.frame()
      for(q in 1:nrow(quex_df)){
        num_row = ifelse(quex_df$type[q] == "numeric",
                         nrow(x$steps[i][[1]]$ranges[which(x$steps[i][[1]]$store == quex_df$var[q])][[1]]),
                         nrow(x$steps[i][[1]]$choices[which(x$steps[i][[1]]$store == quex_df$var[q])][[1]]))

        if(quex_df$type[q] == "numeric" & !is.na(quex_df$refusal[q])){
          num_row = num_row + 1
        }

        temp_skip_df <- data.frame(
          "var" = rep(quex_df$var[q], num_row),
          "id" = rep(quex_df$id[q], num_row),
          "quex" = rep(quex_df$quex[q], num_row),
          "type" = rep(quex_df$type[q], num_row),
          "relevant" = rep(quex_df$relevants[q], num_row)
        )
        skip_df <- dplyr::bind_rows(skip_df, temp_skip_df)
      }

      option_df <- data.frame()
      for(z in unique(skip_df$var)){
        options <- NA

        if(quex_df$type[which(quex_df$var == z)] == "numeric"){
          options <- x$steps[i][[1]]$ranges[which(x$steps[i][[1]]$store == z)] |> dplyr::bind_rows()

          if(quex_df$type[which(quex_df$var == z)] == "numeric" &
             !is.na(quex_df$refusal[which(quex_df$var == z)])){
            options <- dplyr::bind_rows(
              options,
              data.frame(
                "value" = "REFUSED",
                "responses.refusal" = "#",
                "skip_logic" = quex_df$refusal[which(quex_df$var == z)]
              )
            )
          }
        } else if(quex_df$type[which(quex_df$var == z)] != "numeric"){
          options <- x$steps[i][[1]]$choices[which(x$steps[i][[1]]$store == z)] |> dplyr::bind_rows()
        }

        option_df <- dplyr::bind_rows(option_df, options)
      }

      if(is.null(names(option_df$responses))){
        responses <- option_df |> dplyr::select(dplyr::any_of(c("value", "skip_logic", "responses", "responses.refusal", "from", "to")))
        responses$response <- responses$responses
        responses <- responses |> dplyr::select(-dplyr::any_of("responses"))
      } else if(!is.null(option_df$responses$sms$en |> unlist())){
        responses <- option_df |> dplyr::select(dplyr::any_of(c("value", "skip_logic", "responses", "responses.refusal", "from", "to")))
        responses$response <- responses$responses$sms$en
        responses <- responses |> dplyr::select(-responses)
      } else if(!is.null(option_df$responses$ivr |> unlist())){
        responses <- option_df |> dplyr::select(dplyr::any_of(c("value", "skip_logic", "responses", "responses.refusal", "from", "to")))
        responses$response <- as.character(responses$responses$ivr) |> unlist()
        responses <- responses |> dplyr::select(-responses)
      } else{
        responses <- option_df |> dplyr::select(dplyr::any_of(c("value", "skip_logic", "responses", "responses.refusal", "from", "to")))
        responses$response <- responses$responses$ivr
        responses <- responses |> dplyr::select(-dplyr::any_of("responses"))
      }

      if(nrow(skip_df) > 0){
        responses_df <- data.frame("responses" = responses)

        if("responses.response" %in% names(responses_df)){
          responses_df <- responses_df |>
            dplyr::mutate(responses.response = unlist(as.character(responses.response))) |>
            dplyr::rowwise() |>
            dplyr::mutate(skip = ifelse(is.na(responses.skip_logic),
                                        "next question",
                                        unique(quex_df$var[which(quex_df$id == responses.skip_logic)])))
        } else{
          responses_df <- responses_df |>
            dplyr::mutate(responses.response = "NULL") |>
            dplyr::rowwise() |>
            dplyr::mutate(skip = ifelse(is.na(responses.skip_logic),
                                        "next question",
                                        unique(quex_df$var[which(quex_df$id == responses.skip_logic)])))
        }

        codes <- dplyr::bind_rows(codes, dplyr::bind_cols(skip_df, responses_df))
      }
    }

    codes <- codes |>
      dplyr::select(-c(id, responses.skip_logic)) |>
      dplyr::mutate(
        responses.response = ifelse(responses.response == "NULL" & var != "age",
                                    responses.responses.refusal,
                                    responses.response)
      ) |>
      dplyr::mutate(
        responses.value = gsub("^[0-9]+(?=[a-zA-Z])", "", responses.value, perl = TRUE)
      )

    return(codes)
  }

  raw <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE)

  .empty_skiplogic <- function() {
    data.frame(
      var = character(),
      quex = character(),
      type = character(),
      relevant = character(),
      responses.value = character(),
      responses.responses.refusal = character(),
      responses.from = character(),
      responses.to = character(),
      responses.response = character(),
      skip = character(),
      stringsAsFactors = FALSE
    )
  }

  .safe_chr <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    as.character(x[[1]])
  }

  .safe_relevant <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    if (is.logical(x) && length(x) == 1) return(as.character(x))
    as.character(x[[1]])
  }

  .prompt_text <- function(step) {
    txt <- NULL
    if (!is.null(step$prompt$en$sms) && length(step$prompt$en$sms) == 1) {
      txt <- step$prompt$en$sms
    } else if (!is.null(step$prompt$en$ivr$text) && length(step$prompt$en$ivr$text) == 1) {
      txt <- step$prompt$en$ivr$text
    }
    if (is.null(txt)) txt <- NA_character_
    as.character(txt)
  }

  .extract_response_code <- function(choice) {
    code <- NULL

    if (!is.null(choice$responses$sms$en) && length(choice$responses$sms$en) > 0) {
      code <- choice$responses$sms$en[[1]]
    }

    if (is.null(code) && !is.null(choice$responses$ivr) && length(choice$responses$ivr) > 0) {
      code <- choice$responses$ivr[[1]]
    }

    if (is.null(code) || identical(code, "")) {
      code <- NA_character_
    }

    as.character(code)
  }

  all_steps <- list()
  steps_out <- list()

  for (section in raw$steps) {
    if (is.null(section$steps) || length(section$steps) == 0) next

    for (step in section$steps) {
      if (is.null(step$store) || is.null(step$id) || is.null(step$type)) next

      all_steps[[length(all_steps) + 1]] <- list(
        var = .safe_chr(step$store),
        id = .safe_chr(step$id),
        type = .safe_chr(step$type),
        quex = .prompt_text(step),
        relevant = .safe_relevant(step$relevant),
        refusal_skip = .safe_chr(step$refusal$skip_logic),
        raw = step
      )

      if (step$type %in% c("multiple-choice", "numeric")) {
        steps_out[[length(steps_out) + 1]] <- list(
          var = .safe_chr(step$store),
          id = .safe_chr(step$id),
          type = .safe_chr(step$type),
          quex = .prompt_text(step),
          relevant = .safe_relevant(step$relevant),
          refusal_skip = .safe_chr(step$refusal$skip_logic),
          raw = step
        )
      }
    }
  }

  if (length(steps_out) == 0) {
    return(.empty_skiplogic())
  }

  quex_df <- dplyr::bind_rows(lapply(steps_out, function(s) {
    data.frame(
      var = s$var,
      id = s$id,
      type = s$type,
      quex = s$quex,
      refusal = s$refusal_skip,
      relevants = s$relevant,
      stringsAsFactors = FALSE
    )
  }))

  all_steps_df <- dplyr::bind_rows(lapply(all_steps, function(s) {
    data.frame(
      var = s$var,
      id = s$id,
      stringsAsFactors = FALSE
    )
  })) |>
    dplyr::distinct(id, .keep_all = TRUE)

  id_to_var <- stats::setNames(all_steps_df$var, all_steps_df$id)

  rows_out <- list()

  for (s in steps_out) {
    step <- s$raw
    step_type <- s$type
    step_var <- s$var
    step_id <- s$id
    step_quex <- s$quex
    step_relevant <- s$relevant
    step_refusal <- s$refusal_skip

    if (identical(step_type, "multiple-choice")) {
      if (!is.null(step$choices) && length(step$choices) > 0) {
        for (choice in step$choices) {
          skip_id <- .safe_chr(choice$skip_logic)
          skip_var <- if (!is.na(skip_id) && skip_id %in% names(id_to_var)) id_to_var[[skip_id]] else "next question"

          rows_out[[length(rows_out) + 1]] <- data.frame(
            var = step_var,
            id = step_id,
            quex = step_quex,
            type = step_type,
            relevant = step_relevant,
            responses.value = .safe_chr(choice$value),
            responses.skip_logic = skip_id,
            responses.responses.refusal = NA_character_,
            responses.from = NA_character_,
            responses.to = NA_character_,
            responses.response = .extract_response_code(choice),
            skip = skip_var,
            stringsAsFactors = FALSE
          )
        }
      }
    }

    if (identical(step_type, "numeric")) {
      numeric_rows <- list()

      if (!is.null(step$ranges) && length(step$ranges) > 0) {
        for (rg in step$ranges) {
          numeric_rows[[length(numeric_rows) + 1]] <- data.frame(
            var = step_var,
            id = step_id,
            quex = step_quex,
            type = step_type,
            relevant = step_relevant,
            responses.value = ifelse(is.null(rg$value), NA_character_, as.character(rg$value)),
            responses.skip_logic = .safe_chr(rg$skip_logic),
            responses.responses.refusal = ifelse(is.null(rg[["responses.refusal"]]), NA_character_, as.character(rg[["responses.refusal"]])),
            responses.from = ifelse(is.null(rg$from), NA_character_, as.character(rg$from)),
            responses.to = ifelse(is.null(rg$to), NA_character_, as.character(rg$to)),
            responses.response = NA_character_,
            stringsAsFactors = FALSE
          )
        }
      }

      if (length(numeric_rows) > 0) {
        numeric_df <- dplyr::bind_rows(numeric_rows) |>
          dplyr::distinct() |>
          dplyr::filter(
            is.na(responses.from) | is.na(responses.to) |
              !grepl("^[0-9]+$", responses.from) |
              !grepl("^[0-9]+$", responses.to) |
              as.numeric(responses.from) <= as.numeric(responses.to)
          )

        if (identical(step_var, "age") && any(numeric_df$responses.from == "999")) {
          numeric_df <- numeric_df |>
            dplyr::filter(!(responses.from == "99" &
                              toupper(trimws(ifelse(is.na(responses.value), "", responses.value))) %in% c("REFUSED", "")))
        }

        explicit_refusal_present <- FALSE
        if (nrow(numeric_df) > 0) {
          explicit_refusal_present <- any(
            (!is.na(numeric_df$responses.value) &
               toupper(trimws(numeric_df$responses.value)) %in% c("REFUSED", "REFUSAL")) |
              (!is.na(numeric_df$responses.from) &
                 numeric_df$responses.from %in% c("88", "99", "999"))
          )
        }

        if (!is.na(step_refusal) && !explicit_refusal_present) {
          numeric_df <- dplyr::bind_rows(
            numeric_df,
            data.frame(
              var = step_var,
              id = step_id,
              quex = step_quex,
              type = step_type,
              relevant = step_relevant,
              responses.value = "REFUSED",
              responses.skip_logic = step_refusal,
              responses.responses.refusal = "#",
              responses.from = NA_character_,
              responses.to = NA_character_,
              responses.response = NA_character_,
              stringsAsFactors = FALSE
            )
          )
        }

        numeric_df <- numeric_df |>
          dplyr::rowwise() |>
          dplyr::mutate(
            skip = ifelse(is.na(responses.skip_logic),
                          "next question",
                          ifelse(responses.skip_logic %in% names(id_to_var),
                                 id_to_var[[responses.skip_logic]],
                                 "next question"))
          ) |>
          dplyr::ungroup()

        rows_out[[length(rows_out) + 1]] <- numeric_df
      }
    }
  }

  if (length(rows_out) == 0) {
    return(.empty_skiplogic())
  }

  codes <- dplyr::bind_rows(rows_out) |>
    dplyr::select(-dplyr::any_of(c("id", "responses.skip_logic"))) |>
    dplyr::mutate(
      responses.response = ifelse(
        responses.response == "NULL" & var != "age",
        responses.responses.refusal,
        responses.response
      )
    ) |>
    dplyr::mutate(
      responses.response = ifelse(
        is.na(responses.response) & !is.na(responses.responses.refusal) & var != "age",
        responses.responses.refusal,
        responses.response
      )
    ) |>
    dplyr::mutate(
      responses.value = gsub("^[0-9]+(?=[a-zA-Z])", "", responses.value, perl = TRUE)
    )

  return(codes)
}
