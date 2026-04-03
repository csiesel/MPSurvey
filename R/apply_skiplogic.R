#' Apply Skip Logic to Survey Data
#'
#' This function applies skip logic to a survey data frame based on the skip
#' logic rules defined in a JSON file. It modifies the survey data frame by
#' setting values to "NOT APPLICABLE" where the skip logic conditions are met.
#'
#' @param path A character string specifying the path to the JSON file
#'   containing skip logic rules. Default is NULL.
#' @param x A data frame or tibble containing the survey data. Default is NULL.
#' @param CATI Logical. Defaults to FALSE. If TRUE, use CATI-compatible skip
#'   logic parsing while preserving the legacy output behavior.
#'
#' @return A modified data frame with skip logic applied.
#' @export
#'
#' @examples
#' \dontrun{
#' modified_data <- apply_skiplogic(path = "path/to/your/skiplogic.json", x = your_survey_data)
#' modified_data_cati <- apply_skiplogic(path = "path/to/your/cati_manifest.json", x = your_survey_data, CATI = TRUE)
#' }
apply_skiplogic <- function(path = NULL, x = NULL, CATI = FALSE){

  if (!CATI) {
    skips <- get_skiplogic(path = path)
    skips <- skips |>
      dplyr::filter(!grepl("intro|age|gender", var, ignore.case = TRUE))

    skip_vars <- unique(skips$skip)[
      !is.na(unique(skips$skip)) &
        unique(skips$skip) != "next question" &
        unique(skips$skip) %in% names(x)
    ]

    for(i in skip_vars){
      cat(paste0("\n\n***** Skip Logic Variable: ", i, " *****\n"))
      parent_vars <- unique(skips$var[which(skips$skip == i)])

      x <- x |>
        dplyr::mutate(!!rlang::sym(i) := dplyr::na_if(!!rlang::sym(i), ""))

      if(!all(is.na(skips$relevant[which(skips$var == i)])) &
         all(skips$relevant[which(skips$var == i)] != FALSE) &
         length(parent_vars) == 0 &
         all(skips$relevant[which(skips$var == i)]) == FALSE){
        x <- x |>
          dplyr::mutate(!!rlang::sym(i) := !!rlang::sym(i))

      } else if(length(parent_vars) > 1){

        multiple_skips <- skips %>%
          dplyr::filter(var %in% parent_vars,
                        skip == i) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(test = ifelse(type == "numeric" & !is.na(`responses.from`),
                                      list(seq(`responses.from`, `responses.to`)),
                                      list(responses.value)))

        if(length(parent_vars) == 2){
          x <- x |>
            dplyr::rowwise() |>
            dplyr::mutate(
              !!rlang::sym(i) := dplyr::case_when(
                (!!rlang::sym(parent_vars[1]) %in% unlist(multiple_skips$test[which(multiple_skips$var == parent_vars[1])])) |
                  (!!rlang::sym(parent_vars[2]) %in% unlist(multiple_skips$test[which(multiple_skips$var == parent_vars[2])])) ~ !!rlang::sym(i),
                TRUE ~ "NOT APPLICABLE"
              )
            )
        }

      } else {
        for(q in parent_vars){
          cat(paste0("Parent variable: ", q, "\n"))
          response_values <- skips$responses.value[which(skips$var == q & skips$skip == i)]

          if(unique(skips$type[which(skips$var == q)]) == "numeric"){
            low <- skips$responses.from[which(skips$var == q & skips$skip == i)]
            low <- low[!is.na(low)]
            high <- skips$responses.to[which(skips$var == q & skips$skip == i)]
            high <- high[!is.na(high)]
            skip <- skips$responses.value[which(skips$var == q & skips$skip == i)]
            skip <- skip[!is.na(skip)]

            cat(paste0("Response values: from ", low, " to ", high, "\n", "Skip values: ", skip, "\n"))

            if(length(skip) == 0){
              x <- x |>
                dplyr::mutate(
                  !!rlang::sym(i) := dplyr::case_when(
                    !(as.numeric(!!rlang::sym(q)) >= low & as.numeric(!!rlang::sym(q)) <= high) ~ "NOT APPLICABLE",
                    is.na(as.numeric(!!rlang::sym(q))) ~ "NOT APPLICABLE",
                    TRUE ~ !!rlang::sym(i)
                  )
                )
            } else{
              x <- x |>
                dplyr::mutate(
                  !!rlang::sym(i) := dplyr::case_when(
                    !!rlang::sym(q) == "NOT APPLICABLE" ~ "NOT APPLICABLE",
                    !(as.numeric(!!rlang::sym(q)) >= low & as.numeric(!!rlang::sym(q)) <= high) & !(!!rlang::sym(q) == skip) ~ "NOT APPLICABLE",
                    TRUE ~ !!rlang::sym(i)
                  )
                )
            }

          } else {
            cat(paste0("Response values: ", response_values, "\n"))
            x <- x |>
              dplyr::mutate(
                !!rlang::sym(i) := dplyr::case_when(
                  !(!!rlang::sym(q) %in% response_values) ~ "NOT APPLICABLE",
                  TRUE ~ !!rlang::sym(i)
                )
              )
          }
        }
      }
    }

    return(x)
  }

  skips <- get_skiplogic(path = path, CATI = TRUE)

  # keep original values for CATI safeguard checks
  x_orig <- x

  skips <- skips |>
    dplyr::filter(!grepl("intro|age|gender|sex", var, ignore.case = TRUE))

  if (nrow(skips) == 0) {
    return(x)
  }

  skip_vars <- unique(skips$skip)[
    !is.na(unique(skips$skip)) &
      unique(skips$skip) != "next question" &
      unique(skips$skip) %in% names(x)
  ]

  if (length(skip_vars) == 0) {
    return(x)
  }

  .to_num <- function(z) suppressWarnings(as.numeric(as.character(z)))

  .in_numeric_rule <- function(parent_value, rules_df) {
    if (length(parent_value) == 0 || is.na(parent_value) || parent_value == "" || parent_value == "NOT APPLICABLE") {
      return(FALSE)
    }

    pv_num <- .to_num(parent_value)

    for (r in seq_len(nrow(rules_df))) {
      low_chr <- rules_df$responses.from[r]
      high_chr <- rules_df$responses.to[r]
      value_chr <- rules_df$responses.value[r]

      low_num <- .to_num(low_chr)
      high_num <- .to_num(high_chr)
      value_up <- ifelse(is.na(value_chr), NA_character_, toupper(trimws(as.character(value_chr))))

      # explicit refusal-like rule
      if (!is.na(value_up) && value_up %in% c("REFUSED", "REFUSAL")) {
        if ((!is.na(low_num) && !is.na(pv_num) && pv_num == low_num) ||
            as.character(parent_value) == as.character(low_chr)) {
          return(TRUE)
        }
      }

      # normal numeric range rule
      if (!is.na(low_num) && !is.na(high_num) && !is.na(pv_num)) {
        if (pv_num >= low_num && pv_num <= high_num) {
          return(TRUE)
        }
      }
    }

    FALSE
  }

  .in_choice_rule <- function(parent_value, rules_df) {
    if (length(parent_value) == 0 || is.na(parent_value) || parent_value == "" || parent_value == "NOT APPLICABLE") {
      return(FALSE)
    }

    valid_vals <- unique(as.character(rules_df$responses.value[!is.na(rules_df$responses.value)]))
    as.character(parent_value) %in% valid_vals
  }

  .all_routes_to_target_if_answered <- function(q_rules_all, target_name) {
    if (is.null(q_rules_all) || nrow(q_rules_all) == 0) return(FALSE)
    rule_targets <- unique(as.character(q_rules_all$skip[!is.na(q_rules_all$skip)]))
    length(rule_targets) == 1 && identical(rule_targets, target_name)
  }

  .has_any_answer <- function(v) {
    !(length(v) == 0 || is.na(v) || v == "" || v == "NOT APPLICABLE")
  }

  for (i in skip_vars) {
    cat(paste0("\n\n***** Skip Logic Variable: ", i, " *****\n"))

    parent_vars <- unique(skips$var[which(skips$skip == i)])
    parent_vars <- parent_vars[parent_vars %in% names(x)]

    x <- x |>
      dplyr::mutate(!!rlang::sym(i) := dplyr::na_if(as.character(!!rlang::sym(i)), ""))

    if (length(parent_vars) == 0) {
      next
    }

    # Multiple parent variables: keep value if any parent routes here
    if (length(parent_vars) > 1) {

      allowed <- rep(FALSE, nrow(x))

      for (q in parent_vars) {
        cat(paste0("Parent variable: ", q, "\n"))

        q_rules <- skips |>
          dplyr::filter(var == q, skip == i)

        q_type <- unique(q_rules$type)
        q_type <- q_type[!is.na(q_type)][1]

        all_to_same_target <- .all_routes_to_target_if_answered(
          skips |> dplyr::filter(var == q),
          i
        )

        if (all_to_same_target) {
          q_allow <- vapply(x_orig[[q]], .has_any_answer, logical(1))
        } else if (identical(q_type, "numeric")) {
          q_allow <- vapply(x[[q]], function(v) .in_numeric_rule(v, q_rules), logical(1))
        } else {
          q_allow <- vapply(x[[q]], function(v) .in_choice_rule(v, q_rules), logical(1))
        }

        allowed <- allowed | q_allow
      }

      x[[i]] <- ifelse(allowed, x[[i]], "NOT APPLICABLE")
      next
    }

    # Single parent variable
    q <- parent_vars[1]
    cat(paste0("Parent variable: ", q, "\n"))

    q_rules <- skips |>
      dplyr::filter(var == q, skip == i)

    q_type <- unique(q_rules$type)
    q_type <- q_type[!is.na(q_type)][1]

    all_to_same_target <- .all_routes_to_target_if_answered(
      skips |> dplyr::filter(var == q),
      i
    )

    if (identical(q_type, "numeric")) {

      cat(
        paste0(
          "Response values: from ",
          paste(unique(q_rules$responses.from[!is.na(q_rules$responses.from)]), collapse = ", "),
          " to ",
          paste(unique(q_rules$responses.to[!is.na(q_rules$responses.to)]), collapse = ", "),
          "\n",
          "Skip values: ",
          paste(unique(q_rules$responses.value[!is.na(q_rules$responses.value)]), collapse = ", "),
          "\n"
        )
      )

      if (all_to_same_target) {
        allow <- vapply(x_orig[[q]], .has_any_answer, logical(1))
      } else {
        allow <- vapply(x[[q]], function(v) .in_numeric_rule(v, q_rules), logical(1))
      }

      parent_na <- is.na(x[[q]]) | x[[q]] == "" | x[[q]] == "NOT APPLICABLE"
      x[[i]] <- ifelse(parent_na | !allow, "NOT APPLICABLE", x[[i]])

    } else {

      response_values <- unique(q_rules$responses.value[!is.na(q_rules$responses.value)])
      cat(paste0("Response values: ", paste(response_values, collapse = ", "), "\n"))

      if (all_to_same_target) {
        allow <- vapply(x_orig[[q]], .has_any_answer, logical(1))
      } else {
        allow <- vapply(x[[q]], function(v) .in_choice_rule(v, q_rules), logical(1))
      }

      parent_na <- is.na(x[[q]]) | x[[q]] == "" | x[[q]] == "NOT APPLICABLE"
      x[[i]] <- ifelse(parent_na | !allow, "NOT APPLICABLE", x[[i]])
    }
  }

  return(x)
}
