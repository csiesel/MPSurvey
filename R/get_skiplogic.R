#' Extract Skip Logic from JSON File
#'
#' This function reads a JSON file from the specified path, extracts skip logic rules, and returns a data frame with the skip logic information.
#'
#' @param path A character string specifying the path to the JSON file containing skip logic rules. Default is NULL.
#'
#' @return A data frame containing the skip logic information, including variables, question IDs, question text, response values, and skip logic conditions.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' skip_logic <- get_skiplogic(path = "path/to/your/skiplogic.json")
#' }
get_skiplogic <- function(path = NULL){
  x <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)[[1]]
  x <- tibble::as_tibble(x)

  codes <- data.frame()
  for(i in 2:nrow(x %>% filter(!grepl("complete", x$title, ignore.case=T)))){
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


    quex_df <- data.frame("var"=prompts,
                          "id"=ids,
                          "type"=types,
                          "quex"=quex,
                          "refusal"=refusals,
                          "relevants"=relevants)

    skip_df <- data.frame()
    for(q in 1:nrow(quex_df)){
      # print(q)
      num_row = ifelse(quex_df$type[q]=="numeric",
                      nrow(x$steps[i][[1]]$ranges[which(x$steps[i][[1]]$store==quex_df$var[q])][[1]]),
                      nrow(x$steps[i][[1]]$choices[which(x$steps[i][[1]]$store==quex_df$var[q])][[1]]))


      if(quex_df$type[q]=="numeric" & !is.na(quex_df$refusal[q])){
        # cat(("FLAG1\n"))
        num_row = num_row + 1
      }

      temp_skip_df <- data.frame("var" = rep(quex_df$var[q], num_row),
                                   "id" = rep(quex_df$id[q], num_row),
                                 "quex" = rep(quex_df$quex[q], num_row),
                                 "type" = rep(quex_df$type[q], num_row),
                                 "relevant" = rep(quex_df$relevants[q], num_row))
      skip_df <- dplyr::bind_rows(skip_df, temp_skip_df)
    }

    option_df <- data.frame()
    for(z in unique(skip_df$var)){
      # print(z)
      options <- NA

      if(quex_df$type[which(quex_df$var==z)]=="numeric"){
        options <- x$steps[i][[1]]$ranges[which(x$steps[i][[1]]$store==z)] |> dplyr::bind_rows()

        if(quex_df$type[which(quex_df$var==z)]=="numeric" & !is.na(quex_df$refusal[which(quex_df$var==z)])){
          options <- dplyr::bind_rows(options, data.frame("value"="REFUSED",
                                                          "responses.refusal"="#",
                                                          "skip_logic"=quex_df$refusal[which(quex_df$var==z)]))
        }
      } else if(quex_df$type[which(quex_df$var==z)]!="numeric"){
        options <- x$steps[i][[1]]$choices[which(x$steps[i][[1]]$store==z)] |> dplyr::bind_rows()
      }

      option_df <- dplyr::bind_rows(option_df, options)

      # print(options)
    }


    if(is.null(names(option_df$responses))){
      responses <- option_df |> select(dplyr::any_of(c("value", "skip_logic", "responses", "responses.refusal", "from", "to")))
      responses$response <- responses$responses
      responses <- responses |>
        select(-any_of("responses"))
    } else if(!is.null(option_df$responses$sms$en |> unlist())){
      responses <- option_df |> select(dplyr::any_of(c("value", "skip_logic", "responses", "responses.refusal", "from", "to")))
      responses$response <- responses$responses$sms$en
      responses <- responses |>
        select(-responses)
    } else if(!is.null(option_df$responses$ivr |> unlist())){
      responses <- option_df |> select(dplyr::any_of(c("value", "skip_logic", "responses", "responses.refusal", "from", "to")))
      responses$response <- as.character(responses$responses$ivr) |> unlist()
      responses <- responses |>
        select(-responses)
    } else{
      responses <- option_df |> select(dplyr::any_of(c("value", "skip_logic", "responses", "responses.refusal", "from", "to")))
      responses$response <- responses$responses$ivr
      responses <- responses |>
        select(-dplyr::any_of("responses"))
    }


    if(nrow(skip_df)>0){
      responses_df <- data.frame("responses"=responses)

     if("responses.response" %in% names(responses_df)){
       responses_df <- responses_df |>
         mutate(responses.response = unlist(as.character(responses.response))) |>
         rowwise() |>
         mutate(skip = ifelse(is.na(responses.skip_logic),
                              "next question",
                              unique(quex_df$var[which(quex_df$id==responses.skip_logic)])))
     } else{
       responses_df <- responses_df |>
         mutate(responses.response = "NULL") |>
         rowwise() |>
         mutate(skip = ifelse(is.na(responses.skip_logic),
                              "next question",
                              unique(quex_df$var[which(quex_df$id==responses.skip_logic)])))

     }

      codes <- dplyr::bind_rows(codes, dplyr::bind_cols(skip_df, responses_df))

    }
  }


  codes <- codes |>
    select(-c(id, responses.skip_logic)) |>
    mutate(responses.response=ifelse(responses.response=="NULL" & var!="age",
                                     responses.responses.refusal,
                                     responses.response))

  return(codes)

}
