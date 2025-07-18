#' Apply Skip Logic to Survey Data
#'
#' This function applies skip logic to a survey data frame based on the skip logic rules defined in a JSON file. It modifies the survey data frame by setting values to "NOT APPLICABLE" where the skip logic conditions are met.
#'
#' @param path A character string specifying the path to the JSON file containing skip logic rules. Default is NULL.
#' @param x A data frame or tibble containing the survey data. Default is NULL.
#'
#' @return A modified data frame with skip logic applied.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' modified_data <- apply_skiplogic(path = "path/to/your/skiplogic.json", x = your_survey_data)
#' }
apply_skiplogic <- function(path=NULL, x=NULL){
  skips <-get_skiplogic(path=path)
  # skips <- skips |>
  #   filter(!grepl("intro|age", var, ignore.case=TRUE)) |>
  #   filter(!grepl("1|smoke2", skip))
  skips <- skips |>
    filter(!grepl("intro|age|gender", var, ignore.case=TRUE))



  skip_vars <- unique(skips$skip)[!is.na(unique(skips$skip)) & unique(skips$skip)!="next question" & unique(skips$skip) %in% names(x)]

  for(i in (skip_vars)){
    cat(paste0("\n\n***** Skip Logic Variable: ", i, " *****\n"))
    parent_vars <- unique(skips$var[which(skips$skip==i)])

    x <- x |>
      mutate(!!sym(i) := na_if(!!sym(i), ""))


    if(!all(is.na(skips$relevant[which(skips$var==i)])) & all(skips$relevant[which(skips$var==i)]!=FALSE) & length(parent_vars)==0 & all(skips$relevant[which(skips$var==i)])==FALSE){
      x <- x |>
        mutate(!!sym(i) := !!sym(i))
    } else if(length(parent_vars)>1){
      multiple_skips <- skips %>%
        filter(var %in% parent_vars,
               skip==i) %>%
        rowwise() %>%
        mutate(test = ifelse(type=="numeric" & !is.na(`responses.from`),
                                           list(seq(`responses.from`, `responses.to`)),
                                           list(responses.value)))


      if(length(parent_vars)==2){
        x <- x |>
          rowwise() |>
          mutate(!!sym(i) := case_when((!!sym(parent_vars[1]) %in% unlist(multiple_skips$test[which(multiple_skips$var==parent_vars[1])])) |
                                         (!!sym(parent_vars[2]) %in% unlist(multiple_skips$test[which(multiple_skips$var==parent_vars[2])])) ~ !!sym(i),
                                       TRUE ~ "NOT APPLICABLE"))

      } else if(length(parent_vars)==3){


      }


    } else{
      for(q in parent_vars){
        cat(paste0("Parent variable: ", q, "\n"))
        response_values <- skips$responses.value[which(skips$var == q & skips$skip == i)]

        if(unique(skips$type[which(skips$var==q)])=="numeric"){
          low <- skips$responses.from[which(skips$var == q & skips$skip == i)]
          low <- low[!is.na(low)]
          high <- skips$responses.to[which(skips$var == q & skips$skip == i)]
          high <- high[!is.na(high)]
          skip <- skips$responses.value[which(skips$var == q & skips$skip == i)]
          skip <- skip[!is.na(skip)]

          cat(paste0("Response values: from ", low, " to ", high,"\n", "Skip values: ", skip, "\n"))

          if(length(skip)==0){
            x <- x |>
              mutate(!!sym(i) := case_when(!(as.numeric(!!sym(q)) >=low & as.numeric(!!sym(q)) <=high) ~ "NOT APPLICABLE",
                                           is.na(as.numeric(!!sym(q))) ~ "NOT APPLICABLE",
                                           TRUE ~ !!sym(i)))

          } else{
            x <- x |>
              mutate(!!sym(i) := case_when(!!sym(q)=="NOT APPLICABLE" ~ "NOT APPLICABLE",
                                           !(as.numeric(!!sym(q)) >=low & as.numeric(!!sym(q)) <=high) & !(!!sym(q)==skip) ~ "NOT APPLICABLE",
                                           # is.na(as.numeric(!!sym(q))) ~ "NOT APPLICABLE",
                                           TRUE ~ !!sym(i)))
          }

        } else {
          cat(paste0("Response values: ", response_values,"\n"))
          x <- x |>
            mutate(!!sym(i) := case_when(!(!!sym(q) %in% response_values) ~ "NOT APPLICABLE",
                                         TRUE ~ !!sym(i)))
        }
      }

    }





  }
  return(x)
}
