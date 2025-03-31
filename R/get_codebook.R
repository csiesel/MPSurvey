#' get_codebook
#'
#' @param path
#'
#' @return asdf
#' @export
#'
#' @examples asdf
get_codebook <- function(path = NULL){
  x <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)[[1]]
  x <- tibble::as_tibble(x)

  codes <- data.frame()
  for(i in 2:nrow(x)){
    print(i)
    prompts <- x$steps[i][[1]]$store[which(x$steps[i][[1]]$type=="multiple-choice")]
    prompt_df <- data.frame()
    for(q in prompts){
      print(q)
      num_row =  nrow(x$steps[i][[1]]$choices[which(x$steps[i][[1]]$store==q)][[1]])
      temp_prompt_df <- data.frame("var" = rep(q, num_row))
      prompt_df <- dplyr::bind_rows(prompt_df, temp_prompt_df)
    }

    choices <- x$steps[i][[1]]$choices |> dplyr::bind_rows()

    if(!is.null(choices$responses$sms$en |> unlist())){
      responses = choices$responses$sms$en |> unlist()
    }
    if(!is.null(choices$responses$ivr |> unlist())){
      responses = choices$responses$ivr |> unlist()
    }


    if(nrow(prompt_df)>0){
      responses_df <- data.frame("responses"=responses)
      choices_df <- choices |> dplyr::select(value)
      codes <- dplyr::bind_rows(codes, dplyr::bind_cols(prompt_df, choices_df, responses_df))

    }
  }
  codes <- codes |>
    dplyr::rename(values= "value",
                  options="responses",
                  grp="var") |>
    dplyr::select(grp, values, options)
  return(codes)
}
