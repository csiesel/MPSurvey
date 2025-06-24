#' Visualize Questionnaire Skip Logic
#'
#' This function visualizes the skip logic of a questionnaire based on the skip logic rules defined in a JSON file. It creates a directed graph showing the flow of questions and their skip patterns.
#'
#' @param path A character string specifying the path to the JSON file containing skip logic rules. Default is NULL.
#' @param file A character string specifying the file name to save the graph visualization (.pdf, .svg, .png). Default is NULL.
#' @param responses A dataframe from the output of get_inr and the 'responses' dataframe from the list output. Default is NULL.
#'
#' @return A graph object visualizing the skip logic of the questionnaire.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' visualize_quex(path = "path/to/your/skiplogic.json", file = "skip_logic_graph.png")
#' }
visualize_quex <- function(path=NULL, file=NULL, responses=NULL){
  skips <-get_skiplogic(path=path)

  skips <- skips  %>%
    mutate(skip = ifelse(skip=="next question", NA, skip))



  # Function to fill NA values in 'skip' column
  fill_na_skip <- function(df) {
    # Get the indices of rows where 'relevant' is TRUE
    relevant_indices <- which(df$relevant == TRUE)

    # Loop through the 'skip' column
    for (i in seq_along(df$skip)) {
      if (is.na(df$skip[i])) {
        # Find the next relevant index
        next_relevant <- relevant_indices[which(relevant_indices > i & df$var[i]!=df$var[relevant_indices])]
        if(df$var[i] %in% c("intro", "intro1", "age")) {
          df$skip[i] <- "end"
        }
        else if (df$var[i] %in% c("gender")){
          df$skip[i] <- "age"
        }
        else if (length(next_relevant) > 0) {
          # Fill NA with the corresponding 'var' value
          df$skip[i] <- df$var[next_relevant[1]]
        }
        else {
          df$skip[i] = "end"
        }
      }
    }
    return(df)
  }

  # Apply the function to the data frame
  filled_data <- fill_na_skip(skips) %>%
    mutate(responses.value = ifelse(is.na(responses.value), paste0(responses.from, "-", responses.to), responses.value))

  edges <- data.frame(from=filled_data$var,
                      to=filled_data$skip,
                      rel=filled_data$responses.value)

  node_df <- DiagrammeR::create_node_df(n=length(unique(filled_data$var)),
                            label = unique(filled_data$var)) %>%
    add_row(id=length(unique(filled_data$var))+1, label="end")



  if(is.null(responses)){
    edges_df <- edges %>%
      rowwise() %>%
      mutate(from = node_df$id[which(node_df$label==from)],
             to = node_df$id[which(node_df$label==to)]) %>%
      mutate(rel = ifelse(rel %in% c("Don't know", "Don't Know"), "Dont know", rel)) %>%
      mutate(label = rel)
  } else{
    edges_df <- edges %>%
      merge(responses, by.x=c("from", "rel"), by.y=c("name", "value"), all.x=TRUE) %>%
      rowwise() %>%
      mutate(from = node_df$id[which(node_df$label==from)],
             to = node_df$id[which(node_df$label==to)]) %>%
      mutate(rel = ifelse(rel %in% c("Don't know", "Don't Know"), "Dont know", rel)) %>%
      mutate(label = paste0(rel, "\n", n))
  }



  graph_diag <- DiagrammeR::create_graph(nodes_df = node_df,
                             edges_df = edges_df,
                             attr_theme = "lr")

  DiagrammeR::render_graph(graph_diag)

  graph_diag %>% DiagrammeR::export_graph(file_name = file, title="Questionnaire Skip Logic")


}
