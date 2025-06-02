#' Export Results to Excel
#'
#' This function takes the results from a statistical analysis, processes the data to extract relevant statistics, and exports the processed data to an Excel file.
#'
#' @param results A list containing the results of a statistical analysis. The list should have a component named `table_body` which is a data frame containing the raw data.
#' @param file A string specifying the path to the output Excel file.
#'
#' @return None. The function writes the processed data to an Excel file.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' export_results(results = your_survey_results, file = "path/to/export/file.xlsx")
#' }
export_results <- function(results, file){
 start_data <- results$table_body

 # Function to split and convert the data
 convert_data <- function(data) {
   data %>%
     mutate(
       Overall_n = as.numeric(sub("/.*", "", stat_0)),
       Overall_N = as.numeric(sub(".*?/(.*?)\\n.*", "\\1", stat_0)),
       Overall_prop = as.numeric(sub(".*?\\n(.*?)%.*", "\\1", stat_0)),
       Overall_95_low = as.numeric(sub("%,.*", "", ci_stat_0)),
       Overall_95_upp = as.numeric(sub(".*, (.*?)%.*", "\\1", ci_stat_0)),

       Female_n = as.numeric(sub("/.*", "", stat_1)),
       Female_N = as.numeric(sub(".*?/(.*?)\\n.*", "\\1", stat_1)),
       Female_prop = as.numeric(sub(".*?\\n(.*?)%.*", "\\1", stat_1)),
       Female_95_low = as.numeric(sub("%,.*", "", ci_stat_1)),
       Female_95_upp = as.numeric(sub(".*, (.*?)%.*", "\\1", ci_stat_1)),

       Male_n = as.numeric(sub("/.*", "", stat_2)),
       Male_N = as.numeric(sub(".*?/(.*?)\\n.*", "\\1", stat_2)),
       Male_prop = as.numeric(sub(".*?\\n(.*?)%.*", "\\1", stat_2)),
       Male_95_low = as.numeric(sub("%,.*", "", ci_stat_2)),
       Male_95_upp = as.numeric(sub(".*, (.*?)%.*", "\\1", ci_stat_2))
     ) %>%
     select(
       variable, var_type, var_label, row_type, label,
       Overall_n, Overall_N, Overall_prop, Overall_95_low, Overall_95_upp,
       Male_n, Male_N, Male_prop, Male_95_low, Male_95_upp,
       Female_n, Female_N, Female_prop, Female_95_low, Female_95_upp
     )
 }

 # Convert the data
 end_data <- convert_data(start_data)

 xlsx::write.xlsx(end_data %>% as.data.frame(), file=file, row.names = F)

}
