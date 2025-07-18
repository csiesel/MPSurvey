% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_results.R
\name{export_results}
\alias{export_results}
\title{Export Results to Excel}
\usage{
export_results(results, file)
}
\arguments{
\item{results}{A list containing the results of a statistical analysis. The list should have a component named \code{table_body} which is a data frame containing the raw data.}

\item{file}{A string specifying the path to the output Excel file.}
}
\value{
None. The function writes the processed data to an Excel file.
}
\description{
This function takes the results from a statistical analysis, processes the data to extract relevant statistics, and exports the processed data to an Excel file.
}
\examples{
\dontrun{
# Example usage:
export_results(results = your_survey_results, file = "path/to/export/file.xlsx")
}
}
