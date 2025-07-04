% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_skiplogic.R
\name{get_skiplogic}
\alias{get_skiplogic}
\title{Extract Skip Logic from JSON File}
\usage{
get_skiplogic(path = NULL)
}
\arguments{
\item{path}{A character string specifying the path to the JSON file containing skip logic rules. Default is NULL.}
}
\value{
A data frame containing the skip logic information, including variables, question IDs, question text, response values, and skip logic conditions.
}
\description{
This function reads a JSON file from the specified path, extracts skip logic rules, and returns a data frame with the skip logic information.
}
\examples{
\dontrun{
# Example usage:
skip_logic <- get_skiplogic(path = "path/to/your/skiplogic.json")
}
}
