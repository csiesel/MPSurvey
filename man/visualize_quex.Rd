% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize_quex.R
\name{visualize_quex}
\alias{visualize_quex}
\title{Visualize Questionnaire Skip Logic}
\usage{
visualize_quex(path = NULL, file = NULL, responses = TRUE, analysis = NULL)
}
\arguments{
\item{path}{A character string specifying the path to the JSON file containing skip logic rules. Default is NULL.}

\item{file}{A character string specifying the file name to save the graph visualization (.pdf, .svg, .png). Default is NULL.}

\item{responses}{A logical to determine if responses should be added to the graph visualization. Default is TRUE.}

\item{analysis}{A \code{dataframe} object containing the analytic dataframe to use for generating the responses. This should be the dataframe after recoding takes place using \code{recode_var()}. Default is NULL.}
}
\value{
A graph object visualizing the skip logic of the questionnaire.
}
\description{
This function visualizes the skip logic of a questionnaire based on the skip logic rules defined in a JSON file. It creates a directed graph showing the flow of questions and their skip patterns.
}
\examples{
\dontrun{
# Example usage:
visualize_quex(path = "path/to/your/skiplogic.json", file = "skip_logic_graph.png")
}
}
