% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_single_line_address.R
\name{tidy_single_line_address}
\alias{tidy_single_line_address}
\title{Tidy a single line address}
\usage{
tidy_single_line_address(df, col, remove_postcode = FALSE)
}
\arguments{
\item{df}{Database table}

\item{col}{Single line address column}

\item{remove_postcode}{If to remove the postcode. Default is FALSE}
}
\description{
Tidy a single line address ready for tokenising.
}
