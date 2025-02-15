% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read_clean}
\alias{read_clean}
\title{Read and clean names and set variable labels}
\usage{
read_clean(file, ...)
}
\arguments{
\item{file}{Either a path to a file, a connection, or literal data
(either a single string or a raw vector).

Files ending in \code{.gz}, \code{.bz2}, \code{.xz}, or \code{.zip} will
be automatically uncompressed. Files starting with \verb{http://},
\verb{https://}, \verb{ftp://}, or \verb{ftps://} will be automatically
downloaded. Remote gz files can also be automatically downloaded and
decompressed.

Literal data is most useful for examples and tests. To be recognised as
literal data, the input must be either wrapped with \code{I()}, be a string
containing at least one new line, or be a vector containing at least one
string with a new line.

Using a value of \code{\link[readr:clipboard]{clipboard()}} will read from the system clipboard.}
}
\description{
Reads data from file, but clean names with \code{\link[janitor:make_clean_names]{janitor::make_clean_names()}}.
}
\details{
It keeps the variable labels attributes to have improved support in the RStudio IDE,
with gt tables and with ggplots (>= 3.6.0).
}
\examples{
read_clean(system.file("extdata",  "challenge.csv", package = "readr"))

}
