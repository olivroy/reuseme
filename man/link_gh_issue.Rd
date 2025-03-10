% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/markup.R
\name{link_gh_issue}
\alias{link_gh_issue}
\title{Create a markdown link to a GitHub issue}
\usage{
link_gh_issue(x, home_repo = NULL)
}
\arguments{
\item{x}{A string, usually lines of files that contains issue numbers.}

\item{home_repo}{Optional, but if supplied, will be stripped.}
}
\value{
A markdown link linked issue to GitHub issue
}
\description{
In RStudio, links to issues are automatically recognized.
This function creates intermediate markdown links to entries of the form rstudio/rstudio#1100
}
\details{
Note: doesn't (yet) support without \if{html}{\out{<OWNER>}}/\if{html}{\out{<REPO>}}

Basically transform repo/org#xx -> \verb{[repo/org#xx](https://github.com/repo/org/issues/xx)}.

Afterwards, we use \code{\link[=markup_href]{markup_href()}} to create a cli link
}
\examples{
link_gh_issue(c("We really need rstudio/gt#1469 to be fixed."))
}
\seealso{
Other inline markup internal helpers: 
\code{\link{markup_href}()}
}
\concept{inline markup internal helpers}
\keyword{internal}
