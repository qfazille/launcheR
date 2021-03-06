% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vis.R
\name{vis}
\alias{vis}
\title{vis}
\usage{
vis(queueid = NULL, last = NULL, show_xlabs = TRUE, show_ylabs = TRUE,
  show_queue = TRUE, show_group = TRUE, show_owner = TRUE,
  show_batch = "alias")
}
\arguments{
\item{queueid}{Numeric Vector of queueid. (Default NULL)}

\item{last}{Numeric Number of last queue to display. Used only if queueid not specified (Default NULL)}

\item{show_xlabs}{Logical Display x labs. (Default TRUE)}

\item{show_ylabs}{Logical Display y labs. (Default TRUE)}

\item{show_queue}{Logical Display queue name. (Default TRUE)}

\item{show_group}{Logical Display queue group. (Default TRUE)}

\item{show_owner}{Logical Display queue owner. (Default TRUE)}

\item{show_batch}{Character "alias" or "path" to choose the label to display. (Default "alias")}
}
\description{
visualize one or several queue in the time.
}
\examples{
\dontrun{
vis(c(1:3))
}
}
