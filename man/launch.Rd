% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/queue.R
\docType{methods}
\name{launch}
\alias{launch}
\alias{launch}
\alias{launch,queue-method}
\title{launch}
\usage{
launch(object)

\S4method{launch}{queue}(object)
}
\arguments{
\item{object}{An object of class queue}
}
\description{
Function to launch a queue.
}
\examples{
\dontrun{
q <- createQueue()
q <- addBatch(q, "/path/batch.R")
launch(q)
}
}
