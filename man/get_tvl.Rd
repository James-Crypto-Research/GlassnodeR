% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_tvl.R
\name{get_tvl}
\alias{get_tvl}
\title{Get the Total Value Locked for all DeFi platforms}
\usage{
get_tvl(since = NULL, until = NULL, api_key = Sys.getenv("GN_API_KEY"))
}
\arguments{
\item{since, until}{A POSIX compatible date-time object. It's converted to a unix date number}

\item{api_key}{The API key to use. Checks the env variable API_KEY}
}
\value{
Returns a tidy data set denominated in USD
}
\description{
Get the Total Value Locked for all DeFi platforms
}
\examples{
\dontrun{
# Need a API key to run
x <- get_tvl()
}
}
