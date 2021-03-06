\name{phys2eventtime}
\alias{phys2eventtime}

\title{
  Convert data from physical to event time
}

\description{\sQuote{phys2eventtime} is used to convert data from
  physical to event time using information on events identified by the
  user.}

\usage{
phys2eventtime(z, events, width = 10)
}

\arguments{
  \item{z}{an object of class \pkg{zoo} or \pkg{xts} containing data to
    be converted into event time.}

  \item{events}{
    \sQuote{data.frame} containing event identifiers. See \sQuote{Details}.
  }

  \item{width}{an \sQuote{integer} specifying the event window within
    which data should be available to consider the outcome a 'success'. See
  \sQuote{Details}.}

}

\details{

  \dQuote{events} object contains two columns: \dQuote{name}
  consists of names of the event, and \dQuote{when} is the
  respective event time. \sQuote{class} of \sQuote{index} of \dQuote{z}
  and \dQuote{when} should be same and one of the \sQuote{date-time} or
  \sQuote{Date} classes.

  If an event date does not lie within the index of \dQuote{z}, the function
  approximates to the nearest previous time using
  \code{\link{findInterval}}. Note that \sQuote{findInterval} assumes the
  index of \sQuote{z} is non-decreasing.

  The argument \dQuote{width} provides the user with an option to define
  successful events as those that have data within a window around the
  event. Window is defined as (-width, +width]. If \dQuote{width} is 10
  periods, those events with \sQuote{NA} within 10 periods before and
  after the event will be classified as \dQuote{wdatamissing},
  otherwise, the event \dQuote{outcome} will be classified as
  \dQuote{success}.

  Currently this function requires \dQuote{z} to have at least one
  \emph{column}. It relies on \sQuote{colnames} of \dQuote{z} for the series
  names, and matches it with the \dQuote{events} object. One can use
  \code{drop = FALSE} with \sQuote{[} to achieve a single-column object.
}

\value{
  Returns a \sQuote{list} of two elements:

  \item{z.e}{a \pkg{zoo} object containing data of successful events
  indexed with event time; \dQuote{NULL} if there are no
  \dQuote{success} in \dQuote{outcomes}. \sQuote{colnames} of z.e are
  event numbers: row numbers of \dQuote{events}.}

  \item{outcomes}{a character vector with outcome definition for each event.}
  \itemize{
    \item{success: the successful use of an event.}
    \item{wdatamissing: when there are NAs within the event window.}
    \item{wrongspan: when event time cannot be mapped to physical
      time in \dQuote{z} because event window is outside \sQuote{index(z)}}.
    \item{unitmissing: when the unit (a column) is missing in \dQuote{z}.}
  }
}

\seealso{
  \code{\link{findInterval}}
}

\author{Ajay Shah, Chirag Anand, Vikram Bahure, Vimal Balasubramaniam}

\examples{
data(StockPriceReturns)
data(SplitDates)

result <- phys2eventtime(z = StockPriceReturns,
			 events = SplitDates,
			 width = 5)
print(result$z.e[as.character(-4:5)])
print(SplitDates[result$outcomes == "success", ])
}
\keyword{phys2eventime}
