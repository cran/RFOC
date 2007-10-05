\name{KAMCORN}
\alias{KAMCORN}
\docType{data}
\title{SDR data from the Harvard CMT catalog}
\description{
  Strike-Dip-Rake and Locations of Harvard CMT catalog for the
  intersection of the Kamchataka and Aleutian arcs
}
\usage{data(KAMCORN)}
\format{
  The format is:
 chr "KAMCORN"
}
\details{
  The data is selected fromt eh CMT catalog.  Parameters are
  extracted from the normal distribution.  Format of the list
  of data save in KAMCORN is:
  list(LAT=0  , LON =0  ,   DEPTH=0  ,  STRIKE=0  , DIP=0  ,    RAKE=0  )

}
\source{
 http://www.globalcmt.org/CMTsearch.html

}
\references{
  G. Ekstrom.     Rapid    earthquake   analysis  utilizes the internet.  Computers     in Physics,   8:632-638,  1994.
}
 
\examples{
data(KAMCORN)
## maybe str(KAMCORN) ; plot(KAMCORN) ...
}
\keyword{datasets}
