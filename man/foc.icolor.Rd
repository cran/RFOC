\name{foc.icolor}
\alias{foc.icolor}

\title{Get Fault Style}
\description{
  Use Rake Angle to determine style of faulting
}
\usage{
foc.icolor(rake)
}

\arguments{
  \item{rake}{degrees, rake angle of fault plane}
}
\details{
  The styles are determined by the rake angle

  
  strikeslip  abs(rake) <= 15.0 or   abs((180.0 - abs(rake))) <= 15.0

  rev-obl strk-slp   (rake >= 15.0 and rake < 45) or (rake >= 135 and rake
  < 165)
  
  oblique reverse  (rake >= 45.0 and rake < 75) or (rake >= 105 and rake <
  135)
  
  reverse   rake >= 75.0 and rake < 105.0
  
  norm-oblq strkslp  (rake < -15.0 and rake >= -45) or (rake < -135 and
  rake >= -165)
  
  oblq norm   (rake < -45.0 and rake >= -75) or (rake < -105 and rake >=
  -135)
  
  normal rake < -75.0 and rake >= -105

  
  
}
\value{
index (1-6)
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}
\seealso{foc.color }
\examples{
foc.icolor(25)


}

\keyword{misc}
