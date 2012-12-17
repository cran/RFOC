\name{AlongGreat}
\alias{AlongGreat}

\title{Get Points Along Great Circle}
\description{
  Using a Starting LAT-LON, return points along an azimuth 
}
\usage{
AlongGreat(LON1, LAT1, km1, ang,  EARTHRAD= 6371)
}

\arguments{
  \item{LON1}{Longitude, point}
  \item{LAT1}{Latitude, point  }
  \item{km1}{Kilometers in direction ang}
  \item{ang}{Direction from North}
  \item{EARTHRAD}{optional earth radius,  default  = 6371}

  
}
\details{
  Returns LAT-LON points along a great circle, so many kilometers away
  in a specified direction
}
\value{
LIST:
  \item{lat}{Latitude, destination point}
  \item{lon}{Longitude, destination point}
  \item{distdeg}{distance in degrees}
 \item{distkm}{distance in km}

}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}


\examples{
london = c(51.53333, -0.08333333)

AlongGreat(london[2], london[1], 450, 56)


}

\keyword{misc}
