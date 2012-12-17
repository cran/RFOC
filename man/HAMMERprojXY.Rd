\name{HAMMERprojXY}
\alias{HAMMERprojXY}
\title{Hammer Projection
}
\description{Hammer Equal Area projection
}
\usage{
HAMMERprojXY(phi, lam)
}
\arguments{
  \item{phi}{Latitude, radians
}
  \item{lam}{Longitude, radians
}
}

\value{list:
  \item{x}{coordinate for plotting}
  \item{y}{coordinate for plotting}
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{

HAMMERprojXY(-25*pi/180, -16*pi/180)

}
\keyword{misc}
