\name{FixDip}
\alias{FixDip}

\title{Fix Dip Angle}
\description{
  Fix az, dip angles so they fall in correct quadrant.
}
\usage{
FixDip(A)
}

\arguments{
  List:
  \item{A}{
    \describe{
    \item{az}{azimuthm angle, degrees}
    \item{dip}{dip angle, degrees}
    }
    }
}
\details{
  Quadrants are determined by the sine and cosine of the dip angle:
  \code{co = cos(dip) }
  \code{si = sin(dip) }
  \code{quad[co>=0 & si>=0] = 1 }
  \code{quad[co<0 & si>=0] = 2 }
  \code{quad[co<0 & si<0] = 3 }
  \code{quad[co>=0 & si<0] = 4 }
}

\value{
    List:
  \item{az}{azimuthm angle, degrees}
  \item{dip}{dip angle, degrees}

}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{fmod}
\examples{

B = list(az=231, dip = -65)

FixDip(B)


}

\keyword{misc}
