\name{FixDip}
\alias{FixDip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fix Dip Angle}
\description{
  Fix az, dip angles so they fall in correct quadrant.
}
\usage{
FixDip(A)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  List:
  \item{A} { \item{az}{azimuthm angle, degrees}
    \item{dip}{dip angle, degrees}
    }
}
\details{
Quadrants are determined by the sine and cosine of the dip angle:\cr
  \code{co = cos(dip) }\cr
  \code{si = sin(dip) }\cr
  \code{quad[co>=0 & si>=0] = 1 }\cr
  \code{quad[co<0 & si>=0] = 2 }\cr
  \code{quad[co<0 & si<0] = 3 }\cr
  \code{quad[co>=0 & si<0] = 4 }\cr
}


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
