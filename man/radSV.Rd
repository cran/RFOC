\name{radSV}
\alias{radSV}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Radiation pattern for SV waves}
\description{
 calculate the radiation patterns for SV waves
}
\usage{
radSV(del, phiS, lam, ichi, phi)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{del}{degrees, angle }
  \item{phiS}{degrees,angle }
  \item{lam}{degrees, angle }
  \item{ichi}{degrees, take off angle}
  \item{phi}{degrees, take off azimuth}
}
\details{
  Given a focal mechanism strike-dip-rake
  and a given incident angle (take-off angle)
  and azimuth, return the SV amplitude
}
\value{
Amplitude of the SV wave
}
\references{K.~Aki and P.~G. Richards.\emph{Quantitative seismology}. University Science Books, Sausalito, Calif., 2nd edition, 2002.}
\author{Jonathan M. Lees <jonathan.lees@unc.edu>}
\seealso{radP, radSH, imageSV}
\examples{
phiS=65
del=25
lam=13
x = seq(-1, 1, 0.01)
y = x

X = matrix(rep(x, length(y)), nrow= length(x))
Y = t(X)
RAD2DEG = 180/pi
p = RAD2DEG*(pi/2 -atan2(Y, X))
p[p<0] = p[p<0] + 360

R = sqrt(X^2+Y^2)
R[R>1] = NaN
dip =RAD2DEG*2*asin(R/sqrt(2))

###  Calculate the radiation pattern
G = radSV(del, phiS, lam, dip, p)

###  plot values
image(x,y,G, asp=1)

}
\keyword{misc}
