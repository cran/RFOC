\name{alpha95}
\alias{alpha95}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{95 percent confidence for Spherical Distribution}
\description{
  Calculates conical projection angle for 95\%
  confidence bounds for mean of spherically distributed data.
}
\usage{
alpha95(az, iang)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{az}{vector of azimuths, degrees}
  \item{iang}{vector of dips, degrees}
}
\details{

  Program calculates the cartesian coordinates
  of all poles, sums and returns the resultant vector, its azimuth and
  length (R).
  For N points, statistics include:
  \deqn{
   K = \frac {N-1} { N-R}
 }
  \deqn{
   S = \frac{81^{\circ} }{\sqrt{K}}
 }
 
 
  \deqn{
   \kappa = \frac{log( \frac{\epsilon_1}{\epsilon_2}  )}{log(\frac{\epsilon_2}{\epsilon_3} )}
 }
   \deqn{
   \alpha_{95} = cos^{-1} \left[ 1 - \frac {N-R}{R} \left(
   20^{\frac{1}{N-1}} - 1  \right)  \right]
 }
 where \eqn{\epsilon}'s are the relevant eigenvalues of matrix MAT and
 angles are in degrees.
}
\value{
  LIST:
  \item{Ir}{resultant inclination, degrees}
  \item{Dr}{resultant declination, degrees}
  \item{R}{resultant sum of vectors, normalized}
  \item{K}{K-dispersion value}
  \item{S}{spherical variance}
  \item{Alph95}{95\% confidence angle, degrees}
  \item{Kappa}{log ratio of eignevectors}
  \item{E}{Eigenvactors }
  \item{MAT}{matrix of cartesian vectors}

}
\references{Davis, John C., 2002, Statistics and data analysis in geology, Wiley, New York, 637p.}
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{addsmallcirc}
\examples{


paz = rnorm(100, mean=297, sd=10)
pdip = rnorm(100, mean=52, sd=8)
ALPH = alpha95(paz, pdip)

#########  draw stereonet
net()
############  add points
focpoint(paz, pdip, col='red',  pch=3, lab="", UP=FALSE)
###############  add 95 percent confidence bounds
addsmallcirc(ALPH$Dr, ALPH$Ir, ALPH$Alph95, BALL.radius = 1, N = 25,
add = TRUE, lwd=1, col='blue')

############  second example:
paz = rnorm(100, mean=297, sd=100)
pdip = rnorm(100, mean=52, sd=20)
ALPH = alpha95(paz, pdip)

net()
focpoint(paz, pdip, col='red',  pch=3, lab="", UP=FALSE)

addsmallcirc(ALPH$Dr, 90-ALPH$Ir, ALPH$Alph95, BALL.radius = 1, N = 25,
add = TRUE, lwd=1, col='blue')




}

\keyword{misc}
\keyword{univar}
