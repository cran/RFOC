\name{ternfoc.point}
\alias{ternfoc.point}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Ternary Point}
\description{
 Add a point to a ternary plot
}
\usage{
ternfoc.point(deltaB, deltaP, deltaT)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{deltaB}{angle, degrees}
  \item{deltaP}{angle, degrees}
  \item{deltaT}{angle, degrees}
}
\details{
  Plot point on a Ternary diagram using Froelich's algorithm.
}
\value{
  List
  \item{h}{vector of x coordinates}
  \item{v}{vector of y coordinates}
}
\references{C. Frohlich.   Triangle   diagrams:   ternary  graphs   to display  similarity and   diversity  of earthquake
  focal mechanisms.    Physics  of the Earth  and Planetary Interiors,  75:193-198,   1992.}
\author{Jonathan M. Lees <jonathan.lees@unc.edu>}
\note{
  Use Bfocvec(az1, dip1,  az2,  dip2) to get the deltaB angle.
}
\seealso{Bfocvec}
\examples{
Msdr = CONVERTSDR(55.01, 165.65,  29.2   )
 MEC = MRake(Msdr$M)
  MEC$UP = FALSE
   az1 = Msdr$M$az1
  dip1 = Msdr$M$d1
  az2 = Msdr$M$az2
  dip2 = Msdr$M$d2
  BBB = Bfocvec(az1, dip1,  az2,  dip2)
  V = ternfoc.point(BBB$Bdip, Msdr$M$pd, Msdr$M$td )


}

\keyword{misc}
