\name{Bfocvec}
\alias{Bfocvec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Angles for Ternary plot}
\description{
Calculates Angles for determining ternary distribution of faults based
on P-T axis orientation.
}
\usage{
Bfocvec(Paz, Pdip, Taz, Tdip)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Paz}{vector of azimuths, degrees}
  \item{Pdip}{vector of dips, degrees}
  \item{Taz}{vector of azimuths, degrees}
  \item{Tdip}{vector of dips, degrees}
}
\details{
  This calculation is based on Froelich's paper.
}
\value{
LIST:
  \item{Bdip }{azimuths, degrees}
  \item{Baz}{ dips, degrees}
 
}
\references{C. Frohlich.   Triangle   diagrams:   ternary  graphs   to display  similarity and   diversity  of earthquake    focal mechanisms.    Physics  of the Earth  and Planetary Interiors,  75:193-198,   1992.}
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{ternfoc.point}
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
