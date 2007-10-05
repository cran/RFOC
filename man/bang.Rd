\name{bang}
\alias{bang}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Angle between two 2D normalized vectors}
\description{
  Calculates the angle between two 2D normalized vectors using dot and
  cross product
}
\usage{
bang(x1, y1, x2, y2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{ x coordinate of first normalized vector }
  \item{y1}{ y coordinate of first normalized vector }
  \item{x2}{ x coordinate of second normalized vector }
  \item{y2}{ y coordinate of second normalized vector }
}
\details{
 The sign of angle is determined by the sign of the cross product of the two
 vectors.  
}
\note{
  Vectors must be normalized prior to calling this routine.  Used mainly
  for vectors on the unit sphere.
  }
\value{
  angle in radians
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\examples{
v1 = c(5,3)
v2 = c(6,1)

a1 = c(5,3)/sqrt(v1[1]^2+v1[2]^2)
a2 = c(6,1)/sqrt(v2[1]^2+v2[2]^2)

plot(c(0, v1[1],v2[1] ) , c(0, v1[2],v2[2]), type='n', xlab="x", ylab="y" )
text(c(v1[1],v2[1]) , c(v1[2],v2[2]), labels=c("v1", "v2"), pos=3, xpd=TRUE)

arrows(0, 0, c(v1[1],v2[1] ), c(v1[2],v2[2]))

B  = 180*bang(a1[1], a1[2], a2[1], a2[2])/pi
title(paste(sep=" ", "Angle from V1 to V2=",format(B, digits=2)) )



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{array}
