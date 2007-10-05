\name{TRANmat}
\alias{TRANmat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Translation Matrix }
\description{
 Create a 4 by 4 translation matrix
}
\usage{
TRANmat(x, y, z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{x-translation}
  \item{y}{y-translation}
  \item{z}{z-translation}
}

\value{
  Matrix suitaqble for translating a 3D body.
}
\references{Rogers and Adams, 1990, Mathematical Elements for Computer
  Graphics, McGraw-Hill, 611p.}
\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{ROTX, ROTZ, ROTY}
\examples{

zT = TRANmat(5, 4, 2)
}

\keyword{misc}
