\name{cross.prod}
\alias{cross.prod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Vector Cross Product}
\description{
 Vector Cross Product with list as arguments and list as values
}
\usage{
cross.prod(B, A)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{B}{list of x,y,z }
  \item{A}{list of x,y,z}
}

\value{
  
  LIST
  \item{x,y,z}{vector of cross product}

}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{xprod}
\examples{
B1 = list(x=4, y=9, z=2)
B2 = list(x=2,y=-5,z=4)

cross.prod(B1, B2)


}

\keyword{misc}
