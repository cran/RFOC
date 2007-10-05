\name{RectDense}
\alias{RectDense}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Divide a region into rectangles based on density }
\description{
  Given a set of (x,y) points, partition the field into
  rectangles each containing a minimum number of points
  }
\usage{
RectDense(INx, INy, icut = 1, u = par("usr"), ndivs = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{INx}{x-coordinates }
  \item{INy}{y-coordinates}
  \item{icut}{cut off for number of points}
  \item{u}{user coordinates}
  \item{ndivs}{number of divisions in x-coordinate}
}
\details{
  Based on the user coordinates as returned from par('usr').
  Each rectangular region is tested for the number of
  points that fall within icut or greater.
  
}
\value{
List:
  \item{icorns}{matrix of corners that passed test}
  \item{ilens}{vector,number of points in each icorns box}
   \item{ ipass}{vector, index of the corners that passed icut}
   \item{ corners}{matrix of all corners}
   \item{ lens}{vector,number of points for each box}
 
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\examples{

x = rnorm(100)
y = rnorm(100)

plot(x,y)
u = par('usr')
RI = RectDense(x, y, icut=3, u=u, ndivs=10)

 rect(RI$icorns[,1],RI$icorns[,2],RI$icorns[,3],RI$icorns[,4], col=NA, border='blue')


}

\keyword{misc}
\keyword{spatial}
