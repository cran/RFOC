\name{foc.color}
\alias{foc.color}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get color of Focal Mechansim}
\description{
  Based on the rake angle, focal styles are assigned
  an index and assigned a color by foc.color
}
\usage{
foc.color(i, pal = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i}{index to list of focal rupture styles}
  \item{pal}{vector of colors}
}
\details{
  Since the colors used by focal programs are arbitrary,
  this routines allows one to change the coloring scheme
  easily.
  
  foc.icolor returns an index that is used to get the color
  associated with that style of faulting
 
}
\value{
  Color for plotting, either a name or HEX RGB
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{foc.icolor}
\examples{

 fcolors=c("DarkSeaGreen", "cyan1","SkyBlue1" , "RoyalBlue" ,"GreenYellow","orange","red")
      foc.color(3, fcolors)
}

\keyword{misc}
