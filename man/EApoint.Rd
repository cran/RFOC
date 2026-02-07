\name{EApoint}
\alias{EApoint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Equal-area point stereonet}
\description{
  Interactive locator to
  calculate x,y orientation, dip
  coordinates and plots on an equalarea stereonet
}
\usage{
EApoint()
}
%- maybe also 'usage' for other objects documented here.
\details{
  Used for returning a set of strike/dip angles on Equal-area stereonet
  plot.  
}
\value{
 
LIST:
  \item{phi}{orientation, degrees}
  \item{iang}{angle of dip, degrees}
 \item{ x}{x-coordinate}
  \item{ y}{y-coordinate}
 
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{qpoint, focpoint}
\examples{

####################  this is interactive

###  collect points with locator()

if(interactive()){ 
net()
eps = EApoint()

###  plot results
net()
qpoint(eps$phi , eps$iang)
}

}

\keyword{iplot}
