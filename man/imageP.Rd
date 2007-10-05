\name{imageP}
\alias{imageP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{P-wave radiation pattern}
\description{
 Amplitude of P-wave radiation pattern from Double-Couple earthquake
}
\usage{
imageP(phiS, del, lam, SCALE = FALSE, UP = FALSE, col = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{phiS}{strike }
  \item{del}{dip }
  \item{lam}{lambda}
  \item{SCALE}{logical, TRUE=add scale on side of plot }
  \item{UP}{upper/lower hemisphere }
  \item{col}{color }
}
\details{
 This program calls radP to calculate the radiation pattern and it plots
 the result using the standard image function
}
\value{
  Used for the graphical side effect
}
\references{
K.~Aki and P.~G. Richards.{\em Quantitative seismology}. University Science Books, Sausalito, Calif., 2nd edition, 2002.
}
\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{radP, SDRfoc}
\examples{

MEC =SDRfoc(65,25,13, u=FALSE, ALIM=c(-1,-1, +1, +1), PLOT=FALSE)
imageP(MEC$az1, MEC$dip1, MEC$rake1, SCALE=TRUE, UP=MEC$UP, col=rainbow(100) )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
