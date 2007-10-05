\name{radiateSH}
\alias{radiateSH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot radiation pattern for SH-waves}
\description{
 Plots focal mechanism and makes radiation plot with mark up
}
\usage{
radiateSH(MEC, SCALE = FALSE, col = col, TIT = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MEC}{focal mechanism structure}
  \item{SCALE}{logical, TRUE=add scale}
  \item{col}{color palette}
  \item{TIT}{title for plot}
}

\value{
 Used for side graphical effect
}
\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{radSH, SDRfoc}
\examples{
MEC =SDRfoc(65,25,13, u=FALSE, ALIM=c(-1,-1, +1, +1), PLOT=FALSE)
radiateSH(MEC, SCALE = FALSE, col = rainbow(100) , TIT = FALSE)

}

\keyword{hplot}
