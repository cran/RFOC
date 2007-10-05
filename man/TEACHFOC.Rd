\name{TEACHFOC}
\alias{TEACHFOC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphical Plot of Focal Mechanism}
\description{
  Plots Beachball figure with numerous vectors and
  points added and labeled. Useful for teaching about focal mechanisms.
  }
\usage{
TEACHFOC(s, d, r, up = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{ strike }
  \item{d}{ dip }
  \item{r}{rake }
  \item{up}{ logical, TRUE = upper }
}
\value{
 Graphical side effects
}
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\seealso{CONVERTSDR, MRake,foc.icolor,focleg, foc.color, focpoint,
  PlotPlanes, nipXY , fancyarrows }
\examples{
TEACHFOC(65, 32, -34, up=TRUE)

}
\keyword{hplot}
