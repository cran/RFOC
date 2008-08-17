\name{Beachfoc}
\alias{Beachfoc}
\title{Plot a BeachBall Focal Mechanism}
\description{
  Plots a focal mechanism in beachball style
}
\usage{
Beachfoc(MEC, fcol = gray(0.9), fcolback = "white", ALIM = c(-1, -1, +1, +1))
}
\arguments{
  \item{MEC}{Mechanism Structure}
  \item{fcol}{color for the filled portion of the beachball}
  \item{fcolback}{color for the background portion of the beachball, default='white'}
  \item{ALIM}{Bounding box for beachball, default=c(-1, -1, +1, +1)}
}
\details{
  Beachfoc is run after MEC is set using SDRfoc.
  Options for plotting the beachball in various modes are controlled by
  flags set in MEC
}
\value{
  Used for its graphical side effect
}
\references{K. Aki and P. G. Richards. \emph{Quantitative seismology}.
  University Science Books, Sausalito, Calif., 2nd
  edition, 2002. Keiiti Aki, Paul G. Richards. ill. ; 26 cm.
}
\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{CONVERTSDR, SDRfoc, justfocXY}
\examples{

MEC = SDRfoc(65,25,13, u=FALSE, ALIM=c(-1,-1, +1, +1), PLOT=TRUE)

Beachfoc(MEC, fcol=MEC$fcol, fcolback="white")

}

\keyword{hplot}
\keyword{aplot}
