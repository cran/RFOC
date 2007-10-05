\name{thrust.fault}
\alias{thrust.fault}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Thrust Fault Cartoon}
\description{
Illustrate a thrust fault using animation
}
\usage{
thrust.fault(anim = seq(from = 0, to = 1, by = 0.1), KAPPA = 2,
             Light = c(45, 45))
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{anim}{animation vector }
  \item{KAPPA}{Phong parameter for lighting}
  \item{Light}{lighting point}
}
\details{
  Program will animate a thrust fault for educational purposes.
  Animation must be stopped by  halting execution.
}
\value{
  Graphical Side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{strikeslip.fault, thrust.fault}
\examples{


thrust.fault(anim=0, KAPPA=4, Light=c(-20, 80))

\dontrun{
#### execute a stop command to stop this animation
anim= seq(from=0, to=1, by=.1) 
thrust.fault(anim=anim, KAPPA=4, Light=c(-20, 80))
}

}

\keyword{hplot}

