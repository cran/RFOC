\name{normal.fault}
\alias{normal.fault}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Normal Fault Cartoon}
\description{
Illustrate a normal fault using animation
}
\usage{
normal.fault(ANG = (45), anim = seq(from = 0, to = 1, by = 0.1),
            KAPPA = 4, Light = c(45, 45))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ANG}{Angle of dip}
  \item{anim}{animation vector }
  \item{KAPPA}{Phong parameter for lighting}
  \item{Light}{lighting point}
}
\details{
  Program will animate a normal fault for educational purposes.
  Animation must be stopped by  halting execution.
}
\value{
  Graphical Side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{strikeslip.fault, thrust.fault}
\examples{

normal.fault(45, anim=0, KAPPA=4, Light=c(-20, 80))

if(interactive()){
#### execute a stop command to stop this animation
anim= seq(from=0, to=1, by=.1) 

 normal.fault(45, anim=anim, KAPPA=4, Light=c(-20, 80))
 }

}

\keyword{hplot}
