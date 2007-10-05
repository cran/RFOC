\name{strikeslip.fault}
\alias{strikeslip.fault}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Strikeslip Fault Cartoon}
\description{
Illustrate a strikeslip fault using animation
}
\usage{
strikeslip.fault(anim = seq(from = 0, to = 1, by = 0.1), KAPPA = 2,
                 Light = c(45, 45))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 
  \item{anim}{animation vector }
  \item{KAPPA}{Phong parameter for lighting}
  \item{Light}{lighting point}
}
\details{
  Program will animate a strikeslip fault for educational purposes.
  Animation must be stopped by  halting execution.
}
\value{
  Graphical Side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{normal.fault, thrust.fault}
\examples{

strikeslip.fault(anim=0, Light=c(45,90) )

\dontrun{
#### execute a stop command to stop this animation
anim= seq(from=0, to=1, by=.1) 
 strikeslip.fault(anim=anim, Light=c(45,90) )
 }
}

\keyword{hplot}



