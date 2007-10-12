\name{circtics}
\alias{circtics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw circular ticmarks}
\description{
  Draw circular ticmarks
}
\usage{
circtics(r = 1, dr = 0.02, dang = 10, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{radius}
  \item{dr}{length of tics}
  \item{dang}{angle between tics }
  \item{...}{graphical parameters }
}

\value{
 graphical side effects
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu> }

\seealso{}
\examples{

 phi = seq(from =0, to = 2 * pi, length=360)
    x = cos(phi)
    y = sin(phi)
    plot(x, y, col = 'blue', asp=1, type='l')
   circtics(r = 1, dr = 0.02, dang = 10, col='red')




}

\keyword{aplot}
