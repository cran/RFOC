\name{justfocXY}
\alias{justfocXY}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot focal mechanism }
\description{
  Add simple focal mechanisms to plot
}
\usage{
justfocXY(MEC, x = x, y = y, size = c(1, 1), fcol = gray(0.9),
          fcolback = "white", xpd = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MEC}{MEC structure}
  \item{x}{x-coordinate of center}
  \item{y}{y-coordinate of center}
  \item{size}{size of focal sphere in user coordinates}
  \item{fcol}{color of shaded region}
  \item{fcolback}{color of background region}
  \item{xpd}{logical, whether to extend the plot beyond, or to clip}
}
\details{
  This routine can be used to add focal mechanisms on geographic map
  or other plot.
}
\value{
 Used for graphical side effect
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{SDRfoc, foc.color}
\examples{

#### read in some data:


Z1 = c(159.33,51.6,206,18,78,
161.89,54.5,257,27,133,
170.03,53.57,-44,13,171,
154.99,50.16,-83,19,-40,
151.09,47.15,123,23,-170,
176.31,51.41,-81,22,122,
153.71,46.63,205,28,59,
178.39,51.21,-77,16,126,
178.27,51.1,-86,15,115,
177.95,51.14,-83,25,126,
178.25,51.18,215,16,27
)

MZ = matrix(Z1, ncol=5, byrow=TRUE)

plot(MZ[,1], MZ[,2], type='n', xlab="LON", ylab="LAT", asp=1)

for(i in 1:length(MZ[,1]))
{
paste(MZ[i,3], MZ[i,4], MZ[i,5])


MEC =  SDRfoc(MZ[i,3], MZ[i,4], MZ[i,5], u=FALSE, ALIM=c(-1,-1, +1, +1), PLOT=FALSE)
fcol =  foc.color(foc.icolor(MEC$rake1), pal=1)
justfocXY(MEC, x=MZ[i,1], y =MZ[i,2] , size = c(1, 1), fcol =fcol , fcolback = "white", xpd = TRUE)


}

}

\keyword{hplot}
\keyword{aplot}
