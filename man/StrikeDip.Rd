\name{StrikeDip}
\Rdversion{1.1}
\alias{StrikeDip}
\title{Plot Strike Dip Lines
}
\description{given a focal mechanism, add Strike Dip lines to a plot
}
\usage{
StrikeDip(x = x, y = y, MEC, focsiz, addDIP = TRUE, ...)
}
\arguments{
  \item{x}{x-location on plot
}
  \item{y}{y-location on plot
}
  \item{MEC}{Focal Mechanism list from SDRFOC
}
  \item{focsiz}{size of mechanism
}
  \item{addDIP}{Logical, TRUE = add dip line perpendicular to strike
}
  \item{\dots}{graphical parameters
}
}
\details{This is a summary plot to be used instead of Beach Balls.
}
\value{Graphical Side Effects
}
\references{
Lees, J. M., Geotouch: Software for Three and Four Dimensional GIS in the Earth Sciences, Computers & Geosciences, 26, 7, 751-761, 2000.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{nipXY, justfocXY
}
\examples{

###  HAiti Earthquake Jan, 2010
MEC =  SDRfoc(71, 64, 25 , u=FALSE, ALIM=c(-1,-1, +1, +1), PLOT=FALSE)
plot(c(0, 1), c(0,1), type='n', asp=1)
u = par("usr")

focsiz = 0.06* (u[2]-u[1])

justfocXY(MEC, x=.5,  y= .5,  size = c(focsiz, focsiz), fcol ='brown' , fcolback = "white", xpd = TRUE)


 StrikeDip(1.0, .5 , MEC  ,focsiz, col="purple", lwd=3 )
 


nipXY(MEC, x = 0.25, y = .5, size = c(focsiz, focsiz), fcol ='purple', nipcol = "black", cex = 0.4)




}
\keyword{misc}
