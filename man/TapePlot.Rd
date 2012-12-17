\name{TapePlot}
\alias{TapePlot}
\title{Tape style Lune Plot
}
\description{Tape style Lune Plot using Hammer projection
}
\usage{
TapePlot(TapeList = list(), add = FALSE, ann = TRUE, pcol = c(grey(0.85), grey(0.95)))
}
\arguments{
  \item{TapeList}{List of strokes from TapeBase
}
  \item{add}{logical, TRUE=add to existing plot
}
  \item{ann}{logical, TRUE=annotape
}
  \item{pcol}{vector of colors
}
}
\details{Plot an Tape net from the TapeBase function.  
}
\value{Side effects
}
\references{Tape, W., and C. Tape (2012), A geometric comparison of source-type plots for moment tensors, Geophys. J. Int., 190, 499-510.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{TapeBase, HAMMERprojXY
}
\examples{

T1 = TapeBase()
TapePlot(T1)

}
\keyword{misc}
