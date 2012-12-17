\name{TapeBase}
\alias{TapeBase}
\title{Tape Base Lines
}
\description{Create a structure of Tape Base lines
}
\usage{
TapeBase()
}
\details{Program returns the lines and points
  for plotting a Tape plot.  Based on the Hammer projection.
}
\value{List
}
\references{Tape, W., and C. Tape (2012), A geometric comparison of source-type plots for moment tensors, Geophys. J. Int., 190, 499-510.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
  The list includes points and other information
}


\seealso{TapePlot, HAMMERprojXY
}
\examples{

T1 =TapeBase()
TapePlot(T1)

}
\keyword{misc}
