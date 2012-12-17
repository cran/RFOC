\name{nodalLines}
\alias{nodalLines}
\title{Nodal Lines
}
\description{Add nodal planes to focal mechanism
}
\usage{
nodalLines(strike, dip, rake, PLOT=TRUE)
}
\arguments{
  \item{strike}{numeric, strike of fault
}
  \item{dip}{numeric, dip of fault
}
  \item{rake}{numeric, rake  of fault
  }

  \item{PLOT}{logical, add lines to plot, default=TRUE
  }

  
}
\details{Lower Hemisphere focal plane.
}
\value{Side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{Lower Hemisphere based on FOCangles.
}


\seealso{doNonDouble, MapNonDouble, FOCangles
}
\examples{
mo <- list(n=1, m1=1.035675e+017,
   m2=-1.985852e+016, m3=-6.198052e+014,
   m4=1.177936e+017, m5=-7.600627e+016, m6=-3.461405e+017)
moments <- cbind(mo$n, mo$m1, mo$m2, mo$m3, mo$m4, mo$m5, mo$m6)
doNonDouble(moments)


}
\keyword{misc}
