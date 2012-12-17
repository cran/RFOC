\name{PTaxes}
\alias{PTaxes}
\title{Plot P-T axis on CLVD
}
\description{Plot P-T axis on CLVD
}
\usage{
PTaxes(strike, dip, rake)
}
\arguments{
  \item{strike}{strike
}
  \item{dip}{dip
}
  \item{rake}{rake
}
}
\details{Lower Hemisphere. 
  Add PT axes on a moment tensor plot
}
\value{Side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{doNonDouble, MapNonDouble
}
\examples{

mo = list(n=1, m1=1.035675e+017, m2=-1.985852e+016, m3=-6.198052e+014, m4=1.177936e+017, m5=-7.600627e+016, m6=-3.461405e+017)
moments = cbind(mo$n, mo$m1, mo$m2, mo$m3, mo$m4, mo$m5, mo$m6)
doNonDouble(moments)


}
\keyword{misc}
