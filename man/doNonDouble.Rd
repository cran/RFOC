\name{doNonDouble}
\alias{doNonDouble}
\title{Plot Non-double Couple Moment
}
\description{Plot Non-double Couple Moment
}
\usage{
doNonDouble(moments, sel = 1, col=rgb(1, .75, .75))
}
\arguments{
  \item{moments}{list of moments: seven elements. See details.
}
  \item{sel}{integer vector, index of moments to plot
  }
 \item{col}{color, either a single color, rgb, or a color palette
  }
  
  
}
\details{Plot, sequentially the
  moments using the CLVD (non-double couple component.
  The first element of the list is the integer index of the event.
  The next six elements are the moments in
  the following order, c(Mxx, Myy, Mzz, Mzy, Mxz, Mxy) .

  If the data is in spherical coordinates, one must switch the
  sign of the Mrp and Mtp components, so:
    \verb{
    Mrr = Mzz
    Mtt = Mxx
    Mpp = Myy
    Mrt = Mxz
    Mrp = -Myz
    Mtp = -Mxy
}

}
\value{Side effects
}
\references{
   Ekstrom, G.; Nettles, M. and  DziewoDski, A.
  The Global CMT Project 2004-2010: centroid-moment tensors for 13,017
  earthquakes Physics of the Earth and Planetary Interiors, 2012.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
  If events are read in using spherical rather than cartesian
  coordinates
  need a conversion:
 \verb{
    Mrr = Mzz
    Mtt = Mxx
    Mpp = Myy
    Mrt = Mxz
    Mrp = -Myz
    Mtp = -Mxy
}

  
}


\seealso{MapNonDouble, ShadowCLVD, angles, nodalLines, PTaxes
}
\examples{

mo = list(n=1, m1=1.035675e+017, m2=-1.985852e+016,
m3=-6.198052e+014, m4=1.177936e+017, m5=-7.600627e+016,
m6=-3.461405e+017)

moments = cbind(mo$n, mo$m1, mo$m2, mo$m3, mo$m4, mo$m5, mo$m6)

doNonDouble(moments)



}
\keyword{misc}
