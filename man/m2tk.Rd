\name{m2tk}
\alias{m2tk}
\title{Moment tensor to T-k
}
\description{Moment tensor to T-k
}
\usage{
m2tk(m0)
}
\arguments{
  \item{m0}{moment tensor eigenvalues, sorted decending
}
}
\details{Convert 3 eigen values of a moment tensor to
  T-k coordinates
}
\value{list(t, k)
}
\references{Hudson
}
\author{
   Keehoon Kim<keehoon@live.unc.edu>
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{tk2uv, hudson.net, hudson.plot
}
\examples{
v = c(2,-1,-1)
m2tk(v)
}
\keyword{misc}
