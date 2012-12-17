\name{tk2uv}
\alias{tk2uv}
\title{Tk2uv
}
\description{Tk plot to u-v coordinate transformation
}
\usage{
tk2uv(T, k)
}
\arguments{
  \item{T}{T-value
}
  \item{k}{k-value
}
}
\details{T and k come from moment tensor
  analysis.
}
\value{List: u and v
}
\references{Hudson
}
\author{
  Keehoon Kim<keehoon@live.unc.edu>
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{m2tk, hudson.net, hudson.plot
}
\examples{
v = c(2,-1,-1)
m = m2tk(v)
tk2uv(m$T, m$k)

}
\keyword{misc}
