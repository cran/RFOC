\name{flipnodal}
\alias{flipnodal}
\title{Flip Nodal Fault Plane
}
\description{Switch a focal mechanism
so the auxilliary plane is the nodal plane.
}
\usage{
flipnodal(s1, d1, r1)
}
\arguments{
  \item{s1}{Strike
}
  \item{d1}{Dip
}
  \item{r1}{Rake
}
}
\details{Fuunction is used for
orienting a  set of fault planes
to line up according to a geologic interpretation.
}
\value{List:
 \item{s1}{Strike
}
  \item{d1}{Dip
}
  \item{r1}{Rake
}
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{


s=65
d=25
r=13

  mc = CONVERTSDR(s,d,r )

  mc2 = flipnodal(s, d, r)

}
\keyword{misc}
