\name{getUWfocs}
\alias{getUWfocs}
\title{Get UW focals
}
\description{Get UW focal mechansims from a file.
  These are often called A and M cards
}
\usage{
getUWfocs(amfile)
}
\arguments{
  \item{amfile}{character, file name
}
}
\details{
  UW focal mechanisms are stored as A and M cards.
  th A card described the hypocenter the M card describes the
  focal mechanism.
}
\value{
List:
\item{lon}{numeric, longitude}
\item{lat}{numeric, latitude}
\item{str1}{numeric, strike of plane 1}
\item{dip1}{numeric, dip of plane 1}
\item{rake1}{numeric, rake of plane 1}
\item{str2}{numeric, strike of plane 2 }
\item{dip2}{numeric, dip of plane 2 }
\item{rake2}{numeric, rake of plane 2}
\item{sc}{character, some GMT info for scale}
\item{iexp}{character, some GMT info for scale}
\item{name}{character, name}
\item{yr}{numeric, year}
\item{mo}{numeric, month}
\item{dom}{numeric, day of month}
\item{jd}{numeric, julian day}
\item{hr}{numeric, hour}
\item{mi}{numeric, minute}
\item{se}{numeric, second}
\item{z}{numeric, depth}
\item{mag}{numeric, magnitude}
}
\references{http://www.unc.edu/~leesj/XM_DOC/xm_hypo.doc.html
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{Uses UW2 format, so full 4 digit year is required
}


\seealso{getCMT
}

\keyword{misc}
