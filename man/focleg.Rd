\name{focleg}
\alias{focleg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fault style descriptor}
\description{
  Get character string describing type of fault from its style index
}
\usage{
focleg(i)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i}{ index to vector of focal styles }
}

\value{
character string used for setting text on plots 
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}
\note{ 
  String of characters:
  \item{STRIKESLIP}{Strike slip fault}
  \item{REV-OBL STRK-SLP}{Reverse Oblique strike-slip fault}
  \item{REVERSE}{Reverse fault}
  \item{NORM-OBLQ STRKSLP}{Normal Oblique strike-slip fault}
  \item{OBLQ NORM}{Oblique Normal fault}
  \item{NORMAL}{Formal fault}
}
\seealso{foc.icolor, foc.color}
\examples{
focleg(2)
}

\keyword{misc}
