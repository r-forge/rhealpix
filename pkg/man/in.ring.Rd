\name{in.ring}
\alias{in.ring}
\title{Determine which pixels belong to a specified ring.}
\description{Return a list of the pixels which fall within a ring in the sky.}
\usage{
in.ring <- function(nside, ring.number,
                    phi0 = 0.0, delta.phi = pi,
                    nest = FALSE)
}
\arguments{
	\item{nside}{The value of the \code{NSIDE} parameter.}
	\item{ring.number}{The number of the ring.}
	\item{phi0}{It is possible to return only the pixels which lie
	within a arc instead of the full circle. The value of
	\code{phi0} specifies the angle of the center of the arc,
	while \code{delta.phi} specifies the semi-amplitude of the
	arc. (I.e. the arc extends from \code{phi0 - delta.phi} to
	\code{phi0 + delta.phi}.) Both \code{phi0} and
	\code{delta.phi} are in radians.}
	\item{nest}{If \code{TRUE}, the pixel indexes returned by this
	function are in nested order, otherwise they are in ring
	order. The default is to use the ring order, as this is more
	efficient.}
}
\value{
	An array of pixel indexes.
}
\details{

	Setting \code{nest = TRUE} is currently not supported.

}
\author{Maurizio Tomasi \email{maurizio.tomasi@unimi.it}}
\examples{
	pixels <- in.ring(2, 0)
}
