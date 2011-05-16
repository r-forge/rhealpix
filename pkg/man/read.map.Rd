\name{read.map}
\alias{read.map}
\title{Read a map from a FITS file.}
\description{Read a map from a FITS file and return an array containing the values of each pixel.}
\usage{
read.map <- function(file.name, polarisation = FALSE)
}
\arguments{

	\item{file.name}{The name of the FITS file containing the map.
	It is possible to specify gzipped files, as well as FTP and
	HTTP addresses, as these are handled transparently by the
	rfitsio library.}

	\item{polarisation}{If \code{TRUE}, the map is assumed to
	contain a measurement of the polarisation (i.e. each pixel is
	associated with the three Stokes parameters I, Q and U).}

}
\value{
	If \code{polarisation = FALSE}, the function returns an array
	containing the value of each pixel in the map in the ordering
	used by the FITS file (i.e. \code{RING} or \code{NESTED}). If
	\code{polarisation = TRUE}, the function returns three arrays
	containing the value of the Stokes parameters I, Q and U (the
	three arrays all have the same ordering and length).

	The function sets the following attributes for the array(s) it
	returns: \code{ordering} is either \code{"RING"} or
	\code{"NEST"}, \code{nside} is an integer specifying the value
	of the \code{NSIDE} parameter.

}
\details{

	This function loads FITS files containing Healpix maps.

}
\author{Maurizio Tomasi \email{maurizio.tomasi@unimi.it}}
