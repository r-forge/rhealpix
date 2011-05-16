.mask.cfitsio.map <- function(map)
{
  map[map < -1.635e+30] <- NA
  return(map)
}

read.map <- function(file.name, polarisation = FALSE)
{
  fits.obj <- openFITSTable(file.name)
  order <- readKeyValue(fits.obj, "TSTRING", "ORDERING")
  nside <- readKeyValue(fits.obj, "TINT", "NSIDE")
  
  column.info <- getColumnInformation(fits.obj, 1)
  num.of.pixels <- getNumOfRows(fits.obj) * column.info$repeat.count
  
  i.stokes <- .mask.cfitsio.map(readColumn(fits.obj,
                                           column.info$type,
                                           1, 1, 1,
                                           num.of.pixels))
  if(polarisation)
  {
    q.stokes <- .mask.cfitsio.map(readColumn(fits.obj,
                                             column.info$type,
                                             2, 1, 1,
                                             num.of.pixels))
    u.stokes <- .mask.cfitsio.map(readColumn(fits.obj,
                                             column.info$type,
                                             3, 1, 1,
                                             num.of.pixels))
    result <- data.frame(i.stokes = i.stokes,
                         q.stokes = q.stokes,
                         u.stokes = u.stokes)
  } else {
    result <- i.stokes
  }
  
  closeFITSFile(fits.obj)

  attr(result, "ordering") <- order
  attr(result, "nside") <- nside
  return(result)
}
