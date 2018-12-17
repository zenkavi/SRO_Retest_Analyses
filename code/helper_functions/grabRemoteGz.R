grabRemoteGz <- function(path, file_name) {
  temp <- tempfile()
  download.file(paste0(path, file_name), temp)
  aap.file <- read.csv(gzfile(temp), as.is = TRUE)
  unlink(temp)
  return(aap.file)
}
