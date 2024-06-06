xrdml2df <- function(xfile,
                     xdir = "wd"
) {
  if(xdir == "wd") {
    xdir <- getwd()
  }
  xrdmlRaw <- readLines(paste0(xdir, xfile))
  twothet0 <- grep("2Theta", xrdmlRaw)
  beg0 <- xrdmlRaw[twothet0 + 1]
  beg0 <- gsub("\t\t\t\t\t<startPosition>", "", beg0)
  beg0 <- gsub("</startPosition>", "", beg0)
  beg0 <- as.numeric(beg0)
  end0 <- xrdmlRaw[twothet0 + 2]
  end0 <- gsub("\t\t\t\t\t<endPosition>", "", end0)
  end0 <- gsub("</endPosition>", "", end0)
  end0 <- as.numeric(end0)
  c0 <- grep("<counts", xrdmlRaw)
  xrdmlRaw[c0] <- gsub('\t\t\t\t<counts unit="counts">', "", xrdmlRaw[c0])
  xrdmlRaw[c0] <- gsub("</counts>", "", xrdmlRaw[c0])
  counts <- as.numeric(unlist(strsplit(xrdmlRaw[c0], " ")))
  counts2theta <-
    data.frame(Angle = seq(beg0, end0, ((end0 - beg0) / (length(counts) - 1))),
               Counts = counts)
  rm(list = c("xrdmlRaw","twothet0","beg0","end0","c0","counts"))
  return(counts2theta)
}