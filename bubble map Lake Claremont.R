# load packages
library(sf)
library(maptiles)
library(prettymapr)
library(viridis)

# make background map raster
lcext <- st_as_sf(data.frame(x=c(383900,384860), y=c(6461250,6462350)),
                  coords = c("x","y"), crs=st_crs(32750))
lcUTM <- get_tiles(lcext, provider="Thunderforest.Outdoors",
                   apikey="03b0375e8e7c47d0a03018a9001de439",
                   crop=TRUE, zoom=16)

# choose variable and guideline value + units
# =================================================#
v0 <- "P"    # make sure this variable is correct  |
GV <- 0.10   # make sure its guideline is correct  |
GU <- "mg/L" # make sure guideline unit is correct |
# =================================================#

# plot the map
par(oma=c(3,3,1,1), mgp=c(1.7,0.3,0), font.lab=2, tcl=-0.2)
plot_tiles(lcUTM)
axis(1);axis(2);box()
mtext("Easting",1,1.7,font=2,cex=1.2)
mtext("Northing",2,1.7,font=2,cex=1.2)
legend("bottomleft", bty="o", cex=0.8, y.int=0.6,
  legend="Map tiles: Thunderforest Outdoors; CRS: UTM Zone 50S, WGS84 (EPSG:32750)",
       box.col="#00000000", bg="#ffffff80")
addnortharrow()
addscalebar(plotepsg=32750, htin=0.12, label.cex=1.2, padin=c(0.2,0.4))

# make a scaling factor for bubble sizes for data and legend
ff <- signif((0.03*(par("usr")[4]-par("usr")[3]))/sqrt(max(lcwater23$P,na.rm=T)),3)
# plot the data as area-proportional bubbles
with(lcwater23, symbols(Easting.GPS, Northing.GPS, add=TRUE, circles = ff*sqrt(P), inches=F,
                      bg="#ffe00080", fg="sienna"))
# plot symbol to represnt samples exceeding guideline
with(lcwater23[which(lcwater23$P > GV),], points(Easting.GPS, Northing.GPS,
                                                 pch=19, col="red"))
# set up values to plot legend
xy <- par("usr")
if(pretty(lcwater23$P)[1]==0){
  bublo <- pretty(lcwater23$P)[2]/2
} else {
  bublo <- pretty(lcwater23$P)[1]
}
bubmd <- median(pretty(lcwater23$P))
bubhi <- tail(pretty(lcwater23$P),1)

# define dimensions of legend box as c(x1, x2, y1, y2) in axis proportions
bxy <- c(0.02,0.25,0.7,0.99)
# draw legend box
rect(xy[1]+bxy[1]*(xy[2]-xy[1]), xy[3]+bxy[3]*(xy[4]-xy[3]),
     xy[1]+bxy[2]*(xy[2]-xy[1]), xy[3]+bxy[4]*(xy[4]-xy[3]),
     col="#ffffffa0", border = "grey50")
# draw legend bubbles
symbols(rep(xy[1]+0.08*(xy[2]-xy[1]), 3),
  c(xy[3]+0.9*(xy[4]-xy[3]), xy[3]+0.83*(xy[4]-xy[3]), xy[3]+0.78*(xy[4]-xy[3])),
  add=T, inches=F, circles=ff*sqrt(c(bubhi,bubmd,bublo)),
  bg="#ffe00080", fg="sienna")
# draw legend 'exceed guideline' symbol
points(xy[1]+0.05*(xy[2]-xy[1]), xy[3]+0.73*(xy[4]-xy[3]), pch=19, col="red")
# add legend text
text(c(rep(xy[1]+0.1*(xy[2]-xy[1]), 3), xy[1]+0.05*(xy[2]-xy[1])),
     c(xy[3]+0.9*(xy[4]-xy[3]), xy[3]+0.83*(xy[4]-xy[3]),
       xy[3]+0.78*(xy[4]-xy[3]), xy[3]+0.73*(xy[4]-xy[3])),
     labels=c(bubhi,bubmd,bublo,paste0("Exceeds ",GV,"\n",GU," guideline")),
     pos=4, offset=1.2)
# add legend title
text(xy[1]+0.12*(xy[2]-xy[1]), xy[3]+0.96*(xy[4]-xy[3]),
     labels=paste0("P (",GU,")"), font=2, cex=1.2)
