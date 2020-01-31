Image2Hemiphot <- function(image = "", fraction) {
  if (length(dim(image)) < 2){ 
    cat("Cannot convert image. Need at least 2D image to convert", "\n")
    return(im)
  }  
  fisheye.cx <- dim(image)[2]/2 * 1.011719
  # x coordinate of center
  fisheye.cy <- dim(image)[1]/2                    # y coordinate of center
  fisheye.cr <- (dim(image)[1]/2) * fraction             # radius of hemiphot
  cat("centre of circle (", fisheye.cx, ", ", fisheye.cy, ")", "\n", sep = "")
  cat("radius of circle ", fisheye.cr, "\n", sep = "")
  HI <- list(image, fisheye.cx, fisheye.cy, fisheye.cr)
  return(HI)
}

SetCircle <- function(image = "", cx = 0, cy = 0, cr = 0) {
  if(cx > 0) image[[2]] <- cx
  if(cy > 0) image[[3]] <- cy
  if(cr > 0) image[[4]] <- cr
  return(image)
}

DrawCircle <- function(cx = 100, cy = 100, radius = 50){
  x = 0:radius
  y = sin(acos(x/radius)) * radius
  points(cx + x, cy + y, col = "red", cex = 0.1)
  points(cx + x, cy - y, col = "red", cex = 0.1)
  points(cx - x, cy + y, col = "red", cex = 0.1)
  points(cx - x, cy - y, col = "red", cex = 0.1)
  y = x
  x = cos(asin(x/radius)) * radius
  points(cx + x, cy + y, col = "red", cex = 0.1)
  points(cx + x, cy - y, col = "red", cex = 0.1)
  points(cx - x, cy + y, col = "red", cex = 0.1)
  points(cx - x, cy - y, col = "red", cex = 0.1)
  text(cx+radius-25, cy, "W", col = "red")
  text(cx-radius+25, cy, "E", col = "red")
  text(cx, cy+radius-25, "N", col = "red")
  text(cx, cy-radius+25, "S", col = "red")
}

SelectRGB <- function(image = "", channel = ""){
  if(channel %in% c("R", "G", "B")){
    if(channel == "R") layer = image[[1]][,,1]
    if(channel == "G") layer = image[[1]][,,2]
    if(channel == "B") layer = image[[1]][,,3]
    HI = list(layer, image[[2]], image[[3]], image[[4]])
    return(HI)
  } else {
    cat("need to select a color channel (R, G, or B)","\n")
  }
}


PlotHemiImage <- function(image = "", draw.circle = TRUE, channel = "") {
  # can plot only in R 2.11.0 and higher  
  if (exists("rasterImage")) { 
    if(channel %in% c("R", "G", "B")) {
      plot(c(0, dim(image[[1]])[2]), c(0, dim(image[[1]])[1]), asp = 1,
           xlab = "columns", ylab = "rows", type = "n")
      if(channel == "R") rasterImage(image[[1]][, , 1], xleft = 0,
                                     ybottom = 0, xright = dim(image[[1]])[2],
                                     ytop = dim(image[[1]])[1], main = "R")
      if(channel == "G") rasterImage(image[[1]][, , 2], xleft = 0,
                                     ybottom = 0, xright = dim(image[[1]])[2],
                                     ytop = dim(image[[1]])[1], main = "G")
      if(channel == "B") rasterImage(image[[1]][, , 3], xleft = 0,
                                     ybottom = 0, xright = dim(image[[1]])[2],
                                     ytop = dim(image[[1]])[1], main = "B")
      if(draw.circle == TRUE) DrawCircle(image[[2]], image[[3]], image[[4]])
    } else {
      plot(c(0, dim(image[[1]])[2]), c(0, dim(image[[1]])[1]), asp = 1,
           xlab = "columns", ylab = "rows", type = "n")
      rasterImage(image[[1]], xleft = 0, ybottom = 0,
                  xright = dim(image[[1]])[2],
                  ytop = dim(image[[1]])[1])         ## all colors
      if(draw.circle == TRUE) DrawCircle(image[[2]], image[[3]], image[[4]])
    }
  } else { 
    cat("need R 2.11.0 and higher to support plotting of images", "\n")
  }
}

ThresholdImage <- function(image = "", th = 0.5, draw.image = F) {
  on <- image[[1]] > th
  image[[1]][on] <- 1
  image[[1]][!on] <- 0
  if (draw.image == T) PlotHemiImage(image)
  return(image)
}

DrawCircle <- function(cx = 100, cy = 100, radius = 50){
  x = 0:radius
  y = sin(acos(x/radius)) * radius
  points(cx + x, cy + y, col = "red", cex = 0.1)
  points(cx + x, cy - y, col = "red", cex = 0.1)
  points(cx - x, cy + y, col = "red", cex = 0.1)
  points(cx - x, cy - y, col = "red", cex = 0.1)
  y = x
  x = cos(asin(x/radius)) * radius
  points(cx + x, cy + y, col = "red", cex = 0.1)
  points(cx + x, cy - y, col = "red", cex = 0.1)
  points(cx - x, cy + y, col = "red", cex = 0.1)
  points(cx - x, cy - y, col = "red", cex = 0.1)
  text(cx+radius-25, cy, "W", col = "red")
  text(cx-radius+25, cy, "E", col = "red")
  text(cx, cy+radius-25, "N", col = "red")
  text(cx, cy-radius+25, "S", col = "red")
}

DrawCircles <- function(fisheye.cx, fisheye.cy, fisheye.cr){
  deg2rad = pi / 180
  steps = 1:360
  for (i in 1:89){
    x = round(fisheye.cx + cos(steps*deg2rad)*i*fisheye.cr/90,0)
    y = round(fisheye.cy + sin(steps*deg2rad)*i*fisheye.cr/90,0)
    points(x, y, col = "red", cex = 0.1)
  }
}

CalcGapFractions <- function(image = "") {
  deg2rad <- pi/180
  gap.fractions <- matrix(0, 89)
  steps         <- 1:360
  fisheye.cx    <- image[[2]]
  fisheye.cy    <- image[[3]]
  fisheye.cr    <- image[[4]]
  for (i in 1:89) {
    x <- round(fisheye.cx + cos(steps * deg2rad) * i * fisheye.cr/90, 0)
    y <- round(fisheye.cy + sin(steps * deg2rad) * i * fisheye.cr/90, 0)
    for(j in 1:360) gap.fractions[i] <- gap.fractions[i] + image[[1]][y[j], x[j]]
  }
  return(gap.fractions/360)
}

CalcOpenness <- function(fractions) {
  deg2rad        <- pi / 180
  a              <- deg2rad * 1:89
  d05            <- deg2rad * 0.5
  Atot           <- sin(a[89] + d05) - sin(a[1] - d05)
  Aa             <- sin(a + d05) - sin(a - d05)
  canopy.openess <- sum(fractions * Aa/Atot)
  return(canopy.openess)
}

CalcLAI <- function(fractions, width = 6) {
  if(dim(fractions)[1] != 89) return("incorrect dimensions for fractions")
  deg2rad <- pi/180
  angle <- c(7, 23, 38, 53, 68)                  # angles of LAI2000
  wi <- c(0.034, 0.104, 0.160, 0.218, 0.494)  # weights given by Licor canopy analyzer manual
  if(width < 0 | width > 6) width <- 6
  T <- rep(0,5)
  for(i in -width:width) T <- T + fractions[angle + i]
  T <- T/(2 * width + 1)
  LAI <- 2 * sum(-log(T) * wi * cos(angle * deg2rad))
  return(LAI)
}

hemiploteale <- function(file, fraction) {
  file %>%
    jpeg::readJPEG() %>%
    Image2Hemiphot(fraction = fraction) %>% 
    PlotHemiImage(draw.circle = TRUE)
}
