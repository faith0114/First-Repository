library(gdata)
library(jpeg)
library(imager)
library(OpenImageR)
library(TDA)
library(pixmap)
library(data.table)
library(nseval)
library(wvtool)
library(spatstat)

#reading in the image and the working directory
Millais <- jpeg::readJPEG("C:/Users/jenes/OneDrive/Image Recognition Project/220px-Millais_-_Self-Portrait.jpg")
imageShow(Millais)

#turning the image into grayscale
r2g = rgb_2gray(Millais)
imageShow(r2g)

#unwrapping an image's matrix
x <- rnorm(100)
y <- rnorm(100, mean=3+5*x, sd=0.25)
img <- coef( summary(lm( y ~ x )) )
unmatrix(img)

#trying to read the image and find the local binary pattern(s) within the image
library(imager)

array(Millais)
par(mfrow=c(2,2))
r1 <- lbp(r2g, 1)

par(mfrow=c(2,2))
image(rot90c(r1$lbp.u2),col = gray((0:58)/58), main="lbp.u2 (r=1, 8 points)", useRaster=TRUE, asp=1, axes=FALSE)
hist(r1$lbp.u2,breaks=59, main="Histogram of lbp.u2")
hist(r1$lbp.ori,breaks=256, main="Histogram of lbp.ori")

#repeating the process and applying LBP to a stick-figure image.
Fake <- jpeg::readJPEG(normalizePath("C:/Users/jenes/OneDrive/Image Recognition Project/hard_116_1101.jpg"))
imageShow(Fake)

#applying gray-scale
r2g = rgb_2gray(Fake)
imageShow(r2g)

#applying lbp to the picture
array(Fake)
par(mfrow=c(2,2))
r1 <- lbp(r2g, 1)
image(rot90c(r1$lbp.u2),col = gray((0:58)/58), main="lbp.u2 (r=10, 100 points)", useRaster=TRUE, asp=1, axes=FALSE)
hist(r1$lbp.u2,breaks=59, main="Histogram of lbp.u2")
hist(r1$lbp.ori,breaks=256, main="Histogram of lbp.ori")




