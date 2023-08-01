
rm(list=ls())

# If you don't have the "segmented" library, install it by:
# install.packages("segmented")
# at the R prompt

library(segmented)

# To Do:

# get the latitude vs proportion of NGBR (or SGBR) in two columns of Excel file
# with a header line, save the file in .csv format. Column names are
# latitude and PrNGBR.
# let's say the file name is "latVsPrNGRR.csv"
# put the file into the same directory as this script.

dat <- read.table("latVsPrNGRR.csv",
                  header = TRUE, sep = ",")
plot(dat$latitude, dat$PrNGBR)

x <- dat$latitude
y <- dat$PrNGBR

# major current latitude (say 12 degrees)
current.lat <- 12
breaks <- x[which(x >= current.lat & x <= 15)]

mse <- numeric(length(breaks))

# a loop to go through each break point and fit a linear model
# then store the Mean squared errors (or sigma) into MSE vector
for(i in 1:length(breaks)){
  piecewise1 <- lm(y ~ x*(x < breaks[i]) + x*(x>=breaks[i]))
  mse[i] <- summary(piecewise1)[6]
}

# convert the list into a numeric vector
mse <- as.numeric(mse)

# find the minimum MSE then corresponding break point
breaks[which(mse==min(mse))]

# use the derived break point above to do the regression
piecewise2 <- lm(y ~ x*(x < breaks[which(mse==min(mse))]) +
                   x*(x > breaks[which(mse==min(mse))]))
summary(piecewise2)

# do this by yourself replacing the coefficients from the summary output:
plot(x,y, ylim=c(90, 300), pch=16)
curve((3.3133 + 16.6352) + (0.5843-1.3025)*x,
      add=T, from=1, to=breaks[which(mse==min(mse))])
curve((3.3133 - 0.9116) + 0.5843*x, add=T,
      from=breaks[which(mse==min(mse))], to=max(x))
abline(v=15, lty=3)

# segmented
lin.mod <- lm(y~x)
segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi=14)

plot(x,y, pch=16, ylim=c(90, 300))
plot(segmented.mod, add=T)
summary(segmented.mod)




