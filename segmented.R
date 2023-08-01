
rm(list=ls())

library(segmented)

# To Do:

# get the temp vs dive duration data in two columns of Excel with a header line,
# save the file in .csv format. name the file "tempVsDiveDuration.csv"
# put the file into the same directory as this script.

dat <- read.table("tempVsDiveDuration.csv", header = TRUE, sep = ",")
plot(dat$Temp_C, dat$Dur_m)

dat2 <- dat[dat$Dur_m > 90 & dat$Temp_C < 18,]
plot(dat2$Temp_C, dat2$Dur_m)

#
#
# x <- c(1:10, 13:22)
# y <- numeric(length(x))
# ## Create first segment
# # rnorm is a random number generator from normal distribution
# # mean zero and SD of 1.5
# y[1:10] <- 20:11 + rnorm(10, 0, 1.5)
# ## Create second segment
# y[11:20] <- seq(11, 15, len=10) + rnorm(10, 0, 1.5)
#
# ## Plot it
# par(mar=c(4,4,1,1)+0.2)
# plot(x,y, ylim=c(5, 20), pch=16)
x <- dat2$Temp_C
y <- dat2$Dur_m

breaks <- x[which(x >= 14 & x <= 15)]

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




