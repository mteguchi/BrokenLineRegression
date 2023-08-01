
# clear the workspace first:
rm(list=ls())

# Create an Excel spreadsheet with # females and nesting success
# in two columns a header line and save the spreadsheet in .csv format.
# Here, I assume the headers are "female" and "nesting".
#
# name the file "femaleVsNestingSucess.csv" (or anything else but
# make sure to change the file name below). Then put the file into
# the same directory as this script.


# creating data to test my code:
#females <- ceiling(rnorm(25, mean = 400, sd = 150))
#nests <- ifelse(females < 400,
#                300 - 0.7 * females + rnorm(length(females), 0, 50),
#                100 -0.2 * females + rnorm(length(females), 0, 25))
#test.dat <- data.frame(female = females, nesting = nests)
#write.table(test.dat, file = "femaleVsNestingSuccess.csv",
#            sep = ",", row.names = F)

dat <- read.table("femaleVsNestingSuccess.csv",
                  header = TRUE,
                  sep = ",")
plot(dat$female, dat$nesting)   # simple plot

# The first uses mean-squared errors to find where the cut off point
# is. This is called piece-wise regression. Note that the second
# method below will do the same thing in one step.
# This is to show another way...

# A range of cut off values for the number of females;
# can be anything that are possible - choose the range from
# the plot above
breaks <- dat$female[which(dat$female >= 350 & dat$female <= 420)]

# Create an empty vector for output
mse <- numeric(length(breaks))

# a loop to go through each break point and fit a linear model
# then store the Mean squared errors (or sigma) into MSE vector
for(i in 1:length(breaks)){
  piecewise1 <- lm(nesting ~ female * (female < breaks[i]) +
                     female * (female>=breaks[i]),
                   data = dat)
  # the 6th output of "summary" function is MSE (= sigma)
  mse[i] <- summary(piecewise1)[6]
}

# convert the list into a numeric vector
mse <- as.numeric(mse)

# find the minimum MSE then corresponding break point
breaks[which(mse==min(mse))]

# use the derived break point above to do the regression
piecewise2 <- lm(nesting ~ female*(female < breaks[which(mse==min(mse))]) +
                   female*(female > breaks[which(mse==min(mse))]),
                 data = dat)
summary(piecewise2)

plot(dat$female,
     dat$nesting,
     ylim=c(min(dat$nesting), max(dat$nesting)), pch=16)


# This section shows how to use the package segmented
# load the necessary library
library(segmented)

# segmented - psi is a starting value for the cut off point in this case
lin.mod <- lm(nesting ~ female, data = dat)
segmented.mod <- segmented(lin.mod, seg.Z = ~ female,
                           psi = breaks[which(mse==min(mse))],
                           data = dat)

plot(dat$female,dat$nesting, pch = 16,
     ylim=c(min(dat$nesting), max(dat$nesting)))
plot(segmented.mod, add=T)
summary(segmented.mod)




