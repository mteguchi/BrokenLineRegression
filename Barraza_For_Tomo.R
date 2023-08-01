gmet<- read.csv("Barraza_Metals_V1.csv", header=TRUE )

gmetb <- gmet[which(gmet$Sample_type=="RBC"),] #Metal data partitioned to just include blood samples
gmetb


#Broken line regression Cadmium and Selenium
#cdse.breaks<- gmetb$Cd[which(gmetb$Cd >= 0.01 & gmetb$Cd<= 0.15)]

cdse.breaks <- seq(from = 0.01, to = 0.15, by = 0.01)
mse<- numeric(length(cdse.breaks))

1:length(cdse.breaks)
for(i in 1:length(cdse.breaks)){
  piecewise1 <- lm(Se ~ Cd * (Cd<(cdse.breaks[i])) +
                     Cd * (Cd >= (cdse.breaks[i])), data= gmetb)
  # the 6th output of "summary" function is MSE (= sigma)
  mse[i] <- summary(piecewise1)[6]
}
mse <- as.numeric(mse) #Convert to numeric
cdse.breaks[which(mse==min(mse))] #minimum break point at 0.095

library(segmented)

lin.mod<- lm(Se ~ Cd, data=gmetb)
summary(lin.mod)

segmented.mod <- segmented(lin.mod,
                           seg.Z = ~ Cd,
                           psi =cdse.breaks[which(mse==min(mse))],
                           data=gmetb )

plot(gmetb$Cd,
     gmetb$Se,
     col = ifelse(gmetb$Location=="SDB","orange","dodgerblue"),
     pch=ifelse(gmetb$Location=="SDB",1,2),
     xlab="Cadmium (ug/ml)",
     ylab="Selenium (ug/ml)")

plot(segmented.mod, lty=2, add=T)
summary(segmented.mod)
