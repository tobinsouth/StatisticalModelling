#Entering data
Child<- c(1:12) #🤱
Height<-c(108.7,161.29,95.25,100.33,115.57,97.79,109.22,57.15,93.98,59.69,83.82,147.32) #📏
Weight<-c(18.14,42.41,16.10,13.61,23.59,7.71,17.46,3.86,14.97,4.31,9.53,35.83) #🍔
Length<-c(37,49.5,34.5,36,43,28,37,20,33.5,30.5,38.5,47.0)
catheter <-data.frame(Child, Height, Weight, Length)
head(catheter)

#Comparing Variables with Pair-Wise Scatter plots
pairs(subset(catheter, select=c(2:4)))
#lots of strong positive linear fun

#Fitting models
lm1<-lm(Length~Height+Weight, data=catheter)
lm2<-lm(Length~Height, data=catheter)
lm3<-lm(Length~Weight, data=catheter)


#Model assumptions: linearity, homoscedasticity, normality, independence
#for whole thing and each predictor. Please don't forget

#Diagnostic plot
plot(lm1)
plot(lm2)
plot(lm3)
res1<-residuals(lm1)
plot(catheter$Height, res1)
plot(catheter$Weight, res1)


summary(lm1)
summary(lm2)
summary(lm3)
