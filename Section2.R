library(ggplot2)
install.packages("GGally")
library(GGally)
## Import Data set----

mammo <- read.csv('mammo.txt', header = TRUE)
head(mammo)




#Introduction (description of data and purpose of the analysis). (5 marks)  (Tobin)

#Data entry and data cleaning. (5 marks) (Tobin?)

# TODO: Ask if Age is forced to be a parameter 
# Can these varibles be interpolated

#Data visualisation and data summaries. (10 marks) (Tobin)

pairs(mammo) # need to make prettier

ggpairs(mammo)

par(mfrow=c(2,2))
ggplot(data = mammo, aes(y= Age, x=BI.RADS )) + geom_point()

#Model fitting and model selection. (5 marks) (Lily)

#Justification for choice of final model. (5 marks) (Lily)

#Interpretation of parameters from final model. (5 marks)  (Lily)

#Predicting probabilities and interpretation. (10 marks) (James?) 😩