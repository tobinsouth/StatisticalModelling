library(ggplot2)
install.packages("GGally")
library(GGally)
## Import Data set----

mammo <- read.csv('mammo.txt', header = TRUE)
head(mammo)

<<<<<<< HEAD



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
=======
class(mammo$BI.RADS)

class(mammo$Age)
mammo$Age[mammo$Age == "?"] <- NA
mammo$Age <- as.numeric(mammo$Age)
summary(mammo$Age, exclude = FALSE) # 5 NAs

class(mammo$Shape)
mammo$Shape <- as.character(mammo$Shape)
mammo$Shape[mammo$Shape == "?"] <- NA
mammo$Shape <- factor(mammo$Shape)
summary(mammo$Shape, exclude = FALSE) # 31 NAs

class(mammo$Shape)
mammo$Shape[mammo$Shape == "?"] <- NA
class(mammo$Margin)
mammo$Margin[mammo$Margin == "?"] <- NA
class(mammo$Density)
mammo$Density[mammo$Density == "?"] <- NA
class(mammo$Severity)

table(mammo$BI.RADS) # 2 question marks # Not important
table(mammo$Age) # 5 question marks
table(mammo$Shape) # 31 question marks
table(mammo$Margin) # 48 question marks
table(mammo$Density) # 76 question marks
table(mammo$Severity) # No m

mammo[!is.na(mammo$Age),] # Use complete data for models
>>>>>>> e22d65d653adedac56084ad0bcf686d2c96aace6
