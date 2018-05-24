library(ggplot2)
install.packages("GGally") # Not needed but it looks cool as shit
library(GGally)


#Introduction (description of data and purpose of the analysis). (5 marks)  (Tobin)

#Data entry and data cleaning. (5 marks) (Tobin?)

mammo <- read.csv('mammo.txt', header = TRUE)
head(mammo)

table(mammo$BI.RADS) # 2 question marks # Not important
table(mammo$Age) # 5 question marks
table(mammo$Shape) # 31 question marks
table(mammo$Margin) # 48 question marks
table(mammo$Density) # 76 question marks
table(mammo$Severity) # No m

class(mammo$Age)
mammo$Age[mammo$Age == "?"] <- NA
mammo$Age <- as.numeric(mammo$Age)
summary(mammo$Age, exclude = FALSE) # 5 NAs

class(mammo$Shape)
mammo$Shape <- as.character(mammo$Shape)
mammo$Shape[mammo$Shape == "?"] <- NA
mammo$Shape <- factor(mammo$Shape)
summary(mammo$Shape, exclude = FALSE) # 31 NAs

class(mammo$Margin)
mammo$Margin <- as.character(mammo$Margin)
mammo$Margin[mammo$Margin == "?"] <- NA
mammo$Margin <- factor(mammo$Margin)
summary(mammo$Margin, exclude = FALSE) # 48 NAs

class(mammo$Density)
mammo$Density <- as.character(mammo$Density)
mammo$Density[mammo$Density == "?"] <- NA
mammo$Density <- factor(mammo$Density)
summary(mammo$Density, exclude = FALSE) # 76 NAs

class(mammo$Severity)
summary(mammo$Severity, exclude = FALSE) # 0 NAs

# Just for yolo, wont be used in model
class(mammo$BI.RADS)
mammo$BI.RADS <- as.character(mammo$BI.RADS)
mammo$BI.RADS[mammo$BI.RADS == "?"] <- NA
mammo$BI.RADS <- factor(mammo$BI.RADS)
summary(mammo$BI.RADS, exclude = FALSE) # 2 NAs, 1 outlier
mammo$BI.RADS[mammo$BI.RADS == 55] <- NA # Set outlier to NA
mammo$BI.RADS <- factor(mammo$BI.RADS) 
summary(mammo$BI.RADS, exclude = FALSE) # 3 NAs

mammo$BI.RADS <- as.numeric(mammo$BI.RADS) # Maybe Use as Numeric?
summary(mammo$BI.RADS)


# TODO: Ask if Age is forced to be a parameter 
# Can these varibles be interpolated

#Data visualisation and data summaries. (10 marks) (Tobin)

pairs(mammo) # need to make prettier

ggpairs(mammo[, 2:6])

par(mfrow=c(2,2))
ggplot(data = mammo, aes(y= Age, x=BI.RADS )) + geom_point()

#Model fitting and model selection. (5 marks) (Lily)

mammo[!is.na(mammo$Age),] # Use complete data for models

#Justification for choice of final model. (5 marks) (Lily)

#Interpretation of parameters from final model. (5 marks)  (Lily)

#Predicting probabilities and interpretation. (10 marks) (James?) 😩

