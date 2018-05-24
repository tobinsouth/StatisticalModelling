## Import Data set----

mammo <- read.csv('mammo.txt', header = TRUE)
head(mammo)

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
