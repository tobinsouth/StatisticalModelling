# Libraries----

library(ggplot2)
library(GGally)
source('conf_int_prop.R')


#Introduction (description of data and purpose of the analysis). (5 marks)  (Tobin)----

#Data entry and data cleaning. (5 marks) (Tobin?)----

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

#Data visualisation and data summaries. (10 marks) (Tobin)----

pairs(mammo[2:6]) # need to make prettier # Lily: I cut out BI.RADS

ggpairs(mammo[, 2:6])

par(mfrow=c(2,2)) # TOBIN IF YOU MAKE PLOTS A MATRIX PLEASE SET IT BACK TO ONE AFTER
ggplot(data = mammo, aes(y= Age, x=BI.RADS )) + geom_point()
par(mfrow = c(1,1))

ggplot(data = mammo, aes(y = factor(Severity), x = Age)) + 
  geom_boxplot()

ggplot(data = mammo, aes(y = factor(Severity), x = Shape)) + 
  geom_histogram()

# Confidence Intervals

# Age
# For Age I had to split the ages into categories

summary(mammo$Age)

hist(mammo$Age)
mammo$ageGroup <- cut(mammo$Age,
                        breaks = c(0,20,40,60,80),
                        labels = c("20 and Under","21-40","41-60","Over 60"))

mammo$ageGroup[mammo$Age == 60]

confidenceInt <- mammo %>% 
  split(.$ageGroup) %>%
  map_df(~conf_int_prop(.$Severity), .id = "AgeGroup")

ggplot(confidenceInt, aes(x = AgeGroup, y = Proportion)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = Upper, ymin = Lower))

# Shape

confidenceInt <- mammo %>% 
  split(.$Shape) %>%
  map_df(~conf_int_prop(.$Severity), .id = "Shape")

ggplot(confidenceInt, aes(x = Shape, y = Proportion)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = Upper, ymin = Lower))

# Margin

confidenceInt <- mammo %>% 
  split(.$Margin) %>%
  map_df(~conf_int_prop(.$Severity), .id = "Margin")

ggplot(confidenceInt, aes(x = Margin, y = Proportion)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = Upper, ymin = Lower))

# Density

confidenceInt <- mammo %>% 
  split(.$Density) %>%
  map_df(~conf_int_prop(.$Severity), .id = "Density")

ggplot(confidenceInt, aes(x = Density, y = Proportion)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = Upper, ymin = Lower))

#Model fitting and model selection. (5 marks) (Lily)----

complete=!is.na(mammo$Age)&!is.na(mammo$Shape)&!is.na(mammo$Margin)&!is.na(mammo$Density) # Use complete data for models

mod.full <- glm(formula = Severity ~ Age + Shape + Margin + Density,
               family = binomial,
               data = mammo[complete,]) #Defining full model
summary(mod.full)

s2 <- sum((mod.full$residuals)^2)/mod.full$df.residual
s2

mod.step <- step(mod.full, scale = s2) # woohoo it works
summary(mod.step)

#Justification for choice of final model. (5 marks) (Lily)----

#Interpretation of parameters from final model. (5 marks)  (Lily)-----

#Predicting probabilities and interpretation. (10 marks) (James?) 😩----

pred.CV <- data.frame(YoungestHouseholdMember=seq(min(DogPark$YoungestHouseholdMember,na.rm=TRUE),
                                                  max(DogPark$YoungestHouseholdMember,na.rm=TRUE),
                                                  length.out=100))
pred.CV$CurrentlyVisit.hat <- predict(mod.fit,newdata=pred.CV)


#Cool