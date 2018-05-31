# Libraries----

#library(ggplot2) #This is in tidyverse
library(tidyverse)
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
mammo$Severity <- factor(mammo$Severity)
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

#Data visualisation and data summaries. (10 marks) 

pairs(mammo[2:6]) # need to make prettier # Lily: I cut out BI.RADS

#James: This makes it easier to make changes to the dataset that only effects the plot
mammo %>%
  mutate(Severity = as.factor(Severity)) %>%
  select(2:6) %>%
  ggpairs(upper = list(continuous = "blank",
                       combo ="blank",
                       discrete = "blank",
                       na = "blank"),
          lower = list(continuous = "cor",
                       combo = "box_no_facet",
                       discrete = "facetbar",
                       na = "na")) # I think removing the upper half makes it look cleaner and those plots were pretty messy


ggplot(data = mammo, aes(y= Age, x=BI.RADS )) + geom_point()

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

# We are 95% confident the true proportion of people 20 and under with a malignant mass lies between 3% and 14%

# Shape

mammo$Shape.orig <- mammo$Shape

confidenceInt <- mammo %>% 
  split(.$Shape.orig) %>%
  map_df(~conf_int_prop(.$Severity), .id = "Shape")

ggplot(confidenceInt, aes(x = Shape, y = Proportion)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = Upper, ymin = Lower))

levels(mammo$Shape)[levels(mammo$Shape)=="1"] <- "1 and 2"
levels(mammo$Shape)[levels(mammo$Shape)=="2"] <- "1 and 2"
table(mammo$Shape)

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

# Full Model

mammo$Margin <- relevel(mammo$Margin, ref="1")

mod.full1 <- glm(formula = Severity ~ Age + Shape + Margin + Density,
               family = binomial,
               data = mammo[complete,]) #Defining full model

mammo$Margin <- relevel(mammo$Margin, ref="2")

mod.full2 <- glm(formula = Severity ~ Age + Shape + Margin + Density,
                 family = binomial,
                 data = mammo[complete,]) #Defining full model

mammo$Margin <- relevel(mammo$Margin, ref="3")

mod.full3 <- glm(formula = Severity ~ Age + Shape + Margin + Density,
                 family = binomial,
                 data = mammo[complete,]) #Defining full model

mammo$Margin <- relevel(mammo$Margin, ref="4")

mod.full4 <- glm(formula = Severity ~ Age + Shape + Margin + Density,
                 family = binomial,
                 data = mammo[complete,]) #Defining full model

mammo$Margin <- relevel(mammo$Margin, ref="5")

mod.full5 <- glm(formula = Severity ~ Age + Shape + Margin + Density,
                 family = binomial,
                 data = mammo[complete,]) #Defining full model

summary(mod.full1)
summary(mod.full2)
summary(mod.full3)
summary(mod.full4)
summary(mod.full5)

table(mammo$Margin)

## We should use 4

mod.full <- mod.full4

s2 <- sum((mod.full$residuals)^2)/mod.full$df.residual
s2

# Stepwise Selection

mod.step <- step(mod.full, scale = s2) # woohoo it works
summary(mod.step)

# Backwards by significance

mod.back <- mod.full

summary(mod.back)
mod.back <- update(mod.back, .~. - Density)
summary(mod.back)
mod.back <- update(mod.back, .~. - Shape)
summary(mod.back)

### From prac 5
summary(mod.full)
summary(mod.back)
summary(mod.full)
qchisq(1-0.05, df = 7)
mod.step$deviance - mod.full$deviance #2202.106
anova(mod.step, mod.full) # reject hypothesis that all coefficients are equal

qchisq(1-0.05, df = 2)
mod.step$deviance - mod.back$deviance
anova(mod.step, mod.back) # These literally say the same thing but in the prac we use the difference

AIC(mod.full, mod.step, mod.back)

#Justification for choice of final model. (5 marks) (Lily)----


#Interpretation of parameters from final model. (5 marks)  (Lily)-----

#Predicting probabilities and interpretation. (10 marks) (James) ----

pred1 <- data.frame(Age=seq(min(mammo$Age,na.rm=TRUE), max(mammo$Age,na.rm=TRUE),length.out=74-2), Shape = "1")
pred2 <- data.frame(Age=seq(min(mammo$Age,na.rm=TRUE), max(mammo$Age,na.rm=TRUE),length.out=74-2), Shape = "2")
pred3 <- data.frame(Age=seq(min(mammo$Age,na.rm=TRUE), max(mammo$Age,na.rm=TRUE),length.out=74-2), Shape = "3")
pred4 <- data.frame(Age=seq(min(mammo$Age,na.rm=TRUE), max(mammo$Age,na.rm=TRUE),length.out=74-2), Shape = "4")

pred <- cbind(pred1, pred2, pred3, pred4)

pred$Severity <- predict(mod.step,newdata=pred)
pred$SeverityTrans <- exp(pred$Severity)/(1+exp(pred$Severity))

plot(x = pred$Age, y = pred$SeverityTrans)


## This is my appraoch to prediction - James

predict <- predict(mod.step, type="response")
predict.df <- data.frame(predict.prob = predict)

predict.df.indexed <- data.frame(predict.df, id = row.names(predict.df))
mammo.indexed <- data.frame(mammo, id = row.names(mammo))

mammo.predict <- left_join(mammo.indexed, predict.df.indexed, by="id")
mammo.predict <- mammo.predict[ , !names(mammo.predict) %in% c("id")]


#Calculate the percentage that our model correctly predicts Severity
mammo.predict %>%
  mutate(predict = (predict.prob >= 0.5),
         predict = as.integer(predict),
         correct = (predict == Severity)) %>%
  count(correct) %>%
  summarise(hit.rate = n[2]/(n[1] + n[2]))

# Using mod.step on the original non-missing data our prediction rate is 79.7%

predict <- predict(mod.full, type="response")
predict.df <- data.frame(predict.prob = predict)

predict.df.indexed <- data.frame(predict.df, id = row.names(predict.df))
mammo.indexed <- data.frame(mammo, id = row.names(mammo))

mammo.predict <- left_join(mammo.indexed, predict.df.indexed, by="id")
mammo.predict <- mammo.predict[ , !names(mammo.predict) %in% c("id")]


#Calculate the percentage that our model correctly predicts Severity
mammo.predict %>%
  mutate(predict = (predict.prob >= 0.5),
         predict = as.integer(predict),
         correct = (predict == Severity)) %>%
  count(correct) %>%
  summarise(hit.rate = n[2]/(n[1] + n[2]))

# Using mod.full on the original non-missing data our prediction rate is 81.5%. This is not significantly better than the reduced model so the reduced model is probably good

predict <- predict(mod.step, newdata = mammo ,type="response")
predict.df <- data.frame(predict.prob = predict)

predict.df.indexed <- data.frame(predict.df, id = row.names(predict.df))
mammo.indexed <- data.frame(mammo, id = row.names(mammo))

mammo.predict <- left_join(mammo.indexed, predict.df.indexed, by="id")
mammo.predict <- mammo.predict[ , !names(mammo.predict) %in% c("id")]


#Calculate the percentage that our model correctly predicts Severity
mammo.predict %>%
  mutate(predict = (predict.prob >= 0.5),
         predict = as.integer(predict),
         correct = (predict == Severity)) %>%
  count(correct) %>%
  summarise(hit.rate = n[2]/(n[1] + n[2]))

# Using mod.step on the original data which is ONLY MISSING SHAPE OR AGE our prediction rate is 79.6%. This means we only have 36 NAs

