library("CASdatasets")
library("ggplot2")
library("tidyverse")
library("AER")
library("glmnet")
library("gbm")
# Out aim is to predict the number of claims that a policyholder will make given their age, car brand....etc
data(freMTPL2freq)
data <- as_tibble(freMTPL2freq)
head(data)
view(data)
str(data)

# refining  the data
data$VehGas <-  as.factor(data$VehGas)
data$ClaimNb <- as.double(data$ClaimNb)

# distribution of Exposure
ggplot(data, aes(x = Exposure)) + 
  geom_histogram() + 
  labs(title = "Histogram of Exposure") + theme(panel.grid = element_blank())

# distribution of Claims

ggplot(data, aes(x = ClaimNb)) + 
  geom_histogram() + 
  labs(title = "Histogram of Claims") + theme(panel.grid = element_blank()) + 
  scale_x_continuous(breaks = seq(0,10, by = 1))

data$Freq = as.double(data$ClaimNb/data$Exposure)

data$Density <- log(data$Density)

str(data)

# Claims percentage

claimpercent <- data %>% 
  group_by(ClaimNb) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count / sum(count) * 100)

claimpercent

# Claim occurrence indicator
Claim_occurence <- claimpercent %>% 
  mutate(claim_occ = ifelse(ClaimNb >= 1,"Yes", "No")) %>% 
  group_by(claim_occ) %>% 
  summarise(percent = sum(percent))
Claim_occurence  

ggplot(Claim_occurence, aes(x = claim_occ, y = percent, fill = claim_occ)) + 
  geom_col() + 
  labs(title = "Claim occurnace Indicator",
       x = "Claim occurence",
       y = "Percentage (%)",
       fill = "Claim occurence")

# Check for missing data
colSums(is.na(data))

# Influence of Driver age on Frequency of claims
driver_freq <- data %>% 
  group_by(DrivAge) %>% 
  summarise(Avg_freq = mean(Freq))

driver_freq

driver_freq$DrivAge[which.min(driver_freq$Avg_freq)]
driver_freq$DrivAge[which.max(driver_freq$Avg_freq)]
age_claims <- data %>% 
  group_by(DrivAge) %>% 
  summarise(Claims = sum(ClaimNb))

age_claims[age_claims$DrivAge == 92,]
age_claims[age_claims$Claims == 1,]

driver_freq$DrivAge[which.max(driver_freq$Avg_freq)]
driver_freq[driver_freq$DrivAge == 94,]

ggplot(age_claims, aes(x  = DrivAge, y = Claims)) + 
  geom_col(fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal()

ggplot(driver_freq, aes(x = DrivAge, y = Avg_freq)) + 
  geom_area(fill = "lightgreen", color = "black") +
  labs(title = "Average Frequency at Each Age",
       x = "Driver Age",
       y = "Average Frequency")

# Vehicle brand and age influence on frequency


p3 <- data %>% 
  mutate(vehagegroup = as.factor(floor(VehAge/10))) %>% 
  group_by(VehBrand, vehagegroup) %>% 
    summarise(avgFreq = mean(Freq))
p3

levels(p3$vehagegroup) <- c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99","100")
p3

Plot4 <- p3 %>% 
  ggplot(aes(x = vehagegroup, y = avgFreq, fill = VehBrand)) +
  geom_col(position = "dodge") +
  facet_wrap(VehBrand~.)+
  labs(title = "Vehicle brand and age on frequency",
       x = "Vehicle Age (in years)",
       y = "Average Frequency")+
  coord_flip()
Plot4

# Relationship between area and Bonus Malus on Frequency
summary(data$BonusMalus)
p4 <- data
levels(p4$Area) <- c("A - Rural", "B", "C", "D", "E", "F - urban")
plot4 <- p4 %>% 
  ggplot(aes(x = Area, y = BonusMalus)) + 
  geom_boxplot() +
  labs(title = "Area vs BonusMalus",
       x = "Area",
       y = "BonusMalus") +
  scale_y_log10() + coord_flip()
plot4

# Splitting data into Training and Testing set
set.seed(0)
test_n <- round(dim(data)[1]*0.3)
train_n <- dim(data)[1] - test_n
test_rows <- sample(1:dim(data)[1], test_n)

test_data <- data[test_rows,] # test data

train_data <- data[-test_rows,] # training data

# GLM

poissonglm<- glm(ClaimNb ~  VehPower + VehAge + DrivAge + BonusMalus
                 + VehBrand + VehGas + Density + Region + Area, data=train_data, family = "poisson",
                 offset=log(Exposure))
summary(poissonglm)

dispersiontest(poissonglm, trafo = 1)

# Ridge and Lasso regression

lasso_train <- train_data
glm.ridge <- cv.glmnet(x=data.matrix(lasso_train[, c(3,4,5,6,7,8,9,10,11,12)]), 
                       y=data.matrix(lasso_train[, 2]),family="poisson",
                       offset =data.matrix(log(lasso_train[, 3])) ,alpha = 0, intercept=TRUE, type.measure = "mse")

glm.lasso <- cv.glmnet(x=data.matrix(lasso_train[, c(3,4,5,6,7,8,9,10,11,12)]), 
                       y=data.matrix(lasso_train[, 2]),family="poisson",
                       offset =data.matrix(lasso_train[, 3]) ,alpha = 1, intercept=TRUE, type.measure = "mse")

par(mfrow=c(1,2))
plot(glm.ridge, main = "Ridge") ; plot(glm.ridge, main = "Lasso") 

glm.ridge$lambda.min
coef(glm.ridge, s = "lambda.min")

glm.lasso$lambda.min
coef(glm.lasso, s = "lambda.min")

# Gradient Boosting
# set.seed(0)
# boost.train=gbm(ClaimNb ~ offset(log(Exposure)) + VehPower + VehAge + DrivAge + BonusMalus
                + VehBrand + VehGas + Density + Region + Area , data = train_data,
                distribution="poisson",n.trees=3000,shrinkage=0.01,interaction.depth=20)
# summary(boost.train)

# Validation and comparison
pred.glm <- predict(poissonglm, test_data[,c(3,4,5,6,7,8,9,10,11,12)],
                    type = "response", newoffset=log(Exposure))

pred.glm.ridge <- predict(glm.ridge, newx = data.matrix(test_data[,c(3,4,5,6,7,8,9,10,11,12)]), 
                          type = "response", s = "lambda.min", newoffset = data.matrix(log(test_data[,3])))

pred.glm.lasso <- predict(glm.lasso, newx = data.matrix(test_data[,c(3,4,5,6,7,8,9,10,11,12)]), 
                          type = "response", s = "lambda.min", newoffset = data.matrix(log(test_data[,3])))

val.err.glm <- sum( abs(pred.glm - test_data$ClaimNb) ) / test_n

val.err.ridge <- sum( abs(pred.glm.ridge - test_data$ClaimNb)  ) / test_n
val.err.lasso <- sum( abs(pred.glm.lasso - test_data$ClaimNb)  ) / test_n

val.err = tibble(val.err.glm =val.err.glm, val.err.ridge=val.err.ridge, val.err.lasso=val.err.lasso)

val.err <- val.err %>% 
  pivot_longer(cols = c("val.err.glm", "val.err.ridge", "val.err.lasso")) %>% 
  mutate(name = factor(name, levels =  c("val.err.glm", "val.err.ridge", "val.err.lasso") ,
                       labels = c( "glm", "ridge", "lasso"),  ordered = FALSE))

val.err %>% 
  ggplot(mapping=aes(x=name, y=value)) + 
  geom_col(aes(fill = name))+
  labs(title = "Models and Prediction Mean Absolute Error (MAE)",
       x = "Models",
       y = "MAE",
       fill="Models")
