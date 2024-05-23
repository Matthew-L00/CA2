install.packages("readxl")
library(readxl)
body_data <- read_excel("Dataset_2024.xlsx")
str(body_data)
#All of the data is numerical

#Check for incomplete data
incomplete_data <- body_data[!complete.cases(body_data),]
incomplete_data
#There is no missing data from the data set



#Check mean and median of the data
mean_bodyfat <- mean(body_data$`Body fat (%)`)
median_bodyfat <- median(body_data$`Body fat (%)`)

mean_density <- mean(body_data$`Density (g/cm³)`)
median_density <- median(body_data$`Density (g/cm³)`)

mean_age <- mean(body_data$`Age (years)`)
median_age <- median(body_data$`Age (years)`)

mean_chestcir <- mean(body_data$`Chest circumference (cm)`)
median_chestcir <- median(body_data$`Chest circumference (cm)`)

mean_kneecir <- mean(body_data$`Knee circumference (cm)`)
median_kneecir <- median(body_data$`Knee circumference (cm)`)

mean_weight <- mean(body_data$`Weight (lbs)`)
median_weight <- median(body_data$`Weight (lbs)`)

  mean_body_data <- c(mean_bodyfat, mean_density, mean_age, mean_chestcir, mean_kneecir, mean_weight)
  mean_body_data
  
  median_body_data <- c(median_bodyfat, median_density, median_age, median_chestcir, median_kneecir, median_weight)
  median_body_data

# Calculate deviation
sd_bodyfat <- sd(body_data$`Body fat (%)`)
sd_density <- sd(body_data$`Density (g/cm³)`)
sd_age <- sd(body_data$`Age (years)`)
sd_chestcir <- sd(body_data$`Chest circumference (cm)`)
sd_kneecir <- sd(body_data$`Knee circumference (cm)`)
sd_weight <- sd(body_data$`Weight (lbs)`)

  sd_body_data <- c(sd_bodyfat, sd_density, sd_age, sd_chestcir, sd_kneecir, sd_weight)
  sd_body_data
  
    
# Calculate confidence intervals
ci_bodyfat <- t.test(body_data$`Body fat (%)`)$conf.int
ci_density <- t.test(body_data$`Density (g/cm³)`)$conf.int
ci_age <- t.test(body_data$`Age (years)`)$conf.int
ci_chestcir <- t.test(body_data$`Chest circumference (cm)`)$conf.int
ci_kneecir <- t.test(body_data$`Knee circumference (cm)`)$conf.int
ci_weight <- t.test(body_data$`Weight (lbs)`)$conf.int

  ci_body_data <- c( ci_bodyfat, ci_density, ci_age, ci_chestcir, ci_kneecir, ci_weight)
  ci_body_data
  

  
#Check linearity of data
windows(20,10)
pairs(body_data)

#Scatter plots to test for linearity
windows(20,10)
par(mfrow= c(3,2))

scatter.smooth(x = body_data$`Body fat (%)`,
               y = body_data$`Density (g/cm³)`,
               main = "Correlation of Body Fat % & Density (g/cm³)",
               xlab = "Body Fat %",
               ylab = "Density (g/cm³)")

scatter.smooth(x = body_data$`Body fat (%)`,
               y = body_data$`Age (years)`,
               main = "Correlation of Body Fat % & Age (years)",
               xlab = "Body Fat %",
               ylab = "Age (years)")

scatter.smooth(x = body_data$`Body fat (%)`,
               y = body_data$`Chest circumference (cm)`,
               main = "Correlation of Body Fat % & Chest circumference (cm)",
               xlab = "Body Fat %",
               ylab = "Chest circumference (cm)")

scatter.smooth(x = body_data$`Body fat (%)`,
               y = body_data$`Knee circumference (cm)`,
               main = "Correlation of Body Fat % & Knee circumference (cm)",
               xlab = "Body Fat %",
               ylab = "Knee circumference (cm)")

scatter.smooth(x = body_data$`Body fat (%)`,
               y = body_data$`Weight (lbs)`,
               main = "Correlation of Body Fat % & Weight (lbs)",
               xlab = "Body Fat %",
               ylab = "Weight (lbs)")

attach(body_data)
# Examining the correlation between Body Fat % and the other variables
paste("Correlation for Body Fat % and Density: ", round(cor(`Body fat (%)`, `Density (g/cm³)`),2))
paste("Correlation for Body Fat % and Age: ", round(cor(`Body fat (%)`, `Age (years)`),2))
paste("Correlation for Body Fat % and Chest Circumference: ", round(cor(`Body fat (%)`, `Chest circumference (cm)`),2))
paste("Correlation for Body Fat % and Knee Circumference: ", round(cor(`Body fat (%)`, `Knee circumference (cm)`),2))
paste("Correlation for Body Fat % and Weight: ", round(cor(`Body fat (%)`, `Weight (lbs)`),2))

#Body Fat % and Density have a very high correlation
#The next highest correlation is with chest circumference
#After that the next highest is weight
#Then knee circumference
#And the lowest is Age



#Loading the library for the boxplots
install.packages("ggplot2")
library(ggplot2)

#Creating the boxplots
windows(30,20)
par(mfrow= c(3,2))

boxplot(`Body fat (%)`,
        main = "Body Fat %")
boxplot(`Density (g/cm³)`,
        main = "Density")
boxplot(`Age (years)`,
        main = "Age")
boxplot(`Chest circumference (cm)`,
        main = "Chest Circumference")
boxplot(`Knee circumference (cm)`,
        main = "Knee Circumference")
boxplot(`Weight (lbs)`,
        main = "Weight")



#group#Body Fat and Density
ggplot(body_data, aes(x = `Body fat (%)`, y = `Density (g/cm³)`)) + 
  geom_boxplot(fill = "lightblue") + 
  labs(x = "Condition", y = "Scores", title = "Body Fat % & Density (g/cm³)")

#Body Fat and Age
ggplot(body_data, aes(x = `Body fat (%)`, y = `Age (years)`)) + 
  geom_boxplot(fill = "yellow") + 
  labs(x = "Condition", y = "Scores", title = "Body Fat % & Age (years)")

#Body Fat and Chest Circumference
ggplot(body_data, aes(x = `Body fat (%)`, y = `Chest circumference (cm)`)) + 
  geom_boxplot(fill = "orange") + 
  labs(x = "Condition", y = "Scores", title = "Body Fat % & Chest circumference (cm)")

#Body Fat and Knee Circumference
ggplot(body_data, aes(x = `Body fat (%)`, y = `Knee circumference (cm)`)) + 
  geom_boxplot(fill = "green") + 
  labs(x = "Condition", y = "Scores", title = "Body Fat % & Knee circumference (cm)")

#Body Fat and Weight
ggplot(body_data, aes(x = `Body fat (%)`, y = `Weight (lbs)`)) + 
  geom_boxplot(fill = "red") + 
  labs(x = "Condition", y = "Scores", title = "Body Fat % & Weight (lbs)")


    # Use boxplot.stats() function to generate relevant outliers for Body Fat %
    outlier_values <- boxplot.stats(body_data$`Body fat (%)`)$out # outlier values.
    paste("Area outliers: ", paste(outlier_values, sep =", "))
    
    # Remove weight outliers for Body Fat %
    body_data <- subset(body_data,
                        body_data$`Body fat (%)` != 47.5)
    
    
    # Use boxplot.stats() function to generate relevant outliers for Density
    outlier_values <- boxplot.stats(body_data$`Density (g/cm³)`)$out # outlier values.
    paste("Area outliers: ", paste(outlier_values, sep =", "))
    
    
    # Use boxplot.stats() function to generate relevant outliers for Age
    outlier_values <- boxplot.stats(body_data$`Age (years)`)$out # outlier values.
    paste("Area outliers: ", paste(outlier_values, sep =", "))
    
    
    # Use boxplot.stats() function to generate relevant outliers for Chest Circumference
    outlier_values <- boxplot.stats(body_data$`Chest circumference (cm)`)$out # outlier values.
    paste("Area outliers: ", paste(outlier_values, sep =", "))
    
    
    # Use boxplot.stats() function to generate relevant outliers for Knee Circumference
    outlier_values <- boxplot.stats(body_data$`Knee circumference (cm)`)$out # outlier values.
    paste("Area outliers: ", paste(outlier_values, sep =", "))
    
    # Remove weight outliers for Knee Circumference
    body_data <- subset(body_data,
                        body_data$`Knee circumference (cm)` != 45
                        & body_data$`Knee circumference (cm)` != 46)
    
    body_data <- subset(body_data,
                        body_data$`Knee circumference (cm)` != 44.2)
    
    
    # Use boxplot.stats() function to generate relevant outliers for weight
    outlier_values <- boxplot.stats(body_data$`Weight (lbs)`)$out # outlier values.
    paste("Area outliers: ", paste(outlier_values, sep =", "))
    
    # Remove weight outliers for weight
    body_data <- subset(body_data,
                        body_data$`Weight (lbs)` != 363.15
                        & body_data$`Weight (lbs)` != 262.75)



#Correlation Matrix to test for linearity
correlationmatrix <- cor(body_data)
round(correlationmatrix, 2)
windows(20,10)
pairs(correlationmatrix)

attach(body_data)
paste("Correlation for Body Fat % & Density: ",
      round(cor(`Body fat (%)`, `Density (g/cm³)`),2))
paste("Correlation for Body Fat % & Age: ",
      round(cor(`Body fat (%)`, `Age (years)`),2))
paste("Correlation for Body Fat % & Chest Circumference: ",
      round(cor(`Body fat (%)`, `Chest circumference (cm)`),2))
paste("Correlation for Body Fat % & Knee Circumference: ",
      round(cor(`Body fat (%)`, `Knee circumference (cm)`),2))
paste("Correlation for Body Fat % & Weight: ",
      round(cor(`Body fat (%)`, `Weight (lbs)`),2))


#Check normality
normality_test_bfat <- shapiro.test(body_data$`Body fat (%)`)
normality_test_bfat
normality_test_bfat$p.value

normality_test_density <- shapiro.test(body_data$`Density (g/cm³)`)
normality_test_density
normality_test_density$p.value

normality_test_age <- shapiro.test(body_data$`Age (years)`)
normality_test_age
normality_test_age$p.value

normality_test_chestcir <- shapiro.test(body_data$`Chest circumference (cm)`)
normality_test_chestcir
normality_test_chestcir$p.value

normality_test_kneecir <- shapiro.test(body_data$`Knee circumference (cm)`)
normality_test_kneecir
normality_test_kneecir$p.value

normality_test_weight <- shapiro.test(body_data$`Weight (lbs)`)
normality_test_weight
normality_test_weight$p.value

normality_p_values <- c(normality_test_bfat$p.value, normality_test_density$p.value, normality_test_age$p.value,
                        normality_test_chestcir$p.value, normality_test_kneecir$p.value, normality_test_weight$p.value)
normality_p_values
#Body Fat % is normally distributed (p value = 0.141018)
#Density is normally distributed (p value = 0.657075)
#Age is not normally distributed (p value = 0.001043)
#Chest Circumference is  not normally distributed (p value = 0.000118)
#Knee Circumference is not normally distributed (p value = 0.003304)
#Weight is not normally distributed (p value = 1.709e-08)

# Need to transform Age, Chest Circumference, Knee Circumference and Weight
#Age
attach(body_data)
library(MASS)
View(body_data)
box_cox_transform <- boxcox(`Body fat (%)`~`Age (years)`)
box_cox_transform
windows(20,10)
lamda <-box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_Age <-(`Body fat (%)`^lamda-1)/lamda
normalised_Age
hist(normalised_Age)
shapiro.test(normalised_Age)
  ##Modify the variable Age
  body_data$Age_new <- normalised_Age
  new_normality_test_age <- shapiro.test(body_data$Age_new)

#Chest Circumference
attach(body_data)
library(MASS)
View(body_data)
box_cox_transform <- boxcox(`Body fat (%)`~`Chest circumference (cm)`)
box_cox_transform
windows(20,10)
lamda <-box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_ChestCircumference <-(`Body fat (%)`^lamda-1)/lamda
normalised_ChestCircumference
hist(normalised_ChestCircumference)
shapiro.test(normalised_ChestCircumference)
  ##Modify the variable Chest Circumference
  body_data$ChestCircumference_new <- normalised_ChestCircumference
  new_normality_test_chestcir <- shapiro.test(body_data$ChestCircumference_new)

#Knee Circumference
attach(body_data)
library(MASS)
View(body_data)
box_cox_transform <- boxcox(`Body fat (%)`~`Knee circumference (cm)`)
box_cox_transform
windows(20,10)
lamda <-box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_KneeCircumference <-(`Body fat (%)`^lamda-1)/lamda
normalised_KneeCircumference
hist(normalised_KneeCircumference)
shapiro.test(normalised_KneeCircumference)
  ##Modify the variable Chest Circumference
  body_data$KneeCircumference_new <- normalised_KneeCircumference
  new_normality_test_kneecir <- shapiro.test(body_data$KneeCircumference_new)

#Weight
attach(body_data)
library(MASS)
View(body_data)
box_cox_transform <- boxcox(`Body fat (%)`~`Weight (lbs)`)
box_cox_transform
windows(20,10)
lamda <-box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_weight <-(`Body fat (%)`^lamda-1)/lamda
normalised_weight
hist(normalised_weight)
shapiro.test(normalised_weight)
  ##Modify the variable Chest Weight
  body_data$Weight_new <- normalised_weight
  new_normality_test_weight <- shapiro.test(body_data$Weight_new)

new_normality_p_values <- c(normality_test_bfat$p.value, normality_test_density$p.value, new_normality_test_age$p.value,
                          new_normality_test_chestcir$p.value, new_normality_test_kneecir$p.value, new_normality_test_weight$p.value)  
new_normality_p_values  
  
  
#Check Skewness
install.packages("e1071")
library(e1071)
windows(30,20)
par(mfrow = c(3,2)) # divide graph area into 1 row x 2 cols

attach(body_data)
plot(density(body_data$`Body fat (%)`),
     main = "Density plot : Body Fat %",
     ylab = "Frequency", xlab = "BodyFat%",
     sub = paste("Skewness : ", round(e1071::skewness(body_data$`Body fat (%)`), 2)))
polygon(density(body_data$`Body fat (%)`), col = "purple")

plot(density(body_data$`Density (g/cm³)`),
     main = "Density plot : Density",
     ylab = "Frequency", xlab = "Density",
     sub = paste("Skewness : ", round(e1071::skewness(body_data$`Density (g/cm³)`), 2)))
polygon(density(body_data$`Density (g/cm³)`), col = "lightblue")

plot(density(body_data$`Age (years)`),
     main = "Density plot : Age",
     ylab = "Frequency", xlab = "Age",
     sub = paste("Skewness : ", round(e1071::skewness(body_data$`Age (years)`), 2)))
polygon(density(body_data$`Age (years)`), col = "yellow")

plot(density(body_data$`Chest circumference (cm)`),
     main = "Density plot : Chest Circumference",
     ylab = "Frequency", xlab = "Chest Circumference",
     sub = paste("Skewness : ", round(e1071::skewness(body_data$`Chest circumference (cm)`), 2)))
polygon(density(body_data$`Chest circumference (cm)`), col = "orange")

plot(density(body_data$`Knee circumference (cm)`),
     main = "Density plot : Knee Circumference",
     ylab = "Frequency", xlab = "Knee Circumference",
     sub = paste("Skewness : ", round(e1071::skewness(body_data$`Knee circumference (cm)`), 2)))
polygon(density(body_data$`Knee circumference (cm)`), col = "green")

plot(density(body_data$`Weight (lbs)`),
     main = "Density plot : Weight",
     ylab = "Frequency", xlab = "Weight",
     sub = paste("Skewness : ", round(e1071::skewness(body_data$`Weight (lbs)`), 2)))
polygon(density(body_data$`Weight (lbs)`), col = "red")

#Weight is highly skewed with a value of 1.19
#Chest Circumference and Knee Circumference are both moderately skewed with values of 0.67 & 0.51
#Body Fat %, Age and Density are approx symetrical with values of 0.15, 0.28 & -0.02


# check multicollinearity
cor(body_data)

##Regression model using transformed variables
str(body_data)
attach(body_data)
model_1 <- lm( `Body fat (%)` ~ 
                 `Density (g/cm³)` + 
                 Age_new +
                 ChestCircumference_new + 
                 KneeCircumference_new + 
                 Weight_new)

model_1
summary(model_1)

##Regression model using original variables
str(body_data)
attach(body_data)
model_2 <- lm( `Body fat (%)` ~ 
                 `Density (g/cm³)` + 
                 `Age (years)` +
                 `Chest circumference (cm)` + 
                 `Knee circumference (cm)` + 
                 `Weight (lbs)`)

model_2
summary(model_2)

AIC(model_1)
AIC(model_2)
BIC(model_1)
BIC(model_2)

#Residuals normally distributed
shapiro.test(residuals(model_1))
shapiro.test(residuals(model_2))

#Residuals diffrent from zero
t.test(residuals(model_1),mu=0) 
t.test(residuals(model_2),mu=0) 

#Randomness of residuals (autocorrelation) -- Durbin-Watson test
install.packages("lmtest")
library(lmtest)
dwtest(model_1)
dwtest(model_2)

#Check for multi_collinearity
install.packages("faraway")
library(faraway)
v1 <-vif(model_1)
v1
v2 <-vif(model_2)
v2


#Body Fat % and Density have a very high correlation
#Chest circumference has a good correlation with Body Fat % but is moderately skewed
#Weight has a good correlation with Body Fat %, its p-value is > 0.05 but it is highly skewed
str(body_data)
attach(body_data)
model_3 <- lm( `Body fat (%)` ~ 
                 `Density (g/cm³)`+
                 `Chest circumference (cm)`+
                 `Weight (lbs)`)

model_3
summary(model_3)
