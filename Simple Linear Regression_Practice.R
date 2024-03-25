# simple and multi variate linear regression exercise 
# @KamZardouzian 2024

# Intall packages

install.packages("ggplot2", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("broom", dependencies = TRUE)
install.packages("ggsci", dependencies = TRUE)
install.packages("tidyr", dependencies = TRUE)
install.packages("cowplot", dependencies = TRUE)
install.packages("ggpubr", dependencies = TRUE)

# load packages into library
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(readr)

# Read Zip data
heart_data_ <- read_csv("C:/Users/Kam/Desktop/Learning/R/Data/heart.data_.zip")
View(heart_data_)
heart <- heart_data_

summary(heart)

# test the relationship between your independent variables and make sure they aren’t too highly correlated. 0.015 is 1.5% correlation between
#  biking and smoking, hence both variables can be included.
cor(heart$biking, heart$smoking)

# test whether your dependent variable follows a normal distribution. The distribution of observations is roughly bell-shaped, so we can proceed with the linear regression.
hist(heart$heart.disease)

# Check for linearity using two scatter plots: one for biking vs heart disease and another for smoking vs heart disease
plot(heart.disease ~ biking, data=heart)

plot(heart.disease ~ smoking, data = heart)
# biking & heart disease are more linear than smoking & heart disease, but the later has a pattern so we proceed with linear regression.

# The above steps are necessary to qualify our assumption that heart disease is correlated with exercise and smoking.
# Is there an actual correlation between biking, smoking and heart disease. We saw from the summary code that biking ranges between 1-75, smoking between 0.5 - 30, and heart disease 0.5 - 20.5%
# Fit a linear model to test this hypothesis: heart disease as the dependent variable and biking and smoking as the independent variables
heart.disease.lm <- lm(heart.disease ~ biking + smoking, data = heart)
summary(heart.disease.lm)
# Notice Estimate values for biking & smoking: for every 1% increase in biking to work, there is a correlated 0.2% decrease in the incidence of heart disease. Meanwhile, for every 1% increase in smoking, there is a 0.178% increase in the rate of heart disease.The standard errors for these regression coefficients are very small, and the t statistics are very large (-147 and 50.4, respectively). The p values reflect these small errors and large t statistics. For both parameters, there is almost zero probability that this effect is due to chance.
# 
# Check for homoscedasticity: hat our model is actually a good fit for the data, and that we don’t have large variation in the model error
par(mfrow=c(2,2))
plot(heart.disease.lm)
par(mfrow=c(1,1))
# residuals show no bias, so we can say our model fits the assumption of homoscedasticity

# Create visualizations for reporting: plotting the relationship between biking and heart disease at different levels of smoking. In this example, smoking will be treated as a factor with three levels, just for the purposes of displaying the relationships in our data. Following these 7 steps:
# 1. Create a new dataframe with the information needed to plot the model
# 2. Predict the values of heart disease based on your linear model
# 3. Round the smoking numbers to two decimals
# 4. Change the ‘smoking’ variable into a factor
# 5. Plot the original data
# 6. Add the regression lines
# 7. Make the graph ready for publication
# 
# 1. Use the function expand.grid() to create a dataframe with the parameters you supply. Within this function we will:
# Create a sequence from the lowest to the highest value of your observed biking data;
# Choose the minimum, mean, and maximum values of smoking, in order to make 3 levels of smoking over which to predict rates of heart disease.
# 
plotting.data<-expand.grid(
     biking = seq(min(heart$biking), max(heart$biking), length.out=30),
     smoking=c(min(heart$smoking), mean(heart$smoking), max(heart$smoking)))

# 2. Predict the values of heart disease based on your linear model. Next we will save our ‘predicted y’ values as a new column in the dataset we just created.
plotting.data$predicted.y <- predict.lm(heart.disease.lm, newdata=plotting.data)

# 3. Round the smoking numbers to two decimals. This will make the legend easier to read later on.
plotting.data$smoking <- round(plotting.data$smoking, digits = 2)

# 4. Change the ‘smoking’ variable into a factor. This allows us to plot the interaction between biking and heart disease at each of the three levels of smoking we chose.
plotting.data$smoking <- as.factor(plotting.data$smoking)

# 5. Plot the original data
heart.plot <- ggplot(heart, aes(x=biking, y=heart.disease)) + geom_point()
heart.plot

# 6. Add the regression lines
heart.plot <- heart.plot +
     geom_line(data=plotting.data, aes(x=biking, y=predicted.y, color=smoking), size=1.25)

heart.plot

# 7. Make the graph ready for publication
heart.plot <-
     heart.plot +
     theme_bw() +
     labs(title = "Rates of heart disease (% of population) \n as a function of biking to work and smoking",
          x = "Biking to work (% of population)",
          y = "Heart disease (% of population)",
          color = "Smoking \n (% of population)")

heart.plot

# Note: Because this graph has two regression coefficients, the stat_regline_equation() function won’t work here. But if we want to add our regression model to the graph, we can do so like this:
heart.plot + annotate(geom="text", x=30, y=1.75, label=" = 15 + (-0.2*biking) + (0.178*smoking)")

# Report results with brief explanation: Reporting the results of multiple linear regression
# In our survey of 500 towns, we found significant relationships between the frequency of biking to work and the frequency of heart disease and the frequency of smoking and frequency of heart disease (p < 0 and p < 0.001, respectively).
# Specifically we found a 0.2% decrease (± 0.0014) in the frequency of heart disease for every 1% increase in biking, and a 0.178% increase (± 0.0035) in the frequency of heart disease for every 1% increase in smoking.
