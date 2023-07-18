library(dplyr)
library(psych)
library(outliers)
library(ggplot2)



df_women <- read.csv("C:/Users/dsuifh/OneDrive/Desktop/Uni R project/women-stem.csv")
grads_data <- read.csv("C:/Users/omgit/OneDrive/Documents/r studio stuff/recent-grads.csv")
grads_data <- read.csv("C:/Users/dsuifh/OneDrive/Desktop/Uni R project/recent-grads.csv")


options(scipen=999)


summary(grads_data)

#checking the dataset for missing values
is.na(grads_data)
colSums(is.na(grads_data))

# we find one in Unemployment_rate so we remove it

clean = na.omit(grads_data)
summary(clean)
clean
# we check again and our dataset is ready
colSums(is.na(clean))
summary(clean)
#employed distrubution,Employment full time year distribution, Unemployment distribution, Unemployment_rate distribution, Median distributing
columns <- c("Employed", "Employed_full_time_year_round","Unemployed","Unemployment_rate", "Median", "P25th", "P75th")

columns_list <- c("Employed", "Full_time_year_round","Unemployed","Unemployment_rate", "Median","Men","Women")

colnames(clean)

for (column in columns_list) {
  print(ggplot(clean, aes_string(x = column, y = "Major_category")) +
          geom_boxplot() +
          ggtitle(paste0(column, " distribution")) +
          theme(plot.title = element_text(size = 24)+
                  scale_y_continuous(labels = scales::comma)))
}

library(car)

for (column in c("Employed", "Full_time_year_round","Unemployed","Unemployment_rate", "Median","Men","Women")) {
  print(paste("ANOVA results for column:", column))
  model <- aov(clean[,column] ~ clean$Major_category)
  print(summary(model))
  print("\n")
}

#ANOVA comparing median incomes of men and women
aov_result <- aov(Median ~ Men + Women, data = clean)
summary(aov_result)

#Linear regression evaluating the effect of Major_category on Median salaries
model_lm <- lm(Median ~  Major_category , data = clean)
summary(model_lm)

plot(model_lm, 4)

#introduction of new variable employment stability
clean$Employment_stability <- (clean$Employed / (clean$Employed + clean$Unemployed)) * 100

model_lm_stability <- lm(Employment_stability ~ Major_category + Women + Men, data = clean)
summary(model_lm_stability)
plot(model_lm_stability)

#plotting the major categories with the most median income

# Sort the data by median in descending order
sorted_data <- clean[order(-clean$Median),]

# Select the top 20 major categories by median income
top_n <- sorted_data[1:20, ]

# Plot the data using ggplot
ggplot(sorted_data, aes(x = reorder(Major_category, Median), y = Median)) +
  geom_bar(stat = "identity") +
  xlab("Major Category") +
  ylab("Median Income") +
  ggtitle("Top 20 Major Categories by Median Income") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))

#plotting the wage difference between genders

clean$income_difference <- clean$Men - clean$Women

# plot the difference in median income
ggplot(clean, aes(x = Major_category, y = income_difference)) +
  geom_bar(stat = "identity") +
  ggtitle("Difference in median income between men and women by Major category ( calculated by subtracting median income of men by median income of women") +
  xlab("Major category") +
  ylab("Difference in median income") +
  theme(plot.title = element_text(size = 6, face = "bold")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#GLM Model predicting median income via major_category

model <- glm(Median ~ Major_category, data = clean, family = poisson(link = "log"))
summary(model)

#results seem weird so we check if median and variance of Median is equal or not

# Calculate the mean and variance of the variable
mean_median <- mean(clean$Median)
var_median <- var(clean$Median)

mean_median
var_median

# Print the mean and variance of the variable
cat("Mean of Median:", mean_median, "\n")
cat("Variance of Median:", var_median, "\n")

# Plot the mean and variance of the variable
hist(clean$Median, main = "Histogram of Median", xlab = "Median")
lines(density(clean$Median), col = "red")

#our mean and variance are not the same so we cannot use poisson
#we use Negative Binomial regression

# Load the required library
library(MASS)

# Fit the Negative Binomial regression model
model_nb <- glm.nb(Median ~ Major_category + Men + Women, data = clean)

# Summarize the model
summary(model_nb)

#this model seems to work better to support our conclusions

# we will now use multiple regression analysis to see if we missed out anything important out of our dataset which might affect our median income
model_lm <- lm(Median ~ Major_category + Total + Men + Women + College_jobs + Non_college_jobs + Low_wage_jobs , data = clean)

# Summarize the model
summary(model_lm)

# Predict the response variable using the model
predictions <- predict(model_lm)

# Create a data frame to store the predicted values and the actual values
data_frame <- data.frame(Predicted = predictions, Actual = clean$Median)

# Plot the scatter plot
ggplot(data_frame, aes(x = Actual, y = Predicted)) + 
  geom_point(color = "blue") + 
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  xlab("Actual Values") + 
  ylab("Predicted Values") + 
  ggtitle("Scatter Plot of Predicted vs Actual Values")

# it seems regardless of what ever variables we put in the picking a major in Engineering and Communications & Journalism will significantly affect  the median income

model_lm <- lm(Median ~ College_jobs + Non_college_jobs + Unemployment_rate , data = clean)
summary(model_lm)
library(ggplot2)
ggplot(clean, aes(x = College_jobs + Non_college_jobs + Unemployment_rate, y = Median)) +
  geom_point() +
  geom_smooth(method = "lm")
# we find that non college jobs have a negative effect on median income - as in more non-college jobs represent a drop in the median level of that particular degree
#but our model is not very accurate at the moment

# Fit initial linear regression model
model_lm <- lm(Median ~ College_jobs + Non_college_jobs + Unemployment_rate , data = clean)

# Perform stepwise regression
model_step <- step(model_lm, direction="both", trace=0)
summary(model_step)

#next we attempt to model the relationship between employment stability and major category , type of job and gender
model_lm <- lm(Employment_stability ~ Major_category +College_jobs + Non_college_jobs + Men + Women , data = clean)
summary(model_lm)

#we find that when compared alongside major category , gender doesn't play that effective of a role in employment stability

#lets narrow it down and see if it makes a difference
model_lm <- lm(Employment_stability ~ Men + Women , data = clean)
summary(model_lm)

#we find that again employment stability is not affected by gender as much

#lets see if median income is affected by gender

model_lm <- lm(Median ~ Men + Women , data = clean)
summary(model_lm)

# we see that genders play a significant role when isolated
#does this give us enough proof of a wage gap between genders ?

model_lm <- lm(Median ~ Major_category +Men + Women , data = clean)
summary(model_lm)

# we test this hypothesis and we find that in large scale of things a person's major_category has a bigger influence as compared to their gender
#but that is not to say there inst a difference. when we look at the coefficients we observe that men have a positive coefficient while women have a negative one
#this indicates to us that there may be slight cases where women may be at a disadvantage

#we now try all of this with negative binomial regression model

model_nb <- glm.nb(Median ~ Major_category , data = clean)
summary(model_nb)

#gender
model_nb <- glm.nb(Median ~ Men + Women , data = clean)
summary(model_nb)

#shows that our data is Normally Distrubuted
shapiro.test(clean$Employment_stability)

#plot Major Category Count
color_map <- rep("#cfd4d1", nrow(df_ages))
color_map[1] <- "#f73152"

ggplot(df_ages, aes(x = Major_category)) +
  geom_bar(aes(fill = color_map), alpha = 0.7, color = "black", stat = "count", width = 0.6) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.line.y = element_line(color = "#4d4d4d", size = 0.4)) +
  ggtitle("Major category count") +
  theme(plot.title = element_text(size = 24, face = "bold", family = "Serif")) +
  scale_fill_identity()

  
#some more graphs for Women and Men distribution

ggplot(df_women, aes(x = Major_category, y = Women)) +
  geom_boxplot() +
  ggtitle("Women distribution") +
  xlab("Major category") +
  ylab("Women")

ggplot(df_women, aes(x = Major_category, y = Men)) +
  geom_boxplot() +
  ggtitle("Men distribution") +
  xlab("Major category") +
  ylab("Men")

#merging men and women to seperate data
df_men_women <- clean %>% 
  group_by(Major_category) %>% 
  summarize(Men = sum(Men), Women = sum(Women)) %>% 
  mutate(Men_perc = round(Men / (Men + Women) * 100, 2),
         Women_perc = round(Women / (Men + Women) * 100, 2))
df_men_women

for (column in colnames(clean[,c('Men','Women')])) {
  print(paste0("For ",column," column anova results are: "))
  print(aov(get(column) ~ Major_category, data = clean))
}

ggplot(clean, aes(x = Unemployment_rate, y = Median, color = Major_category)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Median earnings vs. Unemployment rate by Major category") +
  xlab("Unemployment rate") +
  ylab("Median earnings")