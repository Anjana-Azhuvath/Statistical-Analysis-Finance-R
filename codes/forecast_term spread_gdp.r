# ECMT 674: Economic Forecasting, Assignment 7 Solution Code
# This answer key is only a suggested solution, there are many ways to write the code

# Clear the Workspace
rm(list=ls())
Sys.setenv("LANGUAGE"="En")

# Install the packages
Packages <- c("fredr","ggplot2","gridExtra","xts")
#install.packages(Packages, dependencies = TRUE)
invisible(update.packages(Packages, dependencies = TRUE))
invisible(lapply(Packages, library, character.only = TRUE))

#========================================================================================================================
# Q1. Read in real GDP growth for the US. Choose a measure of spread. Take the longest possible sample and plot the data.
#========================================================================================================================

# Load data from FRED
rgdp <- fredr_series_observations(series_id = "GDPC1",  frequency = "q", observation_end = as.Date("2022-10-01"))
sprd <- fredr_series_observations(series_id = "T10Y2Y", frequency = "q", observation_end = as.Date("2022-10-01"))

# Convert data to xts format
rgdp <- xts(rgdp$value, order.by = as.Date(rgdp$date))
sprd <- xts(sprd$value, order.by = as.Date(sprd$date))

# Combine the two data sets into one xts object
hw7 <- na.omit(merge(rgdp, sprd, fill = na.locf))

# Plot the data 
dev.new(noRStudioGD = TRUE)
plot1 <- ggplot(hw7, aes(x = Index)) +
  geom_line(aes(y = rgdp)) +
  labs(x = "Date", y = "Real GDP, Billions of Chained 2012 Dollars") +
  theme_bw()
plot2 <- ggplot(hw7, aes(x = Index)) +
  geom_line(aes(y = sprd)) +
  labs(x = "Date", y = "Spread, Percent") +
  theme_bw()
# Combine the two plots into a single window
grid.arrange(plot1, plot2, ncol = 2)


#========================================================================================================================
# Q2. Estimate a simple linear regression model of quarterly annualized real GDP growth (in percent) and term spread. 
# Use various lags of term spread (consider up to 8 lags) to come up with the best specification, 
# Which lag of the spread has the “most” predictive content? Comment on how you select the “best” specification.
# Q3. Summarize the output of the best specification in statistical and economic terms. Be careful about the units of measure.
#========================================================================================================================

# Compute the quarterly annualized growth rate of real GDP
y <- diff(log(hw7[, 1]), lag = 1) * 400

# Create a matrix of lagged term spreads
X <- lag(hw7[, 2], 0:8)

# Keep only the rows that have complete data
# Note here that by binding them all together, we are loosing 8 observations. If I bind them separately, the effective
# sample size will be larger
data_mat <- na.omit(cbind(y, X))

# Fit linear regression models using each individual lag of the term spread
# This is intended to keep the model size the same and comparable
fits <- list()
rsq <- c()

for (i in 2:dim(data_mat)[2]) {
  fits[[i-1]] <- lm(data_mat[,1] ~ data_mat[,i])
  cat("Lag:", i-2, "\n")
  print(summary(fits[[i-1]])$coefficients[, c("Estimate", "Std. Error", "t value")])
  rsq[i-1] = summary(fits[[i-1]])$r.squared
}


# Find the lag with the highest adjusted R-squared value
best_lag <- which.max(rsq)

# Print the results, here, I would choose the lag of order 5 as the best specification.
cat("Best specification:", paste0("X[t-", best_lag-1, "]"), "\n")
cat("Adjusted R-squared:", rsq[best_lag], "\n")
summary(fits[[best_lag]])

# 1 pp point increase in the term spread will increase the quarterly annualized GDP growth by 0.6 pp. In fact, the
# the coefficient is statistically insignificant at all traditional significance levels. The coefficient can be considered 
# economically significant. The average growth rate is roughly 2.5 percent. This coefficient is roughtly
# the size 1/4th in size. However, this variable explains only 1.4 % of the total variation 
# in the GDP growth.

#========================================================================================================================
# Q4. Comment on the in-sample fit of the best specification. 
# Plot the fitted values against the actual realizations of the real GDP growth.
#========================================================================================================================
# Fit the best specification
best_fit  <- fits[[best_lag]]
yhat <- xts(best_fit$fitted.values, order.by = index(data_mat))

# Create a data frame with the actual and fitted values
Q4 <- merge(data_mat[,1], yhat)

dev.new(noRStudioGD = TRUE)
# Create a scatterplot of the actual vs. fitted values
ggplot(Q4, aes(x = Index)) +
  geom_line(aes(y = rgdp, color = "Actual")) +
  geom_line(aes(y = yhat, color = "Fitted"), linewidth = 1.2) +
  labs(title = "Fitted vs. Actual Real GDP Growth", x = "Date", y = "Percent", color = "") +
  scale_color_manual(values = c("Actual" = "red", "Fitted" = "blue")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

# The fitted line is a lot smoother and flatter. It does not pick up most of the ups and downs of the GDP growth, which
# is what the R-square captures.

#========================================================================================================================
# Q5. If you were to use the “best” model estimated roughly ten years ago, how well would it do in forecasting?
# How well would a sample mean forecast real GDP growth over the past ten years? Which forecast
# would be better on average? Is the difference between the predictive ability of the two predictions
# statistically significant?
#========================================================================================================================
# 
# Run the regression for a sub-sample also run the regression to get the mean. This is not the only way to get the 
# mean, but it is symmetric in this case.

sub_sample_reg <- lm(window(data_mat[,1], end = "2011-10-01") ~ window(data_mat[,best_lag], end = "2011-10-01"))
sub_sample_mean <- lm(window(data_mat[,1], end = "2011-10-01") ~ 1)

# The sub-sample results show that the fit is not driven by COVID
oos_predict_1 <- sub_sample_reg$coef[1] + sub_sample_reg$coef[2]*window(data_mat[,best_lag], start = "2012-01-01")
oos_predict_2 <- sub_sample_mean$coef[1] + 0*window(data_mat[,i], start = "2012-01-01")

realization = window(data_mat[,1], start = "2012-01-01")


# Create a data frame with the actual and predicted values
Q5 <- merge(realization,oos_predict_1,oos_predict_2)

# Create a scatterplot of the actual vs. predicted values
dev.new(noRStudioGD = TRUE)
ggplot(Q5, aes(x = Index)) +
  geom_line(aes(y = realization, color = "Actual")) +
  geom_line(aes(y = oos_predict_1, color = "Pred. with Spread"), linewidth = 1.2) +
  geom_line(aes(y = oos_predict_2, color = "Pred. with Avg"), linewidth = 1.2) +
  scale_y_continuous(limits = c(-0.4, 0.4))+
  labs(title = "Actual vs. Predicted Real GDP Growth", x = "Date", y = "Percent", color = "") +
  scale_color_manual(values = c("Actual" = "red", "Pred. with Spread" = "blue", "Pred. with Avg" = "green")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

# Forecast error
fe1 = realization-oos_predict_1
fe2 = realization-oos_predict_2

MSFE = c(mean(fe1^2), mean(fe2^2))

# The model with the term spread is slightly better, but is it significant?
reg_eval1 = lm(fe1^2-fe2^2~1)
summary(reg_eval1)

# There is no statistial evidence that models are different in their predictive performance at any 
# conventional significance level.

#========================================================================================================================
# Q6. Given the setup in the previous point, if you have to evaluate the models until the end of 2019, what
# would you say? Which forecast would be better, the one coming from the model with the term spread
# or the forecast based on the sample mean? Explain.
#========================================================================================================================
# 

fe1_preCOVID = window(fe1, end = "2019-10-01")
fe2_preCOVID = window(fe2, end = "2019-10-01")

MSFE_preCOVID = c(mean(fe1_preCOVID^2), mean(fe2_preCOVID^2))

# The model with the term spread is slightly better, but is it significant?
reg_eval2 = lm(fe1_preCOVID^2-fe2_preCOVID^2~1)
summary(reg_eval2)

# There is no statistial evidence that models are different in their predictive performance at any 
# conventional significance level.
