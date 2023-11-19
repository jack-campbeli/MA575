# Friday, September 29, 2023
# BS755 / MA575: Linear Models
# Lab 4: Ordinary Least Squares
# Air quality data example

# Set the working directory (e.g., change to the directory where ...
# the data is stored on your computer)
# setwd("your/directory/path")
setwd('~/OneDrive - Boston University/Fall 2023/Teaching/Shariq BS755 MA575/Labs/Lab 04/')

# Install and load required packages if not already installed
# install.packages("ggplot2")
library(ggplot2)

# Read data from a CSV file using semicolon as delimiter
# Use '..' to move up one directory level to access the 'Lab 03' directory ...
# ... and read the 'AirQualityData.csv' file.
aq_data <- read.csv("../Lab 03/AirQualityData.csv", header=TRUE, as.is=TRUE, sep=';')

# In this data set missing values are stored as -200
# Replace missing data (-200) with NA
aq_data[aq_data == -200] <- NA
# Note that this replaces -200 to NA across all columns in aq_data

# Create a new data frame 'tempdataset' by selecting specific columns from 'aq_data'
# These columns are named in 'tempdataset' as ...
# ... 'Temperature', 'RelativeHumidity', 'SensorCO', and 'GroundCO'
tempdataset <- data.frame(Temperature = aq_data$Temperature,
                          RelativeHumidity = aq_data$RelativeHumidity,
                          SensorCO = aq_data$PT08.S1.CO.,
                          GroundCO = aq_data$CO.GT.)

# Remove rows with missing values (NA)
# Use 'complete.cases' to exclude those rows in 'tempdataset' where ...
# ... there are missing values in atleast one of the four columns, ...
# resulting in a complete dataset without NA values.
Dataset <- tempdataset[complete.cases(tempdataset), ]

# Remove the 'tempdataset' data frame from the R environment
rm(tempdataset)

# Create a scatterplot to visualize the relationship between ...
# ... 'SensorCO' and 'GroundCO' variables. 'SensorCO' is plotted ...
# ... on the x-axis, and 'GroundCO' is plotted on the y-axis.
# - 'ylab' sets the label for the y-axis to "Ground Truth (CO)"
# - 'xlab' sets the label for the x-axis to "Sensor CO"
plot(x = Dataset$SensorCO, y = Dataset$GroundCO,
     ylab="Ground Truth (CO)", xlab="Sensor CO")

# - 'pch' specifies the marker type for points (19 represents filled circles)
# - 'cex' controls the size of the marker (0.2 reduces the size)
# Try different values for these input arguments to see how the plot changes
plot(x = Dataset$SensorCO, y = Dataset$GroundCO,
     ylab="Ground Truth (CO)", xlab="Sensor CO",
     pch=19, cex=0.2)

# Create a scatterplot using the ggplot2 package
# - 'Dataset' is the data frame containing the data to be plotted.
# - aes() specifies the aesthetics of the plot, including the x and y variables.
#   - 'x=SensorCO' sets the x-axis to the 'SensorCO' variable.
#   - 'y=GroundCO' sets the y-axis to the 'GroundCO' variable.
# - geom_point() adds points to the plot to represent the data points.
#   - 'size=0.15' sets the size of the points to 0.15 (adjust as needed).
#   - 'col='blue'' sets the color of the points to blue.
# - theme_bw() applies a black-and-white theme to the plot background.
ggplot(Dataset, aes(x=SensorCO, y=GroundCO)) + 
  geom_point(size=0.15, col='blue') +
  theme_bw()

# Perform Ordinary Least Squares (OLS) linear regression.
# Model GroundCO as a function of SensorCO using the lm() function.
# Store the regression model in the variable 'm.ols'.
# The 'data' argument specifies the data frame 'Dataset' where ...
# ... the variables are located.
m.ols <- lm(GroundCO ~ SensorCO, data = Dataset)

# Generate a summary of the Ordinary Least Squares (OLS) model
summary(m.ols)
# Summary Output Information:
# - Call: Exact model formula used, including response and predictors.
# - Residuals: Statistics on model residuals (differences between ...
#   ... observed and predicted values).
# - Coefficients: Estimated values of coefficients, standard errors, ...
#   ... t-values, p-values, and the significance codes.
# - Residual Standard Error: Quantifies the average prediction error.
# - R-squared: Measures the proportion of response variable variance ...
#   ... explained by the model. Includes adjusted R-squared.
# - F-statistic: F-statistics, degrees of freedom p-value.

# Calculates 95% confidence intervals for the model coefficients.
confint(m.ols, level=0.95)
# Provides lower and upper bounds at the 95% confidence level.
# Width of the interval indicates the level of uncertainty in ...
# ... the coefficient's estimate. That is, narrow intervals indicate ...
# ... more precise coefficient estimates, while wider intervals ...
# ... imply greater uncertainty.

# Round numerical values to a specified number of decimal digits.
round(confint(m.ols, level=0.95), digits=4)

# Variance-covariance matrix for the coefficient estimates in the model.
vcov(m.ols)
# Diagonal elements of the variance-covariance matrix represent the ...
# ... variance of each coefficient estimate. Off-diagonal elements represent ... 
# ... covariances between coefficient pairs. This matrix is crucial for ... 
# ... assessing the uncertainty and relationships among the coefficients, ...
# ... which is vital in hypothesis testing and making inferences about the model.
# Note: For example, the 'e-09' part indicates that the number should be ...
# ... multiplied by 10 to the power of -9.

# Scatterplot of SensorCO vs. GroundCO with the OLS regression line.
# - 'abline()' is used to add a line to the current plot.
# - 'lsfit()' performs a least-squares regression fit line for the specified variables.
plot(Dataset$SensorCO, Dataset$GroundCO,
     ylab="Ground Truth (CO)", xlab="Sensor CO",
     pch=19, cex=0.2)
abline(lsfit(x=Dataset$SensorCO, y=Dataset$GroundCO), col="blue")
# - A straight line (OLS fit) is overlaid on the scatterplot. This line ...
# ... visually illustrates the relationship between the two variables.

# Generate sequences of numbers 'from' a starting value 'to' an ...
# ... ending value. 'length.out' specifies the length of the sequence.
# Here length.out is set to the length of the 'SensorCO' column ...
# ... in the 'Dataset' data frame.
SensorCONew <- seq(from=600, to=2200, length.out=length(Dataset$SensorCO))

# Generate predictions/fitted-values from the model.
# - 'newdata' specifies the data frame containing the predictor ...
#   ... variable 'SensorCO' for which we want to make predictions.
predLinear <- predict(m.ols, newdata=data.frame(SensorCO=SensorCONew))
# 'predLinear' contains the predicted values of the response variable ...
# ... (in this case, 'GroundCO') based on the provided 'SensorCO' values ...
# ... and the estimated coefficients of the OLS model.

# Create the scatterplot with OLS regression line using ggplot2
# - 'geom_line()' adds a line plot layer using 'SensorCONew' and ...
#   ... 'predLinear' for x and y, respectively.
# - 'scale_color_discrete(name="Prediction", labels=c("Linear"))' ...
# ... sets the color scale for the plot, naming it "Prediction" ... 
# ... with a single label "Linear."
ggplot(Dataset, aes(x=SensorCO, y=GroundCO)) + 
  geom_point(size=0.15, col='blue') + 
  geom_line(aes(x=SensorCONew, y=predLinear, color="blue")) +
  scale_color_discrete(name="Prediction", labels=c("Linear")) +
  theme_bw()

# Perform model diagnostics
# 'plot' function returns a set of diagnostic plots to assess the assumptions ...
# ... and goodness of fit of a linear model fit using 'lm' function. 
plot(m.ols)
# Residuals vs. Fitted Values Plot: Shows relationship between the ...
# ... observed residuals (differences between the actual and ...
# ... fitted values) and the fitted values from the linear model. It ...
# ... helps to check for heteroscedasticity (whether the spread of ...
# ... residuals changes with fitted values) and to identify outliers.
# Normal Q-Q Plot: Quantile-quantile (Q-Q) plot that assesses whether ...
# ... the residuals follow a normal distribution. Points along a ...
# ... straight line suggests that the residuals are approximately ...
# ... normally distributed.
# Scale-Location (Spread-Location) Plot: To check for homoscedasticity ...
# ... (constant variance of residuals), linearity and outliers similar to ...
# ... residuals vs fitted values plot.
# Residuals vs. Leverage Plot: To detect potential outliers/influential observations.

# Set the layout of the plotting device to a 2x2 grid
# 'mfrow' parameter specifies the number of rows and columns ...
# ... in which to arrange multiple plots
par(mfrow = c(2, 2))
# Create a plot of the object 'm.ols' (presumably the result of a linear regression)
plot(m.ols)
# Reset the layout of the plotting device to the default (1x1)
par(mfrow = c(1, 1))
# Are the model assumptions satisfied? Is this a good model?

# Perform Quadratic Least Squares (Quad LS) regression
# - 'GroundCO ~ SensorCO + I(SensorCO^2)' indicates that we are modeling ...
#   ... 'GroundCO' as a function of 'SensorCO' and 'SensorCO^2'.
# - This model represents a quadratic relationship between 'GroundCO' ...
#   ... and 'SensorCO', allowing for a curved fit. It estimates the ...
#   ... coefficients of the quadratic equation that best fits the data.
m.quadls <- lm(GroundCO ~ SensorCO + I(SensorCO^2), data = Dataset)

# Generate a summary of the Quadratic Least Squares (Quad LS) model
summary(m.quadls)

# Overlay OLS and Quad LS regression line on the scatterplot
# - 'lines()' is a function in R used to add lines to a plot.
# - 'SensorCONew' is the x-axis values.
# - 'predict(m.ols, newdata=data.frame(SensorCO=SensorCONew))' calculates ...
#   ... the predicted values of the response variable (in this case, 'GroundCO') ...
#   ... based on the provided 'SensorCO' values and the coefficients ...
# ... of the linear regression model ('m.ols').
# - The line connects the predicted values of 'GroundCO' corresponding ...
#   ... to the 'SensorCO' values in 'SensorCONew'.
plot(Dataset$SensorCO, Dataset$GroundCO,
     ylab="Ground Truth (CO)", xlab="Sensor CO",
     pch=19, cex=0.2)
lines(x=SensorCONew,
      y=predict(m.ols, newdata=data.frame(SensorCO=SensorCONew)),
      col="blue")
lines(x=SensorCONew,
      y=predict(m.quadls, newdata=data.frame(SensorCO=SensorCONew)),
      col="red")
legend(x=800, y=11, legend=c("OLS", "Quad LS"), col=c("blue", "red"), lty=1, cex=1)
# - 'legend()' is a function used to add a legend to a plot in R.
# - The first two arguments, '800' and '11', specify the x and y coordinates ...
#   ... where the legend will be placed on the plot.
# - 'legend=c("OLS", "Quad LS")' provides the labels for the legend one ...
#   ... for each plot element.
# - 'col' specifies the colors associated with each label in the legend.


# Create the same plot using ggplot2
predQuad <- predict(m.quadls, newdata=data.frame(SensorCO=SensorCONew))
ggplot(Dataset, aes(x=SensorCO, y=GroundCO)) + 
  geom_point(size=0.15, col='blue') + 
  geom_line(aes(x=SensorCONew, y=predLinear, color="blue")) +
  geom_line(aes(x=SensorCONew, y=predQuad, color="red")) +
  scale_color_discrete(name="Prediction", labels=c("Linear", "Quadratic")) +
  theme_bw()

# Exercise: Perform linear regression on your project data by choosing ...
# ... a response and covariate.