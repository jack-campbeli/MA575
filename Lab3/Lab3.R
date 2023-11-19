# All text following '#' in each line is a comment
# Friday, September 22, 2023
# BS755 / MA575: Linear Models
# Lab 3: Data Visualization
# Air quality data example

# Run step by step with ctrl-enter in windows and linux,
# and cmd-enter for Mac Os

# Set the working directory (e.g., change to the directory where ...
# the data is stored on your computer)
# setwd("your/directory/path")

# setwd('~/OneDrive - Boston University/Fall 2023/Teaching/Shariq BS755 MA575/Labs/Lab 03/')

# Install and load required packages if not already installed
# install.packages("carData")
# install.packages("car")
# install.packages("ggplot2")
# install.packages("hrbrthemes")
# install.packages("GGally")
# library(carData)
library(car)
library(ggplot2)
# library(hrbrthemes)
library(GGally)

# Read data from a CSV file using semicolon as delimiter
aq_data <- read.csv("AirQualityData.csv", header=TRUE, as.is=TRUE, sep=';')

# Plot Ground CO
plot(aq_data$CO.GT.)

# Summary of each column
summary(aq_data)

# In this data set it looks like missing values are stored as -200
# Check which values in Ground CO are -200
which(aq_data$CO.GT. == -200)
# The output gives indices/positions in the vector with values -200

# Check howmany values in Ground CO are -200
length(which(aq_data$CO.GT. == -200))

# Exercise: Check other columns in the data set for missing values 

# Replace missing data (-200) with NA
aq_data[aq_data == -200] <- NA
# Note that this replaces -200 to NA across all columns in aq_data

# Smmary of each column
summary(aq_data)

# Check column types for all columns in the data set
str(aq_data)
# Check column types for specific columsn
# typeof(aq_data$Temperature)

# Force specific columns to be numeric if not numeric/integer already.
# Note that change in variable names of PT08.S1.CO. and CO.GT.
Temperature <- as.numeric(as.character(aq_data$Temperature))
RelativeHumidity <- as.numeric(as.character(aq_data$RelativeHumidity))
AbsoluteHumidity <- as.numeric(as.character(aq_data$AbsoluteHumidity))
SensorCO <- as.numeric(as.character(aq_data$PT08.S1.CO.))
GroundCO <- as.numeric(as.character(aq_data$CO.GT.))

# Plot GroundCO
plot(GroundCO)

# Plot scatterplot of GroundCO vs SensorCO
plot(SensorCO, GroundCO)

# Plot scatterplot of GroundCO vs SensorCO using the scatterplot function ...
# ... from the car package
scatterplot(SensorCO, GroundCO)

# Documentation for scatterplot function
?scatterplot

# Scatterplot with plot function
plot(SensorCO, GroundCO,
     ylab="Ground Truth (CO)", xlab="Sensor CO",
     pch=19, cex=0.1)

# Scatterplot matrix of all the variables using the scatterplotMatrix ...
# ... function from the car package
scatterplotMatrix(~ GroundCO + SensorCO + Temperature + AbsoluteHumidity + RelativeHumidity,
                  pch=19, cex=0.1)

# Scatterplot using ggplot2
# Create a data frame with SensorCO and GroundCO
co_data <- data.frame(SensorCO, GroundCO)
# ggplot function for scatterplot
ggplot(co_data, aes(x=SensorCO, y=GroundCO)) + 
  geom_point(size=0.1)

# Using ggplot2 for scatterplot matrix
# Create a data frame with GroundCO, SensorCO, Temperature, ...
# ... AbsoluteHumidity, RelativeHumidity
co_temp_data <- data.frame(GroundCO, SensorCO, Temperature, AbsoluteHumidity, RelativeHumidity)

# Scatterplot matrix with 2D contours
ggpairs(co_temp_data,
        lower=list(continuous=wrap("density", alpha=0.5), combo="box"),
        upper=list(continuous=wrap("points", alpha=0.3, size=0.1)))

# Scatterplot matrix with correlation coefficients
ggpairs(co_temp_data, 
        upper=list(continuous=wrap("points", alpha=0.3, size=0.1)),
        lower=list(continuous=wrap('cor', size=4)))