library(ggplot2)
library(GGally)
library(MASS)
library(rms)
library(car)
library(mice)
library(MLmetrics)
library(caret)
facebook<-read.csv("facebook_updated.csv",
                   header=T,sep=",") #read in dataset
md.pattern(facebook)#check missing data
facebook$like[is.na(facebook$like)] <- 0 #replace missing data
facebook$share[is.na(facebook$share)] <- 0 
facebook$Paid[is.na(facebook$Paid)] <- 0 
facebook$comment[is.na(facebook$comment)] <- 0 
md.pattern(facebook)# double check missing data
facebook$Category<-as.character(facebook$Category)
#create category variable
facebook$season<-NA
facebook$season[facebook$Post.Month<=2]<-'winter'
facebook$season[facebook$Post.Month>11]<-'winter'
facebook$season[facebook$Post.Month>=3 & facebook$Post.Month<6]<-'spring'
facebook$season[facebook$Post.Month>5 & facebook$Post.Month<9]<-'summer'
facebook$season[facebook$Post.Month>8&facebook$Post.Month<12]<-'autumn'
facebook$weekday<-NA
facebook$weekday[facebook$Post.Weekday<6]<-1
facebook$weekday[facebook$Post.Weekday>5]<-0
facebook$worktime<-0
facebook$worktime[facebook$Post.Hour>9 & facebook$Post.Hour<18]<-1
#transform
facebook$ln.Page.Total.likes<-log(facebook$Page.total.likes)
facebook$ln.Lifetime.Post.Consumers<-log(facebook$Lifetime.Post.Consumers)
#facebook<-subset(facebook,row.names(facebook) != 442)
md.pattern(facebook)# double check missing data
#summary all column
summary(facebook)


#create training and validation dataset
training_data <- subset(facebook, obs_type == "Training")
validation_data <- subset(facebook, obs_type == "Validation")
#model selection
m.mlr <- lm(ln.Lifetime.Post.Consumers ~ Type + season + worktime +I(log(Page.total.likes))
            + weekday + Category + Paid + Paid*ln.Page.Total.likes
            + Type*ln.Page.Total.likes + season*ln.Page.Total.likes + worktime*ln.Page.Total.likes +
              weekday*ln.Page.Total.likes + Category*ln.Page.Total.likes
            ,data = training_data)
summary(m.mlr)
stepwise_model <- step(m.mlr, direction = "both")
#remove insignificant variables 
stepwise<-lm(ln.Lifetime.Post.Consumers~  ln.Page.Total.likes + Type:ln.Page.Total.likes + 
               season:ln.Page.Total.likes, data=training_data)
summary(stepwise)
#diagnose
plot(stepwise)
avPlots(stepwise)
vif(stepwise)

#prediction
validation_data$Predicted_ln.Lifetime.Post.Consumers <- predict(stepwise, newdata = validation_data)
observed_values <- validation_data$ln.Lifetime.Post.Consumers
predicted_values <- validation_data$Predicted_ln.Lifetime.Post.Consumers

rmse <- RMSE(predicted_values, observed_values)
mae <- MAE(predicted_values, observed_values)
r_squared <- R2_Score(predicted_values, observed_values)
cat("Root Mean Squared Error (RMSE):", round(rmse, digits = 4))
cat("Mean Absolute Error (MAE):", round(mae, digits = 4))
cat("R-squared (R^2) Score:", round(r_squared, digits = 4))

ggplot(validation_data, aes(x = ln.Lifetime.Post.Consumers, y = Predicted_ln.Lifetime.Post.Consumers)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Observed Values", y = "Predicted Values",
       title = "Observed vs. Predicted Values") +
  xlim(c(2,10)) +
  ylim(c(4,8)) +
  theme_bw()

# Residuals plot
ggplot(validation_data, aes(x = 1:nrow(validation_data), y = ln.Lifetime.Post.Consumers-Predicted_ln.Lifetime.Post.Consumers)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0, color = "red", linetype = "dashed") +
  labs(x = "Observation Index", y = "Residuals",
       title = "Observed vs. Predicted Values") +
  theme_bw()

