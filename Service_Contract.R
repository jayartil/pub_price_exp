library(ggplot2)
library(caTools)
library(openxlsx)
library(readxl)
library(caret)

# Generating sample data
#set.seed(123)
#customer_id <- seq(1, 20)
#contract_type <- sample(c("Basic", "Standard", "Premium"), 20, replace = TRUE)
#incidents <- sample(0:10, 20, replace = TRUE)
#complaints <- sample(0:5, 20, replace = TRUE)
#complaint_date <- sample(seq(as.Date('2021/01/01'), as.Date('2021/03/31'), by="day"), 20, replace = TRUE)
#time_to_resolve <- sample(0:10, 20, replace = TRUE)
#feedback_score <- sample(1:5, 20, replace = TRUE)
#cancelled <- sample(c(0, 1), 20, replace = TRUE)

# Creating a data frame
#service_data <- data.frame(customer_id, contract_type, incidents, complaints, complaint_date, 
#                           time_to_resolve, feedback_score, cancelled)

#write.xlsx(service_data, file = "service_data.xlsx", sheetName="service_data", colNames=TRUE)

# Read sheet names 'sheet_name' from excel file

service_data = read_excel("service_data.xlsx", sheet="service_data")

# Preprocessing data
service_data$Cancelled <- factor(service_data$cancelled)
service_data$time_to_resolve[is.na(service_data$time_to_resolve)] <- median(service_data$time_to_resolve, na.rm = TRUE)
service_data$feedback_score[is.na(service_data$feedback_score)] <- median(service_data$feedback_score, na.rm = TRUE)

# Splitting data into training and testing sets
set.seed(123)
split <- sample.split(service_data$cancelled, SplitRatio = 0.7)
train <- subset(service_data, split == TRUE)
test <- subset(service_data, split == FALSE)

# Training logistic regression model
model <- glm(Cancelled ~ time_to_resolve + complaints + feedback_score, data = train, family = "binomial")

summary(model)



ggplot(service_data, aes(x = cancelled)) +
  geom_bar(fill = c("steelblue","red"), width = 0.2) +
  scale_x_continuous(breaks=seq(0,1,1)) + 
  labs(title = "Distribution of Cancelled Contracts", x = "Cancelled", y = "Count") +
  theme(plot.title=element_text(size=10), axis.text = element_text(size = 8))

ggplot(service_data, aes(x = incidents, y = feedback_score)) +
  geom_jitter(alpha = 0.5, size = 3, color="darkred") +
  scale_x_continuous(breaks=seq(0, 10, 2)) + 
  labs(title = "Relationship Between Incidents and Feedback Score", x = "Incidents", y = "Feedback Score")

ggplot(service_data, aes(x = time_to_resolve, y = feedback_score)) +
  geom_point(alpha = 0.5, size = 3, color="red") +
  scale_x_continuous(breaks=seq(0, 10, 2)) + 
  labs(title = "Relationship Between Time to Resolve and Feedback Score", 
       x = "Time to Resolve (days)", y = "Feedback Score")

# Predicting cancellation on test set
pred <- predict(model, newdata = test, type = "response")

# Converting probabilities to binary predictions
pred[pred >= 0.5] <- 1
pred[pred < 0.5] <- 0

# Calculating accuracy of predictions
accuracy <- sum(pred == test$Cancelled) / nrow(test)
print(paste0("Accuracy: ", accuracy))

confusionMatrix(factor(pred), test$Cancelled)

