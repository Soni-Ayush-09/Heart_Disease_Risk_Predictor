library(caret)
library(mlr3)
library(randomForest)
df=read.csv("C:\\Users\\91869\\OneDrive\\Desktop\\R_project\\Heart_disease_cleveland_new.csv")


# Convert categorical variables to factors
df$age <- factor(df$age)
df$sex <- factor(df$sex)
df$cp <- factor(df$cp)
df$fbs <- factor(df$fbs)
df$trestbps<- factor(df$trestbps)
df$chol<- factor(df$chol)
df$restecg <- factor(df$restecg)
df$thalach<- factor(df$thalach)
df$exang <- factor(df$exang)
df$oldpeak<- factor(df$oldpeak)
df$slope <- factor(df$slope)
df$ca<- factor(df$ca)
df$thal <- factor(df$thal)
df$target<- factor(df$target)

# Split the data into training and test sets
train_index <- sample(1:nrow(df), nrow(df) * 0.8)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Train a random forest model
#The following code trains a random forest model to predict the risk of heart disease:
rf_model <- train(target ~ ., data = train_data, method = "rf")


rf_pred <- predict(rf_model, newdata = test_data)
rf_pred

saveRDS(rf_model, file = "my_model.rds")