library(RCurl)
library(plyr)
library(randomForest)

# load data
github_data_first <- getURL("https://raw.githubusercontent.com/avantcredit/avant-analytics-interview/master/TitanicData.csv")
github_data_second <- getURL("https://raw.githubusercontent.com/avantcredit/avant-analytics-interview/master/TitanicData2.csv")
data_first <- read.csv(text=github_data_first)
data_second <- read.csv(text=github_data_second)

# by looking at summary(data_first$PassengerId) and summary(data_first$PassengerId)
# one can notice that these datasets supposedly do not intersect (because different
# PassengerId)
# Let's assume that this is indeed the case

# this function is used to convert integer numbers like 2,750.00 to numeric
numerize <- function(vec) {
  as.numeric(gsub("[,]", "", as.character(vec)))
}

# deduce a person's surname from the name field.
# surname comes first, before comma
get_surname <- function(name) {
  strsplit(name, ",")[[1]][1]
}

# convert binary sex representation into human readable format
sex_num_to_word <- function(vec) {
  vec <- as.factor(vec)
  mapvalues(vec, from = c(0, 1), to = c("male", "female"))
}

# convert human readable sex representation into binary format
sex_word_to_num <- function(vec) {
  vec <- as.factor(vec)
  mapvalues(vec, to = c(0, 1), from = c("male", "female"))
}

# convert binary survived representation into human readable format
survived_num_to_word <- function(vec) {
  vec <- as.factor(vec)
  mapvalues(vec, from = c(0, 1), to = c("no", "yes"))
}

# convert human readable survived representation into binary format
survived_word_to_num <- function(vec) {
  vec <- as.factor(vec)
  mapvalues(vec, to = c(0, 1), from = c("no", "yes"))
}

# figure out if a person was travelling with a spouse.
# the heuristics for that are:
# among the travellers there is a person with the same Surname, different Sex,
# same Embarked and same Pclass
find_singles <- function (df) {
  df$Single <- apply(df, 1, function(row) {
    # always finds at least one value (the person him/herself)
    potential_spouses <- which(row['Surname'] == df$Surname)
    for (idx in potential_spouses) {
      suspect = data[idx,]
      if(suspect['Sex'] != row['Sex'] &&
         suspect['Embarked'] == row['Embarked'] &&
         suspect['Pclass'] == row['Pclass'] && 
         suspect['Name'] != row['Name'])
        return('no')
    }
    return('yes')
  })
  df$Single <- as.factor(df$Single)
  df
}

# make data frames have the same type of contents in the same columns
data_first <- transform(data_first, Survived = survived_num_to_word(Survived),
                        Pclass = as.factor(Pclass), Name = as.character(Name))
data_second <- transform(data_second, Fare = numerize(Fare),
                         Sex = sex_num_to_word(Sex), Survived = survived_num_to_word(Survived),
                         Pclass = as.factor(Pclass), Name = as.character(Name))
# merge
data <- rbind(data_first, data_second)

#Add some more fields: Surname and Single/Married
data$Surname <- as.factor(sapply(data$Name, get_surname, simplify = T))
data <- find_singles(data)

###
# Done with processing data, let's try applying a model
###
PARAMS <- c("Pclass", "Sex", "Age", "Fare", "Embarked", "Single")
FORMULA <- Survived ~ Pclass + Sex + Age + Fare + Embarked + Single

# let's train the model on 80% of the data and evaluate on the other 20%
eighty_percent = 0.8*nrow(data)
train <- data[1:eighty_percent,]
test <- data[(eighty_percent+1):nrow(data),]

# We will try fitting a tree and a random forest, and see which one performed better
fit_tree <- rpart(FORMULA, data=train, method="class")
fit_forest <- randomForest(FORMULA, data=train, na.action=na.omit,
                           importance=TRUE, keep.forest=TRUE)

# check which variables are the most important
varImpPlot(fit_forest, type=1)
# apparently the most important predictor is sex,
# followed by pclass, age, fare, single, embarked

# visualize the tree
post(fit_tree, file = "tree.ps", 
     title = "Classification Tree for Titanic Survival")

tree_model <- predict(fit_tree, newdata=subset(test,select=PARAMS))
predicted_forest <- predict(fit_forest, newdata=subset(test,select=PARAMS))

predicted_tree <- as.factor(apply(tree_model, 1, function(row) {
  result = 'no'
  if (row['yes'] > row['no']) {
    result = 'yes'
  }
  result
}))

# print error rates of the two models
error_tree <- round(length(which(predicted_tree != test$Survived))/nrow(test)*100, 2)
error_forest <- round(length(which(predicted_forest != test$Survived))/nrow(test)*100, 2)

print(paste("Prediction error of tree classifier: ", error_tree, "%", sep = ""))
print(paste("Prediction error of random forest classifier: ", error_forest, "%", sep = ""))
