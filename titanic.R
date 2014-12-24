library(RCurl)
library(plyr)

# load data
github_data_first <- getURL("https://raw.githubusercontent.com/avantcredit/avant-analytics-interview/master/TitanicData.csv")
github_data_second <- getURL("https://raw.githubusercontent.com/avantcredit/avant-analytics-interview/master/TitanicData2.csv")
data_first <- read.csv(text=github_data_first)
data_second <- read.csv(text=github_data_second)

# by looking at summary(data_first$PassengerId) and summary(data_first$PassengerId)
# one can notice that these datasets supposedly do not intersect (because different
# PassengerId)
# Let's assume that this is indeed the case

# convert an integer with commas into numerics
head(data_second$Name)
head(data_first$Name)

# this function is used to convert integer numbers like 2,750.00 to numeric
numerize <- function(vec) {
  as.numeric(gsub("[,]", "", as.character(vec)))
}

# surname comes first, before comma
get_surname <- function(name) {
  strsplit(name, ",")[[1]][1]
}

travels_single <- function(name, sex, embarked, pclass) {
  cat(name)
  return(0)
}

sex_num_to_word <- function(vec) {
  vec <- as.factor(vec)
  mapvalues(vec, from = c(0, 1), to = c("male", "female"))
}

sex_word_to_num <- function(vec) {
  vec <- as.factor(vec)
  mapvalues(vec, to = c(0, 1), from = c("male", "female"))
}

survived_num_to_word <- function(vec) {
  vec <- as.factor(vec)
  mapvalues(vec, from = c(0, 1), to = c("no", "yes"))
}

survived_word_to_num <- function(vec) {
  vec <- as.factor(vec)
  mapvalues(vec, to = c(0, 1), from = c("no", "yes"))
}

# make data frames have the same type of contents in the same columns
data_first <- transform(data_first, Survived = survived_num_to_word(Survived),
                        Pclass = as.factor(Pclass), Name = as.character(Name))
data_second <- transform(data_second, Fare = numerize(Fare),
                         Sex = sex_num_to_word(Sex), Survived = survived_num_to_word(Survived),
                         Pclass = as.factor(Pclass), Name = as.character(Name))

# figure out if a person was travelling with a spouse.
# the heuristics for that are:
# among the travellers there is a person with the same surname, different sex,
# same Embarked and same Pclass
find_singles <- function (df) {
  df$Single <- apply(df[,c('Name','Surname','Embarked','Sex','Pclass')], 1, function(row) {
      # always finds at least one value (the person themselve)
      potential_spouses <- which(row['Surname'] == df$Surname)
      if (length(potential_spouses) == 1) {
        return('yes')
      }
      single = 'yes'
      for (idx in potential_spouses) {
        suspect = data[idx,]
        if(suspect['Name'] == row['Name'])
          next
        if(suspect['Sex'] != row['Sex'] && suspect['Embarked'] == row['Embarked'] && suspect['Pclass'] == row['Pclass'])
          single = 'no'
      }
      return(single)
    })
  df$Single <- as.factor(df$Single)
  df
}

# merge
data <- rbind(data_first, data_second)

head(data)
data$Surname <- as.factor(sapply(data$Name, get_surname, simplify = T))
data <- find_singles(data)
summary(data$Single)
which(data$Single == 'no')[1]
head(data)
which(data$Surname == 'Futrelle')
data[4,]
data[138,]
