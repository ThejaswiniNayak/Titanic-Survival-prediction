#Set working dirctory
#https://campus.datacamp.com/courses/kaggle-r-tutorial-on-machine-learning/chapter-1-raising-anchor?ex=1
# youtube link:
#https://www.youtube.com/watch?v=ajTc5y3OqSQ

#Import the training set: train

#----------------------------------------------------------------------------------
#Set Sail
#----------------------------------------------------------------------------------
# Import the train set: test
train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
train <- read.csv(train_url)

# Import the testing set: test
test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
test <- read.csv(test_url)

# Print train and test to the console
print(train)
print(test)

#----------------------------------------------------------------------------------
#Understanding your data
#----------------------------------------------------------------------------------
str(test)
str(train)

#----------------------------------------------------------------------------------
#Rose vs Jack, or Female vs Male
#----------------------------------------------------------------------------------
# Your train and test set are still loaded
str(train)
str(test)
# Survival rates in absolute numbers
table(train$Survived) 
# Survival rates in proportions
prop.table(table(train$Survived))
# Two-way comparison: Sex and Survived
table(train$Sex, train$Survived)
# Two-way comparison: row-wise proportions
prop.table(table(train$Sex, train$Survived), margin=1)

#----------------------------------------------------------------------------------
#Does age play a role?
#----------------------------------------------------------------------------------
# Create the column child, and indicate whether child or no child
train$Child <- NA
train$Child[train$Age < 18] <- 1
train$Child[train$Age >= 18] <- 0
# Two-way comparison
prop.table(table(train$Child, train$Survived), margin=1)

#----------------------------------------------------------------------------------
#Making your first predictions
#----------------------------------------------------------------------------------
# Copy of test
test_one <- test
# Initialize a Survived column to 0
test_one$Survived <- 0
# Set Survived to 1 if Sex equals "female"
test_one$Survived[test_one$Sex == "female"] <- 1

#----------------------------------------------------------------------------------
#Intro to decision trees
#----------------------------------------------------------------------------------
library(rpart)

#----------------------------------------------------------------------------------
#Intro to decision trees
#----------------------------------------------------------------------------------
#Inside rpart, there is therpart() function to build your first decision tree. 
#The function takes multiple arguments:
#formula: specifying variable of interest, and the variables used for prediction 
#(e.g. formula = Survived ~ Sex + Age).
#data: The data set to build the decision tree (here train).
#method: Type of prediction you want. We want to predict a categorical variable, 
#so classification: method = "class".
#Your call could look like this:
#my_tree <- rpart(Survived ~ Sex + Age,
#data = train,
#method ="class")
#To visualize the resulting tree, you can use the plot(my_tree) and text(my_tree). 
#The resutling graphs will not be that informative, but R has packages to make it all fancier: 
#rattle, rpart.plot, and RColorBrewer.

# Build the decision tree
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")
# Visualize the decision tree using plot() and text()
plot(my_tree_two)
text(my_tree_two)
# Load in the packages to build a fancy plot
#library(rattle)
library(rpart.plot)
#library(RColorBrewer)
# Time to plot your fancy tree
#rattle(my_tree_two)
rpart.plot(my_tree_two)
#RColorBrewer(my_tree_two)

#----------------------------------------------------------------------------------
#Predict and submit to Kaggle
#----------------------------------------------------------------------------------
# Make predictions on the test set
my_prediction <- predict(my_tree_two, newdata = test, type = "class")

# Finish the data.frame() call
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Use nrow() on my_solution
nrow(my_solution)

# Finish the write.csv() call
write.csv(my_solution, file = "C:/Users/pt2/Documents/Personal/Study/Data science Associate/R prcatice/Kaggle's Titanic/my_solution.csv", row.names = FALSE)

#----------------------------------------------------------------------------------
#Overfitting, the iceberg of decision trees
#----------------------------------------------------------------------------------
#If you submitted the solution of the previous exercise, you got a result that outperforms a solution using purely gender. Hurray!
#Maybe we can improve even more by making a more complex model? In rpart, 
#the amount of detail is defined by two parameters:
#"cp" determines when the splitting up of the decision tree stops.
#"minsplit" determines the minimum amount of observations in a leaf of the tree.
#In the super_model on the right, cp = 0 (no stopping of splits) and 
#minsplit = 2 (smallest leaf possible). This will create the best 
#model! Or not? Check out the resulting plot with fancyRpartPlot()

# Change this command
my_tree_three <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                       data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))
# Visualize my_tree_three
rpart.plot(my_tree_three)

#----------------------------------------------------------------------------------
#Re-engineering our Titanic data set
#----------------------------------------------------------------------------------
#A valid assumption is that larger families need more time to get together on a sinking ship, 
#and hence have less chance of surviving. Family size is determined by the variables SibSp 
#and Parch, which indicate the number of family members a certain passenger is traveling with.
#So when doing feature engineering, you add a new variable family_size, which is the sum of 
#SibSp and Parch plus one (the observation itself), to the test and train set.

# Create train_two
train_two <- train
train_two$family_size <- train_two$SibSp + train_two$Parch + 1
# Finish the command
my_tree_four <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size,
                      data = train_two, method = "class")
# Visualize your new decision tree
rpart.plot(my_tree_four)
#family size is not having any significance in the desition tree.

#----------------------------------------------------------------------------------
#Passenger Title and survival rate
#----------------------------------------------------------------------------------
#we have train_new and test_new. These data sets contain a new column with the name Title 
#(referring to Miss, Mr, etc.). Title is another example of feature engineering: 
#it's a new variable that possibly improves the model.

# Import the train_new set: test
train_new_url <- "C:/Users/pt2/PycharmProjects/Kaggle ML Cource/train_new.csv"
train_new <- read.csv(train_new_url)
# Import the test_new set: test
test_new_url <- "C:/Users/pt2/PycharmProjects/Kaggle ML Cource/test_new.csv"
test_new <- read.csv(test_new_url)
my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title,
                      data = train_new, method = "class")
rpart.plot(my_tree_five)
my_prediction <- predict(my_tree_five, test_new, type = "class")
my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "C:/Users/pt2/Documents/Personal/Study/Data science Associate/R prcatice/Kaggle's Titanic/my_solution1.csv", row.names = FALSE)

#----------------------------------------------------------------------------------
#What is a Random Forest
#----------------------------------------------------------------------------------
#The code to clean your entire dataset from missing data and split it up in training 
#and test set is provided in the sample code. Study the code chunks closely so you 
#understand what's going on. Just click Submit Answer to continue.
# Passenger on row 62 and 830 do not have a value for embarkment.

train$Embarked[c(62, 830)] <- "S"
# Factorize embarkment codes.
train$Embarked <- factor(train$Embarked)
# How to fill in missing Age values?
# We make a prediction of a passengers Age using the other variables and a decision tree model.
# This time you give method = "anova" since you are predicting a continuous variable.
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                       data = train[!is.na(train$Age),], method = "anova")
train$Age[is.na(train$Age)] <- predict(predicted_age, train[is.na(train$Age),])

# Factorize embarkment codes.
test$Embarked <- factor(test$Embarked)
# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
test$Fare[153] <- median(test$Fare, na.rm = TRUE)

predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                       data = test[!is.na(test$Age),], method = "anova")
test$Age[is.na(test$Age)] <- predict(predicted_age, test[is.na(test$Age),])

#----------------------------------------------------------------------------------
#A Random Forest analysis in R
#----------------------------------------------------------------------------------
# Load in the package
#install.packages("randomForest")
library(randomForest)
# Set seed for reproducibility
set.seed(111)
# Apply the Random Forest Algorithm
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                          data = train, importance = TRUE, ntree = 1000)
# Make your prediction using the test set
my_prediction <- predict(my_forest, test)
#make a data frame with two columns of PassengerID and Survived
my_solution = data.frame(PassangerId=test$PassengerId,Survived=my_prediction)
#write your solutions to csv file 
write.csv(my_solution,
          file="C:/Users/pt2/Documents/Personal/Study/Data science Associate/R prcatice/Kaggle's Titanic/my_solution2.csv"
          ,row.names=FALSE)
#----------------------------------------------------------------------------------
#Important variables
#----------------------------------------------------------------------------------
#When running the varImpPlot()function, two graphs appear: the accuracy plot shows how much 
#worse the model would perform without the included variables. So a high decrease 
#(= high value x-axis) links to a high predictive variable. The second plot is the 
#Gini coefficient. The higher the variable scores here, the more important it is for the model.
varImpPlot(my_forest)

#Improving the result
my_forest <- randomForest(as.factor(Survived) ~ Sex + Pclass + Fare + Age + SibSp ,
                          data = train, importance = TRUE, ntree = 1000)
# Make your prediction using the test set
my_prediction <- predict(my_forest, test)
#make a data frame with two columns of PassengerID and Survived
my_solution = data.frame(PassangerId=test$PassengerId,Survived=my_prediction)
#write your solutions to csv file 
write.csv(my_solution,
          file="C:/Users/pt2/Documents/Personal/Study/Data science Associate/R prcatice/Kaggle's Titanic/my_solution3.csv"
          ,row.names=FALSE)

my_forest1 <- randomForest(as.factor(Survived) ~ Sex + Pclass + Fare + Age + SibSp ,
                          data = train, importance = TRUE, ntree = 1000,mtry=3)
print(my_forest1)

my_forest2 <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age+SibSp + Parch + Fare + Embarked ,
                           data = train, importance = TRUE, ntree = 1000,mtry=4)
print(my_forest2)
my_forest2$importance

my_forest3 <- randomForest(as.factor(Survived) ~ Sex + Fare + Age +  Pclass ,
                           data = train, importance = TRUE, ntree = 1500,mtry=2)
print(my_forest3)
my_forest3$importance

train[sapply(train,is.character)]=
  lapply(train[sapply(train, is.character)], as.factor)

set.seed(1260)

my_forest4 <- randomForest(as.factor(Survived) ~ Sex +  Age + Fare + Pclass + SibSp +Embarked ,
                           data = train, importance = TRUE, ntree = 1000,mtry=2,
                           samsize=c('0'=549,'1'=342))
print(my_forest4)
my_forest3$importance

