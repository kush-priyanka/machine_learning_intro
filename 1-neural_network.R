## Install packages
install.packages('mice')
install.packages('ISLR')
install.packages('caTools')
install.packages('neuralnet')

## Load the package
library(mice) ##package to visualize missing data values
library(ISLR) ## package with the college dataset
library(caTools) ## used to split data into training and test
library(neuralnet) #neural network function

##  Load dataset “sleep”, which comes within the package “VIM” 
data(College, package ="ISLR")

## Check the datasets
head(College)

### Data Pre-processing ####
## First, we need to know how many rows in “sleep”
nrow(College)

## Use complete.cases() or na.omit() to see tuples without missing value. 
College[complete.cases(College),] 

# or
na.omit(College)

##  Count the number of rows without missing value
nrow(College[complete.cases(College),])

## To reverse the condition logic (rows containing one or more missing value), 
# we use the exclamation mark highlighted in Red
College[!complete.cases(College),] 
nrow(College[!complete.cases(College),])

### Data visualization ###
## Check how missing values are distributed in data
md.pattern(College)

## Visualize data-- aggr
aggr(College, prop = FALSE, numbers = TRUE)

## Visualize data-- marginplot
marginplot(College[c("Apps", "Accept")], 
           pch = c(20), col= c("darkgray","red","blue"))

#### Neural Network ####
## Create max and min to normalize the data
maxs <- apply(College[,2:18], 2, max)
mins <- apply(College[,2:18], 2, min)

## Scale/normalize the data
scaled.data <- as.data.frame(scale(College[,2:18], 
                                   center = mins, 
                                   scale = maxs-mins))
print(head(scaled.data,2))

## Split the data into training and test set
# convert private column from Yes/No to 1/0
Private <- as.numeric(College$Private)-1
data <- cbind(Private, scaled.data)

set.seed(101) # sets random number

# split data, 70 % as training
split <- sample.split(data$Private, SplitRatio = 0.70)

# Split based off split Boolean Vector
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

# Nueral network function; create a function
feats <- names(scaled.data)
f <- paste(feats , collapse = '+')
f <- paste('Private~' , f)
f <- as.formula(f)

# Output of formula below
# Private ~ Apps + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + 
#   P.Undergrad + Outstate + Room.Board + Books + Personal + 
#   PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate

# Run the model
nn <- neuralnet(f, train, 
                hidden = c(10,10,10), 
                linear.output = FALSE)

# Compute predictions of test set
predicted.nn.values <- compute(nn, test[2:18])
print(head(predicted.nn.values$net.result))

# Round off vlaues
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,
                                         round, digits = 0)

# Create a simple confusion matrix to check the trainer against test
table(test$Private, 
      predicted.nn.values$net.result)

#              Predicted No    Predicted Yes
# Actual No     TN=55           FP=9
# Actual Yes    FN=6            TP =163

## Visualize the plot
# Black lines represent weighted vectors between nuerons.
# Blue lines represents the bias added.
plot(nn)

## Run the model on 80% of the data as training
split1 <- sample.split(data$Private, SplitRatio = 0.80)

# Split based off split Boolean Vector
train1 <- subset(data, split1 == TRUE)
test1 <- subset(data, split1 == FALSE)

# Run the model
nn1 <- neuralnet(f, train1, 
                hidden = c(10,10,10), 
                linear.output = FALSE)

# Compute predictions of test set
predicted.nn.values1 <- compute(nn1, test1[2:18])
print(head(predicted.nn.values1$net.result))

# Round off vlaues
predicted.nn.values1$net.result <- sapply(predicted.nn.values1$net.result,
                                         round, digits = 0)

# Create a simple confusion matrix to check the trainer against test
table(test1$Private, 
      predicted.nn.values1$net.result)
#              Predicted No    Predicted Yes
# Actual No     TN=36           FP=6
# Actual Yes    FN=62           TP =111