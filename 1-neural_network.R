## Install packages
install.packages('vioplot')
install.packages('ISLR')
install.packages('caTools')
install.packages('neuralnet')

## Load the package
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
## Create max and min to scale the data
maxs <- apply(College[,2:18], 2, max)
mins <- apply(College[,2:18], 2, min)

## Scale the data
scaled.data <- as.data.frame(scale(College[,2:18], 
                                   center = mins, 
                                   scale = maxs-mins))
print(head(scaled.data,2))

## Split the data into training and test set
# convert private column from Yes/No to 1/0
Private <- as.numeric(College$Private)-1
data <- cbind(Private, scaled.data)

set.seed(101) # sets random number

# split data
split <- sample.split(data$Private, SplitRatio = 0.70)

# Split based off split Boolean Vector
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)


