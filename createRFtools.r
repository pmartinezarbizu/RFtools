#create a RFtools package:
library(devtools)
library(roxygen2)
setwd('C:/Users/pmartinez/Documents/R/')

#create('RFtools')
# edit DESCRIPTION

# copy R code to R folder
# add roxygen2 tags to functions 
# do NOT forget to add a #'@export tag to export the functions to the namespace 


setwd('./RFtools')
document()

#install package
setwd('..')
install('RFtools') 

--------------------------------------------------
cd C:\Users\pmartinez\Documents\R
"C:\Program Files\R\R-3.3.1\bin\R.exe" CMD build pairwiseAdonis

"C:\Program Files\R\R-3.3.1\bin\R.exe" CMD INSTALL pairwiseAdonis_...


---
data(maldi)
library(randomForest)

unique(maldi$species)
maldi_train <- maldi[maldi$species != 'Tachidius discipes',]
maldi_test <- maldi[maldi$species == 'Tachidius discipes',]

# exclude Tachidius discipes from factors
maldi_train$species <- factor(maldi_train$species)

rf <- randomForest(species ~ ., data = maldi_train[-1])