###################################################
# Loren Collingwood, University of Washington	  #
# UNC -- RTextTools workshop					  #
# Lecture 2 						  		      #
# Very Basic New York Times Example			      #
###################################################

# LOAD THE RTextTools LIBRARY
#install.packags("RTextTools") #Needs to be R-2.14.1 or greater
library(RTextTools)

#CHANGE WORKING DIRECTORY TO YOUR WORKING DIRECTORY
#setwd("/Users/lorencollingwood/Documents/consulting/rtexttools/unc_workshop")

# READ THE CSV DATA from the RTextTools package
nyt_data <- read_data(system.file("data/NYTimes.csv.gz",package="RTextTools"),type="csv")

# [OPTIONAL] SUBSET YOUR DATA TO GET A RANDOM SAMPLE
nyt_data <- nyt_data[sample(1:3000,size=3000,replace=FALSE),]

#Examine the data
class(nyt_data) #make sure it is a data frame object
head(nyt_data) # Look at the first six lines or so
summary(nyt_data) #summarize the data
sapply(nyt_data, class) #look at the class of each column
dim(nyt_data) #Check the dimensions, rows and columns

# CREATE A TERM-DOCUMENT MATRIX THAT REPRESENTS WORD FREQUENCIES IN EACH DOCUMENT
# WE WILL TRAIN ON THE Title and Subject COLUMNS
nyt_matrix <- create_matrix(cbind(nyt_data$Title,nyt_data$Subject), language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTfIdf)
nyt_matrix # Sparse Matrix object

########################################
# 	  CORPUS AND CONTAINER CREATION	   #
########################################

# CREATE A CORPUS THAT IS SPLIT INTO A TRAINING SET AND A TESTING SET
# WE WILL BE USING Topic.Code AS THE CODE COLUMN. WE DEFINE A 2000 
# ARTICLE TRAINING SET AND A 1000 ARTICLE TESTING SET.
corpus <- create_corpus(nyt_matrix,nyt_data$Topic.Code,trainSize=1:2600,testSize=2601:3000,virgin=FALSE)
names(attributes(corpus)) #class matrix_container

# Quick look at Document Term Matrix
example_mat <- corpus@training_matrix
example_names <- corpus@column_names
example_mat2 <- as.matrix(example_mat)
colnames(example_mat2) <- example_names
example_mat2[1:10,1:10]
example_mat2[1:10, 3050:3065]

##########################################
#			   TRAIN MODELS				 #
##########################################
# THERE ARE TWO METHODS OF TRAINING AND CLASSIFYING DATA.
# ONE WAY IS TO DO THEM AS A BATCH (SEVERAL ALGORITHMS AT ONCE)
models <- train_models(corpus, algorithms=c("SVM","MAXENT"))

##########################################
# 			  CLASSIFY MODELS		     #
##########################################

results <- classify_models(corpus, models)

##########################################
# VIEW THE RESULTS BY CREATING ANALYTICS #
##########################################
analytics <- create_analytics(corpus, results)

# RESULTS WILL BE REPORTED BACK IN THE analytics VARIABLE.
# analytics@algorithm_summary: SUMMARY OF PRECISION, RECALL, F-SCORES, AND ACCURACY SORTED BY TOPIC CODE FOR EACH ALGORITHM
# analytics@label_summary: SUMMARY OF LABEL (e.g. TOPIC) ACCURACY
# analytics@document_summary: RAW SUMMARY OF ALL DATA AND SCORING
# analytics@ensemble_summary: SUMMARY OF ENSEMBLE PRECISION/COVERAGE. USES THE n VARIABLE PASSED INTO create_analytics()

head(analytics@algorithm_summary)
head(analytics@label_summary)
head(analytics@document_summary)
analytics@ensemble_summary

# WRITE OUT THE DATA TO A CSV --- look in your working directory
write.csv(analytics@algorithm_summary,"SampleData_AlgorithmSummary.csv")
write.csv(analytics@label_summary,"SampleData_LabelSummary.csv")
write.csv(analytics@document_summary,"SampleData_DocumentSummary.csv")
write.csv(analytics@ensemble_summary,"SampleData_EnsembleSummary.csv")

#Clear all the objects to restore memory settings
rm( list=ls() )