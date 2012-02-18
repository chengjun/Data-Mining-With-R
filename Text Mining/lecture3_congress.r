#################################################
# Loren Collingwood, University of Washington	#
# UNC -- RTextTools workshop					#
# Lecture 3										#
# Labeling U.S. Congres data					#
#################################################

#install.packags("RTextTools") #Needs to be R-2.14.1 or greater
library(RTextTools)
#Set working directory
#setwd("/Users/lorencollingwood/Documents/consulting/rtexttools/unc_workshop")

list.files()
###########################################
#install.packages("gmodels")
library(gmodels)

congress <- read_data(system.file("data/USCongress.csv.gz",package="RTextTools"),type="csv")
head(congress)
dim(congress)

# Let's look at our topic distribution
CrossTable(congress$major)

# Randomize Data [OPTIONAL]
set.seed(999)
congress <- congress[sample(1:nrow(congress),size=nrow(congress),replace=FALSE),]

###########################################
# 		Document Term Matrix Creation	  #
###########################################

# WE WILL TRAIN ON THE "text" AND "cong" COLUMNS
congress_matrix <- create_matrix(cbind(congress$text,congress$cong), language="english", 
			removeNumbers=TRUE, stemWords=TRUE, weighting=weightTfIdf)
congress_matrix

###########################################
#		Corpus and Container Creation     #
###########################################
# Train on first 4000, test on 4001:4449
corpus <- create_corpus(congress_matrix,congress$major,trainSize=1:4000, testSize=4001:4449, 
		virgin=FALSE)
names(attributes(corpus))

###########################################
#		 	   Train Models     		  #
###########################################

# Create Learners
models <- train_models(corpus, algorithms=c("SVM","MAXENT")) #Make take some time.

###########################################
#		      Classify Data     		  #
###########################################

results <- classify_models(corpus, models)

######################################
# 				Analytics			 #
######################################

analytics <- create_analytics(corpus, results)

# Summary of Algorithm Accuracy
analytics@algorithm_summary

# Plot SVM Recall to see which labels are poor
x <- as.numeric(rownames(analytics@algorithm_summary))[-20]
y <- analytics@algorithm_summary$SVM_RECALL[-20]

plot(x, y, type="l", lwd=3, main="Support Vector Machine Topic Accuracy", ylab="Recall Accuracy", xlab="Topic")
abline(h=.75, lwd=2, col="maroon")
text(x, y, adj=1.2)

# Summary of Label Accuracy
analytics@label_summary
head(analytics@document_summary)

# Confusion Matrices -- look for possible problems
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$CONSENSUS_CODE)
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$PROBABILITY_CODE)
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$SVM_LABEL)
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$MAXENTROPY_LABEL)

# CHECK OVERALL ACCURACY OF ALGORITHMS
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$CONSENSUS_CODE)
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$PROBABILITY_CODE)
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$SVM_LABEL)
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$MAXENTROPY_LABEL)

###########################
# 	 Ensemble Agreement   #
###########################

# Set standard of accuracy. If everything needs to be at over 80%, then you will need to 
# manually code the documents that do not reach that.

analytics@ensemble_summary

final_data <- analytics@document_summary
final_data$recall_ensemble[final_data$CONSENSUS_AGREE == 2] <- "88% recall"
final_data$recall_ensemble[final_data$CONSENSUS_AGREE == 1] <- "72% recall"
head(final_data)

# Write out data for analysis in your favorite statistics program (aka Excel)
getwd()
write.csv(final_data, "congress_coded.csv", row.names=FALSE)

rm ( list=ls() )