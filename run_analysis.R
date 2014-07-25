## Load data.table package
library("data.table")

## Read from seperate file that contain data 
features <- read.table("features.txt", header = FALSE, stringsAsFactors = FALSE)
activity_labels <- read.table("activity_labels.txt", header = FALSE, stringsAsFactors = FALSE)

subject_train <- read.table("./train/subject_train.txt", header = FALSE, stringsAsFactors = FALSE)
X_train <- read.table("./train/X_train.txt", header = FALSE, stringsAsFactors = FALSE)
y_train <- read.table("./train/y_train.txt", header = FALSE, stringsAsFactors = FALSE)

subject_test <- read.table("./test/subject_test.txt", header = FALSE, stringsAsFactors = FALSE)
x_test <- read.table("./test/X_test.txt", header = FALSE, stringsAsFactors = FALSE)
y_test <- read.table("./test/y_test.txt", header = FALSE, stringsAsFactors = FALSE)

## Find out all columns that contain mean and std in name.
extract_columns <- grep("mean\\(\\)|std\\(\\)", features$V2)

#Tidy up the column name.
features$V2[extract_columns] <- gsub("BodyBody", "Body",
                                     gsub("fB", "FreqB",
                                          gsub("tG", "timeG", 
                                               gsub("tB", "timeB",
                                                    gsub("\\(\\)", "", 
                                                         gsub("-", ".",features$V2[extract_columns]))))))

# Combine data set for train and test into one dataset.
subject_data <- rbind(subject_train, subject_test)
x_data <- rbind(X_train,x_test)
y_data <- rbind(y_train, y_test)

# Assign the meaningful activity labels.
y_data_name <- merge(y_data, activity_labels , by.x = "V1", by.y = "V1")

#Tidy up column name, remove ( ) 
features$V2 <- gsub("\\(\\)", "", gsub("-", ".",features$V2))
colnames(x_data) <- features$V2

#Combine column
xy_data <- data.frame(cbind(x_data, data.frame("activity_labels" = y_data_name$V2), 
                            "subject" = subject_data$V1 ))

final_data <- cbind("Subject" = xy_data$subject, 
                    "ActivityLabels" = xy_data$activity_labels, 
                    xy_data[extract_columns])

##final_data <- final_data[order(final_data$ActivityLabels, as.numeric(as.character(final_data$Subject))),]
##final_data <- final_data[order(as.numeric(as.character(final_data$ActivityLabels)),as.numeric(as.character(final_data$Subject))),]

# Export/ Write complete data into TidyData.csv file
write.csv(final_data, file = "TidyData.csv")

# Creates a second, independent tidy data set with the average of each variable for each activity and subject.
final_data_mean <- aggregate(final_data[,3] ~ final_data$Subject + final_data$ActivityLabels, data = final_data, FUN = mean)

for (i in 4:ncol(final_data)){
        final_data_mean[,i] <- aggregate( final_data[,i] ~ final_data$Subject + final_data$ActivityLabels, data = final_data, FUN = mean )[,3]
}
colnames(final_data_mean) <-  c("Subject", "ActivityLabels", features$V2[extract_columns])

write.csv(final_data_mean, file="AverageVariables.csv")

