# 1.Merges the training and the test sets to create one data set.
xtrain <- read.table("train/X_train.txt")
ytrain <- read.table("train/Y_train.txt")
subtrain <- read.table("train/subject_train.txt")
xtest <- read.table("test/X_test.txt")
ytest <- read.table("test/Y_test.txt")
subtest <- read.table("test/subject_test.txt")
features <- read.table("features.txt")
features_names <- features[,2]
colnames(xtest) <- features_names
colnames(xtrain) <- features_names
xmerge <- rbind(xtest, xtrain)
ymerge <- rbind(ytest, ytrain)
submerge <- rbind(subtest, subtrain)
colnames(submerge) <- c("subject")
colnames(ymerge) <- c("activity")
allmerge <- cbind(xmerge, ymerge, submerge)
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
features_means <- grep("mean", features_names)
features_std <- grep("std", features_names)
means_and_std <- allmerge[sort(c(features_means, features_std))]
# 3. Uses descriptive activity names to name the activities in the data set
activity <- read.table("activity_labels.txt")
activity_names <- activity[,2]
allmerge$activity <- factor(allmerge$activity)
levels(allmerge$activity) <- activity_names
# 4. Appropriately labels the data set with descriptive variable names. 
colnames(allmerge) <- gsub("Acc", "Accelerometer", names(allmerge))
colnames(allmerge) <- gsub("Mag", "Magnitude", names(allmerge))
colnames(allmerge) <- gsub("Gyro", "Gyroscope", names(allmerge))
means_and_std <- allmerge[sort(c(features_means, features_std))]
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(reshape2)
DF <- data.frame(means_and_std, subject = allmerge$subject, activity = allmerge$activity)
activityMelt <- melt(DF,id=c("subject", "activity"),measure.vars=colnames(DF[,1:79]))
fullymelted <- dcast(activityMelt, activity + subject ~ variable,mean)
write.table(fullymelted, "tidy.txt", row.names = FALSE)