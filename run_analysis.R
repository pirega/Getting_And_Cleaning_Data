setwd("~/FORMACION/Coursera/Gettin_and_Clean_Data/project/UCI HAR Dataset")


#Merges the training and the test sets to create one data set.

# Reading common files
feature_names <- read.table('features.txt')
head(feature_names)

# Reading test files
x_test <- read.table("test/X_test.txt")
y_test <- scan("test//y_test.txt")
subject_test<- scan("test/subject_test.txt")
names(x_test) <- feature_names$V2
x_test$subject <- subject_test
x_test$activity <- y_test
names(x_test)

# Reading train files
x_train <- read.table("train/X_train.txt")
y_train <- scan("train/y_train.txt")
names(x_train) <- feature_names$V2
subjects_train = scan('train/subject_train.txt')
x_train$subject <- subjects_train
x_train$activity <- y_train
names(x_train)

# Merge train and test files
data_set <- rbind(x_train,x_test)
head(data_set)

# Extracts only the measurements on the mean and standard deviation for each measurement. 

mean_and_std_measurement <- grepl( '(-mean\\(\\)|-std\\(\\))', feature_names$V2 )
mean_and_std_measurement <- append(mean_and_std_measurement, TRUE) 
mean_and_std_measurement <- append(mean_and_std_measurement, TRUE)

means_and_stds = data_set[, mean_and_std_measurement]
names(means_and_stds)

# Uses descriptive activity names to name the activities in the data set
activity_labels = read.table('activity_labels.txt')
means_and_stds$activity_label = factor(means_and_stds$activity, levels=c(1,2,3,4,5,6),
                                       labels=activity_labels$V2)


# Creates a second, independent tidy data set with the average of each variable for each activity
# and each subject.
tidy.frame = data.frame()

subj <- sort( unique(means_and_stds$subject) )
activities <- sort( unique(means_and_stds$activity) )

for (i in subj) {
        for (act in activities) {
                # subset by subject and activity
                subset = means_and_stds[ means_and_stds$subject==i & means_and_stds$activity == act, ]
                # get mean values for subject-activity pair, coerce to data.frame
                by_subject_and_activity = as.data.frame( lapply( subset[,1:66], FUN=mean ) )
                # resupply subject, activity and activity label
                by_subject_and_activity$subject = i
                by_subject_and_activity$activity = act
                by_subject_and_activity$activity_label = activity_labels[act,2]
                # build up tidy dataset
                tidy.frame = rbind(tidy.frame, by_subject_and_activity)
        }
}

write.table( tidy.frame, file="tidy-data.csv" )
