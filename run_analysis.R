# load required packages
require(data.table, quietly = TRUE)

# set the current working directory for the data directory
setwd(file.path(getwd(), "data"))

# list all available files
list.files(getwd(), recursive = TRUE)


# load all datasets
activity.labels = read.table(file.path(getwd(), "activity_labels.txt"))
features = read.table(file.path(getwd(), "features.txt"))

train.x = read.table(file.path(getwd(), "train", "X_train.txt"))
train.y = read.table(file.path(getwd(), "train", "Y_train.txt"))
train.subject = read.table(file.path(getwd(), "train", "subject_train.txt"))

test.x = read.table(file.path(getwd(), "test", "X_test.txt"))
test.y = read.table(file.path(getwd(), "test", "Y_test.txt"))
test.subject = read.table(file.path(getwd(), "test", "subject_test.txt"))


# merges the training and test data sets into a single data set
x = rbind(train.x, test.x)

# extracts only the measurements for mean and std deviation for each measurement
colnames(x) = c(as.character(features[,2]))
x.mean = grep("mean()", colnames(x), fixed=TRUE)
x.std = grep("std()", colnames(x), fixed=TRUE)

only.mean.std = x[, c(x.mean, x.std)]

# giving better names for activities
y = rbind(train.y, test.y)
activity.data = cbind(y, only.mean.std)
colnames(activity.data)[1] = "Activity"

# label the activity data set
activity.labels$V2 = as.character(activity.labels$V2)
for(i in 1:nrow(activity.data)){
    activity.data[i,1] = activity.labels[activity.data[i,1],2]
}

# create independent data set with the average of each activity
subject = rbind(train.subject, test.subject)
all = cbind(subject, activity.data)
colnames(all)[1] = "Subject"

# create tidy data set
tidy = aggregate(all[,3] ~ Subject + Activity, data = all, FUN = "mean")
for(i in 4:ncol(all)){
    tidy[,i] = aggregate(all[,i] ~ Subject + Activity, data = all, FUN = "mean")[,3]
}

colnames(tidy)[3:ncol(tidy)] = colnames(only.mean.std)


write.table(tidy, "tidy.txt", row.names = FALSE)



    
