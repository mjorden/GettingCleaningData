library(plyr)
library(dplyr)

# Read in TEST data
test_x <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_y <- read.table("./UCI HAR Dataset/test/y_test.txt")
test_sub <- read.table("./UCI HAR Dataset/test/subject_test.txt")
# Read in TRAINING data
train_x <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("./UCI HAR Dataset/train/y_train.txt")
train_sub <- read.table("./UCI HAR Dataset/train/subject_train.txt")
#read activity key so that activities have descriptive names
activity_key <- read.table("./UCI HAR Dataset/activity_labels.txt")
colnames(activity_key) <- c("activityID","activityName")
#features dataset contains variable names
labs <- read.table("./UCI HAR Dataset/features.txt")
#convert t and f to Time and FFT so that variable names are descriptive
labs[,2] <- gsub("^t","Time_",labs[,2])
labs[,2] <- gsub("^f","FFT_",labs[,2])
labs[,2] <- gsub("*mean()","Mean",labs[,2])
labs[,2] <- gsub("*std()","STD",labs[,2])
labs[,2] <- gsub("*Acc","_Acceleration",labs[,2])
#give column names
colnames(test_sub) <- "subjectID"
colnames(test_y) <- "activityID"
colnames(test_x) <- labs[,2]

#give column names
colnames(train_sub) <- "subjectID"
colnames(train_y) <- "activityID"
colnames(train_x) <- labs[,2]

#create data sets
test_dat <- cbind(test_sub,test_y,test_x)
train_dat <- cbind(train_sub,train_y,train_x)
#remove object not needed anymore
rm(train_sub)
rm(train_y)
rm(train_x)
rm(test_sub)
rm(test_x)
rm(test_y)
#create merged data set
all_dat <- rbind(test_dat,train_dat)
#create searchable charracter list of column names for subsetting
all_dat_names <- colnames(all_dat)
#create indexed list of column locations where mean or std is found
gr_id <- grepl("subjectID",all_dat_names) | grepl("activityID",all_dat_names) | grepl("activityName",all_dat_names) | grepl("type",all_dat_names) | grepl("Mean",all_dat_names) | grepl("STD",all_dat_names)
rm(all_dat_names)
#subset out only columns where we found mean or std
sub_dat <- all_dat[,gr_id]
rm(all_dat)
#rm(all_dat)

#subset the second tidy data
tidy_data <- aggregate(. ~subjectID + activityID, sub_dat, mean)
#order the data to be more readable
tidy_data <- tidy_data[order(tidy_data$subjectID,tidy_data$activityID),]
tidy_data <- left_join(tidy_data, activity_key, by="activityID")
#tidy_data <- mutate(tidy_data, activityID=activityName)
write.table(tidy_data, "./UCI HAR Dataset/tidy2.txt")
sub_dat <- left_join(tidy_data,activity_key, by="activityID")
write.table(sub_dat, "./UCI HAR Dataset/tidy1.txt")
readme_list <- c(names(tidy_data))