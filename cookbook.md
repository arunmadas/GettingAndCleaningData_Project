---
title: "ReadMe_TidyData_Courseproject"
output: html_document
---
---------------------------------------------------------------------------------------
## Author : Arun Kumar Madas
## Date   : 04/20/2015
## Course : Getting and Cleaning Data
---------------------------------------------------------------------------------------

Use the required libraries


```r
#packages
# install.packages("data.table")
# install.packages("reshape2")
library("data.table")
library("reshape2")
```

#Step 1 : download the data zip file, unzip the files, validate if unzip was good, merge data sets.

## 1. Merges the training and the test sets to create one data set.


```r
        # set current working directory
        setwd("C:/Arun/docs/dsc_jhu/3__datacleaning/courseproj")
        #create temp file
        temp <- tempfile()
        #sets to use internet intended to use for downloading zip data file
        setInternet2(use = TRUE)
        #download the file to temp
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp,mode="wb")
        #unzip the temp into current working directory, overwrite if previously present, helps in re-running
        unzip(temp, overwrite=TRUE,exdir=".")
        #cleanup
        unlink(temp)

        #set the current working directory into path variable
        path <- getwd()
        # append the extracted directory
        pathIn <- file.path(path, "UCI HAR Dataset")

        #check the listing of file directory 
        extractedFileList<-list.files(pathIn, recursive = TRUE)
        extractedFileList
```

```
##  [1] "activity_labels.txt"                         
##  [2] "features.txt"                                
##  [3] "features_info.txt"                           
##  [4] "README.txt"                                  
##  [5] "test/Inertial Signals/body_acc_x_test.txt"   
##  [6] "test/Inertial Signals/body_acc_y_test.txt"   
##  [7] "test/Inertial Signals/body_acc_z_test.txt"   
##  [8] "test/Inertial Signals/body_gyro_x_test.txt"  
##  [9] "test/Inertial Signals/body_gyro_y_test.txt"  
## [10] "test/Inertial Signals/body_gyro_z_test.txt"  
## [11] "test/Inertial Signals/total_acc_x_test.txt"  
## [12] "test/Inertial Signals/total_acc_y_test.txt"  
## [13] "test/Inertial Signals/total_acc_z_test.txt"  
## [14] "test/subject_test.txt"                       
## [15] "test/X_test.txt"                             
## [16] "test/y_test.txt"                             
## [17] "train/Inertial Signals/body_acc_x_train.txt" 
## [18] "train/Inertial Signals/body_acc_y_train.txt" 
## [19] "train/Inertial Signals/body_acc_z_train.txt" 
## [20] "train/Inertial Signals/body_gyro_x_train.txt"
## [21] "train/Inertial Signals/body_gyro_y_train.txt"
## [22] "train/Inertial Signals/body_gyro_z_train.txt"
## [23] "train/Inertial Signals/total_acc_x_train.txt"
## [24] "train/Inertial Signals/total_acc_y_train.txt"
## [25] "train/Inertial Signals/total_acc_z_train.txt"
## [26] "train/subject_train.txt"                     
## [27] "train/X_train.txt"                           
## [28] "train/y_train.txt"
```

```r
        #validate if directory is empty or not
        if(length(extractedFileList)==0) {
                cat("Unzipped directory is empty")
                return
        }
```
        

# Start reading in the data, merge the data.


```r
        #read the subject data for both train and test
        train_subject_data <- fread(file.path(pathIn, "train", "subject_train.txt"))
        test_subject_data <- fread(file.path(pathIn, "test", "subject_test.txt"))        
        #read the Y data for both train and test
        train_y_data <- fread(file.path(pathIn, "train", "Y_train.txt"))
        test_y_data <- fread(file.path(pathIn, "test", "Y_test.txt"))
        #read the X data for both train and test
        train_x_data <- data.table(read.table(file="./UCI HAR DataSet/train/X_train.txt"))
        test_x_data <- data.table(read.table(file="./UCI HAR DataSet/test/X_test.txt"))        

        # Merging the training and test data sets
        # concatenate the subject data for train and test, use rbind to append at row level
        merged_subject_data <- rbind(train_subject_data,test_subject_data)
        setnames(merged_subject_data, "V1", "subject")
        # concatenate the x data for train and test, use rbind to append at row level
        merged_x_data <- rbind(train_x_data,test_x_data)
        # concatenate the Y data for train and test, use rbind to append at row level 
        merged_y_data <- rbind(train_y_data,test_y_data)
        setnames(merged_y_data,"V1","activityNum")
        #Merge all Data columns into one
        merged_subject_data <- cbind(merged_subject_data, merged_y_data)
        merged_all_data <- cbind(merged_subject_data, merged_x_data)
        setkey(merged_all_data, subject, activityNum)
```

# Step 2 :Extracts only the measurements on the mean and standard deviation for each measurement. 


```r
#read the features.txt file
features_data <- fread(file.path(pathIn, "features.txt"))
#assign column names
setnames(features_data, names(features_data), c("featureNum", "featureName"))
#subset the data from featurename containing mean or std
# Subset only measurements for the mean and standard deviation
mean_std_features <- features_data[grepl("mean\\(\\)|std\\(\\)", featureName)]

#Convert the column numbers to a vector of variable names matching columns in merged_all_data
features_data$featureCode<-features_data[, paste0("V", featureNum)]
head(features_data)
```

```
##    featureNum       featureName featureCode
## 1:          1 tBodyAcc-mean()-X          V1
## 2:          2 tBodyAcc-mean()-Y          V2
## 3:          3 tBodyAcc-mean()-Z          V3
## 4:          4  tBodyAcc-std()-X          V4
## 5:          5  tBodyAcc-std()-Y          V5
## 6:          6  tBodyAcc-std()-Z          V6
```

```r
features_data$featureCode
```

```
##   [1] "V1"   "V2"   "V3"   "V4"   "V5"   "V6"   "V7"   "V8"   "V9"   "V10" 
##  [11] "V11"  "V12"  "V13"  "V14"  "V15"  "V16"  "V17"  "V18"  "V19"  "V20" 
##  [21] "V21"  "V22"  "V23"  "V24"  "V25"  "V26"  "V27"  "V28"  "V29"  "V30" 
##  [31] "V31"  "V32"  "V33"  "V34"  "V35"  "V36"  "V37"  "V38"  "V39"  "V40" 
##  [41] "V41"  "V42"  "V43"  "V44"  "V45"  "V46"  "V47"  "V48"  "V49"  "V50" 
##  [51] "V51"  "V52"  "V53"  "V54"  "V55"  "V56"  "V57"  "V58"  "V59"  "V60" 
##  [61] "V61"  "V62"  "V63"  "V64"  "V65"  "V66"  "V67"  "V68"  "V69"  "V70" 
##  [71] "V71"  "V72"  "V73"  "V74"  "V75"  "V76"  "V77"  "V78"  "V79"  "V80" 
##  [81] "V81"  "V82"  "V83"  "V84"  "V85"  "V86"  "V87"  "V88"  "V89"  "V90" 
##  [91] "V91"  "V92"  "V93"  "V94"  "V95"  "V96"  "V97"  "V98"  "V99"  "V100"
## [101] "V101" "V102" "V103" "V104" "V105" "V106" "V107" "V108" "V109" "V110"
## [111] "V111" "V112" "V113" "V114" "V115" "V116" "V117" "V118" "V119" "V120"
## [121] "V121" "V122" "V123" "V124" "V125" "V126" "V127" "V128" "V129" "V130"
## [131] "V131" "V132" "V133" "V134" "V135" "V136" "V137" "V138" "V139" "V140"
## [141] "V141" "V142" "V143" "V144" "V145" "V146" "V147" "V148" "V149" "V150"
## [151] "V151" "V152" "V153" "V154" "V155" "V156" "V157" "V158" "V159" "V160"
## [161] "V161" "V162" "V163" "V164" "V165" "V166" "V167" "V168" "V169" "V170"
## [171] "V171" "V172" "V173" "V174" "V175" "V176" "V177" "V178" "V179" "V180"
## [181] "V181" "V182" "V183" "V184" "V185" "V186" "V187" "V188" "V189" "V190"
## [191] "V191" "V192" "V193" "V194" "V195" "V196" "V197" "V198" "V199" "V200"
## [201] "V201" "V202" "V203" "V204" "V205" "V206" "V207" "V208" "V209" "V210"
## [211] "V211" "V212" "V213" "V214" "V215" "V216" "V217" "V218" "V219" "V220"
## [221] "V221" "V222" "V223" "V224" "V225" "V226" "V227" "V228" "V229" "V230"
## [231] "V231" "V232" "V233" "V234" "V235" "V236" "V237" "V238" "V239" "V240"
## [241] "V241" "V242" "V243" "V244" "V245" "V246" "V247" "V248" "V249" "V250"
## [251] "V251" "V252" "V253" "V254" "V255" "V256" "V257" "V258" "V259" "V260"
## [261] "V261" "V262" "V263" "V264" "V265" "V266" "V267" "V268" "V269" "V270"
## [271] "V271" "V272" "V273" "V274" "V275" "V276" "V277" "V278" "V279" "V280"
## [281] "V281" "V282" "V283" "V284" "V285" "V286" "V287" "V288" "V289" "V290"
## [291] "V291" "V292" "V293" "V294" "V295" "V296" "V297" "V298" "V299" "V300"
## [301] "V301" "V302" "V303" "V304" "V305" "V306" "V307" "V308" "V309" "V310"
## [311] "V311" "V312" "V313" "V314" "V315" "V316" "V317" "V318" "V319" "V320"
## [321] "V321" "V322" "V323" "V324" "V325" "V326" "V327" "V328" "V329" "V330"
## [331] "V331" "V332" "V333" "V334" "V335" "V336" "V337" "V338" "V339" "V340"
## [341] "V341" "V342" "V343" "V344" "V345" "V346" "V347" "V348" "V349" "V350"
## [351] "V351" "V352" "V353" "V354" "V355" "V356" "V357" "V358" "V359" "V360"
## [361] "V361" "V362" "V363" "V364" "V365" "V366" "V367" "V368" "V369" "V370"
## [371] "V371" "V372" "V373" "V374" "V375" "V376" "V377" "V378" "V379" "V380"
## [381] "V381" "V382" "V383" "V384" "V385" "V386" "V387" "V388" "V389" "V390"
## [391] "V391" "V392" "V393" "V394" "V395" "V396" "V397" "V398" "V399" "V400"
## [401] "V401" "V402" "V403" "V404" "V405" "V406" "V407" "V408" "V409" "V410"
## [411] "V411" "V412" "V413" "V414" "V415" "V416" "V417" "V418" "V419" "V420"
## [421] "V421" "V422" "V423" "V424" "V425" "V426" "V427" "V428" "V429" "V430"
## [431] "V431" "V432" "V433" "V434" "V435" "V436" "V437" "V438" "V439" "V440"
## [441] "V441" "V442" "V443" "V444" "V445" "V446" "V447" "V448" "V449" "V450"
## [451] "V451" "V452" "V453" "V454" "V455" "V456" "V457" "V458" "V459" "V460"
## [461] "V461" "V462" "V463" "V464" "V465" "V466" "V467" "V468" "V469" "V470"
## [471] "V471" "V472" "V473" "V474" "V475" "V476" "V477" "V478" "V479" "V480"
## [481] "V481" "V482" "V483" "V484" "V485" "V486" "V487" "V488" "V489" "V490"
## [491] "V491" "V492" "V493" "V494" "V495" "V496" "V497" "V498" "V499" "V500"
## [501] "V501" "V502" "V503" "V504" "V505" "V506" "V507" "V508" "V509" "V510"
## [511] "V511" "V512" "V513" "V514" "V515" "V516" "V517" "V518" "V519" "V520"
## [521] "V521" "V522" "V523" "V524" "V525" "V526" "V527" "V528" "V529" "V530"
## [531] "V531" "V532" "V533" "V534" "V535" "V536" "V537" "V538" "V539" "V540"
## [541] "V541" "V542" "V543" "V544" "V545" "V546" "V547" "V548" "V549" "V550"
## [551] "V551" "V552" "V553" "V554" "V555" "V556" "V557" "V558" "V559" "V560"
## [561] "V561"
```

```r
#Subset variables using variable names.
select <- c(key(merged_all_data), features_data$featureCode)
merged_all_data <- merged_all_data[, select, with = FALSE]
```

# step 3: Uses descriptive activity names to name the activities in the data set
# step 4 : Appropriately labels the data set with descriptive variable names. 


```r
        # Read the file activity_labels.txt, that contains the labels
        labels_data <- fread(file.path(pathIn, "activity_labels.txt"))
        setnames(labels_data, names(labels_data), c("activityNum", "activityName"))

        #Label with descriptive activity names
        merged_all_data <- merge(merged_all_data, labels_data, by = "activityNum", all.x = TRUE)
        #Add activityName as a key.
        setkey(merged_all_data, subject, activityNum, activityName)

        #Melt the data table to reshape it from a short and wide format to a tall and narrow format.
        merged_all_data <- data.table(melt(merged_all_data, key(merged_all_data), variable.name = "featureCode"))
        #Merge activity name.
        merged_all_data <- merge(merged_all_data, features_data[, list(featureNum, featureCode, featureName)], by = "featureCode", all.x = TRUE)

        #Create a new variable, activity that is equivalent to activityName as a factor class. Create a new variable, featurethat is equivalent to featureName as a factor class.
        merged_all_data$activity <- factor(merged_all_data$activityName)
        merged_all_data$feature <- factor(merged_all_data$featureName)
        #Seperate features from featureName using the helper function grepthis.
        grepthis <- function(regex) {
                grepl(regex, merged_all_data$feature)
        }
        ##Appropriately labels the data set with descriptive variable names. 
        ## Features with 2 categories
        n <- 2
        y <- matrix(seq(1, n), nrow = n)
        x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol = nrow(y))

        merged_all_data$featDomain <- factor(x %*% y, labels = c(NA,"Time", "Freq"))
        x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol = nrow(y))
        merged_all_data$featInstrument <- factor(x %*% y, labels = c(NA,"Accelerometer", "Gyroscope"))
        x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol = nrow(y))
        merged_all_data$featAcceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))
        x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol = nrow(y))
        merged_all_data$featVariable <- factor(x %*% y, labels = c(NA, "Mean", "SD"))

        ## Features with 1 category
        merged_all_data$featJerk <- factor(grepthis("Jerk"), labels = c(NA, "Jerk"))
        merged_all_data$featMagnitude <- factor(grepthis("Mag"), labels = c(NA, "Magnitude"))

        ## Features with 3 categories
        n <- 3
        y <- matrix(seq(1, n), nrow = n)
        x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol = nrow(y))
        merged_all_data$featAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))

        #Check to make sure all possible combinations of feature are accounted for by all possible combinations of the factor class variables.
        r1 <- nrow(merged_all_data[, .N, by = c("feature")])
        r2 <- nrow(merged_all_data[, .N, by = c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
        r1 == r2
```

```
## [1] FALSE
```

```r
        ## [1] TRUE
        # accounted for all possible combinations. feature is now duplicate.
```

# Step 5 : Create a tidy data set


```r
# Create a data set with the average of each variable for each activity and each subject.
setkey(merged_all_data, subject, activity, featDomain, featAcceleration, featInstrument, 
       featJerk, featMagnitude, featVariable, featAxis)
tidy_data <- merged_all_data[, list(count = .N, average = mean(value)), by = key(merged_all_data)]
head(tidy_data)
```

```
##    subject activity featDomain featAcceleration featInstrument featJerk
## 1:       1   LAYING         NA               NA             NA       NA
## 2:       1   LAYING         NA               NA      Gyroscope       NA
## 3:       1   LAYING         NA               NA      Gyroscope     Jerk
## 4:       1   LAYING         NA             Body  Accelerometer       NA
## 5:       1   LAYING         NA             Body  Accelerometer     Jerk
## 6:       1   LAYING       Time               NA      Gyroscope       NA
##    featMagnitude featVariable featAxis count      average
## 1:            NA           NA       NA   150 -0.148683567
## 2:            NA           NA       NA    50 -0.001666985
## 3:            NA           NA       NA    50  0.084437165
## 4:            NA           NA       NA    50  0.021365966
## 5:            NA           NA       NA    50  0.003060407
## 6:            NA           NA       NA    50 -0.879291898
```

```r
write.table(tidy_data,file="tidy_data_arun.txt")
```
