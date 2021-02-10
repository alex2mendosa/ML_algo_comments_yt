# we would require 3 libraries to 
# fully exolore potential and options of data processing
library(dplyr)
library(tibble)
library(tidyr)

# 1 Dataset description

db1<-as.data.frame(mtcars,rownames = NA)
db1<-rownames_to_column(db1,var="Model")
db1["Manufacturer"]<-sapply(strsplit(db1$Model," "),function(x) x[[1]]) 
db1<-select(db1,Manufacturer,everything(), -c(gear,carb,drat))
# Here I modify mtcars data set by adding 2 columns


# to describe dataset, we requre fucntions which can
# compactly display information contained in data
# object

# to explore dataset we can use
str() 
str(db1)
# here we can check name of each column,
# after "colon",  type of data is indicated,
# next we can check sample of data each column contains
# usually the respective fucntion is not accompanied
# with additional arguments

#! str() will be really useful when we are 
#! unsure about the contents of an data object.
#! Fucntion allows quick preview of the 
#! contents and structure. This will 
#! also help in revealing issues in column names
#! type of data, presence of missing values, if any exist.



#We can also use View() function to fully, explicityly check 
#all records in data structure, similar to spreadsheet-style
View(db1)
# Data is displayed in separate window
#window name inherits name of argument

#another  2 sommon methods are used to check
#last or first records of
# data structure
tail(db1,10)
head(db1,10)
# index of rows can give an idea how many rows data
#structure contains

#We can combine head and tail with View fucntion
View( tail(db1,10)  )
View( head(db1,10)  )
# View usually is used withou additional
#arguments, however we can sypply title 
#parameter to make window name more informative
View( tail(db1,10),title="Last_10_Observations"  )


# another approach to 
#visually inspect data onbect 
#requires convergence of data to tibble
as_tibble(db1)
# here under column name we have description of data type,
# and first 10 records are displayed
# as_tibble is alco used to convert object to 
# tibble class. 

# another approach requres yse of glimse fucntion from
# tibble library
glimpse(db1)

# glimse easily allows to check sample records and data type.
# in contrast to str(), glimpse displays 
#as many records as possible to fit the screen
# just expans console and run 2 commands

# and finally ,rarely used , but still , edit() function
# to start text like editor of data frame
edit(db1) # Open data editor
# db1 is not changed, however, we can store 
# result of edditint into another variable

db2<-edit(db1)
# fucntion can be usefull to edit small structures but
# for large datasets with hundreds of records, 
#it is better to avoid manual manipulations


# addtional  tool is to use summary function 
summary(db1)
# A very useful multipurpose function in R, summary(X), 
# displays statistical properties of
# ech column, for numeric data we get measure of spred
#and central tendency, for categorical data 
# it is length and mode of data.

#fucntion can be applied to specific columns
summary(db1[, c("Manufacturer", "hp")])


# or,  we might be interested in names
#assigned to columns
# Knowledge og column names hepls to correcly define input
# varialbes
names(db1)

# here is a common way to check indices of column which 
# are of our interest 
which( sapply(db1,typeof)=="character" )
# we use sapply to call typeof function on 
#each column of db1, next we use which to
#convert values of TRUE in indices. 


# to sum up, remeber and practice to use
# str() ,  tail, head
# View()
# summary() functions to explore data before actual 
# analysis. You need to know type of data to 
# avoid errors related to situation when a function 
#is applied to an object of an incorrect type.










# 2 Importing dataset

#! The working directory is just a file 
#! path on your computer that sets the default location 
#! of any files you read into R, or save out of R.
# in other words, if file is located in 
# workign category we can read data from it 
#only using its name as argument.

# lets check our workign directiry
getwd()
setwd()

# another funtion is extremely isefull
# to check wwhat files are located in directory
list.files()

# lets assume that out data is located in CSV file
# common way is to use read.csv fucntion 
t1<-Sys.time()
sample_1<-read.csv("1_in.csv")
Sys.time()-t1
glimpse(sample_1)
# common paramenters to define
# are header and stringsAsFactors

sample_1<-read.csv("1_in.csv",header = FALSE)
glimpse(sample_1)

sample_1<-read.csv("1_in.csv",header = TRUE,stringsAsFactors = TRUE)
glimpse(sample_1)
# we will go back to factors later, but remember it
# if data in initial file is organised in 
#spreadshit form, read.csv should perform well.


# lets check fucntion read_csv from readr
# which demontrates faster perfomace fompate to read.csv
t1<-Sys.time()
sample_1<-readr::read_csv("1_in.csv")
Sys.time()-t1
# definately it workds faster for our large file
# arguemnts to remember again are col_names insted of header
# skip_empty_rows which by default equls to TRUE 
# and it skips rows with no values.
# also, by defauls , read_csv convers data to 
# tibble which overall is more preferablle
#than base data frame. 



# How to read form xlsx or xls file:
readxl::read_excel()
# common arguments are sheet where we can specify
#both name of sheet or its order

# if you dont remember sheet names, we can use
# quite usefull fucntion from readxl
# library
readxl::excel_sheets() 
# it works even for sheets which are hided
# therefore, if we forget file name and 
# hwat sheet contains data , we can cobine

list.files()
readxl::excel_sheets() 

# overall, 3 fucnton are good to remember,
# getwd()
# setwd()
# list.file()
# read_csv() or read.csv()
# read.xlsx for excel files.





# 3 Taking care of Missing Data

#lets create sample vector with 4 NA Values
vector_1<-sample( c(rnorm(5,10,2)), 10, replace = TRUE)
index_na<-sample( 1:10,3,replace=FALSE )
vector_1[index_na]<-NA

# we need to replace NA values, 2
# approaches can be used
# first approach is to identify indices[location]
# of values which are equal NA
# In this case, must know fucntion 
# whould be is.na() 

is.na(vector_1) # false indicates that values is NA
loc<-which( is.na(vector_1) ) # we identify indices of na values
vector_1[ loc ]<-mean(vector_1,na.rm = TRUE) 
# here we replcae values of indices in loc with 
# mean values of vecor, which is contast for all missing values

# it can also me dediab of data
vector_1[index_na]<-NA
vector_1[ loc ]<-median(vector_1,na.rm = TRUE) 

# besides indeing we can use r base fucntions
# 2 common approaches are
na.omit(vector_1) # to prop NA values
# if you are discourage with atribures list,  we can remove it 
# by converting  vector_1 to numeric type
as.numeric(na.omit(vector_1))





# What about 2 dimentional structures like data frames or tibble
# first of all, 2 dimentions measn we need to know both
# row and column index of NA values, 
# which would be more computationally expensive compare to 
# vector , therefore, lets start with 
# methods which save typing time

DF <- data.frame(x = c(NA, 2, 3), y = c(0, 10, NA))
na.omit(DF)

# to iplement positiona indexing we would requre 
# use of apply function to check presence of NA in a row
apply(DF, MARGIN=1, is.na)
# here logical vector is obtained
apply(DF, MARGIN=1, function(x) !(any(is.na(x))) )
# here are rows which contain at leat one na value
DF[apply(DF, MARGIN=1, function(x) !(any(is.na(x))) ),]

# solution to remove na are abunded , here is the option 
# to remove fact thant TRUE is 1 and FALSE is 0
rowSums( is.na(DF) )==0
DF[rowSums( is.na(DF) )==0,]


# removing rows with missing data is challenge, but not a big one,
# beter solution , epscecially only 1 row contains missing data,
# is to  replace missing data
# lets start with solution offered by 
#R libraries

# we can also call for fucntions for tibble
replace_na(vector_1,mean(vector_1,na.rm = TRUE))

# how replace na should work with tibble or data frames:

DF <- data.frame(x = c(NA, 2, 3), y = c(15, 10, NA))
replace_na(DF,list( x=mean(DF$x,na.rm = TRUE),y=mean(DF$y,na.rm = TRUE) ) )
DF <- data.frame(x = c(NA, 2, 3,5,6), y = c(15, 10, NA,32,34),z=c("a",NA,"a","c","b"))
replace_na(DF,list( x=mean(DF$x,na.rm = TRUE),y=mean(DF$y,na.rm = TRUE),
                    z=mean(DF$z,na.rm = TRUE)) )
# this is example where summary fucntion woul dbe helpfull
replace_na(DF,list( x=mean(DF$x,na.rm = TRUE),y=mean(DF$y,na.rm = TRUE),
                    z=mode(DF$z)) )

# R does not have a standard in-built function to calculate mode.
# So we create a user function to calculate mode of a data set in R.

mode_z<-names( which.max(table(DF$z)) )

replace_na(DF,list( x=mean(DF$x,na.rm = TRUE),y=mean(DF$y,na.rm = TRUE),
                    z=mode_z) )



# 4 Splitting the dataset
# this skiils is crusil one, machine learning 
#is about letting  machine to find relations which
# we cannot observe

indices<-1:nrow(db1)
ind_train<-sample(indices,size=20)
ind_test<- indices[ !(indices %in% ind_train) ]

# we canr create random indices and reshagle data
indices<-sample(1:nrow(db1),replace = FALSE)
db1[indices,]
