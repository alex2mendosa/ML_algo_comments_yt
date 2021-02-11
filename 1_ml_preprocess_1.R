# we would require 3 libraries to 
#  regorously exolore potential and options of data processing in R
library(dplyr)
library(tibble)
library(tidyr)
library(readr)

# 1 Dataset description

db1<-as.data.frame(mtcars,rownames = NA)
db1<-rownames_to_column(db1,var="Model")
db1["Manufacturer"]<-sapply(strsplit(db1$Model," "),function(x) x[[1]]) 
db1<-select(db1,Manufacturer,everything(), 1:5)
# Here I modify mtcars data set by adding 2 columns

# Analysis always start with aknowledgment of 
# data object, how many columns, row it contains, datatype of columns,
# presence of missing values ets.
# To describe dataset, we requre fucntions which can
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

# str() will be really useful when we are 
# unaware about the content of data object.
# Function allows quick preview of the 
# contents and structure. Preview will 
# help in revealing issues with column names,
# type of data, presence of missing values, if any exist.


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

# and finally ,rarely used , but quite intresting
#, edit() function
# to start text like editor of data frame
edit(db1) # Open data editor
View(db1)
# db1 is not changed, however, we can store 
# result of edditint into another variable

db2<-edit(db1)
View(db1)
# fucntion can be usefull to edit small structures but
# for large datasets with hundreds of records, 
# it is better to avoid manual manipulations


# addtional  tool is to use summary function 
summary(db1)
# A very useful multipurpose function in R, summary(X), 
# displays statistical properties of
# ech column, for numeric data we get measure of spred
#and central tendency, for categorical data 
# it is length, in other owrds number of records.
# word mode is not the most commom character, it 
# indicate that data is stored character type. 
# cosider its similar to type of data.

#fucntion summary can be applied to specific columns
summary(db1[, c("Manufacturer", "hp")])


# or,  we might be interested in names
#assigned to columns
# Knowledge of column names hepls to correcly assign values
# to varialbes, for example , ican forhet
# is column nemas written in upper or lower case
l_100_km<-with(db1, 282.5/MPG)
names(db1)
l_100_km<-with(db1, 282.5/mpg)
#mpg is not hard to remeber, but for conviluted column names
# it always good to check how exactly they are writen.


# here is a common way to check indices of column which 
# are of our interest 
which( sapply(db1,typeof)=="character" )
# we use sapply to call typeof function on 
#each column of db1, next we use which to
#convert values of TRUE in indices. 

# now can extart the respective coluns
db1[ ,which( sapply(db1,typeof)=="character" ) ]

# to sum up, remeber and practice to use
# str() ,  tail, head
# View()
# summary() functions to explore data before actual 
# analysis. You need to know type of data to 
# avoid errors related to situation when a function 
# is applied to an object of an incorrect type.



# 2 Importing dataset

# Firt we reiterate notion of working directory:
#! The working directory is  file 
#! path  that sets the default location 
#! of any files you read into or save out of R.
# in other words, if file is located in 
# workign category we can read data from it 
# only using its name as argument for read function

# lets check our workign directiry
getwd()
setwd()

# another funtion is extremely isefull
# to check wwhat files are located in directory
list.files()
# quite usefull to check correct name of file, extension or 
# to import files via loop



# lets assume that out data is located in CSV file
# common way is to use read.csv fucntion 
# File I would upload occupied more than million rows
#lets check how fast it would be uploaded
t1<-Sys.time()
sample_1<-read.csv("1_in_sample.csv")
Sys.time()-t1
glimpse(sample_1)
# our file had column names and by default read_csv
# asumes that column naes exist

# common paramenters to define
# are header and stringsAsFactors
sample_1<-read.csv("1_in_sample.csv",header = FALSE)
glimpse(sample_1)
# R automacally assigned unique column names

sample_1<-read.csv("1_in_sample.csv",header = TRUE,stringsAsFactors = TRUE)
glimpse(sample_1)
# factors are used to addign behavoit of numbers to characters
# we will go back to factors later, but remember 
# if data in initial file is organised in 
# spreadshit form, where each column form a separate variable,
# read.csv should perform very well.

# one more work apect to remembet
sample_1<-read.csv("1_in_sample.csv",header = TRUE)
glimpse(sample_1)
#Usually R is pretty god in distinguishing numbers from 
# characters but for large files, it might conver all
#records to characters
sample_1<-read.csv("1_in_sample2.csv",header = TRUE)
glimpse(sample_1)

# Unless colClasses is specified,
# all columns are read as character columns and then converted using
# type.convert to logical, integer, numeric, complex
# if all values are characters, read,csv fails to detect
# approproate type

# clearly some data are numbers but are imported as characters,
#how to solve it:
names(sample_1)
sample_1<-read.csv("1_in_sample2.csv",header = TRUE,
     colClasses=c('numeric','numeric','numeric','character','numeric',
                  "character","character","character",
                  'numeric','numeric','numeric') )
glimpse(sample_1)
# now is much better


# There is better way both interms of speed and correct
#data type coertion
# lets check fucntion read_csv from readr
# which demontrates faster perfomace fompate to read.csv
t1<-Sys.time()
sample_1<-readr::read_csv("1_in_sample.csv")
Sys.time()-t1
# definately it workds faster for our large file
# arguemnts to remember again are col_names insted of header
# skip_empty_rows which by default equls to TRUE 
# and it skips rows with no values.
# also, by defauls , read_csv convers data to 
# tibble which overall is more preferablle
# than base data frame. 

sample_1<-readr::read_csv("1_in_sample.csv", col_names = FALSE)
 # as you can see , now actual column names form first record,
# and therefore, al lcolumn are coerced to character type


# CSV is a good mean to transfer data, but 
# how about excel file
# How to read form xlsx or xls file:
# we would require 
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

