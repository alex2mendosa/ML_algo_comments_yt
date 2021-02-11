#  we would require 5 libraries to 
#   exolore potential and options of data preprocessing in R
library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(readxl)


# 1 Dataset description

# lets create dataset and experiment over it.
# dataset woul dbe based on mtcars dataset

db1<-as.data.frame(mtcars,rownames = NA)
db1<-rownames_to_column(db1,var="Model")
db1["Manufacturer"]<-sapply(strsplit(db1$Model," "),function(x) x[[1]]) 
db1<-select(db1,Manufacturer, 1:5)
# Here I modify mtcars data set by adding 2 columns
#and leaving 6 columns in total

# Analysis always start with exploration of 
# data object, how many columns, row it contains, 
# datatype of columns,
# presence of missing values ets.
# To describe dataset, we requre functions which can
# compactly display information contained in data
# object

# to explore dataset we can use
str() 
str(db1)  
# here we can check number of rows, refered as observations
# number of columns referred as variables
# name of each column,
# after "colon",  type of data is indicated,
# next we can check sample of data each column contains
# usually the respective fucntion is not accompanied
# with additional arguments

# str() will be really useful when we are 
# unaware about the content of data object.
# Function allows quick preview of the 
# contents and structure. 

#We can also use View() function to fully, explicityly check 
#all records in data structure, similar to spreadsheet-style
View(db1)
# Data is converiently displayed in separate window
# window name inherits name of argument

# how about size of object, here we can go with
dim(db1) 
#we get number of rows and columns


# another  2 sommon methods are used to check
# last or first records of
# data structure are 
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
tibble::as_tibble(db1)
# here under column name we have description of data type,
# and first 10 records are displayed
# as_tibble is mainly used to convert object to 
# tibble class and this is how
# you should use it,
# so its better yo use str or View fucntuon()


# another approach requres use of glimse fucntion from
# tibble library
tibble::glimpse(db1)

# glimse easily allows to check
# dimentions, sample records and data type.
# in contrast to str(), glimpse displays 
# as many records as possible to fit the screen
# just expans console and run 2 commands

# and finally ,rarely used , but quite intresting
#, edit() function
# to start text like editor of data frame
edit(db1) # Open data editor
View(db1)
# db1 is not changed, however, we can store 
# result of edditint into another variable

db2<-edit(db1)
View(db2)
# fucntion can be usefull to edit small structures but
# for large datasets with hundreds of records, 
# it is better to avoid manual manipulations


# Next , more invasive approach, is to use summary function 
summary(db1)
# A very useful multipurpose function in R, summary(X), 
# displays statistical properties of
# ech column, for numeric data we get measure of spred
# and central tendency, for categorical data 
# it is length, in other owrds number of records.
# word mode is not the most commom character, it 
# indicates that data is stored as character type. 
# cosider its similar to type of data.

#fucntion summary can be applied to specific columns
summary(db1[, c("Manufacturer", "hp")])


# or,  we might be interested in names
#assigned to columns
# Knowledge of column names hepls to correcly assign values
# to varialbes, for example , i can forget
# if column nemas written in upper or lower case
l_100_km<-with(db1, 282.5/MPG)
names(db1)
l_100_km<-with(db1, 282.5/mpg)
#mpg is not hard to remeber, but for conviluted column names
# it always good to check how exactly they are named.


# here is a common way to check indices of column which 
# are of our interest 
which( sapply(db1,typeof)=="character" )
# we use sapply to call typeof function on 
#each column of db1, next we use which to
#convert values of TRUE in indices. 

# now we can extart the respective column using 
#expression above
db1[ ,which( sapply(db1,typeof)=="character" ) ]

# to sum up, remeber and practice to use
# str() ,  tail, head
# View() and
# summary() functions to explore data before actual 
# analysis. You need to know type of data to 
# avoid errors related to situation when a function 
# is applied to an object of an incorrect type,
# we need to know dimention and name of columns to
# correctly define data transformation workflow



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
# fucntion serves well
# to check correct name of file, extension or 
# to import files via loop


# lets assume that out data is located in CSV file
# in our working directory
# common way is to use read.csv fucntion 
# File I would upload occupies  almost million rows 
# lets check how fast it would be uploaded
t1<-Sys.time()
sample_1<-read.csv("1_in_sample.csv")
Sys.time()-t1
glimpse(sample_1)
# data in our file has column names and by default read.csv
# asumes that column names exist

# common paramenters to remember
# are header and stringsAsFactors
sample_1<-read.csv("1_in_sample.csv",header = FALSE)
glimpse(sample_1)
# R automacally assigned unique column names
# notice that now all column are of character type

sample_1<-read.csv("1_in_sample.csv",header = TRUE,stringsAsFactors = TRUE)
glimpse(sample_1)
# factors are used to add behaviour of numbers to characters
# we will go back to factors later, but remember 
# if data in initial file is organised in 
# spreadshit form, where each column forms a separate variable,
# read.csv should perform very well.

# one more work apect to remembet
sample_1<-read.csv("1_in_sample.csv",header = TRUE)
glimpse(sample_1)
# R is pretty god in distinguishing numbers from 
# characters 

# read.csv() imports
# all columns as character and then converte it to
# logical, integer, numeric, complex, characters or factors
# making the best guess which type of data 
# is suited to values in column

# what should we do if all record are worgly classified as
# characters, to initiate this error we will
# make column names as first row value

sample_1<-read.csv("1_in_sample.csv",header = FALSE)
glimpse(sample_1)

#how to solve it:
names(sample_1)
# not the best , but still a solution,
# we can use colClasses argument to specify 
#type of each column
sample_1<-read.csv("1_in_sample.csv",header = FALSE,
                   colClasses=c('numeric','numeric','numeric','character','numeric',
                                "character","character","character",
                                'numeric','numeric','numeric') )
glimpse(sample_1)
# now is much better
# so remebner, most common argument for
# read.csv are header,stringsasfactor and
# colclasses 


# There is better way both in terms of speed and correct
# data type coertion
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
# and therefore, all lcolumn are coerced to character type
# Overall, if 
# possible use read_csv() as faster option

# CSV is a good mean to transfer data, but 
# how about excel file
# How to read form xlsx or xls file:
# we would require read_excel() fucntion
readxl::read_excel()
# common arguments are "sheet" where we can specify
# both name of sheet or its order

# if you dont remember sheet names, we can use
# quite usefull fucntion from readxl
# library
readxl::excel_sheets() 
# it works even for sheets which are hided
# therefore, if we forget excel file name and 
# what sheets it contains , we can cobine

list.files()
readxl::excel_sheets() 

# overall, 3 fucnton are good to remember,
# getwd()
# setwd()
# list.file()
# read_csv() or read.csv()
# read.xlsx for excel files.


# 3 Taking care of Missing Data

#lets create sample vector with  NA Values
vector_1<-sample( c(rnorm(5,10,2)), 10, replace = TRUE)
index_na<-sample( 1:10,3,replace=FALSE )
vector_1[index_na]<-NA

# we need to replace NA values, 2
# approaches can be used
# first approach is to identify indices[location]
# of values which are equal NA
# In this case, must know fucntion 
# whould be is.na() 

is.na(vector_1) # true indicates that values are NA
loc<-which( is.na(vector_1) ) # we identify indices of na values
vector_1[ loc ]<-mean(vector_1,na.rm = TRUE) 
# here we replcae values of indices in loc with 
# mean value of vector, excluding NA values

# we can also use dedian for data
vector_1[index_na]<-NA
vector_1[ loc ]<-median(vector_1,na.rm = TRUE) 

# besides indeing we can use r base fucntions
# 2 common approaches are
na.omit(vector_1) # to prop NA values
# if you are discourage with atribures list,  we can remove it 
# by converting  vector_1 to numeric type
as.numeric(na.omit(vector_1))

# another method, is to use 
dplyr::coalesce() fcuntion ,
# now its main purpose is different from replacing NA , but
# it can be easily applied and NA replacing tool

dplyr::coalesce(vector_1,300)
# i use 300  to highligh taht we actually repaced mussing values 
# with number we definced
# we can also use expession which resuti in single value
coalesce(vector_1,mean(vector_1,na.rm = TRUE) )


# What about 2 dimentional structures like data frames or tibble
# first of all, 2 dimentions measn we need to know both
# row and column index of NA values, 
# which would be more computationally expensive compare to 
# vector , therefore, lets start with 
# methods which save typing time

DF <- data.frame(x = c(1,1,NA,3,NA,8,13), y = c(NA,7,1,8,2,NA,1))
#our sample data freamne contains NA values, 
# now lets define wats to remove rows which contain NA
# first off all na.omit
na.omit(DF)
#Evan if only 1 column contain NA, row would be omitted

# to iplement positiona indexing we would requre 
# use of apply function to check presence of NA in a row
apply(DF, MARGIN=1, is.na)
# here logical vector is obtained
# 2 lines for each column , value of TRUE indoicates NA
# to check if na is present in aby row we add any function

apply(DF, MARGIN=1, function(x) !(any(is.na(x))) )
#with exp mark true indicates what values are not NA

# here are rows which contain at leat one na value
DF[apply(DF, MARGIN=1, function(x) !(any(is.na(x))) ),]

# solution to remove na are abunded , here is the option 
# to remove given fact thant TRUE is 1 and FALSE is 0
rowSums( is.na(DF) )==0
DF[rowSums( is.na(DF) )==0,]


# removing rows with missing data is challenge, but not a big one,
# beter solution , 
# is to replace missing data with most expected value
# lets start with solution offered by 
#R libraries

# the most common and popular is replace_na
# for vector we specify vector name, and what value to use
# to replace na values, vary silimat to use of
# coalunse fucntion
replace_na(vector_1,mean(vector_1,na.rm = TRUE))

#  For tibble or dataframes, we can specify method to 
# replace na unique for each column

DF <- data.frame(x = c(1,1,NA,3,NA,8,13), y = c(NA,7,1,8,2,NA,1))
replace_na(DF,list( x=mean(DF$x,na.rm = TRUE),y=median(DF$y,na.rm = TRUE) ) )
# values in column x are replaced with mean ,
#values in column y are replaced with median
# note than we specify a pair column and replacement value as
# paramenets in list fcuntion

DF <- data.frame(x = c(1,1,NA,3,NA,8,13), y = c(NA,7,1,8,2,NA,1),
                 e=c("a","b",NA,"d","a",NA,"a"))

replace_na(DF,list( x=mean(DF$x,na.rm = TRUE),y=mean(DF$y,na.rm = TRUE),
                    z=mean(DF$z,na.rm = TRUE)) )
# this is example where summary fucntion would  be helpfull
# to select appropriate method to replace NA
summary(DF)

# now we know that column z contain characters and
# we cant use neiter mean or median for imputaion
# lets try to replace missing character withe hte most 
#freauent character in column

replace_na(DF,list( x=mean(DF$x,na.rm = TRUE),y=mean(DF$y,na.rm = TRUE),
                    z=mode(DF$z)) )
# this is not waht we require
# R does not have a standard in-built function to calculate mode.
# So we create a user function to calculate mode of a data set in R.

mode_z<-names( which.max(table(DF$z)) )

replace_na(DF,list( x=mean(DF$x,na.rm = TRUE),y=mean(DF$y,na.rm = TRUE),
                    z=mode_z) )

# I cant agree that any approach is codint can be classofied as
# abused, but sometimes its good to leave for a while 
# routine and try another solution, repalce_na is greate but lets have a look on another
# oprion

# first of all if_else

ifelse( is.na(DF$x),mean(DF$x,na.rm = TRUE),DF$x) # or
if_else( is.na(DF$x),mean(DF$x,na.rm = TRUE),DF$x) 

# now to avoid typind doller accessor eact time , we can use
# with to create envirinment

with(DF, if_else( is.na(x),mean(x,na.rm = TRUE),x)  )

# so far , if dealing with 2 dim object, i recommend to stich to 
# replace na, other wise you will require to make 
# user define function combined with apply family function

# here the solution to the end of chapter

rep_na<- function(x) {
     if ( is.character(x)  ) {
       mode_z<-names( which.max(table(x)) )
       loc<-which(is.na(x))
       x[loc]<-mode_z
       return(x)
     }  else {
       loc<-which(is.na(x))
       x[loc]<-mean(x,na.rm = TRUE)
       return(x)
             }  
     }


sapply( DF, rep_na )




