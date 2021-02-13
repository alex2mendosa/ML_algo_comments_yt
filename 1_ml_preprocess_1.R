library(tibble)
library(dplyr)
setwd("C:/Users/UACecetoAl/Desktop/R_Input/2_ML_Course")

# Create sample data objects
n<-10^6
sku_list<-c("Product_A","Product_B","Product_C","Product_D","Product_E")
sample_1<-tibble(
  Date_Full=sample( seq(as.Date("2020/1/1"), by = "month", length.out = 12),n,replace=TRUE),
  Yr_Or=format(Date_Full,format="%Y"),
  Mon_Name=format(Date_Full,format="%b"),
  Week=format(Date_Full,format="%U"),
  Sales=sample(500:1000,n,replace=TRUE),
  Investment=sample(100:300,n,replace=TRUE), 
  SKU=sample(sku_list,n,replace=TRUE) ,
  SKU_Type=sample( c("Promo_Bundle","Standard","Discounted"),n,replace=TRUE ),
  Business=sample( c("Meat_Poultry","Beverage","Dairy","Spices","Waters"),n,replace=TRUE   ),
  Freshness=sample( c("1+","2+","3+"),n,replace=TRUE )  ,
  Country=sample( c("Russia","Romania","Rwanda"),n,replace=TRUE ),
  CEO=sample(c("Mikael Greaves","Nadeem Long","Shyla Short","Preston Wilkins",
               "Conal Carter","Elliott Irwin","Amman Henson"),n,replace = TRUE),
  Tax_rate=sample(seq(0.05,0.3,0.01),n,replace=TRUE),
  Customer=sample(c("KADI","AAckRi","NamToc","Daram","Garme"),n,replace=TRUE))

write.csv(sample_1,"1_sample.csv",row.names = FALSE)
write.table(sample_1,"1_sample.txt",row.names = FALSE,sep=",")
write.table(sample_1,"2_sample.txt",row.names = FALSE,sep="\t")
write.table(sample_1,"3_sample.txt",row.names = FALSE,sep="/")




#   we would require 5 libraries to 
#   exolore potential and options of data preprocessing in R
library(dplyr) 
library(tibble)
library(tidyr)
library(readr)
library(readxl)

# As soon as you installed respecitve libraries , you can 
# call fucntion from  library in 2 ways
# 1-st  by indicating both library name and fucntion via double colon
dplyr::select()

#or just fucntion name
select()

# we will use first approach, so you can judje
# by yourself  how important library is to 
# data processing challenges

# notice that the same fucntion can be a part of different libraries
# like select

select # f1 , it is part of MASS and dplyr libraries, therefore,
# to avoid mistakes , in some cases it is necessary to specify what
# library functuin belongs to 

### 1 Dataset description

# lets create dataset and experiment over it.
# dataset would be based on mtcars dataset

db1<-as.data.frame(datasets::mtcars,rownames = NA)
db1<-tibble::rownames_to_column(db1,var="Model")
db1["Manufacturer"]<-sapply(strsplit(db1$Model," "),function(x) x[[1]]) 
db1<-dplyr::select(db1,Manufacturer, 1:5)
# Here I modify mtcars data set by adding 2 columns
# and leaving 6 columns in total
# mtcars by itself is avaibalbe in r base library: datasets

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
# next we can check sample of data each column contains,
# usually the respective function is not accompanied
# with additional arguments

# str() will be really useful when we are 
# unaware about the content of data object.
# Function allows quick preview of the 
# contents and structure. 

# We can also use View() function to fully, explicityly check 
# all records in data structure, similar to spreadsheet-style
View(db1)
# Data is converiently displayed in separate window
# window name inherits name of argument

# how about size of object, here we can go with
dim(db1) 
#we get number of rows and columns
# Dimention are requred usually when 
# designing a loop around data frame or debugging in 
# case you want to row or col bind multiple objects

# to check size of object, we can use 
object.size()
object.size(db1)  # for me it ies more convernietn to work with
                  # Kb, lets check documentation of fucntion
object.size  #f1

# We see paramaeter units which accepts argument "Kb",
# but it is not part of object.size functiuon , it
# should be parameter of pring fucntion
print( object.size(db1),units="Kb" ) 

# greate job, remember that work with fucntion documentation
# is very importat for optimal parameters tuning and general
# understanding how to define arguments for parameters


# another  2 sommon methods are used to check
# last or first records of
# data structure are 
tail(db1,10) # here we check last 10 obs
head(db1,10)  # here we check first 10 obs
# index of rows can give an idea how many rows data
# structure contains


#We can combine head and tail with View fucntion
View( tail(db1,10)  )
View( head(db1,10)  )
# View usually is used withou additional
#arguments, however we can supply title 
#parameter to make window name more informative
View( tail(db1,10), title="Last_10_Observations"  )

# another approach requres use of glimse fucntion from
# tibble library
tibble::glimpse(db1)

# glimse easily allows to check
# dimentions, sample records and data type.
# in contrast to str(), glimpse displays 
# as many records as possible to fit the screen
# just expans console and run 2 commands

# this is the main 
# difference between str and glimpse


# another approach to 
# visually inspect data onbect 
# requires convergence of data to tibble
# with as_tibble fcuntion 
tibble::as_tibble(db1)
# here under column name we have description of data type,
# and first 10 records are displayed.
# as_tibble is mainly used to convert object like data frame
# to  tibble class and this is how
# you should use it,
# so its better yo use str or View function()



# and finally ,rarely used , but quite intresting
#, edit() function used
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
# each column, for numeric data we get measure of spred
# and central tendency, for categorical data 
# it is length, in other owrds number of records.
# word mode is not the most commom character, it 
# indicates that data is stored as character type. 
# cosider it being similar to type of data.

#fucntion summary can be applied to specific columns
summary(db1[, c("Manufacturer", "hp")])


# Next ,  we might be interested only in names
# assigned to columns
# Knowledge of column names hepls to correcly assign values
# to varialbes, for example , i can forget
# if column name written in upper or lower case
l_100_km<-with(db1, 282.5/MPG)
names(db1)
l_100_km<-with(db1, 282.5/MPG)
# mpg is not hard to remeber, but for conviluted column names
# it always good to check how exactly they are written


# here is a common way to check indices of column which 
# are of our interest 
which( sapply(db1,typeof)=="character" )
# we use sapply to call typeof function on 
# each column of db1 and check if it is character,
# next we use which to
# convert values of TRUE in indices. 

# now we can extart the respective column using 
# expression above
db1[ ,which( sapply(db1,typeof)=="character" ) ]


# if we care about being inline most contemporary and 
# most efficient 
# coding practices , we can use select fucntion 
# fron dplyr
dplyr::select(db1, where( is.character )  )
# where is funtion inside select which is used to 
# specify condition to select column

# Greate Job

# Characters or categorical data
# requres specific treatment in machine learning
# therefore, it is good to know expression which help to
# explore specifically columns with categorical data


# to sum up, remeber and practice to use
# str() ,  tail(), head()
# View() and
# summary(). The respective functions 
#  assist in exploring data before actual 
# analysis. You need to know type of data to 
# avoid errors related to situation when a function 
# is applied to an object of type not suited for respective
# function, also, we need to know dimention 
# and name of columns to
# correctly define data transformation workflow



####            2 Importing dataset

# Firt we reiterate notion of working directory:
#! The working directory is  file 
#! path  that sets the default location 
#! of any files you read into or save out of R.
# Main point is: if file is located in 
# workign directiry, we can read data from it 
# only using its name as argument or read like fucntions
# No need to specify full path

# lets check our workign directiry
getwd()
setwd("C:/Users/UACecetoAl/Desktop/R_Input/2_ML_Course")
getwd()
#This directory of folder contains datasets we would use
# in the following sections.
# another funcion  is extremely isefull
# to check what files are located in directory
list.files()

# fucntion serves well
# to check correct name of file, extension or 
# to import files via loop, or just copy file name

# if we want to check files not in 
#working directory , we need to specify absolute path
list.files("C:/Users/UACecetoAl/Desktop/R_Input/2_ML_Course")


# lets assume that out data is stored as CSV file
# in our working directory
# common way is to use read.csv function.
# File I would upload occupies  almost 
# million rows ,weights apr 100mb, 
# I am russin speaker and I refer as 
# file weight not file size
# lets check how fast it would be uploaded
list.files()
t1<-Sys.time()
sample_1<-read.csv("1_sample.csv") # make error
Sys.time()-t1
tibble::glimpse(sample_1)
print( object.size(sample_1),units="Mb"  )
# data in our file has column names and by default read.csv
# assumes that column names are first row
# of uploaded file, also R autimatically detects
# type of data in each column, 
#by the wau , all data is randomly generated


# here is example where we specify absolute file path
sample_1<-read.csv("C:/Users/UACecetoAl/Desktop/R_Input/2_ML_Course/1_sample.csv")
tibble::glimpse(sample_1)
# Output is the same because file is located in 
# working directory 


# common paramenters to remember when working with 
# read.csv are
# header and stringsAsFactors
sample_1<-read.csv("1_sample.csv",header = FALSE)
# as I said before, if header parameter is not specified
# r assumes that heades is True
tibble::glimpse(sample_1)
# R automacally assigned unique column names
# notice that now all column are of character type

# how about stringsAsFactors argument
sample_1<-read.csv("1_sample.csv",header = TRUE,stringsAsFactors = TRUE)
tibble::glimpse(sample_1)
# Horoso, columns which previously were converted to characters
# , now represent factors
# factors are used to add behaviour of numbers to categorical data,
# we will go back to factors later, but remember 
# if data in initial file is organised in 
# spreadshit form, where each column forms a separate variable,
# and each column has name.
# read.csv should perform very well.

# one more work apect to remember
sample_1<-read.csv("1_sample.csv",header = TRUE)
tibble::glimpse(sample_1)
# R is pretty god in distinguishing numbers from 
# characters 

# read.csv() imports
# all columns as character and then converts it to
# logical, integer, numeric, complex, characters or factors
# making the best guess which type of data 
# is suited to values in column


# There is better way both in terms of speed and correct
# data type coertion
# lets check fucntion read_csv from readr
# which demontrates faster perfomace compare  to read.csv
t1<-Sys.time()
sample_1<-readr::read_csv("1_sample.csv")
Sys.time()-t1
print( object.size(sample_1),units="Mb"  )
# first notice that read_csv shows what type of data 
# was chosen for each column 

# definately it workds faster for our large file.
# arguemnts to remember again are col_names insted of header
# skip_empty_rows which by default equls to TRUE 
# and it skips rows with no values.
# also, by defauls , read_csv convers data to 
# tibble which overall is more preferablle
# than base data frame. 


#first of all col_names
sample_1<-readr::read_csv("1_sample.csv", col_names = FALSE)
tibble::glimpse(sample_1)
# as you can see , now actual column names form first record,
# and therefore, all columns are coerced to character type
# Overall, if 
# possible use read_csv() as faster option, assign and
# meaningfull column names.

# CSV is a good mean to transfer data, but 
# how about excel file
# How to read from xlsx or xls file:
# we would require read_excel() fucntion
readxl::read_excel()
# common arguments are "sheet" where we can specify
# both name of order of sheet to read data form

sample_1<-readxl::read_excel("1_sample.xlsx", col_names = TRUE)
str(sample_1)
# we get empty tibble, because by defaul read_excel imports data 
# from first sheet of excel, but the data we want in stored in 
# second list


# if you dont remember sheet names, we can use
# quite usefull fucntion from readxl
# library
readxl::excel_sheets() 
readxl::excel_sheets("1_sample.xlsx") 
# it works even for sheets which are hided
# therefore, if we forget excel file name and 
# what sheets it contains , we can cobine

list.files()
readxl::excel_sheets("1_sample.xlsx") 

# end now we can read data from 
sample_1<-readxl::read_excel("1_sample.xlsx", sheet=2,col_names = TRUE)
str(sample_1)
print(object.size(sample_1)  ,unit="Mb")

# or we can define sheet name
t1<-Sys.time()
sample_1<-readxl::read_excel("1_sample.xlsx", sheet="1_sample",col_names = TRUE)
Sys.time()-t1 

# if you playing music while coding it can take even more time
# we can speed up it a little bit by
# defining data we know in advance , rather that asking r to guess it
# according to internal rules

t1<-Sys.time()
sample_1<-readxl::read_excel("1_sample.xlsx", sheet="1_sample",col_names = TRUE,
                            n_max=10^6+1,guess_max = 10,
                            trim_ws = FALSE)
Sys.time()-t1
# Difference are miniscule , but for multiple files , this
# differece can sum up into signifficant number


# Greate job, few more fucntion sot check
# lest check how to deal with data sored as text
# here our weapon of choince is read.table fcuntion

# how to read data where column values are separated with comma,
#lets have a look on original file
# well , we adjust sep argument
read.table("1_sample.txt",sep=",",header=TRUE)

# how to read data where column values are separated with tab,
# well , we adjust sep argument , specifying tab via dasjh t
read.table("2_sample.txt",sep="\t",header=TRUE)

# and finally, here is how to upload data separated by dash
read.table("3_sample.txt",sep="/",header=TRUE)



# Horoso , to sup up, 3 fucnton are greate to remember,
# to correctly define location and inpout data
# getwd()
# setwd()
# list.file() and excel_sheets
#
#to upload data we can use
# read_csv() or read.csv()
# read.excel for excel files and
# finally read.table fot text data
# Make sure that column  represent meaninfull 
# names and set header equal to true.






            # 3 Taking care of Missing Data
# Missind data are records or elements which have 
# indices or location ib data object but bring no
# information beneficial information about the data object
# or interaction between varriables

#lets create sample vector with  NA Values
vector_1<-c(4.5,4.6,4.7,NA,4.8,4.9,NA,5,9.7,9.8,9.9,NA,10,9.1)
#again fill free to replicate any piece of code


# we need to replace NA values in 1 dimetional object
# 2 # approaches can be used
# first approach is to identify indices[location]
# of values which are equal NA
# In this case, must know fucntion 
# whould be is.na() and which()

is.na(vector_1) # true indicates that values are NA
loc<-which( is.na(vector_1) ) # we identify indices of na values
vector_1[ loc ]<-mean(vector_1,na.rm = TRUE) 
# here we replcae values with indices in loc with 
# mean value of vector, excluding NA values

# we can also use median for data
vector_1[loc]<-NA
vector_1[ loc ]<-median(vector_1,na.rm = TRUE) 

# we can difene fucntion which allows to select 
#median or mean as option to repace na

f_na_rep<-function(x,option) {
  loc<-which( is.na(x) )
    if (option==1) {
      x[ loc ]<-mean(x,na.rm = TRUE) 
    } else { x[ loc ]<-median(x,na.rm = TRUE)  }
  return(x)  }

# here we use option parameter to choose between
# mean or median
# for mean we have
vector_1[loc]<-NA
f_na_rep(vector_1,option=1) 
# for median we can use 2
f_na_rep(vector_1,option=2)



# Second approach , besides logical and positiona
# indexing or used define fucntions,
# is that we  can use r fucntion.
#  common approachs is
na.omit(vector_1) # to prop NA values
# if you are discourage with atribures list,  we can remove it 
# by converting  vector_1 to numeric type
as.numeric(na.omit(vector_1))

# another method, is to use coalesce fucntion
dplyr::coalesce() 
# now its main purpose is different from replacing NA , but
# it can be easily applied as NA replacing tool

dplyr::coalesce(vector_1,300)
# i use 300  to highligh that we actually repaced mussing values 
# with number we definced
# we can also use expession which results in single value
dplyr::coalesce(vector_1, mean(vector_1,na.rm = TRUE) )


# What about 2 dimentional structures like data frames or tibbles
# first of all, 2 dimentions measn we need to know both
# row and column index of NA values, 
# which would be more computationally expensive compare to 
# vector , therefore, we should avoid indexing via 
# standard nested loop with i and j

for (i in 1:nrow(DF) ) {
  for (i in 1:ncol(DF) ) {
    
  }
}

# lets create sample data frame
DF <- data.frame(x = rnorm(50,10,1), 
                 y = rnorm(50,5,1),
                 z =rnorm(50,1,1))

# now lets assign 10 na values per each column

DF[ sample(1:50,10,replace=FALSE),1 ]<-NA
DF[ sample(1:50,10,replace=FALSE),2 ]<-NA
DF[ sample(1:50,10,replace=FALSE),3 ]<-NA

# lets have a look if each column contain NA
View(DF)
#Greate job


# how about not the most efficient idea to replace NA
#lets try nested loop
colMeans(DF,na.rm = TRUE)
# here are hooe mean we would like to impute instead of NA values
#lets store them in vector

col_means<-colMeans(DF,na.rm = TRUE)
# now we dont need to calsulate them each time we
# find Na values
# finally the loop
DF_2<-DF
for (i in 1:nrow(DF_2) ) {
  for (j in 1:ncol(DF_2) ) {
      if ( is.na(DF_2[i,j]) ) {
        DF_2[i,j]<-col_means[j] } 
  }
}
# Perfomance of the respective loop would depende on 
# number of rows , moreover , it might be that for 
# each column we have unique approach in replacing NA
# values, therefore, lets deop solution via loops and
# consider another options


# Now, regarding DF,  we would lile to remove all rows
# which contain at leas 1 NA
# first off all na.omit
na.omit(DF)
#Even if only 1 column contain NA, row would be omitted

# to implement positiona indexing we would requre 
# use of apply function to check presence of NA in a row
apply(DF, MARGIN=1, is.na)
# here logical vector is obtained per each column
# value of TRUE indicates NA for column row

apply(DF, MARGIN=1, function(x) any(is.na(x)) )
#Next we apply is.na(x) per each row, any()
# givex single True if row contains NA
# Therefore, we have 50 values, TRUE indcates that 
#row contain NA

# here are rows which contain at leat one na value
DF[apply(DF, MARGIN=1, function(x) any(is.na(x))) ,]
# we can add exclamation mark to negate logical values
# to check all rows without NA
DF[!(apply(DF, MARGIN=1, function(x) any(is.na(x)))) ,]

# solution to remove na are abubdant , here is the option 
# to remove given fact thant TRUE is 1 and FALSE is 0
rowSums( is.na(DF) )==0 # We sum ligical values per row, 
DF[rowSums( is.na(DF) )==0,] # and apply logical indexing

# Definatly, if you dont need to practice use of apply
#fucntion, condidet to use na.omit

# Now one more example or usefull R fucntion to detect na
complete.cases()
#Return a logical vector indicating which rows are complete
# i.e., have no missing values

complete.cases(DF) # now again we can use logical indexing
DF[complete.cases(DF),]
# or count how many rows have missing data
sum(!complete.cases(DF))
# we combine exclamation and sum fcuntion


# removing rows with missing data is challenge, but not a 
# big one, If have have 10 value in signle row and only 1 column
# contains NA and we drop the row, we also remove factual data
# from the ramainig 9 columns.  Beter solution , 
# is to replace missing data with most expected value
# lets start with solution offered by 
# R libraries

# the most common and popular is replace_na fucntion
# for vector we specify vector name, and what value to use
# to replace na values, vary silimat to use of
# coalece fucntion

tidyr::replace_na(vector_1,mean(vector_1,na.rm = TRUE))
tidyr::replace_na(vector_1, 300)

#  For tibble or dataframes, we can specify method to 
#  replace na values with value specific for each column

tidyr::replace_na(DF,list( x=mean(DF$x,na.rm = TRUE),
                    y=median(DF$y,na.rm = TRUE)
                    z=IQR(DF$z,na.rm = TRUE)) 
                   )
# values in column x are replaced with mean ,
# values in column y are replaced with median,
#values in z are replaced with IQR
# note than we specify  pairs as list: column and replacement value 

# Lest add column with characters to DF
DF["d"] <- sample(letters,50,replace = TRUE)
View(DF)
DF[ sample(1:50,10,replace=FALSE),4 ]<-NA

tidyr::replace_na(DF,list( x=mean(DF$x,na.rm = TRUE),
                    y=median(DF$y,na.rm = TRUE),
                    z=IQR(DF$z,na.rm = TRUE),
                    d=mean(DF$d,na.rm = TRUE)) )
# we encounter an error
# this is example where summary fucntion would  be helpfull
# to select appropriate method to replace NA
summary(DF)

# now we know that column d contain characters and
# we cant use neiter mean or median for imputation
# lets try to replace missing character with the most 
#frequent  letter in column

tidyr::replace_na(DF,list( x=mean(DF$x,na.rm = TRUE),
                           y=median(DF$y,na.rm = TRUE),
                           z=IQR(DF$z,na.rm = TRUE),
                           d=mode(DF$d,na.rm = TRUE)) )

# this is not what we require
# R does not have a standard in-built function
# to calculate mode.
# So we create a user function function
# to calculate mode of a data set in R.

mode_z<-names( which.max( table(DF$d) ) )
# table counts letters
# which.max find indices of most frequent value 
# and we extract name

# now lets imput values for each column
tidyr::replace_na(DF,list( x=mean(DF$x,na.rm = TRUE),
                           y=median(DF$y,na.rm = TRUE),
                           z=IQR(DF$z,na.rm = TRUE),
                           d=mode_z) )

# repalce_na is greate and I ecourage to use
# it as greate and efficient time saver,
# but lets have a look on another
# option

# lets check ifelse optio

ifelse( is.na(DF$x),mean(DF$x,na.rm = TRUE),DF$x) # or
dplyr::if_else( is.na(DF$x),mean(DF$x,na.rm = TRUE),DF$x) 
# we read it as, if any value in column x is NA, we replace it
#otherwise kep it unchanged

# now to avoid typind doller accessor eact time , we can use
# with to create envirinment

with(DF, if_else( is.na(x),mean(x,na.rm = TRUE),x)  )
#result is the same

# so far , if dealing with 2 dim object,
# i recommend to employ  
# replace_na
# Remember , ieven if for some reason library 
# is not available or you dont want to abuse
# partuclar method, you can always define your own fucntion

# here is the fucntion which replace na if
# with mean if column is of numeric type
#or with mode of column is character

DF3<-tidyr::replace_na(DF,list( x=mean(DF$x,na.rm = TRUE),
                                y=mean(DF$y,na.rm = TRUE),
                                z=mean(DF$z,na.rm = TRUE),
                                d=mode_z) )


rep_na<- function(x) {
  loc<-which(is.na(x))
  if ( is.character(x)  ) {
    mode_z<-names( which.max(table(x)) )
    x[loc]<-mode_z
    return(x)
  }  else {
    x[loc]<-mean(x,na.rm = TRUE)
    return(x)
  }  
}


as.data.frame( lapply( DF,  rep_na ) ) 


# Greate job, in R we can approach 
# challenge with NA bith via User define fucntions
# which usually involve aplication of
# is.na, which and apply family of fucntion

# R also offers great solutions with 
# ducntions like complete.cases,
# replace_na


