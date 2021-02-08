library(dplyr)
library(tibble)
library(tidyr)

# 1 Dataset description

db1<-as.data.frame(mtcars,rownames = NA)
db1<-rownames_to_column(db1,var="Model")
db1["Manufacturer"]<-sapply(strsplit(db1$Model," "),function(x) x[[1]]) 
db1<-select(db1,Manufacturer,everything())

# to explore dataset we can use
str(db1)
# here we can check datatype of each column and example of 
# data it contains

#We can also use View() function to fully explicityly check 
#all records in data structure
View(db1)

#another  2 sommon methods are used to check last or first records of
# data structure
tail(db1,10)
head(db1,10)

#We can combine expressins woth View function 
View( tail(db1,10)  )
View( head(db1,10)  )

# another approach requires convergence of data to tibble
as_tibble(db1)
# here under column name we have description of data type

#another approach requres yse of glimse fucntion
glimpse(db1)
# here output is improved verrion of str() fucntion which 
# easily allows to check sample records and data type

# we can choose numnber of characters to displey in each row
glimpse(db1,width = 30)


# addtinel too is to use summary function 
summary(db1)
# which is prefer is case you ewhat to get idea of 
# statistics on any perticular or subset of column

summary(db1[, c("Manufacturer", "gear")])


# or we might be interested in names
#assighne to columns
names(db1)

# here is a common way to check indices of column which 
# are of our interest 

which( sapply(db1,typeof)=="character" )


# and finally ,rarely used , but still 

edit(db1) # Open data editor
db2<-edit(db1)





                       # 2 Importing dataset
list.files()

# lets assume that out data is located in CSV file
t1<-Sys.time()
sample_1<-read.csv("1_in.csv")
Sys.time()-t1
glimpse(sample_1)
# common arg are header and stringsAsFactors

sample_1<-read.csv("1_in.csv",header = FALSE)
glimpse(sample_1)

sample_1<-read.csv("1_in.csv",header = TRUE,stringsAsFactors = TRUE)
glimpse(sample_1)

# lets check fucntion read_csv from readr
# wgich demontrates faster perfomace fompate to read.csv
t1<-Sys.time()
sample_1<-readr::read_csv("1_in.csv")
Sys.time()-t1
# definately it workds faster for our large file
# arguemnts to remember again are col_names insted of header
# skip_empty_rows before reading 

glimpse(sample_1)

# How to read form xlsx file:
readxl::read_excel()

# common arguments are sheet where we can specify
#both name of sheet or its order

# if you odnt remember or sheets name use
readxl::excel_sheets() 
# it works even for sheets whci are hided

#or you can use 
readxl::read_xlsx() if you know in advance format of excel file

t1<-Sys.time()
sample_1<-readxl::read_excel("1_in.xlsx",sheet=1)
Sys.time()-t1
glimpse(sample_1)


t1<-Sys.time()
sample_1<-readxl::read_xlsx("1_in.xlsx",sheet=1)
Sys.time()-t1
glimpse(sample_1)

#Use read_xls() and read_xlsx()
#directly if you know better and want to prevent such guessing.




                     # 3 Taking care of Missing Data
vector_1<-sample( c(rnorm(5,10,2),rep(NA,3 ), 10, replace = FALSE))

# we need to replace NA values, here is teh common strategy

is.na(vector_1)
which( is.na(vector_1) )
vector_1[ which( is.na(vector_1) ) ]<-mean(vector_1,na.rm = TRUE) 

# what if we want to replace missing value with the lase non NA
#W observation

is.na(vector_1)
which( is.na(vector_1) )
vector_1[ which( is.na(vector_1) ) ]<-vector_1[ which( is.na(vector_1) )-1 ]
# the idea is the same, find missing value using in.na abnd wich fucntion
# and choose who would you like to replace it


# how abour R base fucntion s
na.omit(vector_1) # to prop NA values
na.exclude(vector_1)

#what about data frame 
DF <- data.frame(x = c(1, 2, 3), y = c(0, 10, NA))
na.omit(DF)
na.exclude(DF)
#Only benefit of na.exclude is that it retains the position where data 
#was missing in the final residual vector, which is the case
# of plotting linear regressioon

# we can also call for fucntions for tibble
replace_na(vector_1,mean(vector_1,na.rm = TRUE))

# how replace na should work with tibble or data frames:

DF <- data.frame(x = c(NA, 2, 3), y = c(15, 10, NA))
replace_na(DF,list( x=12,y=mean(vector_1,na.rm = TRUE) ) )

# what should we do with vcaterogrical variable 
DF <- data.frame(x = c(NA, 2, 3), y = c(15, 10, NA), z=c("a","b",NA))
sub_str<-"ABC"
replace_na(DF,list( x=12,y=mean(vector_1,na.rm = TRUE),z="ABC" ) )



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

# 




































