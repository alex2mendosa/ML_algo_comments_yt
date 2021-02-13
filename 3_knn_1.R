library(dplyr)
library(tibble)
library(ggplot2)
n<-1000
sample_1<-tibble( x1=rnorm(n,10,2),
                  x2=runif(n, min = 100, max = 200) , 
                  x3=rexp(n,0.1) , 
                  x4=sample(0:1,n,replace = TRUE) , 
                  x5=rchisq(n, 5, ncp = 50) ,
                  x6=log( rnorm(n,10,2) ) )

glimpse(sample_1)


# knn is example of unsupervised , automated clustering technique
# unsupervised utomated clustering means that we dont know in advance 
# how to classify, we dont know how to label instances or rows
# and our goal to define possible meaningfull groupping based on
# intristic characteristics of data

# to initiate knn the follwong action shoul be taken
Data normalisation or preprocessing
Splitin data into trinint and testinf sets
fitting the model to training data
evaluate perfomance on test data
model tuning , specifically determining oprimal values of k

knn fully reveals itself when applied to type homogenous data, speficically,
it is suited very well to numerical features

General routone for knn is the following:
## ?
  
Now hoe to measere similary of instances or records
Traditioally euclidian distance us applied

assume 2 dimentional space with 2 poins

sample_2<-tibble( feature=c( rnorm(1,10,1),rnorm(1,5,1)  ) , 
                  instance=c( "x1","x2" ) )
here we have 2 instances x1 and x2, with 1 feature

ggplot(data=sample_2,aes(y=feature,x=instance))+
    geom_point(col="red")

Now assume , we need to decide , to which instance 
we should assigh new point equl to 5.99

ggplot(data=sample_2,aes(y=feature,x=instance))+
  geom_point(col="red")+geom_point(aes(y=5.99),col="blue",cex=2)+
  geom_segment(x="x1",y=6,xend = "x1", yend = sample_2$feature[1])+
  geom_segment(x="x2",y=6,xend = "x2", yend = sample_2$feature[2])



boystown<-read.csv("https://umich.instructure.com/files/399119/download?down
load_frd=1", sep=" ")
boystown$sex<-boystown$sex-1
boystown$dadjob<--1*(boystown$dadjob-2)
boystown$momjob<--1*(boystown$momjob-2)
str(boystown)

boystown<-boystown[, -1]
table(boystown$gpa)

boystown$grade<-boystown$gpa %in% c(3, 4, 5)
boystown$grade<-factor(boystown$grade, levels=c(F, T), labels = c("above_avg
", "avg_or_below"))
table(boystown$grade)

normalize<-function(x){
  # be careful, the denominator may be trivial!
  return((x-min(x))/(max(x)-min(x)))
}


boystown_n<-as.data.frame(lapply(boystown[-11], normalize))

bt_train<-boystown_n[1:150, -11]
bt_test<-boystown_n[151:200, -11]

bt_train_labels<-boystown[1:150, 11]
bt_test_labels<-boystown[151:200, 11]

library(class)
bt_test_pred<-knn(train=bt_train, test=bt_test, cl=bt_train_labels, k=14)


bt_test_1<-bt_test[1,]
bt_test_labels_1<-bt_test_labels[1]


dif_1<-bt_test_1[rep(1,nrow(bt_train)),1:ncol(bt_train)]
#bt_test_1[ c(1,1,1),2:4 ]
sqrt( rowSums( (bt_train-dif_1)^2 ))

a1<-tibble(dif=sqrt( rowSums( (bt_train-dif_1)^2 )),bt_train_labels ) %>% 
     arrange(dif) %>% slice(1:14)
table(a1$bt_train_labels)


library(class)
library(ggplot2)
# define a function that generates CV folds
cv_partition <- function(y, num_folds = 10, seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  n <- length(y)
  folds <- split(sample(seq_len(n), n), gl(n = num_folds, k=1, length=n))
  folds <- lapply(folds, function(fold) {
    list(
      training = which(!seq_along(y) %in% fold),
      test = fold
    )
  })
  names(folds) <- paste0("Fold", names(folds))
  return(folds)
}

cv_partition(bt_train_labels, num_folds = 10)





