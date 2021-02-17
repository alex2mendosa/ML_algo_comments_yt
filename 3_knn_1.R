library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
n<-1000
sample_1<-tibble( x1=rnorm(n,10,2),
                  x2=runif(n, min = 100, max = 200) , 
                  x3=rexp(n,0.1) , 
                  x4=sample(0:1,n,replace = TRUE) , 
                  x5=rchisq(n, 5, ncp = 50) ,
                  x6=log( rnorm(n,10,2) ) )

glimpse(sample_1)

#######################  PART_1
# knn is example of supervised , classification technique


#  To initiate knn the follwong action shoul be taken:
#  Data inspection and normalisation
#  Splitin data into training and testing datasets
#  Fitting the model to training data
#  Evaluate perfomance on test data
#  Model tuning , specifically, for knn, determining optimal values of k

# knn is extremely powerfull when applied
# to homogenous data, speficically,
# it is suited very well to numerical features

# General routone for knn is the following:
## ?

#  Now how to measure similary of instances or records
#  Traditionally euclidian distance us applied

# assume 2 dimentional space with 2 poins

sample_2<-data.frame(  feature_BMI=c( runif(1,18.5,24.9),
                                      runif(1, 25.0,29.9)),
                       class=c( "Healthy weight","Overweight" ),
                       row.names=c("inst_1  ","inst_2  ")   )


ggplot(data=sample_2,aes(y= feature_BMI ,x=class))+
  geom_point(col="red",cex=3)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.5 )


# Now assume , we need to decide , to which instance 
# we should assign new point , should it belong to 
# Forst or second class

set.seed(3211)
new_point<-runif(1,18.5,29.9)

ggplot(data=sample_2,aes(y= feature_BMI ,x=class))+
  geom_point(col="red",cex=4)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.1,nudge_x=0.2 )+
  geom_point(col="red")+geom_point(aes(y=new_point),col="blue",cex=5) +
  geom_segment(x="Healthy weight",y=new_point,xend = "Healthy weight", yend = sample_2$feature_BMI[1],
               color="green")+
  geom_segment(x="Overweight",y=new_point,xend = "Overweight", yend = sample_2$feature_BMI[2],
               color="green")




tibble::glimpse(sample_2)
sample_2

# lest estimate Euclidian Distance between 2 poins, we have only 1 instance
#per feature , threfore we would check knn where k in one 

dist_1<- sqrt( (new_point-sample_2$feature_BMI[1])^2 )
dist_2<- sqrt( (new_point-sample_2$feature_BMI[2])^2 )

sample_2["Eu_Dist"]<-c(dist_1 ,dist_2 )


sample_2<-sample_2 %>% mutate(Eu_Dist=sqrt( (new_point-feature_BMI)^2 ))

# now lets add new record

sample_2<-sample_2 %>% dplyr::bind_rows( tibble(feature_BMI=new_point,class=NA,Eu_Dist=NA) )
sample_2$class[3]<-"Healthy weight"
sample_2$class[3]<-sample_2$class[ which.min(sample_2$Eu_Dist) ]


class::knn( sample_2[c(1,2),1],new_point,sample_2[c(1,2),2],k=1, prob=TRUE )

#######################  PART_2

sample_3<-data.frame(  feature_BMI=c( runif(3,18.5,24.9),
                                      runif(3, 25.0,29.9)),
                       class=c( rep("Healthy weight",3),rep("Overweight",3) ) )


ggplot(data=sample_3,aes(y= feature_BMI ,x=class))+
  geom_point(col="red",cex=6)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.1,nudge_x=0.2 )+
  geom_point(col="red")+geom_point(aes(y=new_point),col="blue",cex=5) 

sample_3<-sample_3 %>% mutate(Eu_Dist=sqrt( (new_point-feature_BMI)^2 ))

# Assume that k equals 3 , threrefore we need to choode free nearest neighbours

sample_3 <-sample_3 %>% arrange(Eu_Dist) %>% slice(1:3)

prop.table( table(sample_3$class) )

class::knn( sample_3[,1],new_point,sample_3[,2],k=3, prob=TRUE )




##### PART 3

sample_4<-iris
View(sample_4)


# in this example there are 4 features which describe plant
# out target class is species column 

# lets draw single row we would like to preddict

loc<-sample(1:nrow(sample_4),1 )
test_row_1<-sample_4[ loc   ,  ]

train_row<-sample_4[ -loc   ,  ]

# what classes are available 
sample_4$Species %>% unique()

# in previous example we had only 1 feature 
# here are 4 features which all should be part
# of equation 

# lest fraw 4 graphs, 1 graph per feature where each graph contains
# 1 point from test set and 1 form training set

loc<-sample(1:nrow(train_row),2 )
train_row_2<-train_row[loc,]

# 
train_row_2
test_row_1

# here we have 2 instances from taining set and 
# we need to decide which is closer in terms of 
#euclidian distance to test_row_1

# we need to bind data into tibble to visualise data
# this transformation i srequired 
# to apply ggplot with facets
gg_train<-train_row_2 %>% pivot_longer(Sepal.Length:Petal.Width ,
                                       names_to="Feature")
gg_test<-test_row_1 %>% pivot_longer(Sepal.Length:Petal.Width ,
                                     names_to="Feature") %>%
  mutate(C="Unknown")  

gg_in<-bind_rows(gg_train,gg_test)
ggplot(data=gg_in,aes(y=Species,x=value))+ 
  geom_point(aes(col= Species),cex=3)+
  geom_label(aes(label=round(value,2),color=Species )   ) + 
  facet_grid(~Feature, scales="free_x") 



train_row_2[,-5]-test_row_1[ c(1,1),-5 ]

(train_row_2[,-5]-test_row_1[ c(1,1),-5 ])^2

Eu_Dist=rowSums( (train_row_2[,-5]-test_row_1[ c(1,1),-5 ])^2 ) %>% sqrt()

train_row_2<- train_row_2 %>% 
  mutate(Eu_Dist=Eu_Dist) %>%
  arrange(Eu_Dist)

knn( train=train_row_2[,-5],
     test=test_row_1[ ,-5 ],
     cl=train_row_2[,5],
     prob=TRUE)


### Lets apply knn to each onbservation 


loc<-sample(1:nrow(sample_4),20 )
test_set<-sample_4[ loc   ,  ]
train_set<-sample_4[ -loc   ,  ]

m1_knn<-knn( train=train_set[,-5],
             test=test_set[ ,-5 ],
             cl=train_set[,5], k=3,
             prob=TRUE)

sum(m1_knn==test_set[ ,5 ])/ length(test_set[ ,5 ])

##  now lest write our own fucntion to come up with solution 

  test<-sample_4[ loc   ,  ]
  train<-sample_4[ -loc   ,  ]
  out_2<- test[,5]
  
    n_test<-nrow(test)
    n_train<-nrow(train)
    out<-rep("",n_test)
    
    for ( i in 1:n_test) {
    Eu_Dist=rowSums( (train[,-5]-test[i,-5][c(rep(1,n_train)), ] )^2 ) %>% sqrt()
    train_1<-train %>% mutate(Eu_Dist=Eu_Dist) %>% arrange(Eu_Dist) %>%
                                             slice(1:3)
    class_1<-names( which.max(table(train_1$Species)))
    out[i]<-class_1
                         } 

    sum(out==out_2)/ n_test
    
    # identical 


# now lets multiply each column by random number
    loc<-sample(1:nrow(sample_4),20 )
 sample_5<-iris
 sample_5[,c(1)]<-apply(sample_5[,c(1)],MARGIN=1:2, 
                   function(x) return( x*runif(1,10,10000))  )
 sample_5[,c(1)]/iris[,c(1)] 
   
    test<-sample_5[ loc   ,  ]
    train<-sample_5[ -loc   ,  ]
    out_2<- test[,5]
    
    n_test<-nrow(test)
    n_train<-nrow(train)
    out<-rep("",n_test)
    
    m1_knn<-knn( train=train[,-5],
                 test=test[ ,-5 ],
                 cl=train[,5], k=3,
                 prob=TRUE)
    
    sum(m1_knn==test[ ,5 ])/ length(test[ ,5 ])


    normalize<-function(x){
      return((x-min(x))/(max(x)-min(x)))
    }      
    
    train[,-5]<-apply(train[,-5], MARGIN=2 ,normalize)
    
    m1_knn<-knn( train=train[,-5],
                 test=test[ ,-5 ],
                 cl=train[,5], k=3,
                 prob=TRUE)
    
    sum(m1_knn==test[ ,5 ])/ length(test[ ,5 ])
    
    
 
