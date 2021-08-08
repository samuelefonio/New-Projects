#Iris dataset

getwd()
rm(list=ls())
#install.packages("ggdendro")
#install.packages("reshape2")

#List of needed packages

library(ggplot2)
library(GGally)
library(ggcorrplot)
library(ggdendro)  
library(reshape2)
library(mcclust)

# To start, let's visualize this dataset

head(iris)
summary(iris)
head(iris[,1:4])
ggcorrplot(cor(iris[,1:4]),lab=T,type = "upper",title="Correlation matrix for iris dataset",
           ggtheme = ggplot2::theme_gray())


ggpairs(iris, aes(colour = Species, alpha=0.4),
        columns = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"),
        upper = list(continous="cor"),
        axisLabels="none",
        lower = list(continous="points"))

# or with standard libraries

pairs(iris[1:4],pch=21,cex=2,lower.panel = NULL,bg = c("red", "green3", "blue")[unclass(iris$Species)])

# For the CA we consider the first 4 variables.

data<-iris[,0:4]   #Data considered for the CA

head(data)

summary(data)

# The observations do not differ so much, we do not standardize the data.
# If someone wanted to do it I'll put the part of code to run and just remember to change the data set 
# in the functin call.

#sc_data<-data.frame(scale(data))

#head(sc_data)

# Before making plots we have to do some data handling.

plot_data<-cbind(data,iris$Species)

names(plot_data)[5]<-"Species"

# The mean for every species

means.hat1<-aggregate(plot_data[,1:4],by=list(plot_data$Species),mean)


plot_data1<-melt(means.hat1,id.var = "Group.1")
head(plot_data1)

ggplot(plot_data1, aes(x=Group.1,y=value,group=variable,color=variable)) +
  geom_point()+
  geom_line(aes(lty=variable))+
  labs(title="Mean for each species",x="Species",y="Value")+
  theme(
    plot.title = element_text(size=15, color="Black", hjust=0.5),
    panel.background = element_rect(fill="white",color="black",colour = "black"),
    axis.ticks.length = unit(0.3,"cm")
  )

#This is a way to plot a pair of data with ggplot2


# What distance can we use in our dataset? We can see the different results with all the different distances
# present in the standard R library. The parameters we will use for are:
# - matrix of disagreements (w.r.t the species)
# - Disagreements
# - Rand index (comparison with the species) 
# - Within sum of squares

# For simplicity we will use the following function

distance_study<- function(data,method,cluster) {
  # data: data to perform CA
  # method: distance between observation in order to get the wanted distance matrix
  # cluster: cluster distance
  # Output: A list of 4 elements which stores the terms of comparison cited before.
  l<-NULL
  l<-vector(mode="list",length = 4)
  
  d<-dist(data,method=method)
  iris.study<-hclust(d,method = cluster)
  
  # Within sum of squares
  
  g<-cutree(iris.study,k=3)
  n.i<-table(g)
  out<-by(data,g,var)
  
  wss<-sum(diag(out[[1]])*(n.i[1]-1)+
             diag(out[[2]])*(n.i[2]-1)+
             diag(out[[3]])*(n.i[3]-1)
  )
  
  # Rand index
  
  r<-arandi(g,iris$Species)
  
  # Matrix
  
  m<-table(g,iris$Species)
  m<-addmargins(m)
  
  # Disagreements
  
  mistakes<-(1-arandi(g,iris$Species,adjust=F))*choose(dim(data)[1],2)
  
  #Update
  
  l[[1]]<-m
  l[[2]]<-mistakes
  l[[3]]<-r
  l[[4]]<-wss
  
  names(l)<-c("matrices","disagreements","rand_index","wss")
  
  return(l)
}

#### Manhattan ####

m.single<-distance_study(data,"manhattan","single")
m.complete<-distance_study(data,"manhattan","complete")
m.average<-distance_study(data,"manhattan","average")
m.ward.d<-distance_study(data,"manhattan","ward.D")
m.ward.d2<-distance_study(data,"manhattan","ward.D2")
m.mcquitty<-distance_study(data,"manhattan","mcquitty")
m.median<-distance_study(data,"manhattan","median")
m.centroid<-distance_study(data,"manhattan","centroid")


#### Euclidean ####

e.single<-distance_study(data,"euclidean","single")
e.complete<-distance_study(data,"euclidean","complete")
e.average<-distance_study(data,"euclidean","average")
e.ward.d<-distance_study(data,"euclidean","ward.D")
e.ward.d2<-distance_study(data,"euclidean","ward.D2")
e.mcquitty<-distance_study(data,"euclidean","mcquitty")
e.median<-distance_study(data,"euclidean","median")
e.centroid<-distance_study(data,"euclidean","centroid")

#### Maximum ####

max.single<-distance_study(data,"maximum","single")
max.complete<-distance_study(data,"maximum","complete")
max.average<-distance_study(data,"maximum","average")
max.ward.d<-distance_study(data,"maximum","ward.D")
max.ward.d2<-distance_study(data,"maximum","ward.D2")
max.mcquitty<-distance_study(data,"maximum","mcquitty")
max.median<-distance_study(data,"maximum","median")
max.centroid<-distance_study(data,"maximum","centroid")

#### Canberra ####

c.single<-distance_study(data,"canberra","single")
c.complete<-distance_study(data,"canberra","complete")
c.average<-distance_study(data,"canberra","average")
c.ward.d<-distance_study(data,"canberra","ward.D")
c.ward.d2<-distance_study(data,"canberra","ward.D2")
c.mcquitty<-distance_study(data,"canberra","mcquitty")
c.median<-distance_study(data,"canberra","median")
c.centroid<-distance_study(data,"canberra","centroid")

#### Minkowsky ####

mink.single<-distance_study(data,"minkowski","single")
mink.complete<-distance_study(data,"minkowski","complete")
mink.average<-distance_study(data,"minkowski","average")
mink.ward.d<-distance_study(data,"minkowski","ward.D")
mink.ward.d2<-distance_study(data,"minkowski","ward.D2")
mink.mcquitty<-distance_study(data,"minkowski","mcquitty")
mink.median<-distance_study(data,"minkowski","median")
mink.centroid<-distance_study(data,"minkowski","centroid")

# Note: Binary showed very controversial results since it is used for the presence or not of some particular feature.

#####

# We have to prepare the data for the comparison


data_handling<-function(l1,l2,l3,l4,l5,l6,l7,l8,i){
  
  l<-vector(mode="list",length = 8)
  
  l[1]<-l1[[i]][1]
  l[2]<-l2[[i]][1]
  l[3]<-l3[[i]][1]
  l[4]<-l4[[i]][1]
  l[5]<-l5[[i]][1]
  l[6]<-l6[[i]][1]
  l[7]<-l7[[i]][1]
  l[8]<-l8[[i]][1]

  names(l)<-c("single","complete","average","Ward.D","Ward.D2","Mcquitty","Median","Centroid")
  return (l)
}



m.disagreements<-data_handling(m.single,m.complete,m.average,m.ward.d,
                               m.ward.d2,m.mcquitty,m.median,m.centroid, 2)
m.randindex<-data_handling(m.single,m.complete,m.average,m.ward.d,
                           m.ward.d2,m.mcquitty,m.median,m.centroid, 3)
m.wss<-data_handling(m.single,m.complete,m.average,m.ward.d,
                     m.ward.d2,m.mcquitty,m.median,m.centroid, 4)

e.disagreements<-data_handling(e.single,e.complete,e.average,e.ward.d,
                               e.ward.d2,e.mcquitty,e.median,e.centroid, 2)
e.randindex<-data_handling(e.single,e.complete,e.average,e.ward.d,
                           e.ward.d2,e.mcquitty,e.median,e.centroid, 3)
e.wss<-data_handling(e.single,e.complete,e.average,e.ward.d,
                     e.ward.d2,e.mcquitty,e.median,e.centroid, 4)

max.disagreements<-data_handling(max.single,max.complete,max.average,max.ward.d,
                                 max.ward.d2,max.mcquitty,max.median,max.centroid, 2)
max.randindex<-data_handling(max.single,max.complete,max.average,max.ward.d,
                             max.ward.d2,max.mcquitty,max.median,max.centroid, 3)
max.wss<-data_handling(max.single,max.complete,max.average,max.ward.d,
                       max.ward.d2,max.mcquitty,max.median,max.centroid, 4)

c.disagreements<-data_handling(c.single,c.complete,c.average,c.ward.d,
                               c.ward.d2,c.mcquitty,c.median,c.centroid, 2)
c.randindex<-data_handling(c.single,c.complete,c.average,c.ward.d,
                           c.ward.d2,c.mcquitty,c.median,c.centroid, 3)
c.wss<-data_handling(c.single,c.complete,c.average,c.ward.d,
                     c.ward.d2,c.mcquitty,c.median,c.centroid, 4)

mink.disagreements<-data_handling(mink.single,mink.complete,mink.average,mink.ward.d,
                                  mink.ward.d2,mink.mcquitty,mink.median,mink.centroid, 2)
mink.randindex<-data_handling(mink.single,mink.complete,mink.average,mink.ward.d,
                              mink.ward.d2,mink.mcquitty,mink.median,mink.centroid, 3)
mink.wss<-data_handling(mink.single,mink.complete,mink.average,mink.ward.d,
                        mink.ward.d2,mink.mcquitty,mink.median,mink.centroid, 4)



#Now let's compare the results

#For simplicity I will use a function to plot all the results


distance_plot<-function(list1,list2,list3,list4,list5,measure){
  # list1-list5: list of results we want to perform. 
  # Note: if we add different type of distances we will have to change the function by adding 
  # some features, but in this way we can call the function for each parameter of comparison
  
  l1<-rep(0,length(list1))
  l2<-rep(0,length(list2))
  l3<-rep(0,length(list3))
  l4<-rep(0,length(list4))
  l5<-rep(0,length(list5))
  
  for (i in 1:length(list1)) {
    l1[i]<-list1[[i]][1]
    l2[i]<-list2[[i]][1]
    l3[i]<-list3[[i]][1]
    l4[i]<-list4[[i]][1]
    l5[i]<-list5[[i]][1]
  }
  
  print(l1); print(l2); print(l3);print(l4);print(l5)
  
  data<-data.frame(matrix(c(l1,l2,l3,l4,l5),nrow=5,byrow = T))
  data<-cbind(data,c("Euclidean","Manhattan","Maximum","Canberra","Minkowsky"))
  names(data)<-c("Single","Complete","Average","Ward.d","Ward.d2","Mcquitty",
                 "Median","Centroid","Distance")
  
  data<-melt(data,id.vars = "Distance")
  
  ggplot(data, aes(x=variable,y=value,group=Distance,color=Distance)) +
    geom_point(size=2)+
    geom_line(size=1)+
    scale_color_manual(values = c("blue","red","darkgreen","yellow","red"))+
    labs(title = measure,x="Cluster distance",y="Value")+
    theme(
      panel.background = element_rect(fill="white",color="black",colour = "black"),
      plot.title = element_text(hjust = 0.5,color="black")
    )
}

# Number of disagreements

distance_plot(e.disagreements,
              m.disagreements,
              max.disagreements,
              c.disagreements,
              mink.disagreements,
              "Number of disagreements")

#Rand index

distance_plot(e.randindex,
              m.randindex,
              max.randindex,
              c.randindex,
              mink.randindex,
              "Rand index")

#Within sum of squares

distance_plot(e.wss,
              m.wss,
              max.wss,
              c.wss,
              mink.wss,
              "Within sum of squares")

#We can see that the wss is low using Ward and ward.d2 methods, but this was predictable from the definition of the two methods.


#We know that there are also non hierarchical methods for performing cluster analysis.
#One of the best alternatives is the K-means clustering, which approach is based on getting
#smaller and smaller wss through the steps of the algorithm.


km<- kmeans(data, centers=3, nstart=25, iter.max=100,
             algorithm="MacQueen")

km$withinss
km.wss<-km$tot.withinss
km.wss
table(km$cluster)
g<-km$cluster

r<-arandi(g,iris$Species)
r
# Matrix

m<-table(g,iris$Species)
m<-addmargins(m)
m
# Disagreements

mistakes<-(1-arandi(g,iris$Species,adjust=F))*choose(dim(data)[1],2)
mistakes

# Since in the k-means clustering the number of clusters is fixed in advance, we should try with
# several number of "centers" and decide which one is the best using a screeplot on the wss or 
# using the Calinski method based on Between sum of squares. In the first case we look for the 
# elbow and for the second we look for the maximum. Sometimes it can be helpful taking a look to 
# both in order to decide if the results coincide. Indeed the wss method can sometimes hide 
# the best result.
# But since our aim is to find the best way to recognize the species, for the moment we'll try
# different algorithms.


km_study<-function(data,centers,nstart,max, method){
  
  l<-NULL
  l<-vector(mode="list",length = 4)
  
  km<-kmeans(data, centers=3, nstart=25, iter.max=100,
             algorithm = method)
  
  g<-km$cluster
  
  # Matrix
  
  m<-table(g,iris$Species)
  m<-addmargins(m)
  
  # Disagreements
  
  mistakes<-(1-arandi(g,iris$Species,adjust=F))*choose(dim(data)[1],2)
  
  # Rand index
  
  r<-arandi(g,iris$Species)
  
  # Within sum of squares
  
  wss<-km$tot.withinss
  
  l[[1]]<-m
  l[[2]]<-mistakes
  l[[3]]<-r
  l[[4]]<-wss
  
  names(l)<-c("matrices","disagreements","rand_index","wss")
  
  return(l)
}

km.hw<-km_study(data,centers = 3,nstart=2, max=100,
              method ="Hartigan-Wong")
km.Lloyd<-km_study(data,centers = 3,nstart=2, max=100,
                   method ="Lloyd")
km.Forgy<-km_study(data,centers = 3,nstart=2, max=100,
                   method ="Forgy")
km.MacQueens<-km_study(data,centers = 3,nstart=2, max=100,
                      method ="MacQueen")

wss<-list(c(km.hw$wss,km.Lloyd$wss,km.Forgy$wss,km.MacQueens$wss))
names(wss)[1]<-"Within sum fo squares"
names(wss[[1]])<-c("Hartigan-Wong","Lloyd","Forgy","MacQueen")

mistakes<-list(c(km.hw$disagreements,km.Lloyd$disagreements,
                 km.Forgy$disagreements,km.MacQueens$disagreements))
names(mistakes)[1]<-"Disagreements"
names(mistakes[[1]])<-c("Hartigan-Wong","Lloyd","Forgy","MacQueen")


r.i<-list(c(km.hw$rand_index,km.Lloyd$rand_index,
                 km.Forgy$rand_index,km.MacQueens$rand_index))
names(r.i)[1]<-"Rand index"
names(r.i[[1]])<-c("Hartigan-Wong","Lloyd","Forgy","MacQueen")

wss
mistakes
r.i



#As we can see every algorithm performs the same results.
