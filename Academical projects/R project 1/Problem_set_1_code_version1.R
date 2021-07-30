
# Problem set 1 code, version 1

# This is the first version of the code for the problem set 1 
# As I said in the README file on the GitHub repository, the difference between version 1 and 2
# is the graphical library used for the plots. In version 1 we use, most of the time, the standard
# library provided by R. In version 2 we use the ggplot2 library.

# Note: some plots are done using ggplot2 and/or its extensions since standard libraries didn't 
# provide sufficient results

# List of used packages:

library(tidyverse)
library(ggcorrplot)      #library used for a suitable representation of the correlation matrix
library(ellipse)
library(MASS)
library(ggplot2)
library(ggbiplot)

# Let's explore the data

usair<-read.table("data/usair.txt",header=TRUE) # load the data

head(usair)                                     # take a look at the data
dim(usair)                                      # and to its dimensions


usair1<-select(usair,Neg.Temp:Days)             # Consider only the data mentioned in the text 
                                                # of the exercise

head(usair1)                                    # take a look at the interested data

n<-dim(usair1)[1]
p<-dim(usair1)[2]

#### Exercise 1 ####
# Visual summary analysis

bar.x<-colMeans(usair1)                         # mean for each variable

par(mfrow=c(1,2))
plot(bar.x[c(2,3)],type="h",lwd=3,ylim=c(0,max(bar.x[c(2,3)])),axes=F,xlab="",
     ylab="Mean",main = "Us air pollution")

box()
axis(2)
axis(1,at=1:2,labels=names(usair1)[c(2,3)],las=3)


plot(bar.x[-c(2,3)],type="h",lwd=3,ylim=c(min(bar.x[-c(2,3)]),max(bar.x[-c(2,3)])),
     axes=F,xlab="",ylab="Mean",main = "Us air pollution")

box()
axis(2)
axis(1,at=1:4,labels=names(usair1)[-c(2,3)],las=3)


# In order to overcome the problems related to scales and positivity we can standardize the data

usair_std<-scale(usair1,scale=T)                # Standardized values
usair_std<-as.data.frame(usair_std)

par(mfrow=c(1,1))
bar.x.std<-colMeans(usair_std)
plot(bar.x.std, type = "h",lwd=3,ylim=c(min(bar.x.std),max(bar.x.std)),axes=F,xlab="",
     ylab="Mean",main = "Us air pollution")
box()
axis(2)
axis(1,at=1:6,labels=names(usair1),las=3)

# But this was just an alternative. Let's move on.

R<-cor(usair1)                                  # Correlation matrix
 
ggcorrplot(R,lab=T,type = "upper",title="Correlation matrix for Usair",
           ggtheme = ggplot2::theme_gray())


# Outliers study

l=list()                                        # This is the list in which are stored the 
                                                # indexes of the outliers

par(mfrow=c(1,2))

# First of allwe study the boxplots

for (j in 1:p) {
  x=usair1[,j]
  main.lab<-names(usair1)[j]
  
  boxplot(x,main=main.lab)
  
  values<-boxplot.stats(x)
  values$stats
  
  ind<-which(x<values$stats[1] | x>values$stats[5])
  ind
  
  l[[j]]<-ind
  
  if(j==2)                                      # This is an ad hoc solution to the overlapping
                                                # text problem
  {
    text(rep(1,3),x[ind[c(1,3,4)]],labels=as.character(ind[c(1,3,4)]),pos=4,cex=0.75,offset=0.3)
    text(c(1),x[ind[2]],labels=as.character(ind[2]),pos=2,cex=0.75,offset=0.3)
    points(rep(1,length(ind)),x[ind],col="red",pch=16)
  }
  
  else if(j==5)                                 # This is an ad hoc solution to the overlapping
                                                # text problem
  {
    text(c(1),x[ind[1]],labels=as.character(ind[1]),pos=2,cex=0.75,offset=0.3)
    text(c(1),x[ind[2]],labels=as.character(ind[2]),pos=4,cex=0.75,offset=0.3)
    points(rep(1,2),x[ind],col="red",pch=16)
  }
  
  else if(length(ind)==0)                       # This is an ad hoc solution for the missing
                                                # outliers
  {
    next
  }
  
  else
  {
    text(rep(1,length(ind)),x[ind],labels=as.character(ind),pos=4,cex=0.75,offset=0.3)
    
    points(rep(1,length(ind)),x[ind],col="red",pch=16)
  }
}

# Normality study

# We study two fundamental tools for normality: histograms with density and qqplots

par(mfrow=c(1,2))
for (j in 1:p) {
  x=usair1[,j]
  main.lab<-names(usair1)[j]
  
  hist(x,probability = T,main=main.lab)
  lines(density(x),col="red",lwd=3)
  
  qqnorm(x,main=main.lab,pch=16)
  qqline(x,col="red")
}

# Study of normality on Manuf and Pop

par(mfrow=c(1,2))

for (j in 2:3) {
  x=usair1[,j]
  main.lab<-names(usair1)[j]
  
  color=rep("black",41)
  color[l[[j]]]<-"red"                        #We already had this!
  
  qqnorm(x,main=main.lab,pch=16,col=color)
  
  
  text(qnorm(ppoints(x))[seq(n-length(l[[j]])+1,n)],sort(x)[seq(n-length(l[[j]])+1,n)],
       as.character(l[[j]]),pos=2,cex=0.75,offset=0.4)
                                              # This part can seem a little bit confusing, since
                                              # in the qqnorm plot the values are not sorted by 
                                              # index but by value.
                                              # So we used this ad hoc solution since we see from
                                              # the plot that the outliers are the last points
  
  qqline(x,col="red")
}

# Once detected we see if their weights are important in terms of normality

par(mfrow=c(1,2))
for (j in 2:3) {
  x=usair1[,j]
  main.lab<-names(usair1)[j]
  
  x<-x[-l[[j]]]
  
  color=rep("black",length(x))
  
  qqnorm(x,main=main.lab,pch=16,col=color)
  
  qqline(x,col="red")
}

# Bivariate normal analysis on Manuf and pop

S<-var(usair1)
j<-2
k<-3
d=mahalanobis(usair1[,c(j,k)],center = bar.x[c(j,k)],cov=S[c(j,k),c(j,k)])


color=rep("black",41)
color[n]<-"red"
par(mfrow=c(1,1))

plot(qchisq(ppoints(d),df=2),sort(d),col=color,pch=16, 
     main="Chisq Q-Q plot of Mahalanobis distance for Manuf vs Pop",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
abline(0,1,col="red")
text(qchisq(ppoints(d),df=2)[n],sort(d)[n],labels = order(d)[n],pos=2,cex = 0.75,offset = 0.3)


# Further outlier analysis

l1<-list()

par(mfrow=c(1,2))
for (j in 1:p) {
  y<-usair_std[,j]                     #First standardized variable
  main.lab<-names(usair_std)[j]
  
  bar.y<-mean(y)                       #Mean of the standardized variable
  
  d=rep(0,41)                          #Now we create the vector
  
  for (i in 1:41){
    d[i]<-y[i]-bar.y
  }

  ind0.975<-which(abs(usair_std[,j])>qnorm((n-0.25)/n),arr.ind=T)
  ind0.99<-which(abs(usair_std[,j])>qnorm((n-0.1)/n),arr.ind=T)
  
  l1[[j]]<-ind0.975
  
  color<-rep("black",41)
  color[ind0.975]<-"blue"
  color[ind0.99]<-"yellow"
  color[l[[j]]]<-"red"
  
  plot(abs(d),pch=16,main=main.lab,ylab = "distance",col=color,ylim = c(0,max(abs(d))))                 
  if (length(ind0.975)==0) next
  

  abline(h=qnorm((n-0.5)/n),lty=2,col="green")
  abline(h=qnorm((n-0.25)/n),lty=2,col="blue")
  abline(h=qnorm((n-0.1)/n),lty=2,col="yellow")
}


# Now let's move on and see how to use scatterplots to detect outliers 

color<-rep("black",41)
rain<-c("red","blue","green","yellow","orange","light blue")
for (j in 1:p) {
  color[l[[j]]]<-rain[j]
}
pairs(usair1,pch=16,cex=1.5,lower.panel=NULL, col=color)

# Since we repeat code we tought it was better creating a function for it

plot_outliers<-function(j,k){
  S<-cov(usair1)
  color1=rep("black",41)
  color1[l[[j]]]<-"red"
  color1[l[[k]]]<-"blue"
  x1<-usair1[,j]
  x2<-usair1[,k]
  main.lab1<-names(usair1)[j]
  main.lab2<-names(usair1)[k]
  plot(x1~x2,pch=16,cex=1,col=color1,main=paste (main.lab1,"vs",main.lab2),xlab=main.lab2,
       ylab = main.lab1, xlim=c(min(x2),max(x2)),ylim=c(min(x1),max(x1)))
  lines(ellipse(x=S[c(k,j),c(k,j)],centre=bar.x[c(k,j)],level=(n-0.5)/n ))
}



par(mfrow=c(1,2))
plot_outliers(1,2)
plot_outliers(1,3)

# and so on

# Now we plot the Mahalanobis distance, always regarding the detection of outliers
S=var(usair1)
par(mfrow=c(1,3))
for (j in 1:p) {
  for (k in j:p) {
    if(j==k) next
    
    d=mahalanobis(usair1[,c(j,k)],center = bar.x[c(j,k)],cov=S[c(j,k),c(j,k)])
    color=rep("black",41)
    
    
    color[l[[j]]]<-"red"
    color[l[[k]]]<-"blue"
    main.lab1<-names(usair1)[j]
    main.lab2<-names(usair1)[k]
    plot(d,pch=16,ylim=c(min(d),max(d)+5), main=paste (main.lab1,"vs",main.lab2),col=color)
    
    qchisq((n-0.5)/n,df=2)
    
    abline(h=qchisq((n-0.5)/n,df=2),lty=2)
    
    abline(h=qchisq((n-0.25)/n,df=2),lty=2)
    
    abline(h=qchisq((n-0.1)/n,df=2),lty=2)
    
  }
}

# Multivariate p-dimensional analysis

par(mfrow=c(1,1))
S<-cov(usair1)

d=mahalanobis(usair1,center = bar.x,cov=S)

color=rep("black",41)

ind=which(sort(d)>11.338810)  #Observations on the sorted vector

ind1=which(d>11.338810)      #Observations on the sample
color[ind]="red"

#d[c(9,1,11)]==sort(d)[c(39,40,41)]

plot(qchisq(ppoints(d),df=6),sort(d),col=color,pch=16, 
     main="Multivariate QQ-chisquared plot of Mahalanobis distance",
     ylab="sorted distance",xlab = "Theoretical quantile")
abline(0,1,col="red")
text(qchisq(ppoints(d),df=6)[c(39,40,41)],sort(d)[c(39,40,41)],
     labels = as.character(ind1),pos=2,cex = 0.75,offset = 0.3)

# Plot of Mahalanobis distance

d=mahalanobis(usair1,center = bar.x,cov=S)

color=rep("black",41)
ind1=which(d>11.338810)
color[ind1]<-"red"
plot(d,col=color,pch=16,ylim=c(0,30), ylab="distance",main="Mahalanobis distance")
text(ind1,d[ind1],labels = ind1,pos = 4,cex = 0.75,offset = 0.3)


abline(h=qchisq((n-0.5)/n,df=p),lty=2)
abline(h=qchisq((n-0.25)/n,df=p),lty=2)
abline(h=qchisq((n-0.1)/n,df=p),lty=2)


#### Exercise 2 ####

# In this exercise we deal with simulated data and PCA in a graphical way

simul<-function(rho){
mu_z=c(2,-3)

sigma_z=matrix(c(-2*rho+2,2*rho-1,2*rho-1,-2*rho+2),nrow = 2)

Z<-mvrnorm(n=100,mu=mu_z,Sigma=sigma_z)

colnames(Z)=c("x1","x2")

plot(x2~x1, data=Z, xlab="Z1", ylab="Z2", asp=1,
     main=paste("Plot of Z with rho=",round(rho,3)), pch=16,
     xlim=c(min(x1)-1,max(x1)+1), ylim=c(min(x2)-1,max(x2)+1))                           


pca.norm<- prcomp(Z)

a1<-pca.norm$rotation[,1];a2<-pca.norm$rotation[,2]

bar.z<-colMeans(Z)

abline(a=(a2[1]/a2[2])*bar.z[1]+bar.z[2],
       b=-a2[1]/a2[2], lwd=1.5)
# 1st rotated axis 
abline(a=(a1[1]/a1[2])*bar.z[1]+bar.z[2],
       b=-a1[1]/a1[2],lwd=1.5)
# 2nd rotated axis
points(bar.z[1],bar.z[2],pch=21,bg="red")
lines(ellipse(x=var(Z),centre=bar.z,level=0.95),col="red",lwd=2)
}

simul(-2/3)
simul(2/3)


#### Exercise 3 ####

#PCA

Us<-usair1

standard=scale(Us,center=T,scale=T)

boxplot(standard, main="Boxplot of Us air pollution")

standard.pca=prcomp(standard)
summary(standard.pca)

plot(summary(standard.pca)$importance[3,],xlab="Principal components",
     ylab="Cumulative proportion",pch=16, main="Cumulative proportion of PCs")

# In order to find how many PCs are enough we usually study the screeplot, in which we have to
#detect the elbow and take the previous number of PCs
screeplot(standard.pca,type="lines",pch=16, main="Variance of PCs")


p<-dim(Us)[2]

summary(standard.pca)$importance[2,]

plot(1:p,summary(standard.pca)$importance[2,],type="b",main="Proportion of variance of PCs",
     ylab="Proportion of variance",xlab = "Principal components",pch=16)

# Now we study the scores

Uspca=predict(standard.pca)[,1:3]            # Reduced data frame 
Uspca<-data.frame(Uspca)

par(mfrow=c(1,3))

boxplot(Uspca[,1], main="PC1")
values<-boxplot.stats(Uspca[,1])
ind1<-which(Uspca[,1]<values$stats[1] | Uspca[,1]>values$stats[5])
ind2<-which(Uspca[,2]<values$stats[1] | Uspca[,2]>values$stats[5])
#ind2

text(rep(1,2),Uspca[,1][ind1],labels=as.character(ind1),pos=4,cex=0.75,offset=0.3)
points(rep(1,2),Uspca[,1][ind1],col="red",pch=16)

values<-boxplot.stats(Uspca[,2])
#values

boxplot(Uspca[,2],main="PC2")
text(rep(1,3),Uspca[,2][ind2],labels=as.character(ind2),pos=4,cex=0.75,offset=0.3)
points(rep(1,3),Uspca[,2][ind2],col="red",pch=16)

boxplot(Uspca[,3],main="PC3")

# Multivariate normality for PCs
par(mfrow=c(1,1))
d=mahalanobis(Uspca,center =colMeans(Uspca),cov=var(Uspca))
color=rep("black",41)
ind=which(sort(d)>8)  #Observations on the sorted vector
ind1=which(d>8)      #Observations on the sample
color[ind]="red"
plot(qchisq(ppoints(d),df=3),sort(d),col=color,pch=16,
     main="Chisq Q-Q plot of Mahalanobis distance",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
abline(0,1,col="red")
text(qchisq(ppoints(d),df=3)[c(40,41)],sort(d)[c(40,41)],labels = as.character(ind1),pos=2,cex = 0.75,offset = 0.3)

#Mahalanobis distance

color=rep("black",41)
ind1=which(d>8)
color[ind1]<-"red"
plot(d,main = "Mahalanobis distance",pch=16,col=color)
text(c(ind1),d[ind1],labels = ind1,pos = 4,cex=0.75,offset = 0.3)
n=41
abline(h=qchisq((n-0.5)/n,df=3),lty=2,col="red")

abline(h=qchisq((n-0.25)/n,df=3),lty=2,col="green")

abline(h=qchisq((n-0.1)/n,df=3),lty=2,col="yellow")

# Study for Pc5 and PC6

par(mfrow=c(1,2))
boxplot(predict(standard.pca)[,5], main="PC5")

last<-predict(standard.pca)[,6]
boxplot(last,main="PC6")
values<-boxplot.stats(last)
ind1<-which(last<values$stats[1] | last>values$stats[5])


text(rep(1,1),last[ind1],labels=as.character(ind1),pos=4,cex=0.75,offset=0.3)
points(rep(1,1),last[ind1],col="red",pch=16)

# Multivariate study for PC5 and PC6

par(mfrow=c(1,1))
color=rep("black",41)
color[ind1]<-"red"
x<-predict(standard.pca)[,5]
y<-predict(standard.pca)[,6]
plot(y~x,pch=16,xlab="PC5",ylab="PC6",col=color,main="Scores")

#Mahalanobis distance

Uspca2<-data.frame(predict(standard.pca)[,5:6])

S<-cov(Uspca2)
uspca2_mean<-colMeans(Uspca2)
d<-mahalanobis(Uspca2,center = uspca2_mean,cov=S)
plot(d,pch=16,main="Mahalanobis distance for PC5 and PC6",col=color)

abline(h=qchisq((n-0.5)/n,df=2),lty=2,col="red")


# Now we'll do the prediction as stated for the exercise

#We create a new dataframe

principal_data = prcomp(usair[,2:7], scale. = TRUE)
pca_var = principal_data$x[,1:3]
usair.pca = data.frame(usair[,1],pca_var)
names(usair.pca)[1] = "SO2"

lm.PC1 = lm(SO2~PC1, usair.pca)
summary(lm.PC1)

ggplot(usair.pca, aes(x = PC1, y = SO2)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+
  ggtitle(label= paste("R=",round(summary(lm.PC1)[[8]],5)))

lm.PC2 = lm(SO2~PC2, usair.pca)
#summary(lm.PC2) # R2 = 0.014, R2adj = -0.01129
# Plotting SO2 vs. PC2
ggplot(usair.pca, aes(x = PC2, y = SO2)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+
  ggtitle(label= paste("R=",round(summary(lm.PC2)[[8]],5)))

# Linear model SO2 vs.PC3
lm.PC3 = lm(SO2~PC3, usair.pca)
#summary(lm.PC3) # R2 = 0.0003649, R2adj = -0.02527
# Plotting SO2 vs. PC3
ggplot(usair.pca, aes(x = PC3, y = SO2)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+
  ggtitle(label= paste("R=",round(summary(lm.PC3)[[8]],5)))

#. This is the linear model based on all the PCs present

lm.pca = lm(SO2~PC1+PC2+PC3, usair.pca)
#summary(lm.pca) # R2 = 0.4182, R2adj = 0.371
# Plotting SO2 vs. PC1+PC2+PC3
ggplot(usair.pca, aes(x = PC1+PC2+PC3, y = SO2)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+
  ggtitle(label= paste("R=",round(summary(lm.pca)[[8]],5)))


#A useful method to see how much every variable load on every PC is the following

ggbiplot(principal_data,choices=c(1,2))

#Let's have a look at PC2 and PC3:
ggbiplot(principal_data,choices=c(2,3))

#Let's have a look at PC1 and PC3:
ggbiplot(principal_data,choices=c(1,3))

#In the last part we don't involve the PCs but the primitive data: we study the correlation 
#between the two different kind of variables called at the beginning

lm_normal_he = lm(SO2~Manuf+Pop, usair)
#summary(lm_normal_he) # R2 = 0.586 and R2_adj = 0.5645
# Let's see the plot
ggplot(usair, aes(x = Manuf+Pop, y = SO2)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+
  ggtitle(label= paste("R=",round(summary(lm_normal_he)[[8]],5)))

lm_normal_cv = lm(SO2~Neg.Temp+Wind+Precip+Days, usair)
#summary(lm_normal_cv) # R2 = 0.255 and R2_adj = 0.1722
# Let's see the plot
ggplot(usair, aes(x = Neg.Temp+Wind+Precip+Days, y = SO2)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+
  ggtitle(label= paste("R=",round(summary(lm_normal_cv)[[8]],5)))

#Linear model on all data

lm_normal = lm(SO2~., data = usair)
# Let's see the plot
ggplot(usair, aes(x = Manuf+Pop+Neg.Temp+Wind+Precip+Days, y = SO2)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+
  ggtitle(label= paste("R=",round(summary(lm_normal)[[8]],5)))
