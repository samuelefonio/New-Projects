
# Problem set 1 code, version 2

# This is the second version of the code for the problem set 1 
# As I said in the README file on the GitHub repository, the difference between version 1 and 2
# is the graphical library used for the plots. In version 1 we use, most of the time, the standard
# library provided by R. In version 2 we use the ggplot2 library.


library(ggcorrplot)      # library used for a suitable representation of the correlation matrix
library(ellipse)
library(MASS)
library(ggplot2)
library(ggbiplot)
library(reshape2)        # For using melt function
library(dplyr)           # For data handling
library(ggrepel)         # For producing plots without overlapping text
library(ggpubr)          # For prodcing multiple plots in one page



# Let's explore the data

usair<-read.table("data/usair.txt",header=TRUE) # load the data

head(usair)                                     # take a look at the data
dim(usair)                                      # and to its dimensions

library(tidyverse)

usair1<-select(usair,Neg.Temp:Days)             # Consider only the data mentioned in the text 
                                                # of the exercise

head(usair1)                                    # take a look at the interested data

n<-dim(usair1)[1]
p<-dim(usair1)[2]

#### Exercise 1 ####
# Visual summary analysis

bar.x<-colMeans(usair1)

#?geom_segment

d<-data.frame(bar.x)
ggplot(d)+geom_point(aes(x=rownames(d),y=bar.x))+
          geom_segment(aes(x=rownames(d),xend=rownames(d),y=0,yend=bar.x), size=1 )+
          xlab("Variables")+ylab("Mean")+ggtitle("Mean of primitive variables")+
          ggplot2::theme_classic()

#Standardized values

usair.std<-data.frame(scale(usair1,scale=TRUE))
bar.x.std<-colMeans(usair.std)
d.std<-data.frame(bar.x.std)
ggplot(d.std)+geom_point(aes(x=rownames(d.std),y=bar.x.std))+
  geom_segment(aes(x=rownames(d.std),xend=rownames(d.std),y=0,yend=bar.x.std), size=1 )+
  xlab("Variables")+ylab("Mean")+ggtitle(label = "Mean of standardized variables", subtitle = "subtitle")+
  theme(
        plot.title = element_text(color = "red", size = 12, face = "bold",hjust = 0.5),
        plot.subtitle = element_text(color = "blue",hjust = 0.5),
        #plot.caption = element_text(color = "green", face = "italic")
        panel.background = element_rect(fill="white",color="black",colour="black")
        )


# Outliers study

# First of all we study the boxplots. For studying them with ggplot2 we have to do some data handling  

plot_data<-cbind(usair.std,seq(1,41))
names(plot_data)[p+1]<-"id"
head(plot_data)

library(reshape)

data<-melt(plot_data,id.var="id")
head(data)

ggplot(data)+geom_boxplot(aes(x=variable,y=value),outlier.colour="red")+
             labs(title = "Variables' boxplot")+xlab("Variable")+ylab("Values")+
             theme(
                   plot.title = element_text(color = "black", size = 12, face = "bold",hjust = 0.5),
                   panel.background = element_rect(fill="white",color="black",colour="black")
                  )

#Or with text

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

data %>%
  group_by(variable) %>%
  mutate(outlier = ifelse(is_outlier(value), id, as.numeric(NA))) %>%
  ggplot(., aes(x = variable, y = value)) +
  geom_boxplot(aes(x=variable,y=value),outlier.colour="red") +
  labs(title = "Variables' boxplot")+xlab("Variable")+ylab("Values")+
  theme(
    plot.title = element_text(color = "black", size = 12, face = "bold",hjust = 0.5),
    panel.background = element_rect(fill="white",color="black",colour="black")
  )+
  geom_text(aes(label = outlier, na.rm = TRUE, hjust=-0.3))

# Normality study

# We study two fundamental tools for normality: histograms with density and qqplots

for (j in 1:p) {
  x=usair1[,j]
  main.lab<-names(usair1)[j]
  
  hist(x,probability = T,main=main.lab)
  lines(density(x),col="red",lwd=3)
  
  qqnorm(x,main=main.lab,pch=16)
  qqline(x,col="red")
}

#?geom_histogram

head(usair1)
x<-data.frame(usair1[,1])
names(x)[1]<-names(usair1)[1]
head(x)

h<-ggplot(data.frame(x))+geom_histogram(aes(x=Neg.Temp,y=..density..),binwidth = 1,fill="green",color="black")+
                      geom_density(aes(x=Neg.Temp),color="red",lwd=1)+
                      labs(title=names(x)[1])+
                      theme(
                            panel.background = element_rect(fill="white",color="black",colour="black"),
                            plot.title = element_text(color = "black",hjust=0.5,size = 12)
                            )
q<-ggplot(x, aes(sample=Neg.Temp))+stat_qq(size=2)+stat_qq_line(color="red")+
                                labs(title=names(x)[1])+xlab("Theoretical quantiles")+
                                ylab("Sample quantiles")+
                                theme(
                                      panel.background = element_rect(fill="white",color="black",colour="black"),
                                      plot.title = element_text(color = "black",hjust=0.5,size = 12)
                                      )

ggarrange(h,q, ncol = 2,nrow = 1)

d1<-filter(data,variable=="Manuf")
d2<-mutate(d1,outlier = ifelse(is_outlier(d1$value), id, as.numeric(NA)))
d3<-filter(d2,!is.na(d2$outlier))
v<-c(d3$id)

ggplot(d2, aes(sample=value)) +
  stat_qq(size=2)+
  labs(title="Manuf")+xlab("Theoretical quantiles")+
  ylab("Sample quantiles")+
  theme(
    panel.background = element_rect(fill="white",color="black",colour="black"),
    plot.title = element_text(color = "black",hjust=0.5,size = 12)
        )+
  annotate("text",x=qnorm(ppoints(d1$value))[seq(n-length(v)+1,n)],
                  y=sort(d1$value)[seq(n-length(v)+1,n)],
                  label=v, vjust=-0.5)+
  geom_point(aes(x=qnorm(ppoints(d1$value)),
                 y=sort(d1$value)),color=c(rep("black",n-length(v)),rep("red",length(v))))


#Bivariate analysis on amnuf and pop

S<-var(usair1)
j<-2
k<-3
d=mahalanobis(usair1[,c(j,k)],center = bar.x[c(j,k)],cov=S[c(j,k),c(j,k)])

new_df<-data.frame(cbind(qchisq(ppoints(d),df=2),sort(d)))
names(new_df)<-c("quantiles","values")
names(new_df)
head(new_df)

ggplot(new_df)+geom_point(aes(x=quantiles,y=values),color=color)+
  labs(title="Bivariate manuf and pop plot")+xlab("Theoretical quantiles")+
  ylab("Sample quantiles")+
  theme(
    panel.background = element_rect(fill="white",color="black",colour="black"),
    plot.title = element_text(color = "black",hjust=0.5,size = 20)
  )+
  geom_abline(slope = 1,intercept = 0,color="red")+
  annotate("text",x=new_df$quantiles[n],
         y=new_df$values[n],
         label=order(d)[n], vjust=1.4)

# All the remaining plots of the first point are easily reproducible with the previous tools

#Exercise 2

simul<-function(rho){
  mu_z=c(2,-3)
  
  sigma_z=matrix(c(-2*rho+2,2*rho-1,2*rho-1,-2*rho+2),nrow = 2)
  
  Z<-mvrnorm(n=100,mu=mu_z,Sigma=sigma_z)
  
  Z<-data.frame(Z)
  names(Z)<-c("x1","x2")
  head(Z)
  
  pca.norm<- prcomp(Z)
  
  a1<-pca.norm$rotation[,1];a2<-pca.norm$rotation[,2]
  
  bar.z<-colMeans(Z)
  
  ggplot(Z,aes(x=x1,y=x2))+geom_point()+stat_ellipse()+
    labs(title="Simulation data")+
    theme(
      panel.background = element_rect(fill="white",color="black",colour="black"),
      plot.title = element_text(color = "black",hjust=0.5,size = 20)
    )+
    geom_abline(slope=-a2[1]/a2[2],intercept=(a2[1]/a2[2])*bar.z[1]+bar.z[2],lwd=0.5)+
    geom_abline(slope = -a1[1]/a1[2],intercept = (a1[1]/a1[2])*bar.z[1]+bar.z[2], lwd=0.5)+
    geom_point(aes(x=bar.z[1],y=bar.z[2]),color="red",size=3)
}

simul(-2/3)
simul(2/3)

#Exercise 3

Us<-usair1

standard=scale(Us,center=T,scale=T)

standard.pca=prcomp(standard)
summary(standard.pca)$importance


m<-as.matrix(summary(standard.pca)$importance)

m1<-t(m)

data<-data.frame(m1)

data

ggplot(data)+geom_point(aes(x=seq(1,6),y=Cumulative.Proportion))+
  labs(title="Cumulative proportion")+xlab("PC")+scale_x_continuous(breaks=seq(1,6))+
  theme(
    panel.background = element_rect(fill="white",color="black",colour="black"),
    plot.title = element_text(color = "black",hjust=0.5,size = 20)
  )


ggplot(data,aes(x=seq(1,6),y=Standard.deviation))+geom_point(size=2)+
  labs(title="Standard deviation")+xlab("PC")+scale_x_continuous(breaks=seq(1,6))+
  theme(
    panel.background = element_rect(fill="white",color="black",colour="black"),
    plot.title = element_text(color = "black",hjust=0.5,size = 20)
  )+geom_path()

# The remaining plots are easily reproducible using previous tools or done using ggplot