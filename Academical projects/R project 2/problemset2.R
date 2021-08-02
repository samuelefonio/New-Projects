
#                  ## Problem Set 2 ##

##############################################
##############################################

## Exercise 1


#install.packages("psych")

library(psych)

rm(list=ls())

## pulp paper data

pulp_paper<-read.table("data/pulp_paper.txt") 

names(pulp_paper)<-c("BL", "EM", "SF", "BS",
                     "AFL", "LFF", "FFF", "ZST")

head(pulp_paper)

dim(pulp_paper)

n = dim(pulp_paper)[1]
p = dim(pulp_paper)[2]

##################### 1.1 #########################


# Obtain the maximum likelihood solution for m = 2 and m = 3 common factors on the standardize observations and
# compute the proportion of total sample variance due to each factor.
# List the estimated communalities, specific variances, and the residual matrix S âˆ’ (LË†LË†T + Î¨Ë†). 
# Compare the results. Which choice of m do you prefer? Why?

# standardized pulp paper

p_p_scale = scale(pulp_paper)

# correlation matrix

C = cor(p_p_scale)
round(C,3)

###################
####### m=2 #######
################### 

p_p_scale.fa2<-factanal(covmat=C, factors=2, rotation="none")
p_p_scale.fa2
# loadings matrix with no rotation

output<-cbind(p_p_scale.fa2$loadings[,1:2],
              diag(crossprod(t(p_p_scale.fa2$loadings))),
              p_p_scale.fa2$uniquenesses
)

colnames(output)<-c("ML1","ML2","h2","u2")
round(output,3)

# With no rotation we can see how almost all variables (except FFF) load high on factor 1.
# AFL, LFF load high on factor 2, but not as high as on factor 1.
# FFF loads negative on both (-0.58, -0.50).

# let's see if our results are confirmed with rotation

p_p_scale.fa2<-factanal(covmat=C, factors=2, rotation="varimax")

# loadings matrix with varimax

output<-cbind(p_p_scale.fa2$loadings[,1:2],
              diag(crossprod(t(p_p_scale.fa2$loadings))),
              p_p_scale.fa2$uniquenesses
)
colnames(output)<-c("ML1","ML2","h2","u2")
round(output,3)

# We can see that BL, EM, SF, BS load really high on factor 1.
# AFL, LFF now load higher on factor 2.
# FFF loads negative on both factors (-0.30, -0.70).
# ZST loads the same on both (0.63, 0.63).



#The following are already shown in the variable "output" immediately above.

# communalities with m=2

round(diag(crossprod(t(p_p_scale.fa2$loadings))),3)

# specific variances with m=2

round(p_p_scale.fa2$uniquenesses,3)

# communalities + specific variance = 1 to see if they are correct

round(p_p_scale.fa2$uniquenesses+diag(crossprod(t(p_p_scale.fa2$loadings))),3)


# residual matrix

L<-p_p_scale.fa2$loadings
Residual<-C-L%*%t(L)-diag(p_p_scale.fa2$unique)
round(Residual,3)
sum(Residual^2)

# sum(Residual^2) is a bit high, maybe the ML method with 2 factor is not the best one.


# proportion of total var

colSums(L^2)/p                          

# cumulative proportion                                     

cumsum(colSums(L^2)/p)                          

#plot(seq(1))
#screeplot(cumsum(colSums(L^2)/p))
# plotting the data F1 vs F2

plot(L[,1],L[,2],asp=1,pch=16,
     xlab="Factor 1",ylab="Factor 2",main="Pulp Paper data, rotated ML loadings")
text(L[,1],L[,2],colnames(pulp_paper),pos=c(rep(2,3),3))

#### Alternative with ggplot ####

library(ggplot2)

d<-data.frame(row.names = names(pulp_paper),L[,1],L[,2])

names(d)<-c("Factor 1","Factor 2")

head(d)

ggplot(d)+geom_point(aes(x=L[,1],y=L[,2]),size=1.5)+
        xlab("Factor 1")+ylab("Factor2")+labs(title = "Pulp Paper data, rotated ML loadings")+
        theme(
                
                panel.background = element_rect(fill="white",color="black",colour="black"),
                plot.title = element_text(color = "black",hjust=0.5,size = 15)
        )+
        geom_text(aes(x=L[,1],y=L[,2], label=rownames(d)),hjust=rep(-0.3,8),vjust=c(rep(0,3),-0.5,rep(0,4)))
        



#########
# Just to visualize what we said when we talked about factor loadings.


###################
####### m=3 #######
################### 

p_p_scale.fa3<-factanal(covmat=C, factors=3, rotation="none")

# loadings matrix with no rotation

output<-cbind(p_p_scale.fa3$loadings[,1:3],
              diag(crossprod(t(p_p_scale.fa3$loadings))),
              p_p_scale.fa3$uniquenesses
)

colnames(output)<-c("ML1","ML2","ML3", "h2","u2")
round(output,2)

# With no rotation we can see again how how almost all variables (except FFF) load high on factor 1.
# AFL, LFF load a bit high on factor 2, but not as high again as on factor 1.
# No variable loads high on factor 3 (ZST the biggest with 0.42).

# let's see if our results are confirmed with rotation

p_p_scale.fa3<-factanal(covmat=C, factors=3, rotation="varimax")

# loadings matrix with varimax

outputR<-cbind(p_p_scale.fa3$loadings[,1:3],
              diag(crossprod(t(p_p_scale.fa3$loadings))),
              p_p_scale.fa3$uniquenesses
)

colnames(outputR)<-c("ML1","ML2","ML3", "h2","u2")
round(outputR,2)

# We can see that BL, EM, SF, BS load really high on factor 1 (similar with m=2).
# AFL, LFF load high on factor 2 (similar with 2 factors).
# FFF loads negative on all factors (-0.26, -0.70, -0.37).
# ZST loads the same on both 1 and 2 (0.59, 0.61) and a bit lower on 3 (0.46).
# The results are very similar to those with m=2 an we can notice how the 3rd factor
# does not help a lot. 


# communalities with m=3

diag(crossprod(t(p_p_scale.fa3$loadings)))

# specific variances with m=3

p_p_scale.fa3$uniquenesses

# communalities + specific variance = 1 to see if they are correct (3 factors)

round(p_p_scale.fa2$uniquenesses+diag(crossprod(t(p_p_scale.fa2$loadings))),3)

# residual matrix

L<-p_p_scale.fa3$loadings
Residual3<-C-L%*%t(L)-diag(p_p_scale.fa3$unique)
round(Residual3,3)
sum(Residual3^2)

# sum(Residual^2) is a lot smaller with 3 factors (But this is obvious since we are using more factors).

# proportion of total var

colSums(L^2)/p                          

# cumulative proportion                                     

cumsum(colSums(L^2)/p)   

# We can see how the third factor adds a lot less information respect to the first two.
# (0.08 against 0.48 and 0.37 of the first two factors).


# plotting the data F1 vs F2

plot(L[,1],L[,2],asp=1,pch=16,
     xlab="Factor 1",ylab="Factor 2",main="Pulp Paper data, rotated ML loadings with 3 factors")
text(L[,1],L[,2],colnames(pulp_paper),pos=c(rep(2,3),3))

#### Alternative with ggplot ####

library(ggplot2)

d<-data.frame(row.names = names(pulp_paper),L[,1],L[,2],L[,3])

names(d)<-c("Factor 1","Factor 2","Factor 3")

head(d)

ggplot(d)+geom_point(aes(x=L[,1],y=L[,2],color=row.names(d)),size=1.5,show.legend = F)+
        xlim(-1,2)+
        labs(title = "Pulp Paper data, rotated ML loadings with",
             subtitle = "3 factors",
             x="factor 1",
             y="Factor 2")+
        theme(
                axis.ticks = element_line(size=1),
                axis.ticks.length = unit(0.2,"cm"),
                axis.line = element_line(color = "grey80",size=2),
                panel.background = element_rect(fill="white",color="black",colour="black"),
                plot.title = element_text(color = "red",hjust=0.5,size = 15),
                plot.subtitle = element_text(color = "blue",hjust=0.5)
        )+
        geom_text(aes(x=L[,1],y=L[,2], label=rownames(d),hjust=c(rep(-0.3,2),1.2,rep(-0.3,5))))+
        geom_hline(yintercept = 0)+
        geom_vline(xintercept = 0)

#####

# plotting the data F2 vs F3

plot(L[,2],L[,3],asp=1,pch=16,
     xlab="Factor 2",ylab="Factor 3",main="Pulp Paper data, rotated ML loadings with 3 factors")
text(L[,2],L[,3],colnames(pulp_paper),pos=c(rep(1,8),4))

# plotting the data F1 vs F3

plot(L[,1],L[,3],asp=1,pch=16,
     xlab="Factor 1",ylab="Factor 3",main="Pulp Paper data, rotated ML loadings with 3 factors")
text(L[,1],L[,3],colnames(pulp_paper),pos=c(rep(1,8),3))

# We can notice in these plots how the 1st factor is the more relevant and the 3rd is pretty irrelevant.

##########################
####### Conclusion #######
##########################

# To see which one we do prefer we need to analyze the cumsum of the eigenvalues
eigen.p_p_scale = eigen(C)
eigen.p_p_scale$values

round(cumsum(eigen.p_p_scale$values/p),2)

# We can see how the first two eigenvalues suggest a factor solution with 2 factors,
# where they explain 0.91 of the data (0.80 with only one factor is also high but we did not consider it).
# We also saw, with the proportion of total variance, that the 3rd factor adds less "information"
# (0.08 against 0.48 and 0.37 of the first two factors).
# To confirm our hypothesis, we can use also the regression method comparing the PC factor scores
# with the ML factor scores. If the loading on a particular factor agree, the pairs of scores
# should cluster tightly about the 45Â° line through the origin and the correlation be approximately 1.
# If they do not agree, we can consider to not use that factor.

# FA scores, PC
faPC = principal(r=p_p_scale, nfactors = 3, rotate = "varimax")

# FA scores, ML
faML = factanal(x=p_p_scale, factors = 3, scores = "regression")

plot(faML$scores[,1],faPC$scores[,1],pch=16,
     xlab="ML",ylab="PC",main="Factor 1")
abline(a=0,b=1,lty=2,lwd=2)
round(cor(faML$scores[,1],faPC$scores[,1]),3)

#### Alternative with ggplot ####

d<-data.frame(faML$scores[,1],faPC$scores[,1])
names(d)<-c("ML","PC")
head(d)
ggplot(d)+geom_point(aes(x=ML,y=PC))+
        labs(
                title = "ML vs. PC",
                x="ML",
                y="PC"
        )+
        theme(
                panel.background = element_rect(fill = "White",color = "Black",colour = "black"),
                plot.title = element_text(hjust=0.5)
        )+
        geom_abline(slope = 1,intercept=0,color="red")

#####

plot(faML$scores[,2],faPC$scores[,2],pch=16,
     xlab="ML",ylab="PC",main="Factor 2")
abline(a=0,b=1,lty=2,lwd=2)
round(cor(faML$scores[,2],faPC$scores[,2]),3)

plot(faML$scores[,3],faPC$scores[,3],pch=16,
     xlab="ML",ylab="PC",main="Factor 3")
abline(a=0,b=1,lty=2,lwd=2)
round(cor(faML$scores[,3],faPC$scores[,3]),3)

# Note that with 3 factors there is still correlation with the ML and PC scores (0.669),
# but a lot less than the correlation with only 2 factors (0.894). We can conclude that it seems better
# to use only 2 factors.


##################### 1.2 #########################

#Give an interpretation to the common factors in the m = 2 solution.


#We have seen both the loadings based of none rotation and varimax rotation. Of course varimax rotation is aimed
#at making clear the difference between the different contribution of the variables to the different factors, so 
#from the loadings rotated we can confirm the separation between "paper properties" (the first four) and "pulp
#fiber characteristics" (last four).
#However there are two remarkable facts: the negativity of FFF and the position of ZST. For the first we can
#reasonably say that in absolute value it belongs to the second factor, so there is no controversial analysis on
#it. ZST loads high on both factor 1 and factor 2, a little bit more on factor 1, which is controversial since it
#belongs to the pulp fiber characteristics. 

##################### 1.3 #########################

# Make a scatterplot of the factor scores for m = 2 obtained by the regression
# method. Is their correlation equal to zero? Should we expect so? Comment.

#First of all the factor scores are the estimates of the common factors. In FA we assume that the covariance 
#between the common factors is 0, so we do it also for the estimates.
#However the regression method is based on the jointly normality of thecommon factor and errors, which means
#that normality is assumed. In our case we can see that the shape is elliptical and the 0.95 (red line)level of
#confidence for bivariate normality shows some possible outliers but none of them is over level 0.99(green line),
#so everything seems fine.


faML<-factanal(x=p_p_scale, factors=2, scores="regression")
# correlation matrix is used 
# by default          
#faML$scores[1:4,]

plot(faML$scores[,1],faML$scores[,2],pch=16,
     xlab="Factor1",ylab="Factor2",main="p_p_scale data (ML)", xlim = c(-5,5),ylim = c(-5,5))

#

S<-var(faML$scores)

library(ellipse)
lines(ellipse(x=S,centre=colMeans(faML$scores),level=0.95),col="red")
lines(ellipse(x=S,centre=colMeans(faML$scores),level=0.99),col="green")

#### Alternative with ggplot2 ####

d<-data.frame(faML$scores[,1],faML$scores[,2])
names(d)<-c("ML1","ML2")

ggplot(d,aes(x=ML1,y=ML2))+geom_point()+xlim(-5,5)+ylim(-5,5)+
        labs(
                title = "p_p_scale data (ML)",
                x="Factor 1",
                y="Factor 2"
        )+
        theme(
                plot.title = element_text(hjust=0.5),
                panel.background = element_rect(fill="white",color="black",colour="black")
        )+
        stat_ellipse(level = 0.97,color="red")


#####

#Let's check the correlation

cor(faML$scores)

#We can see that the correlation is quite little. The points seem not to be too much concentrated around the mean
#but in any case it seems reasonable to conclude that the correlation between factor scores is near zero.


##################### 1.4 #########################

#Suppose we have a new observation (15.5, 5.5, 2, ???0.55, 0.6, 65, ???5, 1.2).
#Calculate the corresponding m = 2 factor scores and add this bivariate point to
#the plot in 1.3). How is it placed compared to the rest of the n = 62 points?
#Could you tell without computing the factor scores? Comment.

#First method: we add the observation to the dataset and compare its score to the other, weighted with all the other
#observations as usual with mean and sample variance

newrow = c(15.5, 5.5, 2, -0.55, 0.6, 65, -5, 1.2)
#tail(pulp_paper)
pulp_paper_new<-rbind(pulp_paper,newrow)

pp_scale_new<-scale(pulp_paper_new)

colMeans(pp_scale_new)

tail(pp_scale_new)

faML_new<-factanal(x=pp_scale_new, factors=2, scores="regression")

col_ind<-c(rep("black",62),"red")

plot(faML_new$scores[,1],faML_new$scores[,2],pch=16,
     xlab="Factor1",ylab="Factor2",main="pp_scale_new data (ML)", col=col_ind)

####a Alternative with ggplot ####

d<-data.frame(faML_new$scores[,1],faML_new$scores[,2])
names(d)<-c("Factor_1","Factor_2")

ggplot(d,aes(x=Factor_1,y=Factor_2))+geom_point(color=col_ind,size=2)+
        labs(
                title = "p_p_scale data (ML)",
                x="Factor 1",
                y="Factor 2"
        )+
        theme(
                plot.title = element_text(hjust=0.5),
                panel.background = element_rect(fill="white",color="black",colour="black")
        )

#####

#It is clearly an outlier, we don't have to take a look to the scores to be convinced of this.
#In effect we can also see from the paired scatterplot that it seems to be a controversial point from its
#value: it is probably an outlier not only for the scores (which is clear from the previous plot), but also
#for the original (scaled) variables.

pairs(pp_scale_new,pch=16,cex=2,lower.panel=NULL, col=col_ind)



#Second method: calculate separately the factor score of x without putting it in the dataframe (formula slide 69)

round(p_p_scale.fa2$uniquenesses,3)
p_p_scale

x = c(15.5, 5.5, 2, -0.55, 0.6, 65, -5, 1.2)
faML_new<-factanal(x=p_p_scale, factors=2, scores="regression")

#faPC<-principal(r=dati, nfactors=2, rotate="varimax")
MU = apply(pulp_paper,2 ,mean)
SD = apply(pulp_paper,2, sd)
#SD
#sqrt(diag(cov(pulp_paper)))
L = faML_new$loadings
S = cov(p_p_scale)
R = cor(p_p_scale)
fx=t(L)%*%solve((L%*%t(L)+diag(faML_new$uniquenesses)))%*%((x-MU)/SD)
#fx = t(L)%*%solve(S)%*%((x-MU)/SD)
#fx = t(L)%*%solve(R)%*%((x-MU)/SD)
fx
plot(faML_new$scores[,1],faML_new$scores[,2],pch=16,
     xlab="Factor 1",ylab="Factor 2",main="p_p_scale (ML)", xlim = c(-5,5),ylim = c(-5,5))
points(fx[1],fx[2], col = "red", pch = 16)
abline(h = 0, v = 0, lty = 2)


##############################################
##############################################

#install.packages("glue")
#install.packages("devtools")
library(devtools)
#install_github("fawda123/ggord")
library(ggord)
library(MASS)
library(dplyr)

rm(list=ls())

## glass data

glass<-read.table("data/glass.txt",header=T)

glass
glass$type<-factor(glass$type)
glass
levels(glass$type)<-c("WinF","WinNF","Veh","Con","Tabl","Head")

head(glass)

dim(glass)


#################################################
##################### 2.1 #######################
## linear discriminant analysis via lda() 
lda.fit<-lda(type~.,data=glass)

lda.fit

out<-predict(lda.fit)
out$class

# let's have a look at the loadings of LD1 and LD2
lda_sc = lda.fit[4]
lda_sc
# We can see how RI has the biggest weight both in LD1 and LD2. We can confirm it with the biplot.
# (and with a further analysis)

?lda

# plotting with biplot LD2 vs. LD1 with eigenvectors of variables  (?)
# without RI
?ggord
ord_noRI <- lda(type ~ Na+Mg+Al+Si+K+Ca+Ba+Fe, glass, prior = rep(1, 6)/6)     
ggord(ord_noRI, glass$type)

# with RI
ord <- lda(type ~ ., glass, prior = rep(1, 6)/6)
ggord(ord, glass$type)

########## with standardized data ###############
# Let's standardize the data to confirm our hypothesis that RI is more influential
glass_sc <- scale(glass[1:9])
glass2 = cbind(glass_sc,glass[10]); glass2

## linear discriminant analysis via lda() 
lda.fit2<-lda(type~.,data=glass2)

out2<-predict(lda.fit2)
out2$class

# let's have a look at the loadings of LD1 and LD2
lda_sc2 = lda.fit2[4]
lda_sc2
# We can see how now Na and Si have the biggest weight in LD1 and
# Mg and Ca the biggest in LD2. We can confirm it with the biplot.

# with RI
ord2 <- lda(type ~ ., glass2, prior = rep(1, 6)/6)
ggord(ord2, glass2$type)

#################################################
##################### 2.2 #######################

# matrix with test and prediction
out_matrix<-as.matrix(table(predict(lda.fit)$class, glass$type)); out_matrix
# training error rate
1-sum(diag(out_matrix))/sum(out_matrix)

length(which(glass2$type=="Veh"))

#eterogenei nel senso che essendo di un tipo condividono molte caratteristiche con altri tipi, quindi si 
#confondono. Possiamo vedere questa caratteristica dalla differenza nella confusion matrix e dagli errori
#che abbiamo fatto, concludendo che le meno omogenee sono WinF, WinNF e Veh.

#################################################
##################### 2.3 #######################
#################################################
## 10-fold CV partition

groupCV<-scan(file="data/groupCV.txt")
length(groupCV)

######################################################################
############## 10 fold CV LDA using groupCV partition ################
######################################################################
# Let's create a list with 10 empty DFs for training 10 models and 10 for validation
# that we will use to make 10 predictions in the 10-fold cross validation.
# We will proceed in this way: we will populate the validation DFs using the partition
# given by groupCV. Then, we will populate 10 DFs for training the model using the data
# not considered by the partition given by groupCV. In this way, each of 10 prediction will
# be made using a partition of a 1/10 of the original glass DF not considered when we will
# train the model.

len <- 10

# validation list
validation = NULL
validation <- vector(mode = "list", length = len)

# training list
training = NULL
training <- vector(mode = "list", length = len)

# Let's populate the validation and training list with 10 empty DFs.
# Same variables as in glass DF.
for (i in 1:len) {
        validation[[i]] = data.frame(matrix(ncol = 10, nrow = 0))
        colnames(validation[[i]]) = c("Ri","Na","Mg","Al","Si","K","Ca","Ba","Fe","type")
        
        training[[i]] = data.frame(matrix(ncol = 10, nrow = 0))
        colnames(training[[i]]) = c("Ri","Na","Mg","Al","Si","K","Ca","Ba","Fe","type")
}

# Let's populate the DFs considering the assignment determined by groupCV.
# The DFs for validation are populated using directly the assignment in groupCV,
# the DFs for training the models are populated subtracting each validation DF from
# the original glass DF.

# (Conclusion to add in the last point: we do prefer CV because it avoids overfitting
# and it is more accurate with training DFs in real problem situations
# (when we do not know the validation set), even if it has a bigger error rate (...))

# Assigning each element from glass DF using the assignment from groupCV for the validation DFs
for (i in 1:length(groupCV)) {
        validation[[groupCV[i]]] = rbind(validation[[groupCV[i]]],glass[i,])
}

#validation
#?anti_join
# Subtracting the validation DFs from glass for the training DFs
for (i in 1:len) {
        training[[i]] = anti_join(glass, validation[[i]])
}

dim(validation[[1]])
dim(training[[1]])
err_matrix[[1]]
err_rate[1]
# 10 models, each one for each training DF
models = NULL
models = vector(mode = "list", length = len)

# 10 predictions, each one for each model with its respective validation DF
predictions = NULL
predictions = vector(mode = "list", length = len)

# error matrices for each prediction
err_matrix = NULL
err_matrix = vector(mode = "list", length = len)

# error rates for each error matrices
err_rate = NULL
err_rate = vector(length = len)

#,prior = rep(1, 6)/6

#?predict
# Let's start with our cross validation:
for (i in 1:len) {
        models[[i]] = lda(type~., data=training[[i]])
        predictions[[i]] = predict(models[[i]], validation[[i]])
        
        # produce the error matrices and each error rate for every matrix
        err_matrix[[i]] = as.matrix(table(predictions[[i]]$class, validation[[i]]$type))
        err_rate[i] = 1-sum(diag(err_matrix[[i]]))/sum(err_matrix[[i]])
        
        # Output of the 10 confusion matrices with their respective error rates
        print(list(confusion_matrix = err_matrix[[i]], LDA_error_rate = err_rate[[i]]))
}
err_rate
# Final output: mean error rate of the 10 matrices
mean_err = mean(err_rate)
mean_err

#m<-matrix(nrow = 6,ncol = 6)
m<-err_matrix[[1]]
for (i in 2:10) {
        m<-m+err_matrix[[i]]        
}
#err_matrix[[1]]+err_matrix[[2]]
m

1-sum(diag(m))/sum(m)

validation
validation[[1]]
training[[1]]

validation[[8]]
length(groupCV[groupCV==3])
err_rate

length(predictions[[1]]$class)
dim(validation[[1]])

######################################################################
## 10 fold CV LDA with random sampling, not using groupCV partition ##
######################################################################
#install.packages("caret")
library(caret)
#install.packages("e1071")
library(e1071)

# setting seed to generate a reproducible random sampling
set.seed(123)

# defining training control as cross-validation and value of K equal to 10
train_control <- trainControl(method = "cv", number = 10)

# training the model by assigning sales column as target variable
# and rest other column as independent variable
model <- train(type ~., data = glass, method = "lda", trControl = train_control)

# printing model performance metrics along with other details
print(model)


######################################################################
######## K-fold-CV (up to K=10) with knn and random sampling #########
######################################################################
cv.knn<- function (dataY, dataX, kn=1, K=10, seed=123) {
        n <- nrow(dataX)
        set.seed(seed)
        library(class)
        
        f <- ceiling(n/K)
        s <- sample(rep(1:K, f), n)  
        dataX=scale(dataX)
        CV=NULL;PvsO=NULL
        
        for (i in 1:K) { 
                test.index <- seq_len(n)[(s == i)] #test data
                train.index <- seq_len(n)[(s != i)] #training data
                
                train.X <- dataX[train.index,]
                test.X <- dataX[test.index,]
                train.y <- dataY[train.index]
                test.y <- dataY[test.index]
                #predicted test set y
                knn.pred=knn(train.X, test.X, train.y, k=kn) 
                #observed - predicted on test data 
                error= mean(knn.pred!=test.y) 
                #error rates 
                CV=c(CV,mean(error))
                predvsobs=data.frame(knn.pred,test.y)
                PvsO=rbind(PvsO,predvsobs)
        } 
        
        #Output
        list(k = K, knn_error_rate = CV, min_err_rate=min(CV), best_k = which(CV==min(CV)),
             mean_error = mean(error), confusion=table(PvsO[,1],PvsO[,2]), seed=seed)
}

cv.knn(dataY=glass$type, dataX=glass[,-10], kn=1, K=10, seed=123)

# We can see how the error rate is minimum when k = 3 (0.2). We can do something
# similar with our K-fold CV with LDA and QDA.

#################################################
##################### 2.4 #######################
#################################################
# plotting
lookup<-c("blue", "brown", "violet", "green", "yellow",  "red")
names(lookup)<-as.character(1:6)
comp.col<-lookup[glass$type]
comp.col
#                                           # create vector with color
#                                           # names from class var
#                                           # using lookup table

means.hat<-aggregate(out$x,by=list(glass$type),FUN=mean)
means.hat<-means.hat[,-1]

# plotting LD2 vs. LD1
plot(LD2~LD1,data=out$x,col=comp.col,cex=0.8)
points(means.hat[,1],means.hat[,2],cex=1.5,bg=lookup,pch=21)
as.factor(glass$type)
#distinct_(list(glass2$type))

plot(LD2~LD1,data=out$x,col=comp.col,cex=0.8, pch=16)
points(means.hat[,1],means.hat[,2],cex=1.5,bg=lookup,pch=21)
legend("bottomright",                
       legend = c("WinF", "WinNF","Veh","Con","Tabl","Head"),
       col = lookup,
       pch = 16)

#################################################
##################### 2.5 #######################
# 1957 - 2171

#Compute the training error and the 10-fold cross validation error for each
#reduced-rank LDA classifier. Plot both error curves against the number of
#discriminant directions, add full-rank LDA errors found in points 2.2) and 2.3).
#What classifier do you prefer? Comment

#Adesso calcoliamo il training error per ogni possibile riduzione del rank senza la 10-cross-validation.

classic_er=rep(0,5)

lda.fit_pt5<-lda(type~.,data=glass)
lda.pred_pt5<-predict(lda.fit_pt5, dimen=5)
lda.pred_pt5
dim(lda.pred_pt5$x)
out<-as.matrix(table(lda.pred_pt5$class,glass$type))
training.error<-1-sum(diag(out))/sum(out)
training.error
?predict

for (i in 1:5) {
        lda.fit_pt5<-lda(type~.,data=glass)
        lda.pred_pt5<-predict(lda.fit_pt5, dimen=i)
        dim(lda.pred_pt5$x)
        out<-as.matrix(table(lda.pred_pt5$class,glass$type))
        training.error<-1-sum(diag(out))/sum(out)
        training.error
        
        classic_er[i]=training.error
}
classic_er
d<-data.frame(classic_er)
d
colnames(d)<-seq(1,5)
d<-data.frame(t(classic_er))
d
colnames(d)<-seq(1:5)
d
rownames(d)<-c("Training Error")
d

#Ora calcoliamo il training error per ogni 10-cross-validation e applicando la reduced rank. Di questo
#consideriamo prima la media tra gli errori del 10-cross validation, per ogni riduzione di rank (riga blu).
#Poi (linea gialla) prendiamo il minimo della 10-cross validation, quindi consideriamo il gruppo migliore
#da togliere e per il quale testare il modello costruito sui rimanenti trainig data.
#Questa cosa non è possibile con un real data set, in quanto non si avrebbe una partizione che produce
#per forza risultati migliori, cosa che nel nostro caso accade, in quanto togliendo il gruppo 8 (full rank) e
# allenando i dati rileviamo un errore molto piccolo (0.2). 
#Ecco perchè, per un'analisi generale, potrebbe essere meglio confrontare la media dei 10cv e il metodo classico.

groupCV

tcv_mean=rep(0,5)
tcv_minim=rep(0,5)
list_group=rep(0,5)
mean_train_error=rep(0,5)

for (j in 1:5) {
        len <- 10
        
        # validation list
        validation = NULL
        validation <- vector(mode = "list", length = len)
        
        # training list
        training = NULL
        training <- vector(mode = "list", length = len)
        
        # Let's populate the validation and training list with 10 empty DFs.
        # Same variables as in glass DF.
        for (i in 1:len) {
                validation[[i]] = data.frame(matrix(ncol = 10, nrow = 0))
                colnames(validation[[i]]) = c("Ri","Na","Mg","Al","Si","K","Ca","Ba","Fe","type")
                
                training[[i]] = data.frame(matrix(ncol = 10, nrow = 0))
                colnames(training[[i]]) = c("Ri","Na","Mg","Al","Si","K","Ca","Ba","Fe","type")
        }
        
        # Let's populate the DFs considering the assignment determined by groupCV.
        # The DFs for validation are populated using directly the assignment in groupCV,
        # the DFs for training the models are populated subtracting each validation DF from
        # the original glass DF.
        
        # (Conclusion to add in the last point: we do prefer CV because it avoids overfitting
        # and it is more accurate with training DFs in real problem situations
        # (when we do not know the validation set), even if it has a bigger error rate (...))
        
        # Assigning each element from glass DF using the assignment from groupCV for the validation DFs
        for (i in 1:length(groupCV)) {
                validation[[groupCV[i]]] = rbind(validation[[groupCV[i]]],glass[i,])
        }
        
        #?anti_join
        # Subtracting the validation DFs from glass for the training DFs
        for (i in 1:len) {
                training[[i]] = anti_join(glass, validation[[i]])
        }
        
        # 10 models, each one for each training DF
        models = NULL
        models = vector(mode = "list", length = len)
        
        # 10 predictions, each one for each model with its respective validation DF
        predictions = NULL
        predictions = vector(mode = "list", length = len)
        
        # error matrices for each prediction
        err_matrix = NULL
        err_matrix = vector(mode = "list", length = len)
        
        # error rates for each error matrices
        err_rate = NULL
        err_rate = vector(length = len)
        
        #Training error for each case
        train_err_mod = NULL
        train_err_mod = vector(mode = "list", length = len)
        
        #
        
        train_err = NULL
        train_err = vector(length = len) 
        
        train_err_matrix<- NULL
        train_err_matrix = vector(mode = "list", length = len)
        
        #?predict
        # Let's start with our cross validation:
        for (i in 1:len) {
                models[[i]] = lda(type~.,prior = rep(1, 6)/6, data=training[[i]])
                predictions[[i]] = predict(models[[i]], validation[[i]], dimen=j)
                train_err_mod[[i]]=predict(models[[i]],dimen = j)
                
                # produce the error matrices and each error rate for every matrix
                err_matrix[[i]] = as.matrix(table(predictions[[i]]$class, validation[[i]]$type))
                err_rate[i] = 1-sum(diag(err_matrix[[i]]))/sum(err_matrix[[i]])
                
                train_err_matrix[[i]]=as.matrix(table(train_err_mod[[i]]$class,training[[i]]$type))
                train_err[i]=1-sum(diag(train_err_matrix[[i]]))/sum(train_err_matrix[[i]])
                
                # Output of the 10 confusion matrices with their respective error rates
                #print(list(confusion_matrix = err_matrix[[i]], LDA_error_rate = err_rate[[i]]))
        }
        
        # Final output: mean error rate of the 10 matrices
        #print(mean(train_err))
        mean_train_error[j] = mean(train_err)
        mean_err = mean(err_rate)
        #print(err_rate)
        tcv_mean[j]=mean_err
        
        minore=min(err_rate)
        tcv_minim[j]=minore
        list_group[j]=which.min(err_rate)
}


final_error = vector(length = 5)

for (j in 1:5) {
        len <- 10
        
        # validation list
        validation = NULL
        validation <- vector(mode = "list", length = len)
        
        # training list
        training = NULL
        training <- vector(mode = "list", length = len)
        
        # Let's populate the validation and training list with 10 empty DFs.
        # Same variables as in glass DF.
        for (i in 1:len) {
                validation[[i]] = data.frame(matrix(ncol = 10, nrow = 0))
                colnames(validation[[i]]) = c("Ri","Na","Mg","Al","Si","K","Ca","Ba","Fe","type")
                
                training[[i]] = data.frame(matrix(ncol = 10, nrow = 0))
                colnames(training[[i]]) = c("Ri","Na","Mg","Al","Si","K","Ca","Ba","Fe","type")
        }
        
        # Let's populate the DFs considering the assignment determined by groupCV.
        # The DFs for validation are populated using directly the assignment in groupCV,
        # the DFs for training the models are populated subtracting each validation DF from
        # the original glass DF.
        
        # (Conclusion to add in the last point: we do prefer CV because it avoids overfitting
        # and it is more accurate with training DFs in real problem situations
        # (when we do not know the validation set), even if it has a bigger error rate (...))
        
        # Assigning each element from glass DF using the assignment from groupCV for the validation DFs
        for (i in 1:length(groupCV)) {
                validation[[groupCV[i]]] = rbind(validation[[groupCV[i]]],glass[i,])
        }
        
        #?anti_join
        # Subtracting the validation DFs from glass for the training DFs
        for (i in 1:len) {
                training[[i]] = anti_join(glass, validation[[i]])
        }
        
        # 10 models, each one for each training DF
        models = NULL
        models = vector(mode = "list", length = len)
        
        # 10 predictions, each one for each model with its respective validation DF
        predictions = NULL
        predictions = vector(mode = "list", length = len)
        
        # error matrices for each prediction
        err_matrix = NULL
        err_matrix = vector(mode = "list", length = len)
        
        # error rates for each error matrices
        err_rate = NULL
        err_rate = vector(length = len)
        
        # Let's start with our cross validation:
        for (i in 1:len) {
                models[[i]] = lda(type~., data=training[[i]])
                predictions[[i]] = predict(models[[i]], validation[[i]], dimen=j)
                
                # produce the error matrices and each error rate for every matrix
                err_matrix[[i]] = as.matrix(table(predictions[[i]]$class, validation[[i]]$type))
                # err_rate[i] = 1-sum(diag(err_matrix[[i]]))/sum(err_matrix[[i]])
                
        }
        
        # Final output: mean error rate of the 10 matrices (for every rank)
        final_confusion1<-err_matrix[[1]]
        for (i in 2:len){
                final_confusion1<-final_confusion1+err_matrix[[i]]
        }
        final_confusion = Reduce('+', err_matrix)
        print(final_confusion1)
        print(final_confusion)
        final_error[j] = 1-sum(diag(final_confusion/sum(final_confusion)))
}

validation[[10]]
d<-data.frame(round(final_error,6))
colnames(d)<-c("Error rates with groupCV partition")
rownames(d)<-c("R1","R2","R3","R4","R5")
kbl(d,booktabs = T)%>%
        kable_styling(latex_options = c("centering","hold_position"))
mean_train_error
tcv_minim
list_group
tcv_mean

d<-data.frame(classic_er,tcv_minim,tcv_mean,mean_train_error)
d

vec<-seq(1,5)

#Blu è mean error per 10 cv
#Rosso classic
#Giallo è il minimo

#help("geom_point")
library(ggplot2)
ggplot(d)+geom_line(aes(x=vec,y=classic_er),col="red")+geom_point(aes(x=vec,y=classic_er),col="red")+
        geom_line(aes(x=vec,y=tcv_mean),col="blue")+geom_point(aes(x=vec,y=tcv_mean),col="blue")+
        geom_line(aes(x=vec,y=mean_train_error),col="green")+geom_point(aes(x=vec,y=mean_train_error),col="green")+
        #geom_hline(yintercept = 1-sum(diag(out_matrix))/sum(out_matrix))+
        #geom_hline(yintercept = mean_err)+
        geom_line(aes(x=vec,y=tcv_minim),col="yellow")+geom_point(aes(x=vec,y=tcv_minim),col="yellow")

#A parità di rank, e considerando la possibilità di scegliere quale sia il gruppo migliore con cui allenare i
#dati, (quindi prendendo il minimo), la 10cv risulta il metodo migliore di classificazione tranne che per il 
#rank 4. Tuttavia, in generale, la media produce un errore maggiore del metodo classico, quindi se non riusciamo
#ad identificare il grupo giusto da togliere potremmo incappare in un errore maggiore con qualsiasi rank.

