#####################################
# APA Laboratori 5                 ##
## LDA/QDA/NBayes/kNN/RegLog       ##
## version of November, 2017       ## 
#####################################

library(MASS) 

####################################################################
## Example 1: Visualizing and classifying adultss with LDA and QDA
####################################################################

## We have the results of an analysis on adultss grown in a region in Italy but derived from three different cultivars.
## The analysis determined the quantities of 13 chemical constituents found in each of the three types of adultss. 
## The goal is to separate the three types of adultss:

adults <- read.table("groupedall.txt", sep=",", dec=".", header=FALSE)

dim(adults)

colnames(adults) <- c('age','workclass','fnlwgt','education','educationnum','maritalstatus','occupation',
                    'relationship','race','sex','capitalgain','capitalloss','hoursperweek','nativecountry','morefifty')

# Clean up column names
colnames(adults) <- make.names(colnames(adults))

adults$morefifty<- as.factor(adults$morefifty)
adults$educationnum<-NULL
adults$fnlwgt<-NULL

adults[["capitalgain"]] <- ordered(cut(adults$capitalgain,c(-Inf, 0, 
                                                          median(adults[["capitalgain"]][adults[["capitalgain"]] >0]), 
                                                          Inf)),labels = c(0,1,2))
adults[["capitalloss"]] <- ordered(cut(adults$capitalloss,c(-Inf, 0, 
                                                          median(adults[["capitalloss"]][adults[["capitalloss"]] >0]), 
                                                          Inf)), labels = c(0,1,2))

summary(adults)
## DO IT
#plot(subset(adults,select=-morefifty),col=unclass(adults$morefifty))

## For this example let's practice a different call mode to lda(), using a formula; this is most useful
## when our data is in a dataframe format: 

lda.model <- lda (morefifty ~ ., data = adults)

lda.model

## We can see that neither Magnesium or Proline seem useful to separate the adultss; while
## Flavanoids and Nonflavanoid.phenols do. Ash is mainly used in the LD2.

## Plot the projected data in the first two LDs
## We can see that the discrimination is very good

plot(lda.model)

# alternatively, we can do it ourselves, with more control on color and text (adults number)

adults.pred <- predict(lda.model)

plot(adults.pred$x,type="n")
#as.character(rownames(adults.pred$x))
text(adults.pred$x,labels="o",col=as.integer(adults$morefifty))
legend('bottomright', c("<=50k",">50k"), lty=1, col=c('black', 'red'), bty='n', cex=.75)

# If need be, we can add the (projected) means to the plot


tab<-table(adults$morefifty, adults.pred$class)
tab
1 - sum(tab[row(tab)==col(tab)])/sum(tab)

# Let us switch to leave-one-out cross-validation

adults.predcv <- update(lda.model,CV=TRUE)

tab<-table(adults$morefifty,adults.predcv$class)
tab
1 - sum(tab[row(tab)==col(tab)])/sum(tab)



# 2 mistakes (on 178 observations): 1.12% error

## Quadratic Discriminant Analysis is the same, replacing 'lda' by 'qda'
## problems may arise if for some class there are less (or equal) observations than dimensions
## (is not the case for the adults data)

qda.model <- qda (morefifty ~ ., data = adults)

qda.model
## There is no projection this time (because projection is a linear operator and the QDA boundaries are quadratic ones)

# but let's have a look at classification:

adults.pred <- predict(qda.model)
tab<-table(adults$morefifty, adults.pred$class)
tab
1 - sum(tab[row(tab)==col(tab)])/sum(tab)

# Let us switch to leave-one-out cross-validation

adults.predcv <- update(qda.model,CV=TRUE)
tab<-table(adults$morefifty,adults.predcv$class)
tab
1 - sum(tab[row(tab)==col(tab)])/sum(tab)


####################################################################
# Example 2: The Naïve Bayes classifier
####################################################################

library (e1071)


N <- nrow(adults)
learn <- sample(1:N, round(2*N/3))

nlearn <- length(learn)
ntest <- N - nlearn

# First we build a model using the learn data

model <- naiveBayes(morefifty ~ ., data = adults[learn,])
model

# compute now the apparent error
pred <- predict(model, adults[learn,-1])

# form and display confusion matrix & overall error
tab <- table(pred, adults[learn,]$morefifty) 
tab
1 - sum(tab[row(tab)==col(tab)])/sum(tab)

# compute the test (prediction) error
pred <- predict(model, newdata=adults[-learn,-1])

# form and display confusion matrix & overall error
tab <- table(Pred=pred, True=adults[-learn,]$morefifty) 
tab
1 - sum(tab[row(tab)==col(tab)])/sum(tab)

# note how most errors (9/12) correspond to democrats wrongly predicted as republicans

## in the event of empty empirical probabilities, this is how we would setup Laplace correction (aka smoothing): 
model <- naiveBayes(morefifty ~ ., data = adults[learn,], laplace = 1)

model
