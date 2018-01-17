#####################################
# APA Laboratori 8                 ##
## Random Forests                  ##
## version of January, 2018        ## 
#####################################


####################################################################
# Financial Example: classification model for adults subscription
####################################################################

## Direct marketing campaigns (phone calls) of a Portuguese banking institution. 
## The classification goal is to predict if the client will subscribe a term adults

adults <- read.table("groupedall.txt", sep=",", dec=".", header=FALSE)

dim(adults)

colnames(adults) <- c('age','workclass','fnlwgt','education','educationnum','maritalstatus','occupation',
                      'relationship','race','sex','capitalgain','capitalloss','hoursperweek','nativecountry','morefifty')

# Clean up column names
colnames(adults) <- make.names(colnames(adults))

adults$morefifty<- as.factor(adults$morefifty)
adults$educationnum<-NULL

adults[["capitalgain"]] <- ordered(cut(adults$capitalgain,c(-Inf, 0, 
                                                            median(adults[["capitalgain"]][adults[["capitalgain"]] >0]), 
                                                            Inf)),labels = c(0,1,2))
adults[["capitalloss"]] <- ordered(cut(adults$capitalloss,c(-Inf, 0, 
                                                            median(adults[["capitalloss"]][adults[["capitalloss"]] >0]), 
                                                            Inf)), labels = c(0,1,2))


# precalculate the TR/TE partition and the cross-validation partitions on the TR part

N <- nrow(adults)
all.indexes <- 1:N

learn.indexes <- sample(1:N, round(2*N/3))
test.indexes <- all.indexes[-learn.indexes]

learn.data <- adults[learn.indexes,]

nlearn <- length(learn.indexes)
ntest <- N - nlearn

harm <- function (a,b) { 2/(1/a+1/b) }

# percent by class
prop.table(ct, 1)
# total percent correct
sum(diag(ct))/sum(ct)

# test error is 

round(100*(1-sum(diag(ct))/sum(ct)),2)

# not very good, because the 'yes' class is nearly ignored
(F1 <- harm (prop.table(ct,1)[1,1], prop.table(ct,1)[2,2]))

## Now a random Forest
library(randomForest)

model.rf1 <- randomForest(as.factor(morefifty) ~ ., data = learn.data, ntree=100, proximity=FALSE)

model.rf1

## We get an estimated test error (OOB) of 9.3%, so better; let's compute the real test error:

pred.rf1 <- predict (model.rf1, adults[test.indexes,], type="class")

(ct <- table(Truth=adults$morefifty[test.indexes], Pred=pred.rf1))

# percent by class
prop.table(ct, 1)
# total percent correct
sum(diag(ct))/sum(ct)

# real test error is 

round(100*(1-sum(diag(ct))/sum(ct)),2)

(F1 <- harm (prop.table(ct,1)[1,1], prop.table(ct,1)[2,2]))

## So OOB really works in estimating prediction error and the RF is better than a single tree; 
## however, there is a big issue in the unbalanced classes

# one way to deal with this is to include class weights
model.rf2 <- randomForest(as.factor(morefifty) ~ ., data = learn.data, ntree=100, proximity=FALSE, classwt=c(1,10))

model.rf2

# which helps a little bit, but not much: we get estimated test error (OOB) of 9.86% with a better balance; let's compute the real test error:

pred.rf2 <- predict (model.rf2, adults[test.indexes,], type="class")

(ct <- table(Truth=adults$morefifty[test.indexes], Pred=pred.rf2))

# percent by class
prop.table(ct, 1)
# total percent correct
sum(diag(ct))/sum(ct)

# real test error is 

round(100*(1-sum(diag(ct))/sum(ct)),2)

(F1 <- harm (prop.table(ct,1)[1,1], prop.table(ct,1)[2,2]))
plot(model.rf1)
