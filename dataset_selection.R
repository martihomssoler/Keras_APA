####################################################################
# Create different datasets to compare
####################################################################
set.seed(1313)
### Without preprocessing
adults <- read.table("out.txt", sep=",", dec=".", header=FALSE)
colnames(adults) <- c('age','workclass','fnlwgt','education','educationnum','maritalstatus','occupation',
                      'relationship','race','sex','capitalgain','capitalloss','hoursperweek','nativecountry','morefifty')
colnames(adults) <- make.names(colnames(adults))
adults$morefifty<- as.factor(adults$morefifty)

### All grouped
adults_grouped <- read.table("groupedall.txt", sep=",", dec=".", header=FALSE)
colnames(adults_grouped) <- c('age','workclass','fnlwgt','education','educationnum','maritalstatus','occupation',
                      'relationship','race','sex','capitalgain','capitalloss','hoursperweek','nativecountry','morefifty')
colnames(adults_grouped) <- make.names(colnames(adults_grouped))
adults_grouped$morefifty<- as.factor(adults_grouped$morefifty)
adults_grouped$educationnum<-NULL

adults_grouped[["capitalgain"]] <- ordered(cut(adults_grouped$capitalgain,c(-Inf, 0, 
                                                            median(adults_grouped[["capitalgain"]][adults_grouped[["capitalgain"]] >0]), 
                                                            Inf)),labels = c("None","Low","High"))
adults_grouped[["capitalloss"]] <- ordered(cut(adults_grouped$capitalloss,c(-Inf, 0, 
                                                            median(adults_grouped[["capitalloss"]][adults_grouped[["capitalloss"]] >0]), 
                                                            Inf)), labels = c("None","Low","High"))

####################################################################
# Compare them with all base methods
####################################################################
library(MASS) 
#plot(subset(adults,select=-morefifty),col=unclass(adults$morefifty))

### Base unprocessed data
DataSet <- adults
## LDA
lda.model <- lda (morefifty ~ ., data = DataSet)

DataSet.pred <- predict(lda.model)
b_tab_lda<-table(DataSet$morefifty, DataSet.pred$class)
b_error_lda <- 1 - sum(b_tab_lda[row(b_tab_lda)==col(b_tab_lda)])/sum(b_tab_lda)

## QDA
qda.model <- qda (morefifty ~ ., data = DataSet)

DataSet.pred <- predict(qda.model)
b_tab_qda<-table(DataSet$morefifty, DataSet.pred$class)
b_error_qda <- 1 - sum(b_tab_qda[row(b_tab_qda)==col(b_tab_qda)])/sum(b_tab_qda)

## Naïve Bayes classifier
library (e1071)

N <- nrow(DataSet)
learn <- sample(1:N, round(2*N/3))
nlearn <- length(learn)
ntest <- N - nlearn

model <- naiveBayes(morefifty ~ ., data = DataSet[learn,])
# Train error
pred <- predict(model, DataSet[learn,-1])
b_train_tab_nb <- table(pred, DataSet[learn,]$morefifty)
b_train_error_nb <- 1 - sum(b_train_tab_nb[row(b_train_tab_nb)==col(b_train_tab_nb)])/sum(b_train_tab_nb)

# Test error
pred <- predict(model, newdata=DataSet[-learn,-1])
b_test_tab_nb <- table(Pred=pred, True=DataSet[-learn,]$morefifty)
b_test_error_nb <- 1 - sum(b_test_tab_nb[row(b_test_tab_nb)==col(b_test_tab_nb)])/sum(b_test_tab_nb)

## Random Forest
N <- nrow(DataSet)
all.indexes <- 1:N

learn.indexes <- sample(1:N, round(2*N/3))
test.indexes <- all.indexes[-learn.indexes]

learn.data <- DataSet[learn.indexes,]

nlearn <- length(learn.indexes)
ntest <- N - nlearn

library(randomForest)

model.rf1 <- randomForest(as.factor(morefifty) ~ ., data = learn.data, ntree=100, proximity=FALSE)

# Test error
pred.rf1 <- predict (model.rf1, DataSet[test.indexes,], type="class")
b_tab_rf <- table(Truth=DataSet$morefifty[test.indexes], Pred=pred.rf1)
b_error_rf <- 1-sum(diag(b_tab_rf))/sum(b_tab_rf)

## MLP
N <- nrow(DataSet)
learn <- sample(1:N, round(2*N/3))
nlearn <- length(learn)
ntest <- N - nlearn

model.logreg <- glm (morefifty~., data=DataSet[learn,], family=binomial)
# Simplify the model using the AIC
model.logreg2 <- step(model.logreg)
# Calculation of apparent error in the training set (learn)
glfpred=NULL
glfpred[model.logreg2$fitted.values<0.5]=0
glfpred[model.logreg2$fitted.values>=0.5]=1
b_train_tab_mlp <- table(DataSet$morefifty[learn],glfpred)
b_train_error_mlp <- 1-sum(diag(b_train_tab_mlp))/nlearn

# Estimation of prediction error using the test set

glft = predict(model.logreg2, newdata=DataSet[-learn,]) 
pt = 1/(1+exp(-glft))
glfpredt = NULL
glfpredt[pt<0.5]=0
glfpredt[pt>=0.5]=1
b_test_tab_mlp <- table(DataSet$morefifty[-learn],glfpredt)
b_test_error_mlp <- 1-sum(diag(b_test_tab_mlp))/ntest

#############################################################

### Processed data
DataSet <- adults_grouped
## LDA
lda.model <- lda (morefifty ~ ., data = DataSet)

DataSet.pred <- predict(lda.model)
p_tab_lda<-table(DataSet$morefifty, DataSet.pred$class)
p_error_lda <- 1 - sum(p_tab_lda[row(p_tab_lda)==col(p_tab_lda)])/sum(p_tab_lda)

## QDA
qda.model <- qda (morefifty ~ ., data = DataSet)

DataSet.pred <- predict(qda.model)
p_tab_qda<-table(DataSet$morefifty, DataSet.pred$class)
p_error_qda <- 1 - sum(p_tab_qda[row(p_tab_qda)==col(p_tab_qda)])/sum(p_tab_qda)

## Naïve Bayes classifier
library (e1071)

N <- nrow(DataSet)
learn <- sample(1:N, round(2*N/3))
nlearn <- length(learn)
ntest <- N - nlearn

model <- naiveBayes(morefifty ~ ., data = DataSet[learn,])
# Train error
pred <- predict(model, DataSet[learn,-1])
p_train_tab_nb <- table(pred, DataSet[learn,]$morefifty)
p_train_error_nb <- 1 - sum(p_train_tab_nb[row(p_train_tab_nb)==col(p_train_tab_nb)])/sum(p_train_tab_nb)

# Test error
pred <- predict(model, newdata=DataSet[-learn,-1])
p_test_tab_nb <- table(Pred=pred, True=DataSet[-learn,]$morefifty)
p_test_error_nb <- 1 - sum(p_test_tab_nb[row(p_test_tab_nb)==col(p_test_tab_nb)])/sum(p_test_tab_nb)

## Random Forest
N <- nrow(DataSet)
all.indexes <- 1:N

learn.indexes <- sample(1:N, round(2*N/3))
test.indexes <- all.indexes[-learn.indexes]

learn.data <- DataSet[learn.indexes,]

nlearn <- length(learn.indexes)
ntest <- N - nlearn

library(randomForest)

model.rf1 <- randomForest(as.factor(morefifty) ~ ., data = learn.data, ntree=100, proximity=FALSE)

# Test error
pred.rf1 <- predict (model.rf1, DataSet[test.indexes,], type="class")
p_tab_rf <- table(Truth=DataSet$morefifty[test.indexes], Pred=pred.rf1)
p_error_rf <- 1-sum(diag(b_tab_rf))/sum(b_tab_rf)

## MLP
N <- nrow(DataSet)
learn <- sample(1:N, round(2*N/3))
nlearn <- length(learn)
ntest <- N - nlearn

model.logreg <- glm (morefifty~., data=DataSet[learn,], family=binomial)
# Simplify the model using the AIC
model.logreg2 <- step(model.logreg)
# Calculation of apparent error in the training set (learn)
glfpred=NULL
glfpred[model.logreg2$fitted.values<0.5]=0
glfpred[model.logreg2$fitted.values>=0.5]=1
p_train_tab_mlp <- table(DataSet$morefifty[learn],glfpred)
p_train_error_mlp <- 1-sum(diag(p_train_tab_mlp))/nlearn

# Estimation of prediction error using the test set

glft = predict(model.logreg2, newdata=DataSet[-learn,]) 
pt = 1/(1+exp(-glft))
glfpredt = NULL
glfpredt[pt<0.5]=0
glfpredt[pt>=0.5]=1
p_test_tab_mlp <- table(DataSet$morefifty[-learn],glfpredt)
p_test_error_mlp <- 1-sum(diag(p_test_tab_mlp))/ntest

####################################################################
# Plot the results
####################################################################

colnames <- c("LDA","--","QDA","--","NBtrain","--","NBtest","--","RndF","--","MLPtrain","--","MLPtest","--")

### Error plot
barplot(c(b_error_lda,p_error_lda,b_error_qda,p_error_qda,b_train_error_nb,p_train_error_nb,b_test_error_nb,p_test_error_nb,b_error_rf,p_error_rf,b_train_error_mlp,p_train_error_mlp,b_test_error_mlp,p_test_error_mlp),
        main="Errors", xlab="Methods", ylab="Total",
        names.arg=colnames, 
        col=c("blue","red"))
legend("topleft", c("Base Data","Preprocessed Data"), cex=0.8, 
       bty="n", fill=c("blue","red"))

### Confusion matrix plots
par(mfrow=c(2,2))
display1 <- c(b_tab_lda[1]/(b_tab_lda[1]+b_tab_lda[3]),
              p_tab_lda[1]/(p_tab_lda[1]+p_tab_lda[3]),
              b_tab_qda[1]/(b_tab_qda[1]+b_tab_qda[3]),
              p_tab_qda[1]/(b_tab_qda[1]+b_tab_qda[3]),
              b_train_tab_nb[1]/(b_train_tab_nb[1]+b_train_tab_nb[3]),
              p_train_tab_nb[1]/(p_train_tab_nb[1]+p_train_tab_nb[3]),
              b_test_tab_nb[1]/(b_test_tab_nb[1]+b_test_tab_nb[3]),
              p_test_tab_nb[1]/(p_test_tab_nb[1]+p_test_tab_nb[3]),
              b_tab_rf[1]/(b_tab_rf[1]+b_tab_rf[3]),
              p_tab_rf[1]/(p_tab_rf[1]+p_tab_rf[3]),
              b_train_tab_mlp[1]/(b_train_tab_mlp[1]+b_train_tab_mlp[3]),
              p_train_tab_mlp[1]/(p_train_tab_mlp[1]+p_train_tab_mlp[3]),
              b_test_tab_mlp[1]/(b_test_tab_mlp[1]+b_test_tab_mlp[3]),
              p_test_tab_mlp[1]/(p_test_tab_mlp[1]+p_test_tab_mlp[3]))
barplot(display1, 
        main="% 0 as 0", xlab="Methods", ylab="Total", 
        names.arg=colnames, 
        col=c("blue","red"))
display1 <- c(b_tab_lda[3]/(b_tab_lda[1]+b_tab_lda[3]),
              p_tab_lda[3]/(p_tab_lda[1]+p_tab_lda[3]),
              b_tab_qda[3]/(b_tab_qda[1]+b_tab_qda[3]),
              p_tab_qda[3]/(b_tab_qda[1]+b_tab_qda[3]),
              b_train_tab_nb[3]/(b_train_tab_nb[1]+b_train_tab_nb[3]),
              p_train_tab_nb[3]/(p_train_tab_nb[1]+p_train_tab_nb[3]),
              b_test_tab_nb[3]/(b_test_tab_nb[1]+b_test_tab_nb[3]),
              p_test_tab_nb[3]/(p_test_tab_nb[1]+p_test_tab_nb[3]),
              b_tab_rf[3]/(b_tab_rf[1]+b_tab_rf[3]),
              p_tab_rf[3]/(p_tab_rf[1]+p_tab_rf[3]),
              b_train_tab_mlp[3]/(b_train_tab_mlp[1]+b_train_tab_mlp[3]),
              p_train_tab_mlp[3]/(p_train_tab_mlp[1]+p_train_tab_mlp[3]),
              b_test_tab_mlp[3]/(b_test_tab_mlp[1]+b_test_tab_mlp[3]),
              p_test_tab_mlp[3]/(p_test_tab_mlp[1]+p_test_tab_mlp[3]))
barplot(display1, 
        main="% 0 as 1", xlab="Methods", ylab="Total", 
        names.arg=colnames, 
        col=c("blue","red"))
display1 <- c(b_tab_lda[2]/(b_tab_lda[2]+b_tab_lda[4]),
              p_tab_lda[2]/(p_tab_lda[2]+p_tab_lda[4]),
              b_tab_qda[2]/(b_tab_qda[2]+b_tab_qda[4]),
              p_tab_qda[2]/(b_tab_qda[2]+b_tab_qda[4]),
              b_train_tab_nb[2]/(b_train_tab_nb[2]+b_train_tab_nb[4]),
              p_train_tab_nb[2]/(p_train_tab_nb[2]+p_train_tab_nb[4]),
              b_test_tab_nb[2]/(b_test_tab_nb[2]+b_test_tab_nb[4]),
              p_test_tab_nb[2]/(p_test_tab_nb[2]+p_test_tab_nb[4]),
              b_tab_rf[2]/(b_tab_rf[2]+b_tab_rf[4]),
              p_tab_rf[2]/(p_tab_rf[2]+p_tab_rf[4]),
              b_train_tab_mlp[2]/(b_train_tab_mlp[2]+b_train_tab_mlp[4]),
              p_train_tab_mlp[2]/(p_train_tab_mlp[2]+p_train_tab_mlp[4]),
              b_test_tab_mlp[2]/(b_test_tab_mlp[2]+b_test_tab_mlp[4]),
              p_test_tab_mlp[2]/(p_test_tab_mlp[2]+p_test_tab_mlp[4]))
barplot(display1, 
        main="% 1 as 0", xlab="Methods", ylab="Total", 
        names.arg=colnames, 
        col=c("blue","red"))
display1 <- c(b_tab_lda[4]/(b_tab_lda[2]+b_tab_lda[4]),
              p_tab_lda[4]/(p_tab_lda[2]+p_tab_lda[4]),
              b_tab_qda[4]/(b_tab_qda[2]+b_tab_qda[4]),
              p_tab_qda[4]/(b_tab_qda[2]+b_tab_qda[4]),
              b_train_tab_nb[4]/(b_train_tab_nb[2]+b_train_tab_nb[4]),
              p_train_tab_nb[4]/(p_train_tab_nb[2]+p_train_tab_nb[4]),
              b_test_tab_nb[4]/(b_test_tab_nb[2]+b_test_tab_nb[4]),
              p_test_tab_nb[4]/(p_test_tab_nb[2]+p_test_tab_nb[4]),
              b_tab_rf[4]/(b_tab_rf[2]+b_tab_rf[4]),
              p_tab_rf[4]/(p_tab_rf[2]+p_tab_rf[4]),
              b_train_tab_mlp[4]/(b_train_tab_mlp[2]+b_train_tab_mlp[4]),
              p_train_tab_mlp[4]/(p_train_tab_mlp[2]+p_train_tab_mlp[4]),
              b_test_tab_mlp[4]/(b_test_tab_mlp[2]+b_test_tab_mlp[4]),
              p_test_tab_mlp[4]/(p_test_tab_mlp[2]+p_test_tab_mlp[4]))
barplot(display1, 
        main="% 1 as 1", xlab="Methods", ylab="Total", 
        names.arg=colnames, 
        col=c("blue","red"))

