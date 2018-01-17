set.seed(1313)
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



N <- nrow(adults)
all.indexes <- 1:N

learn.indexes <- sample(1:N, round(2*N/3))
test.indexes <- all.indexes[-learn.indexes]

learn.data <- adults[learn.indexes,]

nlearn <- length(learn.indexes)
ntest <- N - nlearn

harm <- function (a,b) { 2/(1/a+1/b) }

library(randomForest)
set.seed(1313)
model.rf1 <- randomForest(as.factor(morefifty) ~ ., data = learn.data, ntree=100, proximity=FALSE)

model.rf1


pred.rf1 <- predict (model.rf1, adults[test.indexes,], type="class")

(ct <- table(Truth=adults$morefifty[test.indexes], Pred=pred.rf1))

prop.table(ct, 1)
# total percent correct
sum(diag(ct))/sum(ct)


round(100*(1-sum(diag(ct))/sum(ct)),2)

(F1 <- harm (prop.table(ct,1)[1,1], prop.table(ct,1)[2,2]))

set.seed(1313)
# one way to deal with this is to include class weights
model.rf2 <- randomForest(as.factor(morefifty) ~ ., data = learn.data, ntree=100, proximity=FALSE, classwt=c(1,10))

model.rf2


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


(ntrees <- seq(1,10,by=1))

rf.results <- matrix (rep(0,2*length(ntrees)),nrow=length(ntrees))
colnames (rf.results) <- c("weight", "OOB")
rf.results[,"ntrees"] <- ntrees
rf.results[,"OOB"] <- 0

ii <- 1

for (nt in ntrees)
{ 
  print(nt)
  set.seed(1313)
  model.rf <- randomForest(morefifty ~ ., data = learn.data, ntree=398, proximity=FALSE, 
                           classwt=c(1,nt), strata=learn.data$morefifty)
  
  # get the OOB
  rf.results[ii,"OOB"] <- model.rf$err.rate[nt,1]
  
  ii <- ii+1
}

plot(rf.results,type="o", col="blue")

# choose best value of 'ntrees'

lowest.OOB.error <- as.integer(which.min(rf.results[,"OOB"]))
(ntrees.best <- rf.results[lowest.OOB.error,"ntrees"])
set.seed(1313)
# one way to deal with this is to include class weights
model.rf2 <- randomForest(as.factor(morefifty) ~ ., data = learn.data, ntree=398, proximity=FALSE, classwt=c(1,2))

model.rf2


pred.rf2 <- predict (model.rf2, adults[test.indexes,], type="class")

(ct <- table(Truth=adults$morefifty[test.indexes], Pred=pred.rf2))

# percent by class
prop.table(ct, 1)
# total percent correct
1-sum(diag(ct))/sum(ct)
