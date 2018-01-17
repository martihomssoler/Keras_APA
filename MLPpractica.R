library(MASS)
library(nnet)

set.seed(145678987)
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




learn <- sample(1:N, round(2*N/3))

nlearn <- length(learn)
ntest <- N - nlearn

###################################################
(nsize <- seq(1,30,by=1))

rf.results <- matrix (rep(0,2*length(nsize)),nrow=length(nsize))
colnames (rf.results) <- c("nsize", "OOB")
rf.results[,"nsize"] <- nsize
rf.results[,"OOB"] <- 0

ii <- 1

for (nt in nsize)
{ 
  print(nt)
  set.seed(145678987)
  model.nnet <- nnet(morefifty ~., data = adults, subset=learn, size=nt, maxit=200, decay=0.01)
  p2 <- as.factor(predict (model.nnet, newdata=adults[-learn,], type="class"))
  t2 <- table(p2,adults$morefifty[-learn])
  error_rate.test <- (1-sum(diag(t2))/ntest)
  error_rate.test
  # get the OOB
  rf.results[ii,"OOB"] <- error_rate.test
  
  ii <- ii+1
}

plot(rf.results,type="o", col="blue")

# choose best value of 'nsize'

lowest.OOB.error <- as.integer(which.min(rf.results[,"OOB"]))
(nsize.best <- rf.results[lowest.OOB.error,"nsize"])
###################################################
set.seed(145678987)
model.nnet <- nnet(morefifty ~., data = adults, subset=learn, size=nsize.best, maxit=500, decay=0.01)

# Now let's compute the training error

p1 <- as.factor(predict (model.nnet, type="class"))
t1 <- table(p1,adults$morefifty[learn])
t1
error_rate.learn <- 100*(1-sum(diag(t1))/nlearn)
error_rate.learn

# And the corresponding test error

p2 <- as.factor(predict (model.nnet, newdata=adults[-learn,], type="class"))

t2 <- table(p2,adults$morefifty[-learn])
t2
error_rate.test <- 100*(1-sum(diag(t2))/ntest)
error_rate.test

