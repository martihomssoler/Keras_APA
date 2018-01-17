library(MASS)
library(nnet)
#Executem per una llavor en concret, per tenir els mateixos resultats.
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

## We start using logistic regression (a linear classifier)

model.logreg <- glm (morefifty~., data=adults[learn,], family=binomial)

summary(model.logreg)

# Simplify the model using the AIC

model.logreg2 <- step(model.logreg)

# The new model has one variable less and the error (residual deviance) is virtually the same (313.5 vs 312.2)

# Interpretation of the coefficients

exp(model.logreg2$coefficients)

# Calculation of apparent error in the training set (learn)

glfpred=NULL
glfpred[model.logreg2$fitted.values<0.5]=0
glfpred[model.logreg2$fitted.values>=0.5]=1
table(adults$morefifty[learn],glfpred)
error_rate.learn <- 100*(1-sum(diag(table(adults$morefifty[learn],glfpred)))/nlearn)
error_rate.learn

## we get a learning error which is quite high (28.84%)

# Estimation of prediction error using the test set

glft = predict(model.logreg2, newdata=adults[-learn,]) 
pt = 1/(1+exp(-glft))
glfpredt = NULL
glfpredt[pt<0.5]=0
glfpredt[pt>=0.5]=1
table(adults$morefifty[-learn],glfpredt)
error_rate.test <- 100*(1-sum(diag(table(adults$morefifty[-learn],glfpredt)))/ntest)
error_rate.test

## we get a prediction error of 27.07%

### Now we switch to non-linear modelling with a MLP

## The nnet() function is quite powerful and very reliable from the optimization
## point of view, including a L2-regularization mechanism. 
## From the computational point of view, it has two drawbacks:

## 1- it does not have a built-in mechanism for multiple runs or cross-validation
## 2- it only admits networks of one hidden layer (of size 'size')

## Please have a look at nnet before going any further

?nnet

## The basic parameters are 'size' and 'decay' (the regularization constant = lambda)
## As usual, R detects it is a two-class classification problem because 'admit' is a 2-way factor
## It therefore builds a MLP with one output neuron, with the logistic function
## and uses the cross-entropy as error function

## Let's start by scaling of inputs, this is important to avoid network 'stagnation' (premature convergence)

Admis$gpa <- scale(Admis$gpa)
Admis$gre <- scale(Admis$gre)
Admis$rank <- scale(Admis$rank)

## To illustrate the first results, we just fit a MLP with 2 hidden neurons

model.nnet <- nnet(morefifty ~., data = adults, subset=learn, size=2, maxit=200, decay=0)

## Take your time to understand the output
model.nnet 

## In particular, understand why the total number of weights is 11, what 'initial  value' and 'final  value' are
## and what does 'converged' mean

# This is the final value of the error function (also known as fitting criterion)
model.nnet$value

#  fitted values for the training data
model.nnet$fitted.values

# and the residuals
model.nnet$residuals

## Now look at the weights

model.nnet$wts

## I think this way is clearer:

summary(model.nnet)

## i1,i2,i3 are the 3 inputs, h1, h2 are the two hidden neurons, b is the bias (offset)

## As you can see, some weights are large (two orders of magnitude larger then others)
## This is no good, since it makes the model unstable (i.e., small changes in some inputs may
## entail significant changes in the network, because of the large weights)

## One way to avoid this is by regularizing the learning process:

model.nnet <- nnet(morefifty ~., data = adults, subset=learn, size=2, maxit=200, decay=0.01)

## notice the big difference
model.nnet$wts

summary(model.nnet)

# Now let's compute the training error

p1 <- as.factor(predict (model.nnet, type="class"))

t1 <- table(p1,adults$morefifty[learn])
error_rate.learn <- 100*(1-sum(diag(t1))/nlearn)
error_rate.learn

# And the corresponding test error

p2 <- as.factor(predict (model.nnet, newdata=adults[-learn,], type="class"))

t2 <- table(p2,adults$morefifty[-learn])
error_rate.test <- 100*(1-sum(diag(t2))/ntest)
error_rate.test

## We get 26.32%, so it seems that the MLP helps a little bit; however, we need to work harder

## We are going to do the modelling in a principled way now. Using 10x10 CV to select the best
## combination of 'size' and 'decay'

## Just by curiosity, let me show you that we can fit almost any dataset (in the sense of reducing the training error):

model.nnet <- nnet(morefifty ~., data = adults, subset=learn, size=20, maxit=200)

# Now let's compute the training error

p1 <- as.factor(predict (model.nnet, type="class"))

(t1 <- table(p1,adults$morefifty[learn]))
error_rate.learn <- 100*(1-sum(diag(t1))/nlearn)
error_rate.learn

# And the corresponding test error

p2 <- as.factor(predict (model.nnet, newdata=adults[-learn,], type="class"))

(t2 <- table(p2,adults$morefifty[-learn]))
error_rate.test <- 100*(1-sum(diag(t2))/ntest)
error_rate.test
