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

adults <- read.table("out.txt", sep=",", dec=".", header=FALSE)

dim(adults)

colnames(adults) <- c('age','workclass','fnlwgt','education','education-num','marital-status','occupation',
                    'relationship','race','sex','capital-gain','capital-loss','hours-per-week','native-country','morefifty')

# Clean up column names
colnames(adults) <- make.names(colnames(adults))

adults$morefifty<- as.factor(adults$morefifty)

summary(adults)
## DO IT
plot(subset(adults,select=-morefifty),col=unclass(adults$morefifty))

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
legend('bottomright', c("Cultivar 1","Cultivar 2","Cultivar 3"), lty=1, col=c('black', 'red', 'green'), bty='n', cex=.75)

# If need be, we can add the (projected) means to the plot

plot.mean <- function (class)
{
  m1 <- mean(subset(adults.pred$x[,1],adults$morefifty==class))
  m2 <- mean(subset(adults.pred$x[,2],adults$morefifty==class))
  print(c(m1,m2))
  points(m1,m2,pch=16,cex=2,col=as.integer(class))
}

plot.mean ('1')
plot.mean ('2')
plot.mean ('3')

# indeed classification is perfect

table(adults$morefifty, adults.pred$class)

# Let us switch to leave-one-out cross-validation

adults.predcv <- update(lda.model,CV=TRUE)
head(adults.predcv$posterior)
print(table(adults$morefifty,adults.predcv$class))

# 2 mistakes (on 178 observations): 1.12% error

## Quadratic Discriminant Analysis is the same, replacing 'lda' by 'qda'
## problems may arise if for some class there are less (or equal) observations than dimensions
## (is not the case for the adults data)

qda.model <- qda (morefifty ~ ., data = adults)

qda.model

## There is no projection this time (because projection is a linear operator and the QDA boundaries are quadratic ones)

# but let's have a look at classification:

adults.pred <- predict(qda.model)
table(adults$morefifty, adults.pred$class)

# Let us switch to leave-one-out cross-validation

adults.predcv <- update(qda.model,CV=TRUE)
head(adults.predcv$posterior)

print(table(adults$morefifty,adults.predcv$class))

# 1 mistake (on 178 observations): 0.56% error

# it would be nice to ascertain which adults is the "stubborn" one: it is a adults of type '2' classified
# as class '1'. Maybe there is something special with this adults ...

# In the event of numerical errors (e.g., insufficient number of observations per class) and only in this case, we can use 'rda', as in:

library(klaR)
(rda.model <- rda (adults.type ~ ., data = adults,  lambda = 1))

# Note gamma=0, lambda=1 corresponds to LDA and gamma=0, lambda=0 to QDA

####################################################################
# Example 2: The Naïve Bayes classifier
####################################################################

library (e1071)

## Naive Bayes Classifier for Discrete Predictors: we use the 
## 1984 United States Congressional Voting Records; 

## This data set includes votes for each of the U.S. House of Representatives Congressmen on 16 key votes
## In origin they were nine different types of votes: 
##     * voted for, paired for, and announced for (these three simplified to yea or 'y'),
##     * voted against, paired against, and announced against (these three simplified to nay or 'n'), 
##     * voted present, voted present to avoid conflict of interest, and did not vote or otherwise make a position known 
##       (these three simplified to an 'unknown' disposition)

## The goal is to classify Congressmen as Republican or Democrat as a function of their voting profiles,
## which is not immediate because in the US Congressmen have a large freedom of vote 
## (obviously linked to their party but also to their own feelings, interests and compromises with voters)


## 1 = democrat, 0 = republican
## Note "unknown dispositions" have been treated as missing values!


N <- nrow(adults)

## We first split the available data into learning and test sets, selecting randomly 2/3 and 1/3 of the data
## We do this for a honest estimation of prediction performance

learn <- sample(1:N, round(2*N/3))

nlearn <- length(learn)
ntest <- N - nlearn

# First we build a model using the learn data

model <- naiveBayes(morefifty ~ ., data = adults[learn,])

# we get all the probabilities
model

# predict the outcome of the first 20 Congressmen
predict(model, adults[1:20,-1]) 

# same but displaying posterior probabilities
predict(model, adults[1:20,-1], type = "raw") 

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