library(boot)
library(MASS)
library(e1071)
#Load Dataset
heart <- read.csv("heart.csv")


#Give it a look over
View(heart)
names(heart)
dim(heart)
summary(heart)
#Check to see if there's any missing values
any(is.na(heart))

#Logistic Regression (87%)
glm.fits <- glm(HeartDisease ~ .,
  family = binomial, data = heart
)
cv.err <- cv.glm(heart, glm.fits)
cv.err$delta
summary(glm.fits)

glm.probs <- predict(glm.fits, type = "response")
glm.pred <- rep(0, 918)
glm.pred[glm.probs > .5] = 1

table(glm.pred, HeartDisease)
mean(glm.pred == HeartDisease)

#Linear Discriminant Analysis (87%)
lda.fit <- lda(HeartDisease ~ ., data = heart)
lda.fit
plot(lda.fit)

lda.pred <- predict(lda.fit, heart)
lda.class <- lda.pred$class
table(lda.class, HeartDisease)
mean(lda.class == HeartDisease)

#Quadradic Discriminant Analysis (86%)

qda.fit <- qda(HeartDisease ~ ., data = heart)
qda.fit

qda.class <- predict(qda.fit, heart)$class
table(qda.class, HeartDisease)
mean(qda.class == HeartDisease)

#Naive Bayes (86%)
nb.fit <- naiveBayes(HeartDisease ~ ., data = heart)
nb.fit

nb.class <- predict(nb.fit, heart)
table(nb.class, HeartDisease)
mean(nb.class == HeartDisease)


