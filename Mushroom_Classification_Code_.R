getwd()
setwd("C:/Users/choun/OneDrive/Desktop/Assignment/ASL_R/MushroomDataset")
#mushroom <- read.csv("secondary_data.csv",header=TRUE,sep=';',quote="\n")

# Load required packages
library(mice)
library(dplyr)
library(tidyr)
library(pROC)
library(car)
library(dplyr)
library(purrr)

### Reading the csv file
mushroom <- read.csv("secondary_data_Copy.csv")
mmm <-  read.csv("secondary_data_Copy.csv")

###  Convert the categorical into factors
mmm[,!sapply(mmm,is.numeric)] <- lapply(mmm[,!sapply(mmm,is.numeric)],factor)

### Datatype of each column
str(mushroom)

### Dimension of DataFrame
dim(mmm)

mushroom[,!sapply(mushroom, is.numeric)] <- lapply(mushroom[,!sapply(mushroom, is.numeric)],factor)
str(mushroom)
mush_fact <- mushroom

### Finding whether data contain the null values or empty values
for (col in colnames(mmm)) {
  empty_count <- sum(mmm[[col]] == "")
  null_count <- sum(is.na(mmm[[col]]))
  print(paste(col, "has", empty_count, "empty values and", null_count, "null values"))
}

### Find the levels/categories in each variables
for(col in colnames(mmm)){
  print(paste("Column Name : ",col))
  print(paste("No. of Categories : ",length(levels(mmm[[col]]))))
  print(paste("Types : "))
  print(levels(mmm[[col]]))
  print(paste("------------------------------------------------------------"))
}


for (var in colnames(mushroom[,-c(2,10,11)])) {
  cat("Unique categories before encoding:\n")
  print(unique(mushroom[,var]))
  
  # perform label encoding using the caret package
  mushroom[,var] <- as.numeric(as.factor(mushroom[,var]))
  
  # print the unique categories in the variable after encoding
  cat("\nUnique categories after encoding:\n")
  print(unique(mushroom[,var]))
}

#### Correlation between all variables
corr<- cor(mushroom)
library(ggcorrplot)
ggcorrplot(corr, type = "upper", hc.order = TRUE, lab = TRUE)
mushroom$class <- as.factor(mushroom$class)
str(mushroom)

lm_cn <- glm(class~. , family = binomial, data = mush_fact)






# Load the mushroom dataset
mush <- mushroom

# Check for missing values
sum(is.na(mush))

# Replace "?" with NA values
mush[mush == ""] <- NA

# Convert categorical variables to factors
mush <- mush %>% mutate_if(is.character, as.factor)

set.seed(123)
# Perform multiple imputation for missing values
mushrooms_imp <- mice(mush, m = 5, maxit = 20, method = "pmm")

# Check for missing values after imputation
sum(is.na(mushrooms_imp))

# Replace the original dataset with the imputed dataset
mushrooms <- complete(mushrooms_imp)

# Save the imputed dataset to a CSV file
write.csv(mushrooms, file = "mushrooms_imputed_1.csv", row.names = FALSE)



m_f <- mush_fact



mush_fact <- mushrooms
#### Conerting into factors
mush_fact$class <- as.factor(mush_fact$class)
mush_fact$cap.shape <- as.factor(mush_fact$cap.shape)
mush_fact$cap.surface <- as.factor(mush_fact$cap.surface)
mush_fact$cap.color <- as.factor(mush_fact$cap.color)
mush_fact$does.bruise.or.bleed <- as.factor(mush_fact$does.bruise.or.bleed)
mush_fact$gill.attachment <- as.factor(mush_fact$gill.attachment)
mush_fact$gill.spacing <- as.factor(mush_fact$gill.spacing)
mush_fact$gill.color <- as.factor(mush_fact$gill.color)
mush_fact$stem.root <- as.factor(mush_fact$stem.root)
mush_fact$stem.surface <- as.factor(mush_fact$stem.surface)
mush_fact$stem.color <- as.factor(mush_fact$stem.color)
mush_fact$veil.type <- as.factor(mush_fact$veil.type)
mush_fact$veil.color <- as.factor(mush_fact$veil.color)
mush_fact$has.ring <- as.factor(mush_fact$has.ring)
mush_fact$ring.type <- as.factor(mush_fact$ring.type)
mush_fact$spore.print.color <- as.factor(mush_fact$spore.print.color)
mush_fact$habitat <- as.factor(mush_fact$habitat)
mush_fact$season <- as.factor(mush_fact$season)

#mush_fact[,-c(2,10,11)] <- as.factor(mush_fact[,-c(2,10,11)])

### Assigning 0,1 in e,p
levels(mush_fact$class) <- c(0,1) 


### Splitting the data into 70:30 
set.seed(123)
trainIndex <- sample(1:nrow(mush_fact), round(0.7 * nrow(mush_fact)), replace = FALSE)
train <- mush_fact[trainIndex, ]
test <- mush_fact[-trainIndex, ]


#####--------------- Testing with Reduced Model ####-------------------------------
## Logistic Regression
train$class <- as.factor(train$class)
#lm_cn <- glm(class~.-stem.root-gill.attachment-stem.surface-gill.spacing-veil.type , family = binomial, data = train)   #-stem.color-stem.width-veil.type-does.bruise.or.bleed-cap.surface-veil.color-stem.root
lm_cn <- glm(class ~ . - stem.root - gill.attachment - stem.surface - gill.spacing - veil.type, family = binomial, data = train)

alias(lm_cn)
summary(lm_cn)
P_logis <- predict(lm_cn, newdata = test, type = "response")
C_logis <- rep(0, length(test$class))
table(C_logis, test$class)
C_logis[P_logis>0.5] <- 1
table(C_logis, test$class)

##### Navie Bayes
library(e1071)
M_NB <- naiveBayes(class~.-stem.root-gill.attachment-stem.surface-gill.spacing-veil.type, data = train)
summary(M_NB)
M_NB
P_NB <- predict(M_NB, newdata = test, type = "raw")
P_NB
C_NB <- predict(M_NB, newdata = test, type = "class")
table(C_NB, test$class)

#LDA
library(MASS)
M_LDA <- lda(class~.-stem.root-gill.attachment-stem.surface-gill.spacing-veil.type, data = train)
P_LDA <- predict(M_LDA,test)$posterior
C_LDA <- predict(M_LDA,test)$class
table(C_LDA, test$class)

## Random Forest

library(randomForest)
M_RF <- randomForest(class ~ .-stem.root-gill.attachment-stem.surface-gill.spacing-veil.type, data = train, ntree = 100, importance = TRUE)
P_RF <- predict(M_RF, newdata = test, type="response")
table(P_RF,test$class)


library(ROCR)
prd_logis <- prediction(P_logis, test$class)
perf_logis <- performance(prd_logis,"tpr","fpr")
par(new=TRUE)
plot(perf_logis, col = "red",
     asp = 1, lwd = 2,
     main = paste0("ROC curves"))
abline(h = 0:5*.2, v = 0:5*.2, lty = 2)

prd_NB <- prediction(P_NB[,2], test$class)
perf_NB <- performance(prd_NB,"tpr","fpr")
par(new=TRUE)
plot(perf_NB, col = "black",
     asp = 1, lwd = 2,
     main = paste0("ROC curves"))
abline(h = 0:5*.2, v = 0:5*.2, lty = 2)

library(randomForest)
prd_RF <- prediction(as.numeric(P_RF), as.numeric(test$class))
perf_RF <- performance(prd_RF,"tpr","fpr")
dev.off()
plot(perf_RF, col = "darkgreen",
     asp = 1, lwd = 2,
     main = paste0("ROC curves"))
abline(h = 0:5*.2, v = 0:5*.2, lty = 2)

prd_LDA <- prediction(P_LDA[,2], test$class)
perf_LDA <- performance(prd_LDA,"tpr","fpr")
par(new=TRUE)
plot(perf_LDA, col = "blue", asp =1, lwd = 2)



AUC_logis <- performance(prd_logis,"auc")@y.values[[1]]
AUC_NB <- performance(prd_NB,"auc")@y.values[[1]]
AUC_LDA <- performance(prd_LDA,"auc")@y.values[[1]]
AUC_RF <- performance(prd_RF,"auc")@y.values[[1]]
legend(.5, .3, legend = c("AUC scores:",
                          paste0("by logis: ", round(AUC_logis, digits=4)),
                          paste0("by NB: ", round(AUC_NB, digits=4)),
                          paste0("by LDA: ", round(AUC_LDA, digits=4)),
                          paste0("by RF: ", round(AUC_RF, digits=4))
),
col = c(NA,"red" , "black", "blue","darkgreen"),
lwd = 2, 
cex = 0.7)

#####------------------ Training with Reduced Model ####------------------------------- 

## Logistic Regression
lm_cn <- glm(class~. - stem.root - gill.attachment - stem.surface - gill.spacing - veil.type , family = binomial, data = train)   #-stem.color-stem.width-veil.type-does.bruise.or.bleed-cap.surface-veil.color-stem.root
alias(lm_cn)
summary(lm_cn)
P_logis <- predict(lm_cn, newdata = train, type = "response")
C_logis <- rep(0, length(train$class))
table(C_logis, train$class)
C_logis[P_logis>0.5] <- 1
table(C_logis, train$class)

##### Navie Bayes
library(e1071)
M_NB <- naiveBayes(class~.-stem.root-gill.attachment-stem.surface-gill.spacing-veil.type, data = train)
summary(M_NB)
M_NB
P_NB <- predict(M_NB, newdata = train, type = "raw")
P_NB
C_NB <- predict(M_NB, newdata = train, type = "class")
table(C_NB, train$class)

#LDA
library(MASS)
M_LDA <- lda(class~.-stem.root-gill.attachment-stem.surface-gill.spacing-veil.type, data = train)
P_LDA <- predict(M_LDA)$posterior
C_LDA <- predict(M_LDA)$class
table(C_LDA, train$class)

## Random Forest
library(randomForest)
M_RF <- randomForest(class ~ .-stem.root-gill.attachment-stem.surface-gill.spacing-veil.type, data = train, ntree = 100, importance = TRUE)
P_RF <- predict(M_RF, newdata = train)
table(P_RF,train$class)



library(ROCR)
prd_logis <- prediction(P_logis, train$class)
perf_logis <- performance(prd_logis,"tpr","fpr")
dev.off()
plot(perf_logis, col = "red",
     asp = 1, lwd = 2,
     main = paste0("ROC curves"))
abline(h = 0:5*.2, v = 0:5*.2, lty = 2)

prd_NB <- prediction(P_NB[,2], train$class)
perf_NB <- performance(prd_NB,"tpr","fpr")
par(new=TRUE)
plot(perf_NB, col = "black",
     asp = 1, lwd = 2,
     main = paste0("ROC curves"))
abline(h = 0:5*.2, v = 0:5*.2, lty = 2)

prd_RF <- prediction(as.numeric(P_RF), train$class)
perf_RF <- performance(prd_RF,"tpr","fpr")
par(new=TRUE)
plot(perf_RF, col = "darkgreen",
     asp = 1, lwd = 2,
     main = paste0("ROC curves"))
abline(h = 0:5*.2, v = 0:5*.2, lty = 2)

prd_LDA <- prediction(P_LDA[,2], train$class)
perf_LDA <- performance(prd_LDA,"tpr","fpr")
par(new=TRUE)
plot(perf_LDA, col = "blue", asp =1, lwd = 2)



AUC_logis <- performance(prd_logis,"auc")@y.values[[1]]
AUC_NB <- performance(prd_NB,"auc")@y.values[[1]]
AUC_LDA <- performance(prd_LDA,"auc")@y.values[[1]]
AUC_RF <- performance(prd_RF,"auc")@y.values[[1]]
legend(.5, .3, legend = c("AUC scores:",
                          paste0("by logis: ", round(AUC_logis, digits=4)),
                          paste0("by NB: ", round(AUC_NB, digits=4)),
                          paste0("by LDA: ", round(AUC_LDA, digits=4)),
                          paste0("by RF: ", round(AUC_RF, digits=4))
),
col = c(NA,"red" , "black", "blue", "darkgreen"),
lwd = 2, 
cex = 0.7)


######------------------- Reduced Model with Completed Data---------------####---------------------------

## Logistic Regression
lm_cn <- glm(class~. - stem.root - gill.attachment - stem.surface - gill.spacing - veil.type , family = binomial, data = mush_fact)   #-stem.color-stem.width-veil.type-does.bruise.or.bleed-cap.surface-veil.color-stem.root
alias(lm_cn)
summary(lm_cn)
P_logis <- predict(lm_cn, newdata = mush_fact, type = "response")
C_logis <- rep(0, length(mush_fact$class))
table(C_logis, mush_fact$class)
C_logis[P_logis>0.5] <- 1
table(C_logis, mush_fact$class)

##### Navie Bayes
library(e1071)
M_NB <- naiveBayes(class~.-stem.root-gill.attachment-stem.surface-gill.spacing-veil.type, data = mush_fact)
summary(M_NB)
M_NB
P_NB <- predict(M_NB, newdata = mush_fact, type = "raw")
P_NB
C_NB <- predict(M_NB, newdata = mush_fact, type = "class")
table(C_NB, mush_fact$class)

#LDA
library(MASS)
M_LDA <- lda(class~.-stem.root-gill.attachment-stem.surface-gill.spacing-veil.type, data = mush_fact)
P_LDA <- predict(M_LDA)$posterior
C_LDA <- predict(M_LDA,mush_fact)$class
table(C_LDA, mush_fact$class)

## Random Forest

library(randomForest)
M_RF <- randomForest(class ~ .-stem.root-gill.attachment-stem.surface-gill.spacing-veil.type, data = mush_fact, ntree = 100, importance = TRUE)
P_RF <- predict(M_RF, newdata = mush_fact)
table(P_RF,mush_fact$class)


library(ROCR)
prd_logis <- prediction(P_logis, mush_fact$class)
perf_logis <- performance(prd_logis,"tpr","fpr")
dev.off()
plot(perf_logis, col = "red",
     asp = 1, lwd = 2,
     main = paste0("ROC curves"))
abline(h = 0:5*.2, v = 0:5*.2, lty = 2)

prd_NB <- prediction(P_NB[,2], mush_fact$class)
perf_NB <- performance(prd_NB,"tpr","fpr")
par(new=TRUE)
plot(perf_NB, col = "black",
     asp = 1, lwd = 2,
     main = paste0("ROC curves"))
abline(h = 0:5*.2, v = 0:5*.2, lty = 2)

prd_RF <- prediction(as.numeric(P_RF), as.numeric(mush_fact$class))
perf_RF <- performance(prd_RF,"tpr","fpr")
par(new=TRUE)
plot(perf_RF, col = "darkgreen",
     asp = 1, lwd = 2,
     main = paste0("ROC curves"))
abline(h = 0:5*.2, v = 0:5*.2, lty = 2)

prd_LDA <- prediction(P_LDA[,2], mush_fact$class)
perf_LDA <- performance(prd_LDA,"tpr","fpr")
par(new=TRUE)
plot(perf_LDA, col = "blue", asp =1, lwd = 2)



AUC_logis <- performance(prd_logis,"auc")@y.values[[1]]
AUC_NB <- performance(prd_NB,"auc")@y.values[[1]]
AUC_LDA <- performance(prd_LDA,"auc")@y.values[[1]]
#AUC_QDA <- performance(prd_QDA,"auc")@y.values[[1]]
AUC_RF <- performance(prd_RF,"auc")@y.values[[1]]
legend(.5, .3, legend = c("AUC scores:",
                          paste0("by logis: ", round(AUC_logis, digits=4)),
                          paste0("by NB: ", round(AUC_NB, digits=4)),
                          paste0("by LDA: ", round(AUC_LDA, digits=4)),
                          paste0("by RF: ", round(AUC_RF, digits=4))
),
col = c(NA,"red" , "black", "blue","darkgreen"),
lwd = 2, 
cex = 0.7)



#####-------------------Training with Full Model ####-------------------------------




## Logistic Regression
lm_cn <- glm(class~., family = binomial, data = train)   #-stem.color-stem.width-veil.type-does.bruise.or.bleed-cap.surface-veil.color-stem.root
summary(lm_cn)
P_logis <- predict(lm_cn, newdata = train, type = "response")
C_logis <- rep(0, length(train$class))
table(C_logis, train$class)
C_logis[P_logis>0.5] <- 1
table(C_logis, train$class)

##### Navie Bayes
library(e1071)
M_NB <- naiveBayes(class~., data = train)
summary(M_NB)
M_NB
P_NB <- predict(M_NB, newdata = train, type = "raw")
P_NB
C_NB <- predict(M_NB, newdata = train, type = "class")
table(C_NB, train$class)

#LDA
library(MASS)
M_LDA <- lda(class~., data = train)
P_LDA <- predict(M_LDA)$posterior
C_LDA <- predict(M_LDA)$class
table(C_LDA, train$class)

## Random Forest

library(randomForest)
M_RF <- randomForest(class ~ ., data = train, ntree = 100, importance = TRUE)
P_RF <- predict(M_RF, newdata = train)
table(P_RF,train$class)



library(ROCR)
prd_logis <- prediction(P_logis, train$class)
perf_logis <- performance(prd_logis,"tpr","fpr")
dev.off()
plot(perf_logis, col = "red",
     asp = 1, lwd = 2,
     main = paste0("ROC curves"))
abline(h = 0:5*.2, v = 0:5*.2, lty = 2)

prd_NB <- prediction(P_NB[,2], train$class)
perf_NB <- performance(prd_NB,"tpr","fpr")
par(new=TRUE)
plot(perf_NB, col = "black",
     asp = 1, lwd = 2,
     main = paste0("ROC curves"))
abline(h = 0:5*.2, v = 0:5*.2, lty = 2)

prd_RF <- prediction(as.numeric(P_RF), as.numeric(train$class))
perf_RF <- performance(prd_RF,"tpr","fpr")
par(new=TRUE)
plot(perf_RF, col = "darkgreen",
     asp = 1, lwd = 2,
     main = paste0("ROC curves"))
abline(h = 0:5*.2, v = 0:5*.2, lty = 2)

prd_LDA <- prediction(P_LDA[,2], train$class)
perf_LDA <- performance(prd_LDA,"tpr","fpr")
par(new=TRUE)
plot(perf_LDA, col = "blue", asp =1, lwd = 2)


AUC_logis <- performance(prd_logis,"auc")@y.values[[1]]
AUC_NB <- performance(prd_NB,"auc")@y.values[[1]]
AUC_LDA <- performance(prd_LDA,"auc")@y.values[[1]]
#AUC_QDA <- performance(prd_QDA,"auc")@y.values[[1]]
AUC_RF <- performance(prd_RF,"auc")@y.values[[1]]
legend(.5, .3, legend = c("AUC scores:",
                          paste0("by logis: ", round(AUC_logis, digits=4)),
                          paste0("by NB: ", round(AUC_NB, digits=4)),
                          paste0("by LDA: ", round(AUC_LDA, digits=4)),
                          paste0("by RF: ", round(AUC_RF, digits=4))
),
col = c(NA,"red" , "black", "blue", "darkgreen"),
lwd = 2, 
cex = 0.7)







###----------------------------Testing full model----------------------



## Logistic Regression
lm_cn <- glm(class~., family = binomial, data = train)   #-stem.color-stem.width-veil.type-does.bruise.or.bleed-cap.surface-veil.color-stem.root
alias(lm_cn)
summary(lm_cn)
P_logis <- predict(lm_cn, newdata = test, type = "response")
C_logis <- rep(0, length(test$class))
table(C_logis, test$class)
C_logis[P_logis>0.5] <- 1
table(C_logis, test$class)

##### Navie Bayes
library(e1071)
M_NB <- naiveBayes(class~., data = train)
summary(M_NB)
M_NB
P_NB <- predict(M_NB, newdata = test, type = "raw")
P_NB
C_NB <- predict(M_NB, newdata = test, type = "class")
table(C_NB, test$class)

#LDA
library(MASS)
M_LDA <- lda(class~., data = train)
P_LDA <- predict(M_LDA,test)$posterior
C_LDA <- predict(M_LDA,test)$class
table(C_LDA, test$class)

## Random Forest

library(randomForest)
M_RF <- randomForest(class ~ ., data = train, ntree = 100, importance = TRUE)
P_RF <- predict(M_RF, newdata = test)
table(P_RF,test$class)



library(ROCR)
prd_logis <- prediction(P_logis, test$class)
perf_logis <- performance(prd_logis,"tpr","fpr")
dev.off()

plot(perf_logis, col = "red",
     asp = 1, lwd = 2,
     main = paste0("ROC curves"))
abline(h = 0:5*.2, v = 0:5*.2, lty = 2)

prd_NB <- prediction(P_NB[,2], test$class)
perf_NB <- performance(prd_NB,"tpr","fpr")
par(new=TRUE)
plot(perf_NB, col = "black",
     asp = 1, lwd = 2,
     main = paste0("ROC curves"))
abline(h = 0:5*.2, v = 0:5*.2, lty = 2)

prd_RF <- prediction(as.numeric(P_RF), test$class)
perf_RF <- performance(prd_RF,"tpr","fpr")
par(new=TRUE)
plot(perf_RF, col = "darkgreen",
     asp = 1, lwd = 2,
     main = paste0("ROC curves"))
abline(h = 0:5*.2, v = 0:5*.2, lty = 2)

prd_LDA <- prediction(P_LDA[,2], test$class)
perf_LDA <- performance(prd_LDA,"tpr","fpr")
par(new=TRUE)
plot(perf_LDA, col = "blue", asp =1, lwd = 2)


AUC_logis <- performance(prd_logis,"auc")@y.values[[1]]
AUC_NB <- performance(prd_NB,"auc")@y.values[[1]]
AUC_LDA <- performance(prd_LDA,"auc")@y.values[[1]]
AUC_RF <- performance(prd_RF,"auc")@y.values[[1]]
legend(.5, .3, legend = c("AUC scores:",
                          paste0("by logis: ", round(AUC_logis, digits=4)),
                          paste0("by NB: ", round(AUC_NB, digits=4)),
                          paste0("by LDA: ", round(AUC_LDA, digits=4)),
                          paste0("by RF: ", round(AUC_RF, digits=4))
),
col = c(NA,"red" , "black", "blue", "darkgreen"),
lwd = 2, 
cex = 0.7)




####------------------ full model with complete Data----------------------



## Logistic Regression
lm_cn <- glm(class~., family = binomial, data = mush_fact )   #-stem.color-stem.width-veil.type-does.bruise.or.bleed-cap.surface-veil.color-stem.root
alias(lm_cn)
summary(lm_cn)
P_logis <- predict(lm_cn, newdata = mush_fact, type = "response")
C_logis <- rep(0, length(mush_fact$class))
table(C_logis, mush_fact$class)
C_logis[P_logis>0.5] <- 1
table(C_logis, mush_fact$class)

##### Navie Bayes
library(e1071)
M_NB <- naiveBayes(class~., data = mush_fact)
summary(M_NB)
M_NB
P_NB <- predict(M_NB, newdata = mush_fact, type = "raw")
P_NB
C_NB <- predict(M_NB, newdata = mush_fact, type = "class")
table(C_NB, mush_fact$class)

#LDA
library(MASS)
M_LDA <- lda(class~., data = mush_fact)
P_LDA <- predict(M_LDA,mush_fact)$posterior
C_LDA <- predict(M_LDA,mush_fact)$class
table(C_LDA, mush_fact$class)

## Random Forest

library(randomForest)
M_RF <- randomForest(class ~ ., data = mush_fact, ntree = 100, importance = TRUE)
P_RF <- predict(M_RF, newdata = mush_fact)
table(P_RF,mush_fact$class)



library(ROCR)
prd_logis <- prediction(P_logis, mush_fact$class)
perf_logis <- performance(prd_logis,"tpr","fpr")
dev.off()
plot(perf_logis, col = "red",
     asp = 1, lwd = 2,
     main = paste0("ROC curves"))
abline(h = 0:5*.2, v = 0:5*.2, lty = 2)

prd_NB <- prediction(P_NB[,2], mush_fact$class)
perf_NB <- performance(prd_NB,"tpr","fpr")
par(new=TRUE)
plot(perf_NB, col = "black",
     asp = 1, lwd = 2,
     main = paste0("ROC curves"))
abline(h = 0:5*.2, v = 0:5*.2, lty = 2)

prd_RF <- prediction(as.numeric(P_RF), mush_fact$class)
perf_RF <- performance(prd_RF,"tpr","fpr")
par(new=TRUE)
plot(perf_RF, col = "darkgreen",
     asp = 1, lwd = 2,
     main = paste0("ROC curves"))
abline(h = 0:5*.2, v = 0:5*.2, lty = 2)

prd_LDA <- prediction(P_LDA[,2], mush_fact$class)
perf_LDA <- performance(prd_LDA,"tpr","fpr")
par(new=TRUE)
plot(perf_LDA, col = "blue", asp =1, lwd = 2)


AUC_logis <- performance(prd_logis,"auc")@y.values[[1]]
AUC_NB <- performance(prd_NB,"auc")@y.values[[1]]
AUC_LDA <- performance(prd_LDA,"auc")@y.values[[1]]
AUC_RF <- performance(prd_RF,"auc")@y.values[[1]]
legend(.5, .3, legend = c("AUC scores:",
                          paste0("by logis: ", round(AUC_logis, digits=4)),
                          paste0("by NB: ", round(AUC_NB, digits=4)),
                          paste0("by LDA: ", round(AUC_LDA, digits=4)),
                          paste0("by RF: ", round(AUC_RF, digits=4))
),
col = c(NA,"red" , "black", "blue", "darkgreen"),
lwd = 2, 
cex = 0.7)


