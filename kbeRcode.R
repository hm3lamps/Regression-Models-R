rm(list=ls())	

library(readxl)
kbeData <- read_excel("kbeData.xlsx")

#Linear Regression
n = dim(kbeData)[1]
n1 = round(n/10)
set.seed(20180522)
flag = sort(sample(1:n,n1))
kbeTrain = kbeData[-flag,]
kbeTest = kbeData[flag,]

meanY = mean(kbeTrain$Returns)
meanX = colMeans(kbeTrain[2:10])
model1 = lm(kbeTrain$Returns ~ . , data = kbeTrain)
summary(model1)
MSEmod1 = mean( (resid(model1) )^2) 
MSEmod1
ytpred1 = predict(model1, kbeTest[, 2:10])
TE1 = mean((ytpred1 - kbeTest$Returns)^2)
TE1

#Best subset cases of 4, 5, 6  ------> Best One is 5
library(leaps)
model2leaps <- regsubsets(Returns ~ ., data= kbeTrain, nbest= 50, really.big= TRUE); 
model2leaps.models <- summary(model2leaps)$which
model2leaps.models.size <- as.numeric(attr(model2leaps.models, "dimnames")[[1]])
model2leaps.models.rss <- summary(model2leaps)$rss
plot(model2leaps.models.size, model2leaps.models.rss);
model2leaps.models.best.rss <- tapply(model2leaps.models.rss, model2leaps.models.size, min);
model2leaps0 <- lm( Returns ~ 1, data = kbeTrain); 
model2leaps.models.best.rss <- c( sum(resid(model2leaps0)^2), model2leaps.models.best.rss); 

#model size 4
op2 <- which(model2leaps.models.size == 4); 
flag2 <- op2[which.min(model2leaps.models.rss[op2])]; 
mod2autopick <- model2leaps.models[flag2,]; 
mod2X <- paste(names(mod2autopick)[mod2autopick][-1], collapse="+"); 
mod2form <- paste ("Returns ~", mod2X);
model2 <- lm(as.formula(mod2form), data= kbeTrain); 
summary(model2)
MSEmod2 <- mean(resid(model2)^2);
MSEmod2
pred2 <- predict(model2, kbeTest[, 2:10]);
TE2model4<-   mean((pred2 - kbeTest$Returns)^2);
TE2model4

#model size 5
op2 <- which(model2leaps.models.size == 5); 
flag2 <- op2[which.min(model2leaps.models.rss[op2])]; 
mod2autopick <- model2leaps.models[flag2,]; 
mod2X <- paste(names(mod2autopick)[mod2autopick][-1], collapse="+"); 
mod2form <- paste ("Returns ~", mod2X);
model2 <- lm(as.formula(mod2form), data= kbeTrain); 
summary(model2)
MSEmod2 <- mean(resid(model2)^2);
MSEmod2
pred2 <- predict(model2, kbeTest[, 2:10]);
TE2model5 <-   mean((pred2 - kbeTest$Returns)^2);
TE2model5

#model size 6
op2 <- which(model2leaps.models.size == 6); 
flag2 <- op2[which.min(model2leaps.models.rss[op2])]; 
mod2autopick <- model2leaps.models[flag2,]; 
mod2X <- paste(names(mod2autopick)[mod2autopick][-1], collapse="+"); 
mod2form <- paste ("Returns ~", mod2X);
model2 <- lm(as.formula(mod2form), data= kbeTrain); 
summary(model2)
MSEmod2 <- mean(resid(model2)^2);
MSEmod2
pred2 <- predict(model2, kbeTest[, 2:10]);
TE2model6 <-   mean((pred2 - kbeTest$Returns)^2);
TE2model6


##Stepwise Search AIC Criteria 
model3 <- step(lm(Returns ~ ., data = kbeTrain))
summary(model3)
MSEmod3 <- mean(resid(model3)^2);
MSEmod3
ytpred3 <- predict(model3, kbeTest[, 2:10])
TE3 <- mean( (ytpred3 - kbeTest$Returns)^2)
TE3

##Ridge Regression
library(MASS)
model4 <- lm.ridge(Returns ~., data = kbeTrain, lambda= seq(0,1,0.01))
plot(model4) 
lambdaopt <- which.min(model4$GCV)
model4.coef <- coef(model4)[lambdaopt,]
round(model4.coef, 6)
rig1coef <- model4$coef[,lambdaopt];
rig1intercepts <- model4$ym - sum(model4$xm * (rig1coef / model4$scales)); 
pred4a <- scale(kbeTrain[,2:10], center = F, scale = model4$scales)%*%  rig1coef + rig1intercepts;
MSEmod4 <- mean( (pred4a - kbeTrain$Returns)^2);
MSEmod4
pred4b <- scale(kbeTest[,2:10], center = F, scale = model4$scales)%*%  rig1coef + rig1intercepts;
TE4 <- mean( (pred4b - kbeTest$Returns)^2);
TE4

#Lasso not working

#Principal Component Analysis
kbeTrain_pca <- prcomp(kbeTrain[,2:10]);  
round(kbeTrain_pca$sdev,4)
plot(kbeTrain_pca$sdev,type="l", ylab="SD of PC", xlab="PC number")
model6 <- lm(Returns ~ kbeTrain_pca$x[,1:5], data = kbeTrain)
coef6 <- kbeTrain_pca$rot[,1:5] %*% model6$coef[-1]; 
coef6
beta0_mod6 = meanY - t(coef6) %*% meanX
beta0_mod6
library(pls)
yhat6 <- pcr(Returns ~ ., data=kbeTrain, validation="CV");
ypred6a = predict(yhat6, ncomp = 5, newdata = kbeTrain[2:10]);
MSEmod6 = mean( (ypred6a - kbeTrain$Returns)^2); 
MSEmod6; 
ypred6b <- predict(yhat6, ncomp = 5, newdata = kbeTest[2:10]); 
TE6 <- mean( (ypred6b - kbeTest$Returns)^2); 
TE6; 

#Partial Least Squares
library(pls)
model7 <- plsr(Returns ~ ., data = kbeTrain, validation="CV");
summary(model7)
mod7ncompopt <- which.min(model7$validation$adj)
mod7ncompopt
coef7 = coef(model7, ncomp=mod7ncompopt)
round(coef7,6)
beta0_mod7 = meanY - coef7 %*% meanX
beta0_mod7
ypred7a <- predict(model7, ncomp=mod7ncompopt, newdata = kbeTrain[2:10]);
MSEmod7 <- mean( (ypred7a - kbeTrain$Returns)^2); 
MSEmod7 
ypred7b <- predict(model7, ncomp=mod7ncompopt, newdata = kbeTest[2:10]);
TE7 <- mean( (ypred7b - kbeTest$Returns)^2); 
TE7



TE1
TE2model4
TE2model5
TE2model6
TE3
TE4
TE6
TE7

