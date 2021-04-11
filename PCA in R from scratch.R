# Generating data
library('MASS')
mu=rep(3,4)
sigma=matrix(.9, nrow=4, ncol=4) + diag(4)*0.1
set.seed(2021)
data <- as.data.frame(mvrnorm(20, mu = mu, Sigma = sigma), 
                      empirical = T)
y <- apply(data, 1, sum)+rnorm(20, 1, 1)
cor(data)


#Custom PCA
data.scaled <-  scale(data, scale=F) #Center the data
data.cov <- cov(data.scaled) #Var/Covar matrix
data.eigen <- eigen(data.cov) #Eigen vectors/values
data.custom.pca <- data.scaled%*%(data.eigen$vectors) #Custom PC's
data.custom.eigenvector <- t(t(data.custom.pca) %*% as.matrix(data.scaled) %*% solve(t(data.scaled) %*% as.matrix(data.scaled)))
round(cor(data.custom.pca),5)

data.eigen$values/sum(data.eigen$values)
cumsum(data.eigen$values)/sum(data.eigen$values)


#Using package
data.pca <- prcomp(data)
data.pca$x
data.pca$rotation
data.pca$sdev^2
summary(data.pca)




#Custom OLS
x <- as.matrix(cbind(1,data))
solve(t(x)%*%x) %*% t(x) %*% y

#Using in-built function
model <- lm(y~x[,2:5])
model$coefficients




#PCR
#combining components to the data set
data.new <-  cbind(data, data.custom.pca, data.pca$x, y)
colnames(data.new)[5:8] <- c('cust1', 'cust2', 'cust3', 'cust4')

#regression modeling
model.cust <- lm(y~cust1+cust2+cust3+cust4, data.new)
model.pca <- lm(y~PC1+PC2+PC3+PC4, data.new)

#converting coefficients from PC's to original
beta.pca <- as.matrix(model.pca$coefficients[2:5])
beta.original.pca <- as.matrix(data.pca$rotation) %*% beta.pca

beta.cust.pca <- as.matrix(model.cust$coefficients[2:5])
beta.original.cust <- as.matrix(data.custom.eigenvector) %*% beta.cust.pca



#PCR using package
library(pls)

#Generating testing data
set.seed(2021)
fit <- pcr(y ~.,  data = as.data.frame(data.scaled), center=F)
fit$coefficients[1:4, 1, 4]

data.test <- scale(as.data.frame(mvrnorm(20, mu = mu, Sigma = sigma), 
                           empirical = T), scale = F)
y.test <- y <- apply(data.test, 1, sum)+rnorm(20, 1, 1)

#Predictions using both models
y.pred.test1 <- predict(fit,newdata=data.test)
dim(y.pred.test1)
y.pred.test1 <- y.pred.test1[1:20,1, 4]
dim(y.pred.test1) <- c(20,1)


y.pred.test2 <- as.matrix(data.test) %*% beta.X1



#Different number of components
model.pca2 <- lm(y~PC1+PC2+PC3, data.new)
beta.Z2 <- as.matrix(model.pca2$coefficients[2:4])
V2 <- as.matrix(data.pca$rotation[,1:3])
beta.X2 <- V2 %*% beta.Z2
beta.X2
y.pred.test3 <- as.matrix(data.test) %*% beta.X2
y.pred.test1[1:20,1, 3]



#how many PC needed?
plot(data.pca, type='l')
fit2 <- pcr(y ~., data = as.data.frame(data.scaled), center=F,
            validation="CV")
validationplot(fit, val.type="MSEP")

plot(fit, "validation", val.type = "MSEP")
plot(data.pca, type='l')












