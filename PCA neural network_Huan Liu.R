library(readr)

s<-Sys.time()

train <- read_csv("mnist_train.csv")
test <- read_csv("mnist_test.csv")

cat(sprintf("Training set has %d rows and %d columns\n", nrow(train), ncol(train)))
cat(sprintf("Test set has %d rows and %d columns\n", nrow(test), ncol(test)))

X <- train[,-1]
Y <- train[,1]
trainlabel <- train[,1]

#Reducing Train using PCA
Xreduced <- X/255             
Xcov <- cov(Xreduced)
pcaX <- prcomp(Xcov)

# Creating a datatable to store and plot the No of Principal Components vs Cumulative Variance Explained
vexplained <- as.data.frame(pcaX$sdev^2/sum(pcaX$sdev^2))
vexplained <- cbind(c(1:784),vexplained,cumsum(vexplained[,1]))
colnames(vexplained) <- c("No_of_Principal_Components","Individual_Variance_Explained","Cumulative_Variance_Explained")

#Plotting the curve using the datatable obtained
plot(vexplained$No_of_Principal_Components,vexplained$Cumulative_Variance_Explained, xlim = c(0,100),type='b',pch=16,xlab = "Principal Componets",ylab = "Cumulative Variance Explained",main = 'Principal Components vs Cumulative Variance Explained')

#Datatable to store the summary of the datatable obtained
vexplainedsummary <- vexplained[seq(0,100,5),]
vexplainedsummary

#Storing the vexplainedsummary datatable in png format for future reference.
library(gridExtra)
png("datatablevaraince explained.png",height = 800,width =1000)
p <-tableGrob(vexplainedsummary)
grid.arrange(p)
dev.off()
Xfinal <- as.matrix(Xreduced) %*% pcaX$rotation[,1:45]

#Making training labels as factors
trainlabel <- unlist(trainlabel)
trainlabel <- as.factor(trainlabel)
library(nnet)
Y <- unlist(Y)
Y <- class.ind(Y)
print(X[1:5,1:5])
print(Y[1:5,])

#choose no_of_nodes=150 and maxiter=100 (change it as a trade-off between running time and accuracy)
#Training the nnet on totat_training_set
finalseed <- 150
set.seed(finalseed)
model_final <- nnet(Xfinal,Y,size=150,softmax=TRUE,maxit=130,MaxNWts = 80000)



#Load test to reduced and normalize it for predictions
#Applying PCA to test set/Reducing test using PCA
testreduced <- test/255
testfinal <- as.matrix(testreduced) %*%  pcaX$rotation[,1:45]

#Calculating Final Accuracies
prediction <- predict(model_final,testfinal,type="class")
prediction <- as.data.frame(prediction);
finalprediction<- cbind(as.data.frame(1:nrow(prediction)),prediction);
colnames(finalprediction) <- c("ImageId","Label");
write.csv(finalprediction,file="predictions.csv",row.names=FALSE);

a<-read_csv("predictions.csv")
b<-read_csv("test_label.csv") 

a<-a[-1,-1]
diff<-a-b
error<-length(which(diff!=0))/9999
print(error)

a<-read_csv("predictions.csv")
b<-read_csv("test_label.csv") 

a<-a[,2]
a=a<-a[-1,]
a<-unlist(a)
b<-unlist(b)
table(pred=a,true=b)
e<-Sys.time()
time.taken<-e-s
print(time.taken)



# Generate output files with write_csv(), plot() or ggplot()