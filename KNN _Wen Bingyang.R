train<-read.csv("mnist_train.csv")
test<-read.csv("mnist_test.csv")
train_a<-train[,c(2:785)]
cl<-train[,1]


library(class)

knn_1<-knn(train_a, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)
knn_3<-knn(train_a, test, cl, k = 3, l = 0, prob = FALSE, use.all = TRUE)
knn_5<-knn(train_a, test, cl, k = 5, l = 0, prob = FALSE, use.all = TRUE)
knn_7<-knn(train_a, test, cl, k = 7, l = 0, prob = FALSE, use.all = TRUE)
knn_9<-knn(train_a, test, cl, k = 9, l = 0, prob = FALSE, use.all = TRUE)
knn_11<-knn(train_a, test, cl, k = 11, l = 0, prob = FALSE, use.all = TRUE)
knn_21<-knn(train_a, test, cl, k = 21, l = 0, prob = FALSE, use.all = TRUE)
knn_31<-knn(train_a, test, cl, k = 31, l = 0, prob = FALSE, use.all = TRUE)
knn_41<-knn(train_a, test, cl, k = 41, l = 0, prob = FALSE, use.all = TRUE)
knn_51<-knn(train_a, test, cl, k = 51, l = 0, prob = FALSE, use.all = TRUE)

knn1<- as.numeric(as.character(knn_1))
knn3<- as.numeric(as.character(knn_3))
knn5<- as.numeric(as.character(knn_5))
knn7<- as.numeric(as.character(knn_7))
knn9<- as.numeric(as.character(knn_9))
knn11<- as.numeric(as.character(knn_11))
knn21<- as.numeric(as.character(knn_21))
knn31<- as.numeric(as.character(knn_31))
knn41<- as.numeric(as.character(knn_41))
knn51<- as.numeric(as.character(knn_51))

target<-read.csv("test_label.csv")
 

 d1=knn1-target
 d3=knn3-target
 d5=knn5-target
 d7=knn7-target
 d9=knn9-target
 d11=knn11-target
 d21=knn21-target
 d31=knn31-target
 d41=knn41-target
 d51=knn51-target

 e1=length(which(d1!=0))/9999
 e3=length(which(d3!=0))/9999
 e5=length(which(d5!=0))/9999
 e7=length(which(d7!=0))/9999
 e9=length(which(d9!=0))/9999
 e11=length(which(d11!=0))/9999
 e21=length(which(d21!=0))/9999
 e31=length(which(d31!=0))/9999
 e41=length(which(d41!=0))/9999
 e51=length(which(d51!=0))/9999
 
 print(e1)
 print(e3)
 print(e5)
 print(e7)
 print(e9)
 print(e11)
 print(e21)
 print(e31)
 print(e41)
