#-------------------------DATA ACQUISITION-------------------------

#librerie utilizzate
library(psych)
library(yarrr)
library(corrplot)
library(GGally)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(class)
library(gmodels)
library(shiny)
#library(HistogramTools) 

#importo i dataset
data1 <- read.csv("test.csv", header=T)
data2 <-  read.csv("train.csv", header=T)
dataset <- merge(data1, data2, all=TRUE, sort=TRUE) 

#visione generale dataset
head(dataset)
tail(dataset)

#caratteristiche dataset
str(dataset) 
summary(dataset)
describe(dataset)

#conversione da tipo int a tipo factor
dataset$profile.pic <- as.factor(dataset$profile.pic)
#dataset$fullname.words <- as.factor(dataset$fullname.words) la variabile verrà 
#convertià in seguito
dataset$name..username <- as.factor(dataset$name..username)
dataset$external.URL <- as.factor(dataset$external.URL)
dataset$private <- as.factor(dataset$private)
dataset$fake <- as.factor(dataset$fake)

#verifica presenza valori null
sum(is.na(dataset$profile.pic) == TRUE)
require(Amelia)
missmap(dataset, main="Missing Map")
complete.cases(dataset)

#-------------------------DATA EXPLORATION-------------------------

#esplorazione e analisi singole variabili

#funzione per calolcare la moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#dataset$profile.pic
table(dataset$profile.pic)

plot(dataset$profile.pic, ylim=c(0,500), yaxt="n", col=c("red", "blue"))
axis(2, at=seq(0, 500, by = 50), labels=seq(0, 500, by = 50))
legend("topleft", inset=.02, c("ABSENCE PROFILE PIC", "PRESENCE PROFILE PIC"), 
       fill=c( "red", "blue"), horiz=FALSE)

#dataset$nums.length.username
max(dataset$nums.length.username)
min(dataset$nums.length.username)
mean(dataset$nums.length.username)
median(sort(dataset$nums.length.username))
getmode(dataset$nums.length.username)
print(nrow(dataset[dataset$nums.length.username == 0, ]))
var(dataset$nums.length.username)


hist(dataset$nums.length.username, xaxt="n", breaks = seq(0, 1, by=0.05), 
     ylim=c(1,400), col="orange", main="Distribuition of nums.length.username", 
     xlab = "nums.length.username")
axis(1, at=seq(0, 1, by = 0.05), labels=seq(0, 1, by = 0.05))

par(mfrow=c(2,1))

boxplot(dataset$nums.length.username, col="orange", 
        main="Boxplot of nums.length.username", xaxt="n", horizontal=TRUE)
axis(1, at=seq(0, 1, by = 0.05), labels=seq(0, 1, by = 0.05))

hist(dataset$nums.length.username, xaxt="n", breaks = seq(0, 1, by=0.05), 
     ylim=c(1,400), col="orange", main="Distribuition of nums.length.username", 
     xlab = "nums.length.username")
axis(1, at=seq(0, 1, by = 0.05), labels=seq(0, 1, by = 0.05))

par(mfrow=c(1,1))

#dataset$fullname.words
max(dataset$fullname.words)
min(dataset$fullname.words)
mean(dataset$fullname.words)
median(dataset$fullname.words)
getmode(dataset$fullname.words)
var(dataset$fullname.words)

dataset$fullname.words <- as.factor(dataset$fullname.words)
table(dataset$fullname.words)

plot(dataset$fullname.words, col="orange", yaxt="n", ylim=c(0,400), 
     main="Distribuition of fullname.words")
axis(2, at=seq(0, 400, by = 50), labels=seq(0, 400, by = 50))

#dataset$nums.length.fullname
max(dataset$nums.length.fullname)
min(dataset$nums.length.fullname)
mean(dataset$nums.length.fullname)
median(sort(dataset$nums.length.fullname))
getmode(dataset$nums.length.fullname)
print(nrow(dataset[dataset$nums.length.fullname == 0, ]))
var(dataset$nums.length.fullname)

hist(dataset$nums.length.fullname, xaxt="n", yaxt="n", breaks = seq(0, 1, 
                                                                    by=0.05), 
     col="orange", main="Distribuition of nums.length.fullname", 
     xlab = "nums.length.fullname")
axis(1, at=seq(0, 1, by = 0.05), labels=seq(0, 1, by = 0.05))
axis(2, at=seq(0, 700, by = 50), labels=seq(0, 700, by = 50))

#dataset$name..username
table(dataset$name..username)

plot(dataset$name..username, ylim=c(0,700), yaxt="n", col=c("red", "blue"))
axis(2, at=seq(0, 700, by = 50), labels=seq(0, 700, by = 50))
legend("topright", inset=.02, c("NAME != USERNAME", "NAME = USERNAME"), 
       fill=c( "red", "blue"), horiz=FALSE)

#dataset$description.length
max(dataset$description.length)
min(dataset$description.length)
mean(dataset$description.length)
getmode(dataset$description.length)
print(nrow(dataset[dataset$description.length == 0, ]))

hist(dataset$description.length, breaks = seq(0, 150, by=5), xaxt="n", 
     yaxt="n", col="orange", main="Distribuition of description.length", 
     xlab = "description.length")
axis(1, at=seq(0, 150, by = 5), labels=seq(0, 150, by = 5))
axis(2, at=seq(0, 500, by = 25), labels=seq(0, 500, by = 25))

par(mfrow=c(2,1))

boxplot(dataset$description.length, col="orange", 
        main="Box plot of description.length", xaxt="n", horizontal=TRUE)
axis(1, at=seq(0, 150, by = 5), labels=seq(0, 150, by = 5))

hist(dataset$description.length, breaks = seq(0, 150, by=5), xaxt="n", yaxt="n", 
     col="orange", main="Distribuition of description.length", 
     xlab = "description.length")
axis(1, at=seq(0, 150, by = 5), labels=seq(0, 150, by = 5))
axis(2, at=seq(0, 500, by = 25), labels=seq(0, 500, by = 25))

par(mfrow=c(1,1))

#dataset$external.URL
table(dataset$external.URL)

plot(dataset$external.URL, ylim=c(0,700), yaxt="n", col=c("red", "blue"))
axis(2, at=seq(0, 700, by = 50), labels=seq(0, 700, by = 50))
legend("topright", inset=.02, c("ABSENCE EXT URL", "PRESENCE EXT URL"), 
       fill=c( "red", "blue"), horiz=FALSE)

#dataset$private
table(dataset$private)

plot(dataset$private, ylim=c(0,500), yaxt="n", col=c("red", "blue"))
axis(2, at=seq(0, 500, by = 50), labels=seq(0, 500, by = 50))
legend("topright", inset=.02, c("PUBLIC", "PRIVATE"), fill=c( "red", "blue"), 
       horiz=FALSE)

#dataset$X.posts
max(dataset$X.posts)
min(dataset$X.posts)
getmode(dataset$X.posts)
print(nrow(dataset[dataset$X.posts == 0, ]))

hist(dataset$X.posts, breaks= seq(0,8000, by=100), xaxt="n", yaxt="n", 
     col="orange", main="Distribuition of X.posts", xlab = "X.posts")
axis(1, at=seq(0, 8000, by = 100), labels=seq(0, 8000, by = 100))
axis(2, at=seq(0, 700, by = 50), labels=seq(0, 700, by = 50))

#zoom
X.posts_FOCUS <- subset(dataset$X.posts, dataset$X.posts < 300)
#round(mean(X.posts_FOCUS)) media calcolata per questo sottoinsieme
print(nrow(dataset[dataset$X.posts < 300, ]))

hist(X.posts_FOCUS, breaks= seq(0,300, by=25), ylim=c(0,450), xaxt="n", 
     yaxt="n", col="orange", main="FOCUS Distribuition of X.posts", 
     xlab = "X.posts")
axis(1, at=seq(0, 300, by = 25), labels=seq(0, 300, by = 25))
axis(2, at=seq(0, 450, by = 50), labels=seq(0, 450, by = 50))

print(nrow(dataset[dataset$X.posts < 25, ]))

boxplot(X.posts_FOCUS, col="orange", horizontal=TRUE, xaxt="n", 
        main="Box plot of X.posts < 300")
axis(1, at=seq(0, 300, by = 10), labels=seq(0, 300, by = 10))
print(summary(X.posts_FOCUS))

#ulteriore zoom
X.posts_new_FOCUS <- subset(dataset$X.posts, dataset$X.posts < 25)
#round(mean(X.posts_new_FOCUS)) media calcolata per questo sottoinsieme

hist(X.posts_new_FOCUS, breaks= seq(0,25, by=2), ylim=c(0,250), xaxt="n", 
     yaxt="n", col="orange", main="ANOTHER FOCUS Distribuition of X.posts", 
     xlab = "X.posts")
axis(1, at=seq(0, 25, by = 2), labels=seq(0, 25, by = 2))
axis(2, at=seq(0, 250, by = 50), labels=seq(0, 250, by = 50))

print(nrow(dataset[dataset$X.posts < 2, ])) 

#dataset$X.followers
max(dataset$X.followers)
min(dataset$X.followers)
getmode(dataset$X.followers)
print(nrow(dataset[dataset$X.followers == 0, ]))

hist(dataset$X.followers, breaks= seq(0,15500000, by=500000), col="orange", 
     main="Distribuition of X.followers", xlab = "X.followers", xaxt="n", 
     yaxt="n")
axis(1, at=seq(0, 15000000, by = 500000), labels=seq(0, 15000000, by = 500000))
axis(2, at=seq(0, 700, by = 50), labels=seq(0, 700, by = 50))

#zoom
X.followers_FOCUS <- subset(dataset$X.followers, dataset$X.followers < 700)
#round(mean(X.followers_FOCUS)) media calcolata per questo sottoinsieme
print(nrow(dataset[dataset$X.followers < 700, ]))

hist(X.followers_FOCUS, breaks= seq(0, 700, by=100), ylim=c(0,300), xaxt="n", 
     yaxt="n", col="orange", main="FOCUS Distribuition of X.followers", 
     xlab = "X.followers")
axis(1, at=seq(0, 700, by = 25), labels=seq(0, 700, by = 25))
axis(2, at=seq(0, 300, by = 50), labels=seq(0, 300, by = 50))

print(nrow(dataset[dataset$X.followers < 100, ]))

boxplot(X.followers_FOCUS, col="orange", horizontal=TRUE, xaxt="n", 
        main="Box plot of X.followers < 700")
axis(1, at=seq(0, 700, by = 25), labels=seq(0, 700, by = 25))
print(summary(X.followers_FOCUS))

#ulteriore zoom
X.followers_new_FOCUS <- subset(dataset$X.followers, dataset$X.followers < 100)
#round(mean(X.followers_new_FOCUS)) media calcolata per questo sottoinsieme

hist(X.followers_new_FOCUS, breaks= seq(0,100, by=10), ylim=c(0,60), xaxt="n", 
     yaxt="n", col="orange", main="ANOTHER FOCUS Distribuition of X.followers", 
     xlab = "X.followers")
axis(1, at=seq(0, 100, by = 10), labels=seq(0, 100, by = 10))
axis(2, at=seq(0, 60, by = 10), labels=seq(0, 60, by = 10))

print(nrow(dataset[dataset$X.followers < 10, ])) 

#dataset$X.follows
max(dataset$X.follows)
min(dataset$X.follows)
getmode(dataset$X.follows)
print(nrow(dataset[dataset$X.follows == 0, ]))

hist(dataset$X.follows, breaks= seq(0,7550, by=150), ylim=c(0,300), col="orange"
     ,main="Distribuition of X.follows", xlab = "X.follows", xaxt="n", yaxt="n")
axis(1, at=seq(0, 7550, by = 150), labels=seq(0, 7550, by = 150))
axis(2, at=seq(0, 280, by = 20), labels=seq(0, 280, by = 20))

#zoom
X.follows_FOCUS <- subset(dataset$X.follows, dataset$X.follows < 700)
#round(mean(X.follows_FOCUS)) media calcolata per questo sottoinsieme
print(nrow(dataset[dataset$X.follows < 700, ]))

hist(X.follows_FOCUS, breaks= seq(0, 700, by=100), ylim=c(0,250), xaxt="n", 
     yaxt="n", col="orange", main="FOCUS Distribuition of X.follows", 
     xlab = "X.follows")
axis(1, at=seq(0, 700, by = 25), labels=seq(0, 700, by = 25))
axis(2, at=seq(0, 250, by = 50), labels=seq(0, 250, by = 50))

print(nrow(dataset[dataset$X.follows < 100, ]))

boxplot(X.follows_FOCUS, col="orange", horizontal=TRUE, xaxt="n", 
        main="Box plot of X.follows < 700")
axis(1, at=seq(0, 700, by = 25), labels=seq(0, 700, by = 25))
print(summary(X.follows_FOCUS))

#ulteriore zoom
X.follows_new_FOCUS <- subset(dataset$X.follows, dataset$X.follows < 100)
#round(mean(X.follows_new_FOCUS)) media calcolata per questo sottoinsieme

hist(X.follows_new_FOCUS, breaks= seq(0,100, by=10), ylim=c(0,50), xaxt="n", 
     yaxt="n", col="orange", main="ANOTHER FOCUS Distribuition of X.follows", 
     xlab = "X.follows")
axis(1, at=seq(0, 100, by = 10), labels=seq(0, 100, by = 10))
axis(2, at=seq(0, 50, by = 10), labels=seq(0, 50, by = 10))

print(nrow(dataset[dataset$X.follows < 10, ])) 

#dataset$fake
table(dataset$fake)

plot(dataset$fake, ylim=c(0,500), yaxt="n", col=c("red", "blue"))
axis(2, at=seq(0, 500, by = 50), labels=seq(0, 500, by = 50))
legend("topright", inset=.02, c("REAL", "FAKE"), fill=c( "red", "blue"), 
       horiz=FALSE)

#-------------------------RELATIONSHIP BETWEEN VARIABLES-------------------------

#relazioni tra varibaile fake e restanti variabili categoriche
relationship_fake <- function (a, b, c, d){
  table <- table(dataset$fake, b, dnn=c("FAKE", a))
  colnames(table) <- c(c, d)
  rownames(table) <- c("REAL", "FAKE")
  print(table)
  barplot(height = table, beside=TRUE, ylab = "NUMBER OF OBSERVATIONS",  
          legend.text = TRUE, col = c(transparent("red", .2), 
                                      transparent("blue", .2)),
          args.legend = list(x = "topright", bty = "n"))
}

#relazione tra fake e profile.pic
relationship_fake("PROFILE PIC", dataset$profile.pic, "ABSENCE PROFILE PIC", 
           "PRESENCE PROFILE PIC")

#relazione tra fake e private
relationship_fake("PRIVATE", dataset$private, "PUBLIC", "PRIVATE")

#relazione tra fake external.url
relationship_fake("EXT URL", dataset$external.URL, "ABSENCE EXT URL", 
           "PRESENCE EXT URL")

#relazione tra fake name..username
relationship_fake("NAME..USERNAME", dataset$name..username, "NAME!=USERNAME", 
                  "NAME==USERNAME")

#Scatterplot matrices
pairs(formula = ~ dataset$profile.pic + log(dataset$nums.length.username) + 
        dataset$fullname.words + log(dataset$nums.length.fullname) + 
        dataset$name..username + log(dataset$description.length) + 
        dataset$external.URL + dataset$private + log(dataset$X.posts) + 
        log(dataset$X.followers) + log(dataset$X.follows) + 
        dataset$fake, data = dataset, col="red")

#Scatterplot matrices senza considerare le variabili categoriche
pairs(formula = ~ log(dataset$nums.length.username) + 
        log(dataset$nums.length.fullname) + log(dataset$description.length) + 
        log(dataset$X.posts) + log(dataset$X.followers) + log(dataset$X.follows)
      ,data = dataset, col="red")

#scatterplots delle variabili maggiormente correlate

#nums.length.username e nums.length.fullname
plot(log(dataset$nums.length.username), log(dataset$nums.length.fullname), 
     col="red", main="nums.length.username and nums.length.fullname")

#X.posts e X.followers
plot(log(dataset$X.posts), log(dataset$X.followers), col="red", 
     main="X.posts and X.followers")

#X.followers e fake
plot(log(dataset$X.followers), dataset$fake, col="red", yaxt="n", 
     main="X.followers and fake")
axis(2, at=seq(0, 2, by = 1), labels=c(0, 0, 1))
#plot(dataset$fake, log(dataset$X.followers), col="red")

#X.follows e fake
plot(log(dataset$X.follows), dataset$fake, col="red", yaxt="n", 
     main="X.follows and fake")
axis(2, at=seq(0, 2, by = 1), labels=c(0, 0, 1))
#plot(dataset$fake, log(dataset$X.follows), col="red")

#X.posts e fake
plot(log(dataset$X.posts), dataset$fake, col="red", yaxt="n", 
     main="X.posts and fake")
axis(2, at=seq(0, 2, by = 1), labels=c(0, 0, 1))
#plot(dataset$fake, log(dataset$X.posts), col="red")

#correlazione tra variabili non categoriche
ggcorr(dataset, label = TRUE, label_round = 3)

#alternativamente al garfico precedente
selection <- c("nums.length.username","nums.length.fullname",
"description.length","X.posts","X.followers", "X.follows")
dataset_non_categ <- subset(dataset, select = selection )
c <- cor(dataset_non_categ)
corrplot(c,  method = "circle")
corrplot.mixed(c)

#-------------------------DECISION TREE CLASSIFICATION-------------------------

#albero di classificazione

#funzione per dividere il dataset
split.data = function(data, p = 0.7, s = 1){
  set.seed(s)
  index = sample(1:dim(data)[1])
  train = data[index[1:floor(dim(data)[1] * p)], ]
  test = data[index[((ceiling(dim(data)[1] * p))+1):dim(data)[1]], ]
  return(list(train=train, test=test))
}

#creazione del training set (70%) e del test set (30%)
allset_tree= split.data(dataset, p = 0.7)
trainset_tree= allset_tree$train
testset_tree= allset_tree$test

#addestramento
decisionTree = rpart(fake ~ ., data=trainset_tree, method="class")

#albero di classificazione
plot(decisionTree)
text(decisionTree)

#un'alternativa migliore per visualizzare l'albero
fancyRpartPlot(decisionTree)

#testing
testset_tree$Prediction <- predict(decisionTree, testset_tree, type = "class")
#la colonna "testset_tree$Prediction" conterrà le predizioni che sono state 
#fatte mediante il modello precedentemente costruito. Posso confrontorla con la 
#colonna dei veri valori, cioè fake$prediction

#matrice di confusione 
CrossTable(x = testset_tree$fake, y = testset_tree$Prediction, 
           prop.chisq = FALSE)

#matrice di confusione in modo alternativo
confusion.matrix_tree = table(testset_tree$fake, testset_tree$Prediction)
rownames(confusion.matrix_tree) <- c("Actually real", "Actually fake")
colnames(confusion.matrix_tree) <- c("Predicted real", "Predicted fake")
confusion.matrix_tree

#Evaluation Measure for classification

#accuracy
accuracy_tree <- sum(diag(confusion.matrix_tree))/sum(confusion.matrix_tree)

#precision
precision_tree <- confusion.matrix_tree[1,1]/sum(confusion.matrix_tree[,1])

#recall
recall_tree <- confusion.matrix_tree[1,1]/sum(confusion.matrix_tree[1,])

#F-measure
f_tree <- 2 * (precision_tree * recall_tree) / (precision_tree + recall_tree)

#funzione per stampare le varie misure 
print_eval_measure_tree <- function(){
  cat(paste("Accuracy:\t", format(accuracy_tree, digits=4), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision_tree, digits=4), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall_tree, digits=4), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f_tree, digits=4), "\n",sep=" "))
}

print_eval_measure_tree()

#albero potato: valore di cp che minimizza xerror

#complexity parameter 
printcp(decisionTree) 
plotcp(decisionTree, col = "red")
summary(decisionTree)

#decisionTree$cptable[which.min(decisionTree$cptable[,"xerror"]),"CP"]

prunedTree = prune(decisionTree, cp=0.017)

fancyRpartPlot(prunedTree)

#-------------------------K-NEAREST NEIGHBORS CLASSIFICATION-------------------

#knn: algoritmo differente per classificazione

#funzione per normalizzare i dati
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

nums.length.username_norm <- normalize(dataset$nums.length.username)
nums.length.fullname_norm <- normalize(dataset$nums.length.fullname)
description.length_norm <- normalize(dataset$description.length)
X.posts_norm <- normalize(dataset$X.posts)
X.posts_followers <- normalize(dataset$X.followers)
X.posts_follows <- normalize(dataset$X.follows)

#dataframe "normalizzato"
dataset_norm <- data.frame(dataset$profile.pic, nums.length.username_norm, 
                           dataset$fullname.words, nums.length.fullname_norm, 
                           dataset$name..username, description.length_norm,
                           dataset$external.URL, dataset$private, X.posts_norm,
                           X.posts_followers, X.posts_follows, dataset$fake)

#creazione del training set (70%) e del test set (30%)
allset_knn <- split.data(dataset_norm, p = 0.7)

#dati relativi alla target variable
trainset_labels_knn <- allset_knn$train[,12]
testset_labels_knn <- allset_knn$test[,12]

#training set
trainset_knn <- allset_knn$train[,-12]

#testing set
testset_knn <- allset_knn$test[,-12]

#applico knn, k è scelto come la radice quadrata del numero delle osservazioni 
#del training set
pred_knn <- knn(trainset_knn, testset_knn, cl=trainset_labels_knn, k=22)

#matrice di confusione
CrossTable(x = testset_labels_knn, y = pred_knn, prop.chisq = FALSE)

#matrice di confusione in modo alternativo
confusion.matrix_knn = table(testset_labels_knn, pred_knn)
rownames(confusion.matrix_knn) <- c("Actually real", "Actually fake")
colnames(confusion.matrix_knn) <- c("Predicted real", "Predicted fake")
confusion.matrix_knn

#Evaluation Measure for classification

#accuracy
accuracy_knn <- sum(diag(confusion.matrix_knn))/sum(confusion.matrix_knn)

#precision
precision_knn <- confusion.matrix_knn[1,1]/sum(confusion.matrix_knn[,1])

#recall
recall_knn <- confusion.matrix_knn[1,1]/sum(confusion.matrix_knn[1,])

#F-measure
f_knn <- 2 * (precision_knn * recall_knn) / (precision_knn + recall_knn)

#funzione per stampare le varie misure
print_eval_measure_knn <- function(){
  cat(paste("Accuracy:\t", format(accuracy_knn, digits=4), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision_knn, digits=4), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall_knn, digits=4), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f_knn, digits=4), "\n",sep=" "))
}

print_eval_measure_knn()

#-------------------------R SHINY APP-------------------

runApp("app.R")
