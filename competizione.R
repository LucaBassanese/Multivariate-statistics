setwd("C:\\Users\\Luca\\Desktop\\LUCA\\SCUOLA\\Magistrale Stat\\Statistica Multivariata\\Codici\\DatasetMulti")
library(MASS)
library(car)

library(cvTools)
library(ROSE)
library(tree)
library(corrplot)
load("train_credit")
load("test_credit")
str(train)
ripetizione_auc= function(x, k, LDA=TRUE){
  ids=sample(1:nrow(x), round(nrow(x)*.6), replace=FALSE)
  train=x[ids,]
  test=x[-ids,]
  a=combn(1:(ncol(x)-1), k)
  m=matrix(0, ncol = 2, nrow = ncol(a))
  colnames(m)=c("AUC", "Previsione")
  nomi=vector()
  for(i in 1:nrow(m)){
    nomi[i]=paste(a[,i], collapse = "-" )
  }
  nomi
  rownames(m)=nomi
  n=names(train)
  for(i in 1:ncol(a)){
    if(k==1){
      form=as.formula(paste(c(n[length(n)], "~", n[a[1,i]]),collapse = " "))
    }
    if(k==2){
      form=as.formula(paste(c(n[length(n)], "~", n[a[1,i]],  "+", n[a[2,i]] ),collapse = " "))
    }
    if(k==3){
      form=as.formula(paste(c(n[length(n)], "~", n[a[1,i]],  "+", n[a[2,i]],"+",
                              n[a[3,i]] ),collapse = " "))}
    if(k==4){
      form=as.formula(paste(c(n[length(n)], "~", n[a[1,i]],  "+", n[a[2,i]],"+",
                              n[a[3,i]],"+",n[a[4,i]] ),collapse = " "))}
    if(k==5){
      form=as.formula(paste(c(n[length(n)], "~", n[a[1,i]],  "+", n[a[2,i]],"+",
                              n[a[3,i]],"+",n[a[4,i]],"+", n[a[5,i]] ),collapse = " "))}
    if(k==6){
      form=as.formula(paste(c(n[length(n)], "~", n[a[1,i]],  "+", n[a[2,i]],"+",
                              n[a[3,i]],"+",n[a[4,i]],"+", n[a[5,i]], "+", n[a[6,i]] ),collapse = " "))}
    if(k==7){
      form=as.formula(paste(c(n[length(n)], "~", n[a[1,i]],  "+", n[a[2,i]],"+",
                              n[a[3,i]],"+",n[a[4,i]],"+", n[a[5,i]], "+", n[a[6,i]] , "+", n[a[7,i]]  ),collapse = " "))}
    if(k==8){
      form=as.formula(paste(c(n[length(n)], "~", n[a[1,i]],  "+", n[a[2,i]],"+",
                              n[a[3,i]],"+",n[a[4,i]],"+", n[a[5,i]], "+", 
                              n[a[6,i]] , "+", n[a[7,i]]  , "+", n[a[8,i]] ),collapse = " "))}
    if(k==9){
      form=as.formula(paste(c(n[length(n)], "~", n[a[1,i]],  "+", n[a[2,i]],"+",
                              n[a[3,i]],"+",n[a[4,i]],"+", n[a[5,i]], "+", 
                              n[a[6,i]] , "+", n[a[7,i]]  , "+", n[a[8,i]], "+", n[a[9,i]] ),collapse = " "))}
    if(k==10){
      form=as.formula(paste(c(n[length(n)], "~", n[a[1,i]],  "+", n[a[2,i]],"+",
                              n[a[3,i]],"+",n[a[4,i]],"+", n[a[5,i]], "+", 
                              n[a[6,i]] , "+", n[a[7,i]]  , "+", n[a[8,i]], "+", n[a[9,i]],
                              "+", n[a[10,i]]),collapse = " "))}
    if(k==11){
      form=as.formula(paste(c(n[length(n)], "~", n[a[1,i]],  "+", n[a[2,i]],"+",
                              n[a[3,i]],"+",n[a[4,i]],"+", n[a[5,i]], "+", 
                              n[a[6,i]] , "+", n[a[7,i]]  , "+", n[a[8,i]], "+", n[a[9,i]],
                              "+", n[a[10,i]],"+", n[a[11,i]]),collapse = " "))}
    if(k==12){
      form=as.formula(paste(c(n[length(n)], "~", n[a[1,i]],  "+", n[a[2,i]],"+",
                              n[a[3,i]],"+",n[a[4,i]],"+", n[a[5,i]], "+", 
                              n[a[6,i]] , "+", n[a[7,i]]  , "+", n[a[8,i]], "+", n[a[9,i]],
                              "+", n[a[10,i]],"+", n[a[11,i]],"+", n[a[12,i]]),collapse = " "))}
    if(k==13){
      form=as.formula(paste(c(n[length(n)], "~", n[a[1,i]],  "+", n[a[2,i]],"+",
                              n[a[3,i]],"+",n[a[4,i]],"+", n[a[5,i]], "+", 
                              n[a[6,i]] , "+", n[a[7,i]]  , "+", n[a[8,i]], "+", n[a[9,i]],
                              "+", n[a[10,i]],"+", n[a[11,i]],"+", n[a[12,i]],"+", n[a[13,i]]),collapse = " "))}
    if(k==14){
      form=as.formula(paste(c(n[length(n)], "~", n[a[1,i]],  "+", n[a[2,i]],"+",
                              n[a[3,i]],"+",n[a[4,i]],"+", n[a[5,i]], "+", 
                              n[a[6,i]] , "+", n[a[7,i]]  , "+", n[a[8,i]], "+", n[a[9,i]],
                              "+", n[a[10,i]],"+", n[a[11,i]],"+", n[a[12,i]],"+", n[a[13,i]]
                              ,"+", n[a[14,i]]), collapse = " "))}
    if(k==15){
      form=as.formula(paste(c(n[length(n)], "~", n[a[1,i]],  "+", n[a[2,i]],"+",
                              n[a[3,i]],"+",n[a[4,i]],"+", n[a[5,i]], "+", 
                              n[a[6,i]] , "+", n[a[7,i]]  , "+", n[a[8,i]], "+", n[a[9,i]],
                              "+", n[a[10,i]],"+", n[a[11,i]],"+", n[a[12,i]],"+", n[a[13,i]]
                              ,"+", n[a[14,i]],"+", n[a[15,i]]), collapse = " "))}
    if(k>15){return("Troppi k")}
    if(LDA)
    {fit_lda<-lda(form,data=train)}
    else
    {fit_lda<-qda(form,data=train)}
    prevtes=predict(fit_lda, newdata=test)
    
    d=table(prevtes$class, test[, ncol(test)])
    m[i,1]=roc.curve(test[, length(n)], prevtes$class, plotit=F)$auc
    m[i,2]=sum(diag(d))/nrow(test)
    
  }
  return(m) 
}
risultato=ripetizione_auc(train, 1)
risultato
library(corrplot)
table(train$default.payment.next.month)

#analisi
C <- cor(train[,-24])
corrplot(C, type = "upper", method = "number", diag = FALSE, tl.srt=90)#in tl.srt mi dice l'angolatura

x11()
C <- cor(test)
corrplot(C, type = "upper", method = "number", diag = FALSE, tl.srt=30)
#si potrebbe escludere pay_4e 5 e bill dall'1 al 4


#Vediamo la distribuzione delle esplicative condizionatamente alla risposta (ovviamente solo nel training!)
##elimino la variabile colore e la risposta
names.cont.vars <- names(train)[-24]
for(i in names.cont.vars)
{
  boxplot(train[,i]~train$default.payment.next.month, xlab="Quality", ylab=i)
  cat ("Premere [invio] per continuare")
  readline()#ci permette di fare invio per passare da un grafico all'altro
}
#Pay_0, #pay_amt sono simili
#si vedono abbastanza outlier, con il grafico successivo elimino gli outlier
for(i in names.cont.vars)
{
  #outline=F non visualizza gli outliers nei boxplot (opzione per migliorare la visualizzazione)
  boxplot(train[,i]~train$default.payment.next.month, xlab="Default", ylab=i, outline=F)
  cat ("Premere [invio] per continuare")
  readline()
}







#equilibriamo
ids0 <- which(train$default.payment.next.month==0)
ids1 <- which(train$default.payment.next.month==1)
N_TOT <- length(ids1)
ids0_train <- sample(ids0, N_TOT, rep=F)
ids1_train <- sample(ids1, N_TOT, rep=F)
trainequi=train[c(ids0_train,ids1_train),]



risultato=ripetizione_auc(trainequi, 2, LDA=FALSE)
scelta=apply(risultato,MARGIN = 2, FUN = which.max)#6
apply(risultato,MARGIN = 2, FUN = max)
attributi=row.names(risultato)
attributi[scelta]
names.cont.vars[7]
table(train$EDUCATION)
table(test$EDUCATION)
train[,2]=as.factor(train[,2]) 
train[,3]=as.factor(train[,3])
train[,4]=as.factor(train[,4])
train[,24]=as.factor(train[,24])
str(train)

#albero
fit_tree <- tree(default.payment.next.month~., data=trainequi, control=tree.control(nrow(train), mindev=0, mincut=10, minsize=20), split="gini")#gini per classificazione
plot(fit_tree)

CV.tree <- function(obj_tree, K=100, train, sizes=NULL)
{
  if(is.null(sizes))
  {
    pruned <- prune.tree(obj_tree, method="misclass")
    sizes <- pruned$size
    id.remove <- which(sizes==1)
    if(length(id.remove)!=0)
      sizes <- sizes[-id.remove]
  }
  #K-fold cv per ogni alpha
  FOLDS <- cvFolds(nrow(train), K = K)
  AUC.size <- FP.size <- vector(len=length(sizes)) 
  AUC.Kfold <- FP.Kfold <- WEIGHTS <- vector(len=K)	
  
  for(i in 1:length(sizes))
  {
    cat(i, "")
    for(j in 1:K)
    {
      ids.test.Kfold <- FOLDS$subsets[FOLDS$which==j]
      WEIGHTS[j] <- length(ids.test.Kfold)/nrow(train) 
      test.Kfold <- train[ids.test.Kfold,]
      train.Kfold <- train[-ids.test.Kfold,]
      pruned.Kfold <- prune.tree(obj_tree, method="misclass", newdata=train.Kfold, best=sizes[i])
      tmp <- summary(pruned.Kfold)$misclass
      FP.Kfold[j] <- tmp[1]/tmp[2]
      pred.Kfold <- predict(pruned.Kfold, newdata=test.Kfold, type="class")
      AUC.Kfold[j] <- roc.curve(train$default.payment.next.month[ids.test.Kfold], pred.Kfold, plotit=F)$auc
    }
    FP.size[i] <- sum(FP.Kfold*WEIGHTS)
    AUC.size[i] <- sum(AUC.Kfold*WEIGHTS)
    print(c(sizes[i],AUC.size[i],FP.size[i]))
  }
  list(FP.size=FP.size, AUC.size=AUC.size, size=sizes)
}
K.fold=100
MYCV <- CV.tree(fit_tree, K=K.fold, trainequi)

CV.da <- function(formula, train, K, LDA=TRUE)
{
  #K-fold CV
  FOLDS <- cvFolds(nrow(train), K = K)#genera un set di indici, index indica le osservazioni del dataset
  #che vanno in uno specifico folds
  AUC.Kfold <- FP.Kfold <- WEIGHTS <- vector(len=K)	
  for(i in 1:K)
  {
    ids.test.Kfold <- FOLDS$subsets[FOLDS$which==i]
    WEIGHTS[i] <- length(ids.test.Kfold)/nrow(train) 
    if(LDA)
      fit <- lda(formula, data=train[-ids.test.Kfold,])
    else
      fit <- qda(formula, data=train[-ids.test.Kfold,])
    pred.Kfold <- predict(fit, newdata=train[ids.test.Kfold,])
    FP.Kfold[i] <- sum(diag(table(pred.Kfold$class,train$default.payment.next.month[ids.test.Kfold])))/length(pred.Kfold$class)
    AUC.Kfold[i] <- roc.curve(train$default.payment.next.month[ids.test.Kfold], pred.Kfold$class, plotit=F)$auc
  }
  FP <- sum(FP.Kfold*WEIGHTS)
  AUC <- sum(AUC.Kfold*WEIGHTS)
  
  list(FP=FP, AUC=AUC)
}
risu=CV.da(default.payment.next.month~., train=trainequi, K=100)

#######test####
trainrid=trainequi[,-c(7,8,9,13,14,15,16)]#-c(7,8,9,13,14,15,16)
ids=sample(1:nrow(trainrid), round(nrow(trainrid)*.6), replace = F)
trainew=trainrid[ids,]
testnew=trainrid[-ids,]
fit_tree <- tree(default.payment.next.month~., data=trainew, control=tree.control(nrow(trainew), mindev=0, mincut=10, minsize=20), split="gini")#gini per classificazione


tree_cv <- cv.tree(fit_tree, , prune.tree, K=100)
id.min <- which.min(tree_cv$dev)
obj_pruned <- prune.tree(fit_tree, best=8)#abbiamo scelto 8 per il criterio di parsimonia
prev_bil <- predict(obj_pruned, newdata=testnew, type="class")
roc_bil <- roc.curve(testnew$default.payment.next.month, prev_bil, add=T, col=2, plotit = F)$auc
roc_bil
str(test)
test[,2]=as.factor(test[,2]) 
test[,3]=as.factor(test[,3])
test[,4]=as.factor(test[,4])
str(test)
prev_bil <- predict(obj_pruned, newdata=test, type="class")
prev_bil
table(prev_bil)/length(prev_bil)
table(train$default.payment.next.month)/nrow(train)
previsioni=as.vector(prev_bil)
saveRDS(previsioni, file = "zoidberg_previsioni")
