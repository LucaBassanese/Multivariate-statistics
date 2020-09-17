setwd("C:\\Users\\Luca\\Desktop\\LUCA\\SCUOLA\\Esami passati\\Magistrale\\Statistica Multivariata\\Codici\\DatasetMulti")
dati=read.table("incomemod.txt", sep=",")

library(MASS)
library(car)
library(cvTools)
library(ROSE)
library(corrplot)
library(ggplot2)
library(ggcorrplot)
library(tree)
library(rmarkdown)
library(lsr)#pacchetto in cui è presente la funzione cramersV
library(gridExtra)#pacchetto che consente di affiancare più ggplot
library(animation)

View(dati)

nomi=c("age", "workclass","fnlwgt", "education", "education.num", "marital.status", "occupation", "relationship", "race", "sex",
       "capital.gain", "capital.loss", "hours.per.week", "native.country", "income")
names(dati)=nomi#nominiamo le variabili
str(dati)
summary(dati)#notiamo la presenza di " ?" che corrispondono a missing
dati[dati==" ?"]=NA
income=na.omit(dati)#eliminiamo i missing
table(is.na(income))
str(income)
#abbiamo notato da "str" che ci sono troppi livelli nel factor native.country con 
#poche osservazioni, per cui abbiamo deciso di raggrupparli
table(income$native.country)
levels(income$native.country)= list(Others=c(" South", " ?"),
                                    Asia=c(" China"," India"," Iran"," Philippines"," Taiwan"," Japan"," Laos"," Cambodia"," Thailand"),
                                    NorthAmerica=c(" Canada"," United-States", " Mexico"),
                                    CenterAmerica=c(" Cuba"," Dominican-Republic"," Guatemala"," Haiti"," Honduras"," Jamaica"," Puerto-Rico"," El-Salvador"),    
                                    SouthAmerica=c(" Ecuador"," Peru"," Columbia"),
                                    Europe=c(" France"," Germany"," Italy"," Poland"," Portugal"," Scotland"," England"," Yugoslavia"),
                                    Oceania=c(" Outlying-US(Guam-USVI-etc)"))
View(income)

#####analisi esplorative#####
#visto che il dataset risulta squilibrato ed essendo prepronderanti le variabili categoriche vogliamo 
#usare un albero, abbiamo deciso di bilanciare il dataset 
set.seed(123)
ids0 <- which(income$income==" <=50K")
ids1 <-which(income$income==" >50K")
N_TOT <- length(ids1)
ids0_income <- sample(ids0, N_TOT, rep=F)#poichè il dataset ha poche osservazioni, teniamo tutte le osservazioni con income " >50K"
ids1_income<- sample(ids1, N_TOT, rep=F)
zoi=income[c(ids0_income,ids1_income),]

#eliminiamo la modalità " ?" dai levels delle variabili categoriali, in quanto dà problemi
#nell'utilizzo della funzione "cramersV"
levels(zoi$workclass)
a=as.matrix(zoi$workclass)
a=as.factor(a)
zoi$workclass=a
a=as.matrix(zoi$education)
a=as.factor(a)
zoi$education=a
a=as.matrix(zoi$marital.status)
a=as.factor(a)
zoi$marital.status=a
a=as.matrix(zoi$occupation)
a=as.factor(a)
zoi$occupation=a
a=as.matrix(zoi$relationship)
a=as.factor(a)
zoi$relationship=a
a=as.matrix(zoi$race)
a=as.factor(a)
zoi$race=a
a=as.matrix(zoi$sex)
a=as.factor(a)
zoi$sex=a
a=as.matrix(zoi$native.country)
a=as.factor(a)
zoi$native.country=a
str(zoi)


#vediamo correlazione e grafici per variabili quantitative: "age", "fnlwgt", "education.num", 
#"capital.gain", "capital.loss", "hours.per.week"
#dispersione
pairs(zoi[, c(1,3,5,11,12,13)], col=zoi$income)
#correlazione
C <- cor(zoi[,c(1,3,5,11,12,13)])#tolgo le variabili qualitative e la variabile risposta
ggcorrplot(C, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram", 
           ggtheme=theme_bw)#dal grafico si deduce che non c'è correlazione



#boxplot
plot(zoi[,1]~zoi$income, xlab="Income", ylab="Age",col=c("indianred1", "cyan3"), main="Older people earn more")
ggplot(zoi, aes(x = age, color = income, fill = income)) +
  geom_density(alpha = 0.8) +
  labs(x = "Age", y = "Density",
       subtitle = "Density plot")

plot(zoi[,5]~zoi$income, xlab="Income", ylab="education-num", outline=F,col=c("indianred1", "cyan3"), main="People with more education earn more")
ggplot(zoi, aes(x = education.num, color = income, fill = income)) +
  geom_density(alpha = 0.8) +
  labs(x = "Education.num", y = "Density",
       subtitle = "Density plot")
plot(zoi[,13]~zoi$income, xlab="Income", ylab="hours-per-week", outline=F, col=c("indianred1", "cyan3"), main="People work more earn more")
ggplot(zoi, aes(x = hours.per.week, color = income, fill = income)) +
  geom_density(alpha = 0.8) +
  labs(x = "Hours.per.week", y = "Density", 
       subtitle = "Density plot")
table(zoi$hours.per.week)
length(zoi$hours.per.week[zoi$hours.per.week==40])/nrow(zoi)

plot(zoi[,3]~zoi$income, xlab="Income", ylab="fnlwgt", outline=F,col=c("indianred1", "cyan3"))#si può eliminare, 
#perchè non c'è chiara distinzione tra le distribuzioni condizionate alla risposta
ggplot(zoi, aes(x = fnlwgt, color = income, fill = income)) +
  geom_density(alpha = 0.8) +
  labs(x = "Fnlwgt", y = "Density", 
       subtitle = "Density plot")
plot(zoi[,11]~zoi$income, xlab="Income", ylab="capital-gain",col=c("indianred1", "cyan3"))# si elimina perchè entrambe le distribuzioni condizionate sono schiacciate in 0
plot(zoi[,12]~zoi$income, xlab="Income", ylab="capital-loss",col=c("indianred1", "cyan3"))# si elimina perchè entrambe le distribuzioni condizionate sono schiacciate in 0

zoi=zoi[,-c(3,11,12)]#abbiamo eliminato le variabili non significative

#notiamo che education.num porta la stessa informazione di education, uno in forma quantitativa
#e l'altro in forma qualitativa, vogliamo verificare se c'è dipendenza
boxplot(zoi[,4]~zoi$education, xlab="Education", ylab="Education.num") #100% correlazione teniamo solo il qualitativo, perchè più consona al tipo di analisi
##ordino i livelli in senso crescente 
ordered_levels <- levels(zoi$education)[c(14,4,5,6,7,1,2,3,12,16,9,8,10,13,15,11)]
zoi$education <- factor(zoi$education, ordered_levels)
zoi=zoi[,-4]
str(zoi)

#vediamo correlazione e grafici per variabili qualitative: "workclass", "education", "marital.status", "occupation", "relationship", "race", "sex",
#"native.country"
#correlazione
zoinominal=zoi[,-c(1,9,11)]#crea un dataset solo con le variabili qualitative
cramercorr=matrix(ncol = ncol(zoinominal), nrow=ncol(zoinominal))
nomi <- names(zoinominal)
colnames(cramercorr)=rownames(cramercorr)=nomi
for(i in 1:ncol(cramercorr))
{for(j in 1:ncol(cramercorr))
{cramercorr[i,j]=cramersV(table(zoinominal[,i],zoinominal[,j]))}
  }#usiamo la V di Cramer per studiare le correlazioni tra variabili qualitative
  
 
ggcorrplot(cramercorr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Associations", 
           ggtheme=theme_bw
           )+
  scale_fill_gradientn(colours=c( "yellow", "violet"),na.value = "transparent",
                       breaks=c(0,0.5,1),
                       limits=c(0,1))
table(zoi$sex, zoi$relationship)#la correlazione tra "sex" e "relationship" risulta elevata 
#sopratutto perchè in relationship esistono le classi "husband" e "wife" che sono correlate al 100%
#con "male" e "female"


#istogrammi  per ogni variabile: condizionati e proporzionati rispetto 
#alla variabile risposta 

g <- ggplot(zoi, aes(income)) 

a=g + geom_bar(aes(fill=workclass), width = 0.5)  + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Workclass histogram", 
       subtitle="Income across workclass") 

b=ggplot(zoi, aes(x = workclass, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Workclass", y = "Proportion", title = "Income bias \n based on workclass")+
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(a,b, ncol=2)

a=g + geom_bar(aes(fill=education), width = 0.5)  + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Income across education") 

b=ggplot(zoi, aes(x = education, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Education", y = "Proportion", title = "Income bias \n based on education", subtitle="People with more education earn more")+
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(a,b, ncol=2)


a=g + geom_bar(aes(fill=marital.status), width = 0.5)  + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Income across marital-status") 

b=ggplot(zoi, aes(x =marital.status, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Marital.status", y = "Proportion", title = "Income bias \n based on marital.status")+
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(a,b, ncol=2)


a=g + geom_bar(aes(fill= occupation), width = 0.5)  + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Income across occupation") 

b=ggplot(zoi, aes(x = occupation, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Occupation", y = "Proportion", title = "Income bias \n based on occupation")+
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(a,b, ncol=2)


a=g + geom_bar(aes(fill=relationship), width = 0.5)  + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Income across relationship") 

b=ggplot(zoi, aes(x = relationship, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Relationship", y = "Proportion", title = "Income bias \n based on relationship", 
       subtitle="Married people earn more")+ theme(plot.title = element_text(hjust = 0.5))
grid.arrange(a,b, ncol=2)


a=g + geom_bar(aes(fill=race), width = 0.5)  + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Income across race")  

b=ggplot(zoi, aes(x = race, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Race", y = "Proportion", title = "Income bias \n based on race")+
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(a,b, ncol=2)


a=g + geom_bar(aes(fill=sex), width = 0.5)  + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Income across sex") 

b=ggplot(zoi, aes(x = sex, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Sex", y = "Proportion", title = "Income bias \n based on sex", 
       subtitle="Men earn more than women") + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(a,b, ncol=2)


a=g + geom_bar(aes(fill=native.country), width = 0.5)  + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Income across native-country")

b=ggplot(zoi, aes(x = native.country, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Native.country", y = "Proportion", title = "Income bias \n based on native.country")+
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(a,b, ncol=2)
#Oceania, SouthAmerica e Others risultano poco significativi se non inesistenti nel dataset equilibrato


#####albero####
set.seed(999)
#train e test equilibrati
ids=sample(1:nrow(zoi),round(nrow(zoi)*0.6),replace = F)
train=zoi[ids,]
test=zoi[-ids,]

#train e test disequilibrati
ids=sample(1:nrow(income), round(nrow(income)*0.6), replace = F)
train.dis=income[ids,][, -c(3,5,11,12)]
test.dis=income[-ids,]
test.dis=test.dis[, -c(3,5,11,12)]
train.dis

#Albero equilibrato
fit_tree <- tree(income~., data=train,
                 control=tree.control(nrow(train), mindev=0, mincut=7, minsize=14), 
                 split="gini")
plot(fit_tree, col="brown")
text(fit_tree, cex=0.5, col="darkgreen")


summary(fit_tree)$size
Accuratezza=matrix(, nrow=summary(fit_tree)$size, ncol=3)
colnames(Accuratezza)=c("Auc", "Prop. corretta class.(train)", "Prop. corretta class.(test.dis)")
for(i in 2: summary(fit_tree)$size){
obj_pruned <- prune.tree(fit_tree, best=i)
prev <- predict(obj_pruned, newdata=test, type="class")
Accuratezza[i,1] <- roc.curve(test$income, prev, add=T, col=2, plotit = F)$auc
prevg <- predict(obj_pruned, newdata=train, type="class")
Accuratezza[i,2]=sum(diag(table(prevg, train$income)))/nrow(train)
prevdis <- predict(obj_pruned, newdata=test.dis, type="class")
Accuratezza[i,3]=sum(diag(table(prevdis, test.dis$income)))/nrow(test.dis)
}
Accuratezza
tree_cv <- cv.tree(fit_tree, , prune.tree, K=100, method="misclass")
id.min <- which.min(tree_cv$dev)
dimensione<-tree_cv$size[id.min]; dimensione#dimensione coerente con il metodo utilizzato precedentemente

obj_pruned <- prune.tree(fit_tree, best=2)
prev <- predict(obj_pruned, newdata=test, type="class")
roc <- roc.curve(test$income, prev, add=T, col=2, plotit = F)$auc
roc
prevg <- predict(obj_pruned, newdata=train, type="class")
sum(diag(table(prevg, train$income)))/nrow(train)#errore di generalizzazione
sum(diag(table(prev, test$income)))/nrow(test)#avendo equilibrato è molto simili all'auc
fit_tree
summary(obj_pruned)
str(obj_pruned)
obj_pruned
plot(obj_pruned, col="brown")
text(obj_pruned, cex=0.8, col="darkgreen")


#se non avessimo equilibrato il dataset

fit_tree_dis <- tree(income~., data=train.dis,
                 control=tree.control(nrow(train.dis), mindev=0, mincut=7, minsize=14), 
                 split="gini")
plot(fit_tree_dis, col="brown")
text(fit_tree_dis, cex=0.5, col="darkgreen")


Accuratezzadis=matrix(, nrow=summary(fit_tree_dis)$size, ncol=3)
colnames(Accuratezzadis)=c( "AUC", "Prop. corretta class.(train)","Accuratezza" )
for(i in 2: summary(fit_tree_dis)$size){
  obj_pruned <- prune.tree(fit_tree_dis, best=i)
  prev <- predict(obj_pruned, newdata=test.dis, type="class")
  Accuratezzadis[i,1] <- roc.curve(test.dis$income, prev, add=T, col=2, plotit = F)$auc
  prevg <- predict(obj_pruned, newdata=train.dis, type="class")
  Accuratezzadis[i,2]=sum(diag(table(prevg, train.dis$income)))/nrow(train.dis)
  prevdis <- predict(obj_pruned, newdata=test.dis, type="class")
  Accuratezzadis[i,3]=sum(diag(table(prevdis, test.dis$income)))/nrow(test.dis)
}
Accuratezzadis
tree_cvd <- cv.tree(fit_tree_dis, , prune.tree, K=100, method="misclass")
id.min <- which.min(tree_cvd$dev)
dimensione<-tree_cvd$size[id.min]; dimensione #coerente con la tabella sopra


obj_pruned_dis <- prune.tree(fit_tree_dis, best=5)
prevdis <- predict(obj_pruned_dis, newdata=test.dis, type="class")
rocdis <- roc.curve(test.dis$income, prevdis, add=T, col=2, plotit = F)$auc
rocdis
prevgdis <- predict(obj_pruned_dis, newdata=train.dis, type="class")
sum(diag(table(prevgdis, train.dis$income)))/nrow(train.dis)#errore di generalizzazione
sum(diag(table(prevdis, test.dis$income)))/nrow(test.dis)#avendo equilibrato è molto simili all'auc
fit_tree_dis
summary(fit_tree_dis)
plot(obj_pruned_dis, col="brown", main="CIAO")
text(obj_pruned_dis, cex=0.8, col="darkgreen")



#GIF

saveGIF(
for (count in summary(fit_tree)$size:2) {
  obj_pruned=prune.tree(fit_tree, best = count)
  plot(obj_pruned, col="brown")
  text(obj_pruned, cex=0.5, col="darkgreen")
  legend("topright", legend = paste("AUC =", round(Accuratezza[count,1],3)))
}
 , movie.name = "potaturaequi.gif") 


saveGIF(
  for (count in summary(fit_tree_dis)$size:2) {
    obj_pruned=prune.tree(fit_tree_dis, best = count)
    plot(obj_pruned, col="brown")
    text(obj_pruned, cex=0.5, col="darkgreen")
    legend("topright", legend = paste("AUC =", round(Accuratezzadis[count,1],3)))
  }
  , movie.name = "potaturadisequi.gif") 


