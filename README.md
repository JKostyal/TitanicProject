# TitanicProject
#exploratory data analysis and application of machine learning model Random Forests

setwd("C:/Users/joeyk/Desktop/Other/titanic")

Titanic.train = read.csv("train.csv",stringsAsFactors = FALSE,header=TRUE)
Titanic.test=read.csv("test.csv",stringsAsFactors = FALSE,header=TRUE)

Titanic.train$IsTrainSet=TRUE
Titanic.test$IsTrainSet=FALSE
Titanic.test$Survived=NA

Titanic=rbind(Titanic.train,Titanic.test)

Titanic$Embarked == ''
Titanic[Titanic$Embarked=='', "Embarked"] = 'S'

age.median = median(Titanic$Age,na.rm=TRUE)
Titanic[is.na(Titanic$Age), "Age"] <- age.median #or 28

fare.median=median(Titanic$Fare,na.rm=TRUE)
Titanic[is.na(Titanic$Fare), "Fare"]=fare.median

Titanic$Pclass=as.factor(Titanic$Pclass)
Titanic$Sex=as.factor(Titanic$Sex)
Titanic$Embarked=as.factor(Titanic$Embarked)

Titanic.train=Titanic[Titanic$IsTrainSet==TRUE,]
Titanic.test=Titanic[Titanic$IsTrainSet==FALSE,]

Titanic.train$Survived=as.factor(Titanic.train$Survived)

library(randomForest)

Survived.equation="Survived ~ Pclass + Sex + Age + SibSp + Parch+ + Fare + Embarked"
Survived.formula=as.formula(Survived.equation)
Titanic.model = randomForest(Survived.formula,Titanic.train,
                             ntree=500,mtry=sqrt(7),node=0.01*nrow(Titanic.test))
Survived.pred=predict(Titanic.model,newdata = Titanic.test)

PassengerId=Titanic.test$PassengerId
output.df=data.frame(PassengerId)
output.df$Survived = Survived.pred

write.csv(output.df, file = "Titanic.csv",row.names=FALSE)

