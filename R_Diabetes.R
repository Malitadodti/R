install.packages("stats")
install.packages("dplyr", dependencies = TRUE)
install.packages("ggplot2")
install.packages("ggfortify")

library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)

diabetes <- read.csv("/Users/malit/Desktop/data_train/diabetes.csv")


summary(diabetes)

dim(diabetes)
str(diabetes)



colnames(diabetes)

class(diabetes $outcome)
sapply(diabetes,class)


f_outcome<-factor(diabetes $outcome)

diabetes $outcome<- as.factor(diabetes $outcome)
class(diabetes $outcome)

colSums(is.na(diabetes))



diabetes[(is.na(diabetes))]<-0
colSums(is.na(diabetes))


ggplot(data =diabetes,aes(x=skin, y=age))+geom_point(color="purple")

ggplot(data=diabetes)+geom_point(mapping = aes(x=skin, y=age,color=outcome))

test=select(diabetes,c(1,2,3,4,5,6,7,8))
kmeans.finalresult <- kmeans(test,2)
plot(test[c("age", "skin")], col = kmeans.finalresult$cluster, main="Diabetes")
