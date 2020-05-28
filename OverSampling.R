as.data.frame(table(caravana$IsBadBuy))
library(DMwR)
caravana$IsBadBuy<-factor(caravana$IsBadBuy)
balanced.data <- SMOTE(IsBadBuy ~., caravana, perc.over = 400, k = 5, perc.under = 100)
as.data.frame(table(balanced.data$IsBadBuy))
as.data.frame(table(caravana$IsBadBuy))
caravana1<-balanced.data
h1<-subset(caravana1, IsBadBuy == "1")
h1<-h1[sample(1:nrow(h1), 36000,
              replace=FALSE),]

h2<-subset(caravana, IsBadBuy == "0")
#h2<-h2[sample(1:nrow(h2), 30000,
#                    replace=FALSE),]
caravana<-rbind(h1, h2)

as.data.frame(table(caravana$IsBadBuy))
