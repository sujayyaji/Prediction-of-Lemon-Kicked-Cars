caravana <- read.csv("training.csv", header=TRUE)

sapply(caravana, class)



#Removing Outliers

outliers <- boxplot(caravana$VehicleAge, plot=FALSE)$out
caravana<- caravana[-which(caravana$VehicleAge %in% outliers),]
summary(caravana$VehicleAge)
boxplot(caravana$VehicleAge)

caravana$MMRAcquisitionAuctionAveragePrice <- as.numeric(caravana$MMRAcquisitionAuctionAveragePrice)
boxplot(caravana$MMRAcquisitionAuctionAveragePrice)

#2
outliers <- boxplot(caravana$VehOdo, plot=FALSE)$out
caravana<- caravana[-which(caravana$VehOdo %in% outliers),]
summary(caravana$VehOdo)
boxplot(caravana$VehOdo)

#3
outliers <- boxplot(caravana$VehBCost, plot=FALSE)$out
caravana<- caravana[-which(caravana$VehBCost %in% outliers),]

#4

outliers <- boxplot(caravana$WarrantyCost, plot=FALSE)$out
caravana<- caravana[-which(caravana$WarrantyCost %in% outliers),]

#Removing attributes

caravana$PurchDate<-NULL
caravana$WheelType<-NULL

caravana <- subset(caravana, select=-c(Auction))

caravana <- subset(caravana, select = -c(WheelTypeID, BYRNO, VNZIP1,
                                         VNST, RefId,VehYear, SubModel,
                                         WheelTypeID,
                                         IsOnlineSale,
                                         TopThreeAmericanName,PRIMEUNIT,AUCGUART))



caravana <- subset(caravana, select=-c(Make, Model, Color, Trim, Nationality
))

#Mean Imputation

caravana$VehicleAge = ifelse(is.na(caravana$VehicleAge),
                             ave(caravana$VehicleAge, 
                                 FUN = function(x) mean(x, na.rm = TRUE)),caravana$VehicleAge)


caravana$VehOdo = ifelse(is.na(caravana$VehOdo),
                         ave(caravana$VehOdo, 
                             FUN = function(x) mean(x, na.rm = TRUE)),caravana$VehOdo)

caravana$MMRAcquisitionAuctionAveragePrice = ifelse(is.na(caravana$MMRAcquisitionAuctionAveragePrice),
                                                    ave(caravana$MMRAcquisitionAuctionAveragePrice, 
                                                        FUN = function(x) mean(x, na.rm = TRUE)),caravana$MMRAcquisitionAuctionAveragePrice)



caravana$MMRAcquisitionAuctionCleanPrice = ifelse(is.na(caravana$MMRAcquisitionAuctionCleanPrice),
                                                  ave(caravana$MMRAcquisitionAuctionCleanPrice,
                                                      FUN = function(x) mean(x, na.rm = TRUE)),caravana$MMRAcquisitionAuctionCleanPrice)





caravana$MMRAcquisitionRetailAveragePrice = ifelse(is.na(caravana$MMRAcquisitionRetailAveragePrice),
                                                   ave(caravana$MMRAcquisitionRetailAveragePrice,
                                                       FUN = function(x) mean(x, na.rm = TRUE)),caravana$MMRAcquisitionRetailAveragePrice)



caravana$MMRAcquisitonRetailCleanPrice = ifelse(is.na(caravana$MMRAcquisitonRetailCleanPrice),
                                                ave(caravana$MMRAcquisitonRetailCleanPrice,
                                                    FUN = function(x) mean(x, na.rm = TRUE)),caravana$MMRAcquisitonRetailCleanPrice)

caravana$MMRCurrentAuctionAveragePrice = ifelse(is.na(caravana$MMRCurrentAuctionAveragePrice),
                                                ave(caravana$MMRCurrentAuctionAveragePrice,
                                                    FUN = function(x) mean(x, na.rm = TRUE)),caravana$MMRCurrentAuctionAveragePrice)
caravana$MMRCurrentAuctionCleanPrice = ifelse(is.na(caravana$MMRCurrentAuctionCleanPrice),
                                              ave(caravana$MMRCurrentAuctionCleanPrice,
                                                  FUN = function(x) mean(x, na.rm = TRUE)),caravana$MMRCurrentAuctionCleanPrice)


caravana$MMRCurrentRetailAveragePrice = ifelse(is.na(caravana$MMRCurrentRetailAveragePrice),
                                               ave(caravana$MMRCurrentRetailAveragePrice,
                                                   FUN = function(x) mean(x, na.rm = TRUE)),caravana$MMRCurrentRetailAveragePrice)
caravana$MMRCurrentRetailCleanPrice = ifelse(is.na(caravana$MMRCurrentRetailCleanPrice),
                                             ave(caravana$MMRCurrentRetailCleanPrice,
                                                 FUN = function(x) mean(x, na.rm = TRUE)),caravana$MMRCurrentRetailCleanPrice)


caravana$Transmission = factor(caravana$Transmission,
                               levels = c('AUTO', 'MANUAL'),
                               labels = c(0,1))
caravana <- caravana[complete.cases(caravana$Transmission),]

caravana$IsBadBuy <- as.numeric(caravana$IsBadBuy)