## Download the ZIP file and then unzip it. Check if the files exist before processing.
filename <- "repdata_data_StormData.csv.bz2"
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if (!file.exists(filename)){
        download.file(fileURL, filename, method = "curl")
}
## Read the data into R as data.frames
stormData <- read.csv(filename, sep = ",", header = TRUE)

#### Q1: Across the United States, which types of events (as indicated in the EVTYPE variable) 
#### are most harmful with respect to population health? ####################################
fatal_byType <- aggregate(FATALITIES ~ EVTYPE, stormData, sum)
injury_byType <- aggregate(INJURIES ~ EVTYPE, stormData, sum )

fatal_byType_Sorted <- fatal_byType[order(-fatal_byType$FATALITIES),]
injury_byType_Sorted <- injury_byType[order(-injury_byType$INJURIES),]

fatal_byType_Sorted[1:10,]
injury_byType_Sorted[1:10,]

par(mfrow = c(1, 2))
par(mar = c(10, 4, 4, 2), cex = 0.8, cex.main = 1.2, cex.lab = 1.2)
barplot(fatal_byType_Sorted$FATALITIES[1:10], names.arg = fatal_byType_Sorted$EVTYPE[1:10], col = 'blue',
        main = 'Top 10 Weather Events for Fatalities', ylab = 'Number of Fatalities')
barplot(injury_byType_Sorted$INJURIES[1:10], names.arg = injury_byType_Sorted$EVTYPE[1:10], col = 'green',
        main = 'Top 10 Weather Events for Injuries', ylab = 'Number of Injuries')

#### Q2: Across the United States, which types of events have the greatest economic consequences?
library(plyr)

stormData <- mutate(stormData, PropertyDamage = ifelse(toupper(PROPDMGEXP) =='K', PROPDMG*1000, 
                                             ifelse(toupper(PROPDMGEXP) =='M', PROPDMG*1000000, 
                                                    ifelse(toupper(PROPDMGEXP) == 'B', PROPDMG*1000000000, 
                                                           ifelse(toupper(PROPDMGEXP) == 'H', PROPDMG*100, PROPDMG)))))
stormData <- mutate(stormData, CropDamage = ifelse(toupper(CROPDMGEXP) =='K', CROPDMG*1000, 
                                                       ifelse(toupper(CROPDMGEXP) =='M', CROPDMG*1000000, 
                                                              ifelse(toupper(CROPDMGEXP) == 'B', CROPDMG*1000000000, 
                                                                     ifelse(toupper(CROPDMGEXP) == 'H', CROPDMG*100, PROPDMG)))))

propertyDMG_byType <- aggregate(PropertyDamage ~ EVTYPE, stormData, sum)
cropDMG_byType <- aggregate(CropDamage ~ EVTYPE, stormData, sum)
totalDMG_byType <- merge(propertyDMG_byType, cropDMG_byType, by = "EVTYPE")
totalDMG_byType$TOTALDMG <- totalDMG_byType$PropertyDamage + totalDMG_byType$CropDamage

propertyDMG_byType_Sorted <- propertyDMG_byType[order(-propertyDMG_byType$PropertyDamage),]
cropDMG_byType_Sorted <- cropDMG_byType[order(-cropDMG_byType$CropDamage),]
totalDMG_byType_Sorted <- totalDMG_byType[order(-totalDMG_byType$TOTALDMG),]

propertyDMG_byType_Sorted[1:10,]
cropDMG_byType_Sorted[1:10,]
totalDMG_byType_Sorted[1:10,]

par(mfrow = c(1, 3))
par(mar = c(10, 4, 4, 2), cex = 0.8, cex.main = 1.2, cex.lab = 1.2)
barplot(propertyDMG_byType_Sorted$PropertyDamage[1:10], names.arg = propertyDMG_byType_Sorted$EVTYPE[1:10], col = 'blue',
        main = 'Top 10 Weather Events for property damages', ylab = 'Property Damages')
barplot(cropDMG_byType_Sorted$CropDamage[1:10], names.arg = cropDMG_byType_Sorted$EVTYPE[1:10], col = 'green',
        main = 'Top 10 Weather Events for crop damages', ylab = 'Crop Damages')
barplot(totalDMG_byType_Sorted$TOTALDMG[1:10], names.arg = totalDMG_byType_Sorted$EVTYPE[1:10], col = 'orange',
        main = 'Top 10 Weather Events for total damages', ylab = 'Total Damages')
