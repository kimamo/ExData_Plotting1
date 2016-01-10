
plot2 <- function(){
  rawFilePath <- getwd()
  rawData <-
    read.table(
      "../data/household_power_consumption.txt", header = TRUE, sep = ';',
      stringsAsFactors = FALSE, dec = ".", comment.char = "", quote = '\"', check.names =
        FALSE, na.strings = "?"
    )
  
  rawData$Date <- as.Date(rawData$Date, format = "%d/%m/%Y")

#Get a subset of the data  dates 2007-02-01 and 2007-02-02
  dataSubset <-
    subset(rawData, subset = (Date >= "2007-02-01" &
                                Date <= "2007-02-02"))
  dates <- paste(as.Date(dataSubset$Date),dataSubset$Time)
  
  dataSubset$Datetime <- as.POSIXct(dates) 
  
#remove the fully loaded data object from memory
  rm(rawData)
  
  activePower <- as.numeric(dataSubset$Global_active_power)
#turnOn the Mac Screen   
  quartz("Quartz", width = 8, height = 7, pointsize = 21)

#plot graph
  plot(activePower~dataSubset$Datetime, type="l", ylab = "Global Active Power (Kw)", xlab="") 
  
#Save file
  dev.copy(png, file = "plot2.png", height = 480, width = 480)
  dev.off()
  
} 