
plot3 <- function(){
  rawFilePath <- getwd()
  rawData <-
    read.table(
      "../data/household_power_consumption.txt", header = TRUE, sep = ';',
      stringsAsFactors = FALSE, dec = ".", comment.char = "", quote = '\"', check.names =
        FALSE, na.strings = "?"
    )
  
  rawData$Date <- as.Date(rawData$Date, format = "%d/%m/%Y")
  
  #Get a subset of the data
  dataSubset <-
    subset(rawData, subset = (Date >= "2006-12-15" &
                                Date <= "2006-12-20"))
  dates <- paste(as.Date(dataSubset$Date),dataSubset$Time)
  
  dataSubset$Datetime <- as.POSIXct(dates) 
  
  #remove the fully loaded data object from memory
  rm(rawData)
  
  activePower <- as.numeric(dataSubset$Global_active_power)
  #turnOn the Mac Screen   
  quartz("Quartz", width = 8, height = 7, pointsize = 21)
  
  #plot graph
  with(dataSubset,{
  plot(Sub_metering_1~dataSubset$Datetime, type="l", ylab = "Global Active Power (Kw)", 
       xlab="") 
    lines(Sub_metering_2~Datetime, col = "Red")
    lines(Sub_metering_3~Datetime, col = "Blue")
  })
#Add legends
  legend("topright", col = c("Black", "Red", "Blue"), lty = 1, lwd = 2,
            legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
         )
  
  #Save file
  dev.copy(png, file = "plot3.2.png", height = 480, width = 480)
  dev.off()
  
} 