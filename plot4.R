

plot4 <- function(){
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
  
  with(dataSubset,{
    #playing with plot margins
    par(mfrow = c(2,2), mar = c(4,4,2,1),  oma = c(0,0,2,0))
    
    plot(activePower~Datetime, type="l", ylab = "Global Active Power (Kw)", xlab="") 
    plot(Voltage~Datetime, type="l", ylab = "voltage (volts)", xlab="") 
    plot(Sub_metering_1~Datetime, type="l", ylab = "Energy sub mertering", 
         xlab="")
    lines(Sub_metering_2~Datetime, col = "Red")
    lines(Sub_metering_3~Datetime, col = "Blue")
    #Add legends
    legend("topright", col = c("Black", "Red", "Blue"), lty = 1, lwd = 2, bty="n",
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
    )
    
    plot(Global_reactive_power~Datetime, type = "l", xlab = "", ylab = "Global Reactive Power (Kw)")
    
  })
  
  
  #Save file
  dev.copy(png, file = "plot4.png", height = 480, width = 480)
  dev.off()
  
} 