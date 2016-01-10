


plot1 <- function() {
  dataFilePath <- getwd()
  consumptionData <-
    read.table(
      "./data/household_power_consumption.txt", header = TRUE, sep = ';',
      stringsAsFactors = FALSE, dec = ".", comment.char = "", quote = '\"', check.names =
        FALSE, na.strings = "?"
    )
  
  #head(consumptionData)
  
  consumptionData$Date <-
    as.Date(consumptionData$Date, format = "%d/%m/%Y")
  #  head(consumptionData$Date)
  
  #Get a subset of the data
  dataSubset <-
    subset(consumptionData, subset = (Date >= "2006-12-01" &
                                        Date <= "2006-12-16"))
  dates <- paste(as.Date(dataSubset$Date),dataSubset$Time)
  
  dataSubset$Datetime <- as.POSIXct(dates)
  #summary(dataSubset)
  
  #remove the fully loaded data object from memory
  rm(consumptionData)
  
  activePower <- as.numeric(dataSubset$Global_active_power)
  summary(activePower)
  quartz("Quartz", width = 5, height = 4, pointsize = 18)
  
  hist(
    activePower,  main = "Global Active Power", xlab = "Global Active Power (Kw)", ylab = "Frequency", col = "Red"
  )
  
  #Save file
  dev.copy(png, file = "plot1.png", height = 480, width = 480)
  dev.off()
  
  
}
