preprocessData = function() {
  Sys.setlocale("LC_ALL", "English")
  
  fileUrl = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  zipFilePath = "./household_power_consumption.zip"
  dataFilePath = "./household_power_consumption.txt"
  
  if (!file.exists(dataFilePath)) {
    if (!file.exists(zipFilePath)) {
      download.file(fileUrl, destfile = filePath)  
    }
    unzip(zipFilePath)
  }
  
  consumption = read.csv2(dataFilePath, colClasses = "character")
  consumption = subset(consumption, Date %in% c("1/2/2007", "2/2/2007"))
  
  colnames(consumption)[colnames(consumption) == "Date"] = "oldDate"
  colnames(consumption)[colnames(consumption) == "Time"] = "oldTime"
  consumption$Date = strptime(paste(consumption$oldDate, consumption$oldTime), "%d/%m/%Y %H:%M:%S")
  consumption$oldDate = NULL
  consumption$oldTime = NULL
  
  for (i in 1:7) {
    consumption[, i] = as.numeric(consumption[, i])  
  }
  
  par(mfrow = c(1, 1), bg = "transparent")
  
  return(consumption)
}


plot2 = function(consumption) {
  with(consumption, plot(Date, Global_active_power, xlab = "", ylab = "Global Active Power (kilowatts)", type = "l"))
}

consumption = preprocessData()

png("plot2.png")
plot2(consumption)
dev.off()