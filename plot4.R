# plot4.R - plotting the 4th picture

library(dplyr)
library(lubridate)
library(grDevices)

plot4 <- function() {
    
    day1 <- dmy("01/02/2007")
    day2 <- dmy("02/02/2007")
    
    # Read data
    df <- read.csv2("household_power_consumption.txt", header = TRUE, sep = ";", dec = ".",
                    as.is = TRUE, na.strings = "?")
    df <- tbl_df(df)
    
    # Filter data for 01/02/2007 and 02/02/2007 days
    df <- mutate(df, DT = dmy(Date))
    df <- filter(df, DT == day1 | DT == day2)
    
    # Convert Date and Time columns to DT column (POSIXct class)
    df <- mutate(df, DT = dmy_hms(paste(Date,Time)))
    df <- select(df, DT, Global_active_power:Sub_metering_3)

    localPlotting4(df)
    
    # Saving PNG will be here...
    png(file = "plot4.png", bg = "white")
    localPlotting4(df)
    dev.off()
}

localPlotting4 <- function(df) {
    # Plotting 4 times
    prevPar <- par(mfrow=c(2,2))
    # 1st
    plot(df$DT, df$Global_active_power,
         type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
    # 2nd - dummy
    plot(df$DT, df$Voltage,
         type = "l", xlab = "datetime", ylab = "Voltage")
    # 3rd
    plot(df$DT, df$Sub_metering_1,
         type = "l", xlab = "", ylab = "Energy sub metering", col = "black")
    lines(df$DT, df$Sub_metering_2, col = "red")
    lines(df$DT, df$Sub_metering_3, col = "blue")
    legend("topright", legend = c("Sub_metering_1","Sub_metering_1","Sub_metering_1"),
           col = c("black", "red", "blue"), lty = c(1, 1, 1), box.lty = c(0), merge = TRUE)
    # 4th
    plot(df$DT, df$Global_reactive_power,
         type = "l", xlab = "datetime", ylab = "Global_reactive_power")
    
    #restoring previous value
    par(prevPar)
}
