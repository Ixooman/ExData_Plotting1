# plot3.R - plotting the 3rd picture

library(dplyr)
library(lubridate)
library(grDevices)

plot3 <- function() {
    
    day1 <- dmy("01/02/2007")
    day2 <- dmy("02/02/2007")
    
    # Reading a data
    df <- read.csv2("household_power_consumption.txt", header = TRUE, sep = ";", dec = ".",
                    as.is = TRUE, na.strings = "?")
    df <- tbl_df(df)
    
    # Filtering a data for 01/02/2007 and 02/02/2007 days
    df <- mutate(df, DT = dmy(Date))
    df <- filter(df, DT == day1 | DT == day2)
    
    # Converting the Date and Time columns to DT column (POSIXct class)
    df <- mutate(df, DT = dmy_hms(paste(Date,Time)))
    df <- select(df, DT, Global_active_power:Sub_metering_3)

    # Plotting in the Plots (now Plot 3 code here)
    localPlotting3(df)
    
    # Plotting and saving PNG file
    png(file = "plot3.png", bg = "white")
    localPlotting3(df)
    dev.off()
}

localPlotting3 <- function(df) {
    plot(df$DT, df$Sub_metering_1,
         type = "l", xlab = "", ylab = "Energy sub metering", col = "black")
    lines(df$DT, df$Sub_metering_2, col = "red")
    lines(df$DT, df$Sub_metering_3, col = "blue")
    legend("topright", legend = c("Sub_metering_1","Sub_metering_1","Sub_metering_1"),
           col = c("black", "red", "blue"), lty = c(1, 1, 1), merge = TRUE)
}