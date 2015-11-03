# plot1.R - plotting the 1st picture

library(dplyr)
library(lubridate)
library(grDevices)

plot1 <- function() {
    
    day1 <- dmy("01/02/2007")
    day2 <- dmy("02/02/2007")
    
    # Reading data
    df <- read.csv2("household_power_consumption.txt", header = TRUE, sep = ";", dec = ".",
                    as.is = TRUE, na.strings = "?")
    df <- tbl_df(df)
    
    # Filtering data for 01/02/2007 and 02/02/2007 days
    df <- mutate(df, DT = dmy(Date))
    df <- filter(df, DT == day1 | DT == day2)
    
    # Converting Date and Time columns to DT column (POSIXct class)
    df <- mutate(df, DT = dmy_hms(paste(Date,Time)))
    df <- select(df, DT, Global_active_power:Sub_metering_3)

    # Plotting
    localPlotting1(df)
    
    # Saving PNG
    png(file = "plot1.png", bg = "white")
    localPlotting1(df)
    dev.off()
    
}

localPlotting1 <- function(df) {
    hist(df$Global_active_power, col = "red",
         main = "Global Actice Power", xlab = "Global Active Power (kilowatts)")
}