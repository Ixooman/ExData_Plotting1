# plot3.R - plotting the first picture

#library(dplyr)
#library(lubridate)
#library(grDevices)

plot1 <- function() {
    
    day1 <- dmy("01/02/2007")
    day2 <- dmy("02/02/2007")
    
    # Reading a data
    df <- read.csv2("household_power_consumption.txt", header = TRUE, sep = ";", dec = ".",
                    as.is = TRUE, na.strings = "?")
#    df <- read.csv2("hpc_short.txt", header = TRUE, sep = ";", dec = ".",
#                    as.is = TRUE, na.strings = "?")
    df <- tbl_df(df)
    
    # Filtering a data for 01/02/2007 and 02/02/2007 days
    df <- mutate(df, DT = dmy(Date))
    df <- filter(df, DT == day1 | DT == day2)
    
    # Converting the Date and Time columns to DT column (POSIXct class)
    df <- mutate(df, DT = dmy_hms(paste(Date,Time)))
    df <- select(df, DT, Global_active_power:Sub_metering_3)

    # Plotting in the Viewer (now Plot 2 code here)
    plot(df$DT, df$Sub_metering_1,
         type = "l", xlab = "", ylab = "Energy sub metering")
    plot(df$DT, df$Sub_metering_2,
         type = "l", xlab = "", ylab = "Energy sub metering")
    plot(df$DT, df$Sub_metering_3,
         type = "l", xlab = "", ylab = "Energy sub metering")
    
    # Saving PNG file
#    png(file = "plot3.png", bg = "white")
#    plot(df$DT, df$Global_active_power,
#         type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
#    dev.off()
}