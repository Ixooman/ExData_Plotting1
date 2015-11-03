# plot2.R - plotting the first picture

#library(dplyr)
#library(lubridate)
#library(grDevices)

plot1 <- function() {
    
    day1 <- dmy("01/02/2007")
    day2 <- dmy("02/02/2007")
    
    # Read data
    df <- read.csv2("household_power_consumption.txt", header = TRUE, sep = ";", dec = ".",
                    as.is = TRUE, na.strings = "?")
#    df <- read.csv2("hpc_short.txt", header = TRUE, sep = ";", dec = ".",
#                    as.is = TRUE, na.strings = "?")
    df <- tbl_df(df)
    
    # Filter data for 01/02/2007 and 02/02/2007 days
    df <- mutate(df, DT = dmy(Date))
    df <- filter(df, DT == day1 | DT == day2)
    
    # Convert Date and Time columns to DT column (POSIXct class)
    df <- mutate(df, DT = dmy_hms(paste(Date,Time)))
    df <- select(df, DT, Global_active_power:Sub_metering_3)

    # Plotting (now Plot 2 code here)
    plot(df$DT, df$Global_active_power,
         type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
    
    # Saving PNG will be here...
    png(file = "plot2.png", bg = "white")
    plot(df$DT, df$Global_active_power,
         type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
    dev.off()
}