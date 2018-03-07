memory_test <- function(nb_row_estimate =2075259){
    data_memory_estimate_sampling <- read.table("./Exploratory Data Analysis/household_power_consumption.txt",nrows = 100,sep=";",header=TRUE)
    
    info <- data.frame(matrix(ncol=3,nrow = 9))
    colnames(info) <- c("col_name","size","class")
    
    obj_size <- list()
    col_class <- list()
    for (allcol in colnames(data_memory_estimate_sampling)){
      if (allcol =="Date"){
        obj_size <- c(obj_size,object.size(data_memory_estimate_sampling[,allcol]))
        col_class <- c(col_class,"Date")
      }
      else if (allcol == "Time") {
        obj_size <- c(obj_size,object.size(data_memory_estimate_sampling[,allcol]))
        col_class <- c(col_class,"Time")
      }
      else {
      obj_size <- c(obj_size,object.size(data_memory_estimate_sampling[,allcol]))
      col_class <- c(col_class,class(data_memory_estimate_sampling[,allcol]))
      }
    }
    info$col_name <-  colnames(data_memory_estimate_sampling)
    info$size <- unlist(obj_size)
    info$class <- unlist(col_class)
    
    sum_samp_memo <- sum(unlist(info$size))
    nb_row <- nb_row_estimate
    memory_estimate <- ((nb_row/100*sum_samp_memo)/2^(20)) *2
    
    if (memory_estimate< 600){
      data_raw <- read.table("./Exploratory Data Analysis/household_power_consumption.txt",sep=";",header=TRUE)
    
      return(data_raw)

    }
}

raw_data <- memory_test(nb_row_estimate =2075259)


raw_data$Date <- as.Date(raw_data$Date,"%d/%m/%Y") 
our_data <- raw_data %>% filter((Date <="2007- 02- 02") & (Date >="2007- 02- 01"))



library(ggplot2)
library(grid)
library(gridExtra)

#To create the Second Plot
our_data$Global_active_power <- as.numeric(levels(our_data$Global_active_power))[our_data$Global_active_power]
our_data$real_time <- strptime(paste(our_data$Date,our_data$Time),"%Y-%m-%d %H:%M:%S")

our_data$Sub_metering_1 <- as.numeric(levels(our_data$Sub_metering_1))[our_data$Sub_metering_1]
our_data$Sub_metering_2 <- as.numeric(levels(our_data$Sub_metering_2))[our_data$Sub_metering_2]
our_data$Sub_metering_3 <- as.numeric(levels(our_data$Sub_metering_3))[our_data$Sub_metering_3]

our_data$Voltage <- as.numeric(levels(our_data$Voltage))[our_data$Voltage]
our_data$Global_reactive_power <- as.numeric(levels(our_data$Global_reactive_power))[our_data$Global_reactive_power]


a <- ggplot(our_data,aes(Global_active_power))+
  geom_histogram(bins=11,fill="red")+
  xlab("Global Active Power (Kilowatts)")+ylab("Frequency")+ggtitle("Global Active Power") + theme(plot.title = element_text(hjust = 0.5))

b <- ggplot(our_data)+
  geom_line(mapping=aes(x=real_time,y=Voltage,color="black"))+xlab("datetime")


c <- ggplot(our_data)+
  geom_line(mapping=aes(x=real_time,y=Sub_metering_1,color="black"))+
  geom_line(mapping=aes(x=real_time,y=Sub_metering_2,color="red"))+
  geom_line(mapping=aes(x=real_time,y=Sub_metering_3,color="blue"))+
  ylab("Energy sub metering")

d <- ggplot(our_data)+
  geom_line(mapping=aes(x=real_time,y=Global_reactive_power,color="black"))+xlab("datetime")

grid.arrange(a,b,c,d)


dev.copy(png, file = "plot4.png", height = 480, width = 480)
dev.off()













