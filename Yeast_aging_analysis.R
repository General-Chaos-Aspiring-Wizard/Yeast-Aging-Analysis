#importing raw csv

library(readr)

data_18_12_23 <- read_csv("isoleucyne_pilot_18.12.23.csv")
data_20_12_23 <- read_csv("isoleucyna_pilot_20.12.23.csv")
data_22_12_23 <- read_csv("isoleucyne_pilot_22.12.23.csv")
data_28_12_23 <- read_csv("isolecine_pilot_28.12.23.csv")

#cutting off metadata columns and rows
data_18_12_23_1 <- data_18_12_23[, -c(1, 2)]
data_18_12_23_1 <- data_18_12_23_1[-c(1:60), ]
data_20_12_23_1 <- data_20_12_23[, -c(1, 2)]
data_20_12_23_1 <- data_20_12_23_1[-c(1:60), ]
data_22_12_23_1 <- data_22_12_23[, -c(1, 2)]
data_22_12_23_1 <- data_22_12_23_1[-c(1:60), ]
data_28_12_23_1 <- data_28_12_23[, -c(1, 2)]
data_28_12_23_1 <- data_28_12_23_1[-c(1:60), ]

#assigning column names corresponding to the well number

library(dplyr)

colnames(data_18_12_23_1) <- data_18_12_23_1[1, ]
colnames(data_20_12_23_1) <- data_20_12_23_1[1, ]
colnames(data_22_12_23_1) <- data_22_12_23_1[1, ]
colnames(data_28_12_23_1) <- data_28_12_23_1[1, ]

#first measurement was the most messy one. I create needed order of wells, 
# then I'm creating a vector of names meaningful in the experiment context
desired_order <- c('Cycle(Seconds)/Well', 
                   'G1', 'H1', 'A2', 'B2', 'C2', 'D2', 'E2', 'F2',
                   'G2', 'H2', 'A8', 'B3', 'C3', 'D3', 'E3', 'F3',
                   'G3', 'H3', 'A4', 'B4', 'C4', 'A1', 'B1', 'C1', 
                   'D1', 'E1', 'F1', 'D4')
col_names <- c('Time [seconds]', '0_1', '0_2', '0_3', '50_1', '50_2', '50_3', '100_1', '100_2', 
               '100_3', '150_1', '150_2', '150_3', '400_1', '400_2', '400_3', '500_1',
               '500_2', '500_3', '600_1', '600_2', '600_3', '0WT_1', '0WT_2', '0WT_3', 
               '100WT_1', '100WT_2', '100WT_3', 'Blank')

#Here I'm creating a new dataframe, that is constructed from columns
# that I specified.After that I'm adding the needed column names and 
#removing top row that was with column names from wells
data_18_12_23_clean <- data_18_12_23_1 %>%
  select(desired_order)
colnames(data_18_12_23_clean) <- col_names
data_18_12_23_clean <- data_18_12_23_clean[-1, ]

#Correcting the order of columns in the 20.12.2023

desired_order <- c('Cycle(Seconds)/Well', 
                   'A1', 'B1', 'C1', 'D1', 'E1', 'F1', 'G1', 'H1',
                   'A2', 'B2', 'C2', 'D2', 'E2', 'F2', 'G2', 'H2',
                   'A4', 'B4', 'C4', 'D4', 'E4', 'F4', 'G4', 'H4',
                   'A3', 'B3', 'C3', 'D3')

data_20_12_23_clean <- data_20_12_23_1 %>%
    select(desired_order)
colnames(data_20_12_23_clean) <- col_names
data_20_12_23_clean <- data_20_12_23_clean[-1, ]

#Correcting order and assigning column names in 22nd and 28th
desired_order <- c('Cycle(Seconds)/Well', 
                   'A1', 'B1', 'C1', 'D1', 'E1', 'F1', 'G1', 'H1',
                   'A2', 'B2', 'C2', 'D2', 'E2', 'F2', 'G2', 'H2',
                   'A3', 'B3', 'C3', 'D3', 'E3', 'F3', 'G3', 'H3',
                   'A4', 'B4', 'C4', 'D4')

data_22_12_23_clean <- data_22_12_23_1 %>%
    select(desired_order)
colnames(data_22_12_23_clean) <- col_names
data_22_12_23_clean <- data_22_12_23_clean[-1, ]

data_28_12_23_clean <- data_28_12_23_1 %>%
    select(desired_order)
colnames(data_28_12_23_clean) <- col_names
data_28_12_23_clean <- data_28_12_23_clean[-1, ]


#Data analysis part!:)

#creating a column of time in hours
time_hours <- as.numeric(data_18_12_23_clean$`Time [seconds]`)/3600
data_18_12_23_clean['Time [hours]'] <- time_hours

data_18_12_23_clean <- data_18_12_23_clean %>%
    select(1, 'Time [hours]', everything())

time_hours <- as.numeric(data_20_12_23_clean$`Time [seconds]`)/3600
data_20_12_23_clean['Time [hours]'] <- time_hours

data_20_12_23_clean <- data_20_12_23_clean %>%
    select(1, 'Time [hours]', everything())

time_hours <- as.numeric(data_22_12_23_clean$`Time [seconds]`)/3600
data_22_12_23_clean['Time [hours]'] <- time_hours

data_22_12_23_clean <- data_22_12_23_clean %>%
    select(1, 'Time [hours]', everything())

time_hours <- as.numeric(data_28_12_23_clean$`Time [seconds]`)/3600
data_28_12_23_clean['Time [hours]'] <- time_hours

data_28_12_23_clean <- data_28_12_23_clean %>%
    select(1, 'Time [hours]', everything())

#First reading is noise, I'll assign values at time 0 to be the same as 
#at the next reading 
data_18_12_23_clean[1, 3:30] <- data_18_12_23_clean[2, 3:30]
data_20_12_23_clean[1, 3:30] <- data_20_12_23_clean[2, 3:30]
data_22_12_23_clean[1, 3:30] <- data_22_12_23_clean[2, 3:30]
data_28_12_23_clean[1, 3:30] <- data_28_12_23_clean[2, 3:30]

#In my case some data has missing values. Here I'll remove rows with them
data_18_12_23_clean <- data_18_12_23_clean[-97,]
data_20_12_23_clean <- data_20_12_23_clean[-86:-97,]
data_28_12_23_clean <- data_28_12_23_clean[-90:-97,]

#Subtracting blank from readings and deleting blank column
data_18_12_23_normalized <- sapply(data_18_12_23_clean, as.numeric)
data_18_12_23_normalized[, 3:29] <- data_18_12_23_normalized[, 3:29] - data_18_12_23_normalized[, 30]
data_18_12_23_normalized <- data_18_12_23_normalized[, -30]

data_20_12_23_normalized <- sapply(data_20_12_23_clean, as.numeric)
data_20_12_23_normalized[, 3:29] <- data_20_12_23_normalized[, 3:29] - data_20_12_23_normalized[, 30]
data_20_12_23_normalized <- data_20_12_23_normalized[, -30]

data_22_12_23_normalized <- sapply(data_22_12_23_clean, as.numeric)
data_22_12_23_normalized[, 3:29] <- data_22_12_23_normalized[, 3:29] - data_22_12_23_normalized[, 30]
data_22_12_23_normalized <- data_22_12_23_normalized[, -30]

data_28_12_23_normalized <- sapply(data_28_12_23_clean, as.numeric)
data_28_12_23_normalized[, 3:29] <- data_28_12_23_normalized[, 3:29] - data_28_12_23_normalized[, 30]
data_28_12_23_normalized <- data_28_12_23_normalized[, -30]

#Making a new dataframe with columns averaged out
data_18_12_23_averaged <- as.data.frame(data_18_12_23_normalized[, 2])
for (i in seq(3, 29, by = 3)) {
    new_col_name <- paste0("Mean_", i)
    means <- rowMeans(data_18_12_23_normalized[, i:(i+2)])
    data_18_12_23_averaged[[new_col_name]] <- means
}

data_20_12_23_averaged <- as.data.frame(data_20_12_23_normalized[, 2])
for (i in seq(3, 29, by = 3)) {
    new_col_name <- paste0("Mean_", i)
    means <- rowMeans(data_20_12_23_normalized[, i:(i+2)])
    data_20_12_23_averaged[[new_col_name]] <- means
}

data_22_12_23_averaged <- as.data.frame(data_22_12_23_normalized[, 2])
for (i in seq(3, 29, by = 3)) {
    new_col_name <- paste0("Mean_", i)
    means <- rowMeans(data_22_12_23_normalized[, i:(i+2)])
    data_22_12_23_averaged[[new_col_name]] <- means
}

data_28_12_23_averaged <- as.data.frame(data_28_12_23_normalized[, 2])
for (i in seq(3, 29, by = 3)) {
    new_col_name <- paste0("Mean_", i)
    means <- rowMeans(data_28_12_23_normalized[, i:(i+2)])
    data_28_12_23_averaged[[new_col_name]] <- means
}

col_names <- c('Time [hours]', '0', '50', '100', '150', '400', '500', '600', '0WT', '100WT')
colnames(data_18_12_23_averaged) <- col_names
colnames(data_20_12_23_averaged) <- col_names
colnames(data_22_12_23_averaged) <- col_names
colnames(data_28_12_23_averaged) <- col_names

#plotting example growth curves
xdata <- data_22_12_23_averaged$`Time [hours]`
ydata <- c(data_18_12_23_averaged$'0', rep(NA, length(xdata)-length(data_18_12_23_averaged$'0')))
plot(xdata, ydata, xlab = 'Time [hours]', ylab = 'OD', type = 'l', col = 'blue')

ydata <- c(data_20_12_23_averaged$'0', rep(NA, length(xdata)-length(data_20_12_23_averaged$'0')))
lines(xdata, ydata, col = 'yellow')

ydata <- c(data_22_12_23_averaged$'0', rep(NA, length(xdata)-length(data_22_12_23_averaged$'0')))
lines(xdata, ydata, col = 'green')

ydata <- c(data_28_12_23_averaged$'0', rep(NA, length(xdata)-length(data_28_12_23_averaged$'0')))
lines(xdata, ydata, col = 'red')

legend("topleft", legend=c('18.12.2023', '20.12.2023', '22.12.2023', '28.12.2023'),
       col=c('blue', 'yellow', 'green', 'red'), lty = 1, cex=0.8)


ydata <- c(data_18_12_23_averaged$'100WT', rep(NA, length(xdata)-length(data_18_12_23_averaged$'100WT')))
plot(xdata, ydata, xlab = 'Time [hours]', ylab = 'OD', type = 'l', col = 'blue')

ydata <- c(data_20_12_23_averaged$'100WT', rep(NA, length(xdata)-length(data_20_12_23_averaged$'100WT')))
lines(xdata, ydata, col = 'yellow')

ydata <- c(data_22_12_23_averaged$'100WT', rep(NA, length(xdata)-length(data_22_12_23_averaged$'100WT')))
lines(xdata, ydata, col = 'green')

ydata <- c(data_28_12_23_averaged$'100WT', rep(NA, length(xdata)-length(data_28_12_23_averaged$'100WT')))
lines(xdata, ydata, col = 'red')

legend("topleft", legend=c('18.12.2023', '20.12.2023', '22.12.2023', '28.12.2023'),
       col=c('blue', 'yellow', 'green', 'red'), lty = 1, cex=0.8)


ydata <- c(data_18_12_23_averaged$'0WT', rep(NA, length(xdata)-length(data_18_12_23_averaged$'0WT')))
plot(xdata, ydata, xlab = 'Time [hours]', ylab = 'OD', type = 'l', col = 'blue')

ydata <- c(data_20_12_23_averaged$'0WT', rep(NA, length(xdata)-length(data_20_12_23_averaged$'0WT')))
lines(xdata, ydata, col = 'yellow')

ydata <- c(data_22_12_23_averaged$'0WT', rep(NA, length(xdata)-length(data_22_12_23_averaged$'0WT')))
lines(xdata, ydata, col = 'green')

ydata <- c(data_28_12_23_averaged$'0WT', rep(NA, length(xdata)-length(data_28_12_23_averaged$'0WT')))
lines(xdata, ydata, col = 'red')

legend("topleft", legend=c('18.12.2023', '20.12.2023', '22.12.2023', '28.12.2023'),
       col=c('blue', 'yellow', 'green', 'red'), lty = 1, cex=0.8)


ydata <- c(data_18_12_23_averaged$'600', rep(NA, length(xdata)-length(data_18_12_23_averaged$'600')))
plot(xdata, ydata, xlab = 'Time [hours]', ylab = 'OD', type = 'l', col = 'blue')

ydata <- c(data_20_12_23_averaged$'600', rep(NA, length(xdata)-length(data_20_12_23_averaged$'600')))
lines(xdata, ydata, col = 'yellow')

ydata <- c(data_22_12_23_averaged$'600', rep(NA, length(xdata)-length(data_22_12_23_averaged$'600')))
lines(xdata, ydata, col = 'green')

ydata <- c(data_28_12_23_averaged$'600', rep(NA, length(xdata)-length(data_28_12_23_averaged$'600')))
lines(xdata, ydata, col = 'red')

legend("topleft", legend=c('18.12.2023', '20.12.2023', '22.12.2023', '28.12.2023'),
       col=c('blue', 'yellow', 'green', 'red'), lty = 1, cex=0.8)


#Calculating doubling time
# Find the first value >= 0.2 and first value >= 0.5
for (i in colnames(data_18_12_23_averaged[-1])){
    
    logic_vector_02 <- data_18_12_23_averaged$'i' >= 0.2
    logic_vector_05 <- data_18_12_23_averaged$'i' >= 0.5
    index_02 <- which(logic_vector_02)[1]
    index_05 <- which(logic_vector_05)[1]

    # Extract corresponding time values
    t1 <- data_18_12_23_averaged$`Time [hours]`[index_02]
    t2 <- data_18_12_23_averaged$`Time [hours]`[index_05]
    OD1 <- as.numeric(data_18_12_23_averaged$'i'[index_02])
    OD2 <- as.numeric(data_18_12_23_averaged$'i'[index_05])
    
    # Calculate new values for each column
    new_col_name <- i
    data_18_12_23_averaged[[new_col_name]] <- log(2) / ((log(OD2) - log(OD1)) / (t2 - t1))
}
