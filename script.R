table <- read.csv("C:/Users/Andrew Hsu/OneDrive/Desktop/Aquarium/IT/R/repdata_data_activity/activity.csv")

steps_per_day <- with(table, tapply(steps, date, sum, na.rm = T))
steps_per_day <- data.frame(steps_per_day)
steps_per_day <- cbind(date = rownames(steps_per_day), steps_per_day)
rownames(steps_per_day) <- 1:nrow(steps_per_day)
class(steps_per_day$date) <- "date"
hist(steps_per_day$steps_per_day, main = "Total Steps per Day", xlab = "Number of Steps")

mean(steps_per_day$steps_per_day, na.rm = T)
median(steps_per_day$steps_per_day, na.rm = T)

steps_per_interval <- with(table, tapply(steps, interval, mean, na.rm = T))
steps_per_interval <- data.frame(steps_per_interval)
steps_per_interval <- cbind(interval = rownames(steps_per_interval), steps_per_interval)
rownames(steps_per_interval) <- 1:nrow(steps_per_interval)
with(steps_per_interval, plot(interval, steps_per_interval, type = "l", main = "Mean Steps per 5-minute-interval"))

steps_per_interval[which.max(steps_per_interval$steps_per_interval), ]

sum(is.na(table))
table[is.na(table)] <- round(steps_per_interval$steps_per_interval)

new_steps_per_day <- with(table, tapply(steps, date, sum))
new_steps_per_day <- data.frame(new_steps_per_day)
new_steps_per_day <- cbind(date = rownames(new_steps_per_day), new_steps_per_day)
rownames(new_steps_per_day) <- 1:nrow(new_steps_per_day)
with(new_steps_per_day, hist(new_steps_per_day, main = "New Count of Steps per Day", xlab = "Number of Steps"))

table$date <- as.Date(table$date)
table$day <- weekdays(table$date, abbr = T)
weekday_table <- subset(table, day %in% c("Mon", "Tue", "Wed", "Thu", "Fri"))
weekend_table <- subset(table, day %in% c("Sat", "Sun"))
weekday_table <- with(weekday_table, tapply(steps, interval, mean))
weekday_table <- data.frame(weekday_table)
weekday_table <- cbind(interval = rownames(weekday_table), weekday_table)
rownames(weekday_table) <- 1:nrow(weekday_table)
colnames(weekday_table)[2] <- "mean_steps_per_interval"
weekend_table <- with(weekend_table, tapply(steps, interval, mean))
weekend_table <- data.frame(weekend_table)
weekend_table <- cbind(interval = rownames(weekend_table), weekend_table)
rownames(weekend_table) <- 1:nrow(weekend_table)
colnames(weekend_table)[2] <- "mean_steps_per_interval"
par(mfrow = c(1, 2))
with(weekday_table, plot(interval, mean_steps_per_interval, type = "l", main = "Weekdays"))
with(weekend_table, plot(interval, mean_steps_per_interval, type = "l", main = "Weekends"))