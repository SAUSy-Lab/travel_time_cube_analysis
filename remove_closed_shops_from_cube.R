# removes values from travel time cube if destinations are closed
# in this case, destinations are supermarkets

# clear env
rm(list = ls())

# inputs
walk_matrix_all <- read.table("walk_matrix_45.csv", header = TRUE, row.names = 1, sep = ",", check.names = FALSE) # DAs to supermarkets
store_hours <- read.table("super_hours_update_99.csv", header = TRUE, row.names = 1, sep = ",", check.names = FALSE) # supermarket hours

# for each minute in an hour
minute_list = c(0:59)
for (minute in minute_list) {

  # departure time starting
  day <- "Saturday"
  day_num <- 9
  hour <- 12 # 0 to 23 starting hour
  # minute <- 0 # 0 to 59 as in loop

  shop_time <- 20 # min shopping time in minutes

  # grab column names for the day we're lookin' at
  open_field_1 <- paste(day, "_Open", sep='')
  close_field_1 <- paste(day, "_Close", sep='')
  open_field_2 <- paste(day, "_Open_2", sep='')
  close_field_2 <- paste(day, "_Close_2", sep='')

  # out_matrix
  out_matrix <- walk_matrix_all

  # time of arrival to store
  get_there_time <- ((out_matrix / 60) / 60)
  get_there_time[get_there_time == 0] <- NA
  get_there_time <- get_there_time + hour + minute / 60

  # min time of departure from store
  leave_there_time <- get_there_time + (shop_time / 60)

  # grab boolean matrices if store is open when you get there and have time to shop
  open_bool_matrix_1 <- t(t(get_there_time) > (store_hours[,open_field_1]))
  open_bool_matrix_2 <- t(t(get_there_time) > (store_hours[,open_field_2]))
  close_bool_matrix_1 <- t(t(leave_there_time) < (store_hours[,close_field_1]))
  close_bool_matrix_2 <- t(t(leave_there_time) < (store_hours[,close_field_2]))
  first_set <- open_bool_matrix_1 & close_bool_matrix_1
  second_set <- open_bool_matrix_2 & close_bool_matrix_2
  total_truth <- first_set | second_set

  # output matrix and write
  out_matrix <- out_matrix * total_truth
  out_matrix[out_matrix == 0] <- NA
  file_name <- paste("saturday_12_1/walk", day, day_num, hour, minute, ".csv", sep='_')
  write.csv(out_matrix, file = file_name)

}

rm(list = ls())
