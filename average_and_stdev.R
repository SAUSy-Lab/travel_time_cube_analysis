# calc avg and SD for min values matrix
# uses outputs from min_times_by_origin_and_depart.R

# clear env
rm(list = ls())

# input folders - the hours were lookin' at
input_folders <- c("monday_0_1", "monday_12_1", "monday_4_5", "monday_20_21","saturday_0_1", "saturday_12_1", "saturday_4_5", "saturday_20_21")

# out matrix setup
out_matrix <- matrix(data = 0, nrow = 3685, ncol = 1)

# for each input folder
for (f in input_folders) {

  # grab min values matrix
  input_table <- paste(f,'/min_values_', f, '.csv', sep='')
  input_matrix <- read.table(input_table, header = TRUE, row.names = 1, sep = ",", check.names = FALSE)

  # average
  my_avg_fun <- function(q) {
    mean(q, na.rm = TRUE)
  }

  # standard deviation
  my_sd_fun <- function(q) {
    sd(q, na.rm = TRUE)
  }

  # NA count
  my_nac_fun <- function(q) {
    sum(is.na(q))
  }

  # do stats for each row to new matrices
  avg_mat <- matrix(apply(input_matrix,1,my_avg_fun))
  sd_mat <- matrix(apply(input_matrix,1,my_sd_fun))
  na_count <- matrix(apply(input_matrix,1,my_nac_fun))

  # add in row and column names to output
  rownames(avg_mat) <- rownames(input_matrix)
  colnames(avg_mat) <- paste("avg", f, sep="_")
  rownames(na_count) <- rownames(input_matrix)
  colnames(na_count) <- paste("na_count", f, sep="_")
  rownames(sd_mat) <- rownames(input_matrix)
  colnames(sd_mat) <- paste("sd", f, sep="_")

  # bind to final output
  out_matrix <- cbind(out_matrix,avg_mat)
  out_matrix <- cbind(out_matrix,sd_mat)
  out_matrix <- cbind(out_matrix,na_count)

}

out_matrix <- out_matrix[,-1]

# add column names
colnames(out_matrix) <- c("m_avg_0_1","m_sd_0_1","m_na_0_1", "m_avg_12_1","m_sd_12_1","m_na_12_1", "m_avg_4_5","m_sd_4_5","m_na_4_5","m_avg_20_21","m_sd_20_21","m_na_20_21","s_avg_0_1","s_sd_0_1","s_na_0_1", "s_avg_12_1","s_sd_12_1","s_na_12_1", "s_avg_4_5","s_sd_4_5","s_na_4_5","s_avg_20_21","s_sd_20_21","s_na_20_21")

# set nulls to 9999
out_matrix[is.na(out_matrix)] <- 9999
out_matrix[out_matrix == 'NaN'] <- 9999
write.csv(out_matrix, file = "DA_bounds/walking_stats.csv")
