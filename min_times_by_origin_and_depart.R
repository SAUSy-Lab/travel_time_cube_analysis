# finds the min value from each origina to any destination for each matrix in a cube
# outputs to a matrix of origins-depart_time with min travel times as cells

rm(list = ls())

# input folder name and go to it - this case for cube of one hour
in_folder = "saturday_12_1"
setwd(in_folder)

# grab all csvs in there
csvs <- list.files()

# set up output matrix and empty first column
min_minute_matrix <- matrix(data = 0, nrow = 3685, ncol = 1)
colnames(min_minute_matrix) <- "meow"

# loop thorugh csvs
for (csv in csvs) {

  print(csv)

  # read table
  csv_in <- read.table(csv, header = TRUE, row.names = 1, sep = ",", check.names = FALSE)

  # set minimum function without considering NA values
  my_min_function <- function(row) {
    min(row,na.rm = TRUE)
  }

  # get 1 col matrix of minimum value of each row, all NAs, then value is Inf (infinity)
  min_mat <- matrix(apply(csv_in,1,my_min_function))

  # add in row names
  rownames(min_mat) <- rownames(csv_in)
  rownames(min_minute_matrix) <- rownames(min_mat)

  # add in column names as csv file name
  colnames(min_mat) <- csv

  # combine 1 col min matrix to final matrix
  min_minute_matrix <- cbind(min_minute_matrix,min_mat)

}

# remove initial column
min_minute_matrix <- min_minute_matrix[,-1]
min_minute_matrix[min_minute_matrix == Inf] <- NA

# write output
write.csv(min_minute_matrix, file = paste("min_values_",in_folder,".csv", sep = ""))

# go back to parent dir ..
setwd('..')
