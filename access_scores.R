# analysis of cube of Dissemination Areas (population) to
# TAZ (with employment) for typical morning commute (7-9)

# calculates access scores from travel-time cubes using simple threshold and gravity formulas

# clear env if necessary
rm(list = ls())

# input cube directory
schedule_dir <- 'schedule'
sched_matrices <- list.files(schedule_dir)

# output matri lists with access scores
access_T_list <- list()
access_G_list <- list()

# reading employment counts by TAZ
TAZ_emp <- read.csv('TAZ_Employment.csv')

# function basic access score to jobs with an input TTmatrix and threshold T
access_score_T <- function(transit_matrix, T) {
  access_matrix <- transit_matrix
  access_matrix[access_matrix <= T] <- 1
  access_matrix[access_matrix > T] <- NA
  access_matrix <- t(t(access_matrix) * c(TAZ_emp[,"wrkdest24h"])) #employment counts
  access_by_DA <- matrix(rowSums(access_matrix, na.rm = TRUE), nrow = 3685, ncol = 1)
  colnames(access_by_DA) <- T
  return(access_by_DA)
}

# gravity model access score to jobs with input TTmatrix and decay parameter B
access_score_G <- function(transit_matrix, B) {
  access_matrix <- transit_matrix
  access_matrix[access_matrix > 3600] <- NA
  access_matrix <- access_matrix ^ -B
  access_matrix <- t(t(access_matrix) * c(TAZ_emp[,"wrkdest24h"])) #employment counts
  access_by_DA <- matrix(rowSums(access_matrix, na.rm = TRUE), nrow = 3685, ncol = 1)
  colnames(access_by_DA) <- B
  return(access_by_DA)
}

f <- 1
# loop over each matrix csv in cube directory
for (sm in sched_matrices) {

  print(sm)

  # out matrix
  out_matrix <- matrix(data = NA, nrow = 3685, ncol = 1)

  sm_name <- paste(schedule_dir, '/', sm, sep='')

  # input matrix
  in_transit_matrix <- read.table(sm_name, header = TRUE, row.names = 1, sep = ",", check.names = FALSE)

  # row names to output
  rownames(out_matrix) <- rownames(in_transit_matrix)

  # have same for gravity
  G_out_matrix <- out_matrix

  # 0s to NA in transit matrix (e.g. those over 1 hour trave time)
  in_transit_matrix[in_transit_matrix == 0] <- NA

  # calc access scores for thressholds T
  x <- 60
  while (x <= 3600) {
    out_matrix <- cbind(out_matrix, access_score_T(in_transit_matrix, x))
    print(x)
    x <- x + 60
  }
  out_matrix <- out_matrix[,-1]

  # access for gravity
  BS <- c(0.5,1,2,3) # decay parameters
  for (b in BS) {
    G_out_matrix <- cbind(G_out_matrix, access_score_G(in_transit_matrix, b))
    print(b)
  }
  G_out_matrix <- G_out_matrix[,-1]

  access_T_list[[f]] <- out_matrix
  access_G_list[[f]] <- G_out_matrix
  f <- f + 1

}

# writing stats to tables for Threshold access
Reduce("+", access_T_list) / length(access_T_list)
T_means <- apply(simplify2array(access_T_list), 1:2, mean)
T_stdev <- apply(simplify2array(access_T_list), 1:2, sd)
write.csv(T_means, file = "stats/R_mon_access_T_means.csv")
write.csv(T_stdev, file = "stats/R_mon_access_T_sddev.csv")

# same but for gravity
Reduce("+", access_G_list) / length(access_G_list)
G_means <- apply(simplify2array(access_G_list), 1:2, mean)
G_stdev <- apply(simplify2array(access_G_list), 1:2, sd)
write.csv(G_means, file = "stats/Sched_access_G_means.csv")
write.csv(G_stdev, file = "stats/Sched_access_G_sddev.csv")
