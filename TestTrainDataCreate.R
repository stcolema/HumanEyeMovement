#Load Simon Barthelme's package mpp
#Used for multiple point processes, specifically
#inhomogenous spatial poisson point processes
require(mpp)
require(gdata)
require(ROCR)
require(pROC)
require(randomForest)
require(dplyr)
require(plyr)
require(data.table)
#set working directory to local folder
setwd("C:/Users/Captain Bluebear/Desktop/TP/Year 4/Semester 2 - Hillary/MA4492 - Project/R")

#----------------------------------------------------------------
#The relevant functions
#Compute new variable of previous location for given variable
markov_1st <- function(data, quantity.of.interest)
{
  markov <- rep(0, nrow(data))  
  for(i in 1:nrow(data))
  {
    if(i == 1)
    {
      markov[i] <- median(data[[quantity.of.interest]][!(is.na(data[[quantity.of.interest]]))])
    }
    else
    {
      markov[i] <- data[[quantity.of.interest]][i - 1]
    }
    if(is.na(markov[i]))
    {
      markov[i] <- median(data[[quantity.of.interest]])
    }
  }
  return(markov)
}

#creates new columns in data frame consisting of previous positions
#computes this for Lx, Ly, Rx and Ry
markov_data_frame <- function(data, rel_ind)
{
  for(i in names(data)[rel_ind])
  {
    col_name <- paste0("markov_", i)
    data[[col_name]] <- markov_1st(data, i)
  }
  return(data)
}

#markov over list of frames
markov_list_of_frames <- function(data, rel_ind)
{
  for(i in 1:length(data))
  {
    data[[i]] <- markov_data_frame(data[[i]], rel_ind)
  }
  return(data)
}

#creates a new dataframe with relevant "Markov" columns
new_data_frame <- function(old_data, rel_ind)
{
  data_new <- old_data
  data_new <- markov_data_frame(data_new, rel_ind)
  return(data_new)
}

#-------------------------------------------------------------
#Label each observation with an index
ind_create <- function(data)
{
  for(i in 1:length(data))
  {
    data[[i]][["Index"]] <- 1
    for(j in 1:nrow(data[[i]]))
    {
      data[[i]][["Index"]][j] <- j
    }
  }
  return(data)
}


#As well as the basic index we need a further index of which observer it
#is. Now an ordered pair of (Observer, Index) will be unique to each observation
create_observer_index <- function(data)
{
  for(i in 1:length(data))
  {
    data[[i]][["Observer"]] <- i
    if(i == 1)
    {
      new_frame <- data[[i]]
    }
    else
    { 
      new_frame <- rbind(new_frame, data[[i]])
    }
  }
  return(new_frame)
}


#----------------------------------------------------------------
#Fixation check
fixation_check <- function(data, fixation_dist = 5)
{
  data[["Fixation"]] <- 0
  data[["Fixation"]][sqrt((data[["Rx_pos"]] - data[["markov_Rx_pos"]])^2
                          + (data[["Ry_pos"]] - data[["markov_Ry_pos"]])^2) <= fixation_dist] <- 1
  data[["Fixation"]] <- factor(data[["Fixation"]])
  return(data)
}

#Remove all points that don't constitute fixations
remove_saccades <- function(data, fixation_dist = 5)
{
  data <- fixation_check(data, fixation_dist = fixation_dist)
  data_new <- data[data[["Fixation"]] == 1,]
  return(data_new)
}

#----------------------------------------------------------------
#Function to input data of After Kriging Simple format
#Names columns
AfterKrigingSimple_input_data <- function(file.name, krig = T)
{
#   data <- read.csv(file.name, sep = "")
  data <- read.table(file.name, sep = "")
  if(krig)
  {
    col_names <- c("Frame_no", "timestamp", "Lx_pos",  "Ly_pos",
                 "Lpupil_size", "Rx_pos", "Ry_pos", "Rpupil_size",
                 "Lx_var", "Ly_var", "Rx_var", "Ry_var")
  }
  else
  {
    col_names <- c("Frame_no", "timestamp", "Lx_pos",  "Ly_pos",
                   "Lpupil_size", "Rx_pos", "Ry_pos", "Rpupil_size")
  }
  names(data) <- col_names
  return(data)
}

#----------------------------------------------------------------
#creates a list of dataframes for data of Simple After Kriging format
#carries out markov processing too
input_all_people_simple_after_kriging <- function(no_of_people, krig = T, dimension = "2D")
{
  my_list <- list()
  rel_ind <- c(3,4,6,7)
  for(i in 1:no_of_people)
  {
    if(krig == T)
    {
#       file_name <- paste0("Person", i, "/2D/Person", i, "AfterKrigingSimple.csv")
      file_name <- paste0(dimension, "D/Person", i, "/Simple Format/After Kriging/2.txt")
      
      data <- AfterKrigingSimple_input_data(file_name)
    }
    else
    {
      # file_name <- paste0("Person", i, "/2D/Person", i, "BeforeKrigingSimple.csv")
      file_name <- paste0(dimension, "D/Person", i, "/Simple Format/Before Kriging/2.txt")
      data <- AfterKrigingSimple_input_data(file_name, krig = krig)
    }
    data <- markov_data_frame(data, rel_ind)
    data_new <- fixation_check(data)
    #     data_new <- remove_saccades(data_new)
    #     data_new <- markov_data_frame(data_new, rel_ind)
    #     data_new <- ind_create(data_new)
    my_list[[i]] <- data_new
    #       my_list[[i]] <- data
  }
  return(my_list)
}
#----------------------------------------------------------------
#Create list of positions for a given frame for either the x or y direction
create_pos_vec <- function(data, frame_no, x_or_y)
{
  #Create string with correct x or y name
  L_dir <- paste0("L", x_or_y, "_pos")
  R_dir <- paste0("R", x_or_y, "_pos")
  
  #create dataframe of x or y positions for specified frame
  dir.vec <- data.frame(L_dir = data[[L_dir]][data[["Frame_no"]] == frame_no],
                        R_dir = data[[R_dir]][data[["Frame_no"]] == frame_no])
  
  #name the columns of the dataframe (they have names L_dir and R_dir currently
  #instead of the string vlaues these represent)
  names(dir.vec) <- c(L_dir, R_dir)
  
  #create third column of "true" fixation point, average of two eyes poisiton
  true_val <- paste0("True ", x_or_y)
  dir.vec[[true_val]] <- 0.5*(dir.vec[[L_dir]] + dir.vec[[R_dir]])
  
  return(dir.vec)
}

#----------------------------------------------------------------
#A function to return the number of frames in the movie
#Sweeps all people and returns the max frame no
find_number_of_frames <- function(data)
{
  no_of_frames <- 0
  for(i in 1:length(data))
  {
    if(max(data[[i]][["Frame_no"]]) > no_of_frames)
    {
      no_of_frames <- max(data[[i]][["Frame_no"]])
    }
  }
  return(no_of_frames)
}
#----------------------------------------------------------------
#Function to return a list of dataframes
#Each dataframes is all the observations for each frame of the movie
create_list_of_fixations_per_frame <- function(data)
{
  x.list <- list()
  no_of_frames <- find_number_of_frames(data)
  for(i in 1:no_of_frames)
  {
    for(j in 1:length(data))
    {
      x.vec <- create_pos_vec(data[[j]], i, "x")
      y.vec <- create_pos_vec(data[[j]], i, "y")
      ind.vec <- data.frame(Index = data[[j]][["Index"]][data[[j]][["Frame_no"]] == i])
      obs.vec <- data.frame(Observer = rep(j, nrow(x.vec)))
      if(length(x.list) < i)
      {
        x.values <- x.vec
        y.values <- y.vec
        ind.values <- ind.vec
        obs.values <- obs.vec
      }
      else
      {
        x.values <- rbind(x.values, x.vec)
        y.values <- rbind(y.values, y.vec)
        ind.values <- rbind(ind.values, ind.vec)
        obs.values <- rbind(obs.values, obs.vec)
      }
      x.list[[i]] <- x.values
      x.list[[i]][["Ly_pos"]] <- y.values[["Ly_pos"]]
      x.list[[i]][["Ry_pos"]] <- y.values[["Ry_pos"]]
      x.list[[i]][["True y"]] <- y.values[["True y"]]
      x.list[[i]][["Index"]] <- ind.values
      x.list[[i]][["Observer"]] <- obs.values
    }
  }
  return(x.list)
}

#----------------------------------------------------------------
#creates a data frame of fixations
#thus it merges the observations that make up the fixation
#(a fixed viewpoint over the course of a frame) and finds the
#average x and y values
create_frame_of_fixations <- function(data)
{
  end_point <- max(data[["Frame_no"]])
  comp_frame <- data.frame("Index" = c(1:end_point), "Tx" = rep(0,end_point,), "Ty" = rep(0,end_point))
  
  for(i in 1:max(data[["Frame_no"]]))
  {
    comp_frame[i, "Ty"] <- 0.5*(mean(data[["Ly_pos"]][data[["Frame_no"]] == i & data[["Fixation"]] == 1]) + 
                                  mean(data[["Ry_pos"]][data[["Frame_no"]] == i & data[["Fixation"]] == 1]))
    comp_frame[i, "Tx"] <- 0.5*(mean(data[["Lx_pos"]][data[["Frame_no"]] == i & data[["Fixation"]] == 1]) + 
                                  mean(data[["Rx_pos"]][data[["Frame_no"]] == i & data[["Fixation"]] == 1]))
    
  }
  return(comp_frame)
}


#as above across a list of data frames
list_of_fixation_frames <- function(list_of_frames)
{
  new_list <- list()
  for(i in 1:length(list_of_frames))
  {
    new_list[[i]] <- create_frame_of_fixations(list_of_frames[[i]])
    new_list[[i]][["Observer"]] <- i
  }
  return(new_list)
}
#----------------------------------------------------------------

necessary_data_step <- function(no_of_observers, krig = T, dimension = 2)
{
  list_of_frames <- input_all_people_simple_after_kriging(no_of_observers, krig = krig, dimension = dimension)
  comp_list <- list_of_fixation_frames(list_of_frames)
  
  comp_list <- markov_list_of_frames(comp_list, c(2,3))
  
  list_of_frames <- ind_create(list_of_frames)
  
  list_of_obs <- create_list_of_fixations_per_frame(list_of_frames)
  
  df_all_obs_indexed <- create_observer_index(list_of_frames)
  
  for(i in 1 :length(comp_list))
  {
    if(i == 1)
    {
      df_fix <- comp_list[[1]]
    }
    else
    {
      df_fix <- rbind(df_fix, comp_list[[i]])
    }
  }
  return(df_fix)
}


#----------------------------------------------------------------
# Function to correct mistaken input
# (where X and Y inputs were mixed up)
fix_x_y_mix <- function(data)
{
  data[data[["Ty"]] > 1080 & !(is.na(data[["Tx"]])), c(2,3)] <- output <- data[data[["Ty"]] > 1080 & !(is.na(data[["Tx"]])), c(3,2)]
  output <- data
  return(output)
}

# Prep the data frames of Ziggy's data
prep_ziggy <- function(data)
{
  data[["Observer"]] <- factor(data[["Observer"]])
  data[["Frame_no"]] <- factor(data[["Index"]])

  data[["Y.grid"]] <- floor(data[["Ty"]]/(1080/67)) + 1
  data[["X.grid"]] <- floor(data[["Tx"]]/120) + 1

  data[["Measured.Fixation"]] <- 0
  data[["Measured.Fixation"]][!(is.na(data[["Tx"]]))] <- 1
  data[["Measured.Fixation"]] <- factor(data[["Measured.Fixation"]])
  return(data)
}

# Creates a data frame combining the description of each frame
# with the data about recorded fixations (for a given observer)
total_info_frame <- function(frame_data, fix_data)
{
  output <- merge(frame_data, fix_data, by.x = c("X.grid","Y.grid", "Index"), by.y = c("X.grid", "Y.grid", "Index")
                , all.x = T)
  output[["Measured.Fixation"]][is.na(output[["Measured.Fixation"]])] <- 0
  return(output)
}

# Creates a new data frame entirely consisting of fixation points
fix_frame <- function(data)
{
  output <- data[data[["Measured.Fixation"]] == 1,]
  return(output)
}

# this function returns the previous fixations coordinates.
grid_markov <- function(data, observer, no_of_frames = 1)
{
  output <- data[, c(1,2,8,11)]
  initial_view <- rep(c(60,34,0,observer), no_of_frames)
  output <- rbind(initial_view, output)
  # output <- rbind(c(60,34,0,observer), output)
  names(output) <- c(paste0("Prev.Fix.X.", no_of_frames), 
                     paste0("Prev.Fix.Y.", no_of_frames), 
                     "Frame.No", "Observer")

  output[["Frame.No"]] <- output[["Frame.No"]] + no_of_frames
  # output[["Dist_prev_fix"]] <- sqrt((output[["X.grid"]] - output[["Prev.Fix.X"]])^2
                                      # + (output[["Y.grid"]] - output[["Prev.Fix.Y"]])^2)
  return(output)
}

# Function that outputs the data frame in a format ready for
# analysis (bar the creation of test and training sets)
frame_fin_local <- function(data, observer, markov_no_of_frames)
{
  fix_data <- fix_frame(data)
  markov <- grid_markov(fix_data, observer, no_of_frames = markov_no_of_frames)
  output <- merge(data, markov, by.x = c("Index"), by.y = c("Frame.No")
                    , all.x = T)
  prev_x_name <- paste0("Prev.Fix.X.", markov_no_of_frames)
  prev_y_name <- paste0("Prev.Fix.Y.", markov_no_of_frames)
  output[["Dist_prev_fix"]] <- sqrt((output[["X.grid"]] - output[[prev_x_name]])^2
                                      + (output[["Y.grid"]] - output[[prev_y_name]])^2)
  return(output)
}




full_data <- function(fix_data, frame_data, observer)

{
  output <- fix_data[fix_data[["Observer"]] == observer,]
  output <- total_info_frame(frame_data, output)
  output[["Observer"]] <- observer
  output <- frame_fin_local(output, observer, 10)
  return(output)
}

train_test <- function(data)
{
  fix <- data[data[["Measured.Fixation"]] == 1,]
  not_fix <- data[data[["Measured.Fixation"]] == 0,]

  fix_train_ind <- sample(seq_len(nrow(fix)), size = nrow(fix)/3)
  fix_train <- fix[fix_train_ind,]
  fix_test <- fix[-fix_train_ind,]  

  not_fix_train_ind <- sample(seq_len(nrow(not_fix)), size = nrow(not_fix)/7500)
  not_fix_train <- not_fix[not_fix_train_ind,]
  not_fix_test <- not_fix[-not_fix_train_ind,]

  test_ind <- sample(seq_len(nrow(not_fix_test)), size = nrow(not_fix_test)/6000)
  not_fix_test_samp <- not_fix_test[test_ind,]
  test <- rbind(not_fix_test_samp, fix_test)

  train <- rbind(not_fix_train, fix_train)
  test <- rbind(not_fix_test_samp, fix_test)

  output <- list()
  output[[1]] <- train
  output[[2]] <- test
  return(output)
}

final_data <- function(fix_data, frame_data, observer)
{
  df <- full_data(fix_data, frame_data, observer)
  test_train_list <- train_test(df)
  return(test_train_list)
}

list_final_data <- function(fix_data, frame_data, no_of_people)
{
  output <- list()
  for(i in 1:no_of_people)
  {
    output[[i]] <- final_data(fix_data, frame_data, i)
  }
  return(output)
}

final_data_global <- function(fix_data, frame_data, no_of_people)
{
  train_test_list <- list()
  output <- list()
  local_train <- list()
  local_test <- list()
  for(i in 1:no_of_people)
  {
    output[[i]] <- final_data(fix_data, frame_data, i)
    local_train[[i]] <- output[[i]][[1]]
    local_test[[i]] <- output[[i]][[2]]

#     if(i == 1)
#     {
#       train <- local_train
#       test <- local_test
#     }
#     else
#     {
#       train <- rbind(train, local_train)
#       test <- rbind(test, local_test)
#     }
  }
  train_out <- rbind.fill(local_train)
  test_out <- rbind.fill(local_test)
  train_test_list[[1]] <- train_out
  train_test_list[[2]] <- test_out
  # return(output)
  return(train_test_list)
}


# Produces roc curve with 95% CI
roc_comp <- function(fit, test)
{
  fitted.results <- predict(fit, type = 'prob', newdata = test)
  predic <- prediction(fitted.results[,2], test[["Measured.Fixation"]])
  perf_comp <- performance(predic, "tpr", "fpr")
  roc_fit <- roc(test[["Measured.Fixation"]], 
                fitted.results[,2], ci = T, plot = F)
  sens.ci.fit <- ci.se(roc_fit, specificities=seq(0, 1, 0.1))
  plot(roc_fit, main="ROC curve", print.auc=TRUE)
  plot(sens.ci.fit, type="shape", col="lightblue")
  
}


data_prep <- function(no_of_people, krig = T, dimension = 2)
{
  output <- necessary_data_step(no_of_people, krig = krig, dimension = dimension)
  output <- fix_x_y_mix(output)
  output <- prep_ziggy(output)
  return(output)
}



prep_train_test <- function(train, test)
{
  output <- list()
  output[[1]] <- train[, c(4,5,6,12,21,22,24,20)]
  output[[2]] <- test[, c(4,5,6,12,21,22,24,20)]
  output[[1]] <- na.omit(output[[1]])
  output[[2]] <- na.omit(output[[2]])
  return(output)
}

data2grid <- function(data)
{
  output <- mutate(data,
                   X.grid = ceiling(Tx/16),
                   Y.grid = ceiling(Ty/(1080/67)))
  return(output)
}

prep_data_frame <- function(data)
{
  data_for_play <- as.data.table(df_fix_bef_3D) #[, names_to_keep])
  keys <- colnames(data_for_play)[!grepl("Measured.Fixation", colnames(data_for_play))]
  output <- data_for_play[, list(Fixations = sum(as.numeric(Measured.Fixation))), keys]
  output[["Fix.Cat"]] <- 0
  output[["Fix.Cat"]][output[["Fixations"]] != 0] <- 1
  return(output)
}


# Returns the relevant parts the frame description data
prep_for_train_test <- function(fix_data, frame_data)
{
  x <- fix_data[["X.grid"]]
  y <- fix_data[["Y.grid"]]
  frame_no <- fix_data[["Index"]]

  frame_fix <- frame_data[frame_data[["X.grid"]] %in% x &
                             frame_data[["Y.grid"]] %in% y &
                             frame_data[["Frame.No"]] %in% frame_no,]


  rows_in_frame_fix <- as.integer(as.numeric(row.names(frame_fix)))
  frame_no_fix <- frame_data[- rows_in_frame_fix,]
  test_ind <- sample(seq_len(nrow(frame_no_fix)), size = nrow(frame_no_fix)/8)
  frame_no_fix <- frame_no_fix[test_ind,]

  output <- list()
  output[[1]] <- frame_fix
  output[[2]] <- frame_no_fix
  return(output)
}

test_train_large_data <- function(train_test_list, output_of_prep_for_train_test, no_of_people, dimension = 3)
{
  output <- list()
  names(train_test_list[[1]])[4] <- "Frame.No"
  names(train_test_list[[2]])[4] <- "Frame.No"
  frame_fix <- output_of_prep_for_train_test[[1]]
  frame_no_fix <- output_of_prep_for_train_test[[2]]
  no_fix_ind <- sample(seq_len(nrow(frame_fix)), size = min(nrow(frame_fix)/13, 27000))
  if(dimension == 3)
  {
    # names(train_test_list[[1]])[4] <- "Frame.No"
    # names(train_test_list[[2]])[4] <- "Frame.No"
    names_for_test_train <- names(train_test_list[[1]])[c((1:30), 37)] #these are taken from 3d before krig
    # print(names_for_test_train)
  }
  else
  {
    names_for_test_train <- names(train_test_list[[1]])[c((1:21), 28)] #these are taken from 3d before krig
  }
  train_to_bind <- train_test_list[[1]][, names_for_test_train]
  test_to_bind <- train_test_list[[2]][, names_for_test_train]
  frame_no_fix[["Measured.Fixation"]] <- 0
  frame_no_fix_of_interest <- frame_no_fix[no_fix_ind,]
  frame_no_fix_of_interest_for_test <- frame_no_fix[- no_fix_ind,]
  # frame_no_fix_of_interest[["Measured.Fixation"]] <- 0
  # frame_no_fix_of_interest_for_test[["Measured.Fixation"]] <- 0
  frame_no_fix_of_interest <- frame_no_fix_of_interest[, names_for_test_train]
  output[[1]] <- rbind.fill(train_to_bind, frame_no_fix_of_interest)
  output[[2]] <- rbind.fill(test_to_bind, frame_no_fix_of_interest_for_test)
  return(output)
}

prep_data_frame <- function(data)
{
  data_for_play <- as.data.table(data) #[, names_to_keep])
  keys <- colnames(data_for_play)[!grepl("Measured.Fixation", colnames(data_for_play))]
  output <- data_for_play[, list(Fixations = sum(as.numeric(Measured.Fixation))), keys]
  output[["Fix.Cat"]] <- 0
  output[["Fix.Cat"]][output[["Fixations"]] != 0] <- 1
  return(output)
}


prep_train_test_local<- function(fix_data, frame_data, no_of_people, dimension = 2)
{
  data_prep_train_test <- prep_for_train_test(fix_data, frame_data)
  train_test_list <- final_data_global(fix_data, data_prep_train_test[[1]], no_of_people)
  output <- test_train_large_data(train_test_list, data_prep_train_test, no_of_people, dimension = dimension)
  return(output)
}

data2finalstage <- function(no_of_people = 25)
{
  for(i in 1:4)
  {
    print(i)
    if(i == 1)
    {
      dimension = 3
      krig = F
      fix_data_name <- "data_3D_beforeKrig.csv"
    }
    else if(i ==2)
    {
      dimension = 3
      krig = T
      fix_data_name <- "data_3D_afterKrig.csv"
    }
    else if(i == 3)
    {
      dimension = 2
      krig = F
      fix_data_name <- "data_2D_beforeKrig.csv"
    }
    else
    {
      dimension = 2
      krig = T
      fix_data_name <- "data_2D_afterKrig.csv"
    }
    fix_data <- data_prep(no_of_people, krig = krig, dimension = dimension)
#     fix_data <- read.csv(fix_data_name)
    fix_data[["Frame.No"]] <- fix_data[["Index"]]
    
    frame_data_name <- paste0("frame_description_", dimension, "D.csv")
    frame_data <- read.csv(frame_data_name)
    frame_data[["Index"]] <- frame_data[["Frame.No"]]

    print(names(frame_data))
    print(names(fix_data))
    
    output <- prep_train_test_local(fix_data, frame_data, no_of_people, dimension = dimension)
    rm(fix_data, frame_data)
    output1name <- paste0("train_data_dim", dimension, "_krig", krig, ".csv")
    output2name <- paste0("test_data_dim", dimension, "_krig", krig, ".csv")

    print("Got this far")
    
    write.csv(output[[1]],  file = output1name, row.names=FALSE)
    write.csv(output[[2]],  file = output2name, row.names=FALSE)

    output[[1]] <- prep_data_frame(output[[1]])
    output[[2]] <- prep_data_frame(output[[2]])

    output1name <- paste0("train_data_dim", dimension, "_krig", krig, "_prep_merge.csv")
    output2name <- paste0("test_data_dim", dimension, "_krig", krig, "_prep_merge.csv")

    write.csv(output[[1]],  file = paste0(output1name), row.names=FALSE)
    write.csv(output[[2]],  file = paste0(output2name), row.names=FALSE)
    
    rm(fix_data)
  }
}

data2finalstage()
