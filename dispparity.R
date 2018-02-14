# Author: Stephen Coleman
# The purpose of this code is to read in the features of a
# video and create a dataframe containing disparity between left
# and right camera points. This is eqiuvalent to depth.

# The features were extracted using MatLab


setwd("C:/Users/Captain Bluebear/Desktop/TP/Year 4/Semester 2 - Hillary/MA4492 - Project/R")

require(dplyr)
require(plyr)
require(data.table)

# Inputs the local feature csv files allowing for empty inputs
info_list <- function(folder_name, min_frame, max_frame)
{
  my_list <- list()
  for(i in min_frame:max_frame)
  {
    file_name <- paste0(folder_name, "/", folder_name, i, ".csv")
    try(my_list[[i - min_frame + 1]] <- read.csv(file_name, header = F), silent = T)
  }
  return(my_list)
}

#----------------------------------------------------------------------------------
create_true_coordinate <- function(data)
{
  output <- data.frame("X" = double(), "Y" = double())
  for(i in 1:length(data))
  {
    i_th_x_col <- data.frame("X" = rep(i, nrow(data)), "Y" = c(1:nrow(data)))
    output <- rbind(output, i_th_x_col)
  }
  return(output)
}

#----------------------------------------------------------------------------------

local_disp_frame <- function(local_data)
{
  for(i in 1:length(local_data))
  {
    ith_col <- as.data.frame(local_data[,i])
    if(i == 1)
    {
      output <- ith_col
    }
    else
    {
      output <- rbind(output, ith_col)
    }
  }
  names(output) <- c("Disparity")
  return(output)
}

combine_disp_coord <- function(local_data, coord_data)
{
  output <- cbind(local_data, coord_data)
  return(output)
}

#first vector is the x grid position of inputted data
#second is the y grid position
grid_position <- function(data, x.pos.name, y.pos.name)
{
  fix <- list()
  x_diff <- (1920 - max(data[[x.pos.name]]))/2
  y_diff <- (1080 - max(data[[y.pos.name]]))/2
  fix[[1]] <- ceiling((data[[x.pos.name]] + x_diff)/16)
  fix[[2]] <- ceiling((data[[y.pos.name]] + y_diff)/(1080/67))
  return(fix)
}

grid <- function(data)
{
  x_diff <- (1920 - max(data[[x.pos.name]]))/2
  y_diff <- (1080 - max(data[[y.pos.name]]))/2
  data[["X.grid"]] <- ceiling((data[["X"]] + x_diff)/16)
  data[["Y.grid"]] <- ceiling((data[["Y"]] + y_diff)/(1080/67))
  return(data)
}

convert_to_grid <- function(data)
{
  data[["Disp.grid"]] <- 0
  for(i in 1:max(data[["X.grid"]]))
  {
    for(j in 1:max(data[["Y.grid"]]))
    {
      data[["Disp.grid"]][data[["X.grid"]] == i & data[["Y.grid"]] == j] <- mean(data[["Disparity"]][data[["X.grid"]] == i & data[["Y.grid"]] == j])
    }
  }
  return(data)
}

simplify_to_grid <- function(data)
{
  output <- data[, c("X.grid", "Y.grid", "Disp.grid", "Frame.No")]
  output <- unique(output)
  return(output)
}

global_convert <- function(list_data_name, min_frame, max_frame)
{
  dispMap <- info_list(list_data_name, min_frame, max_frame)
  disp_list <- list()
  for(i in 1:length(dispMap))
  {
    coordinate_frame <- create_true_coordinate(dispMap[[i]])
    disp_list[[i]] <- local_disp_frame(dispMap[[i]])
    info_frame <- combine_disp_coord(disp_list[[i]], coordinate_frame)
    info_frame <- convert_to_grid(info_frame)
    output <- simplify_to_grid(info_frame)
    disp_list[[i]] <- info_frame
  }
  return(disp_list)
}

convert_list_to_frame <- function(list)
{
  for(i in 1:length(list))
  {
    if(i == 1)
    {
      output <- list[[i]]
    }
    else
    {
      output <- rbind(output, list[[i]])      
    }
  }
  return(output)
}

#----------------------------------------------------------------------------------

add_frame_no_global <- function(list, frame_no_list)
{
  for(i in 1:length(frame_no_list))
  {
    list[[i]][["Frame.No"]] <- frame_no_list[i]
  }
  return(list)
}

#----------------------------------------------------------------------------------

the_whole_regime <- function(list_data_name, pieces)
{
  for(i in 1:(length(pieces) - 1))
  {
    min_frame <- pieces[i] + 1
    max_frame <- pieces[i + 1]
    list <- global_convert(list_data_name, min_frame, max_frame)
    list <- add_frame_no_global(list, c(min_frame:max_frame))
    info_frame <- convert_list_to_frame(list)
    file_name <- paste0("disparityframe", min_frame, "to", max_frame, ".csv")
    write.csv(info_frame, file = file_name, row.names = FALSE)
  }
}

#----------------------------------------------------------------------------------
combine_disp <- function(pieces)
{
  for(i in 1:(length(pieces) - 1))
  {
    min_frame <- pieces[i] + 1
    max_frame <- pieces[i + 1]
    file_name <- paste0("disparityframe", min_frame, "to", max_frame, ".csv")
    disp <- read.csv(file_name)
    if(i == 1)
    {
      output <- disp
    }
    else
    {
      ouput <- rbind(output, disp)
    }
  }
  return(output)
}

# As in InputFunctionsInfo
create_frame_of_covariates <- function(x_max = 120, y_max = 67)
{
  output <- data.frame("x" = double(), "y"= double())
  for(i in 1:x_max)
  {
    new_rows <- data.frame("x" = rep(i, y_max), "y" = c(1:y_max))
    output <- rbind(output, new_rows)
  }
  return(output)
}

# Removes undesired columns from disparity frame
remove_undesired_columns <- function(data)
{
  len <- length(data)
  for(i in 1:length(data))
  {
    if(!(names(data)[len + 1 - i] %in% c("X.grid", "Y.grid", "Frame.No", "Disparity")))
    {
      data <- data[, -(len + 1 - i)]
    }
  }
  return(data)
}

# Outputs data frame of Disparity from <frame_diff> number
# of frame ago
disp_change <- function(frame_data, frame_diff)
{
  data <- frame_data
  data[["Frame.No"]] <- data[["Frame.No"]] + frame_diff
  Disparity_median <- median(data[["Disparity"]])

  local_data <- create_frame_of_covariates()
  local_data[["Disparity"]] <- Disparity_median
  top_rows <- list()
  output <- data.frame("X.grid" = double(), "Y.grid" = double(),
                       "Disparity" = double(), "Frame.No" = double())
  for(i in 1:frame_diff)
  {
    top_rows[[i]] <- local_data
    top_rows[[i]][["Frame.No"]] <- i
    output <- rbind(output, top_rows[[i]])
  }
  output_to_drop <- max(data[["Frame.No"]]) - frame_diff + 1
  data_to_keep <- data[data[["Frame.No"]] < output_to_drop,]
  
  len <- length(data_to_keep)
  data_of_interest <- remove_undesired_columns(data_to_keep)
  names(output) <- c("X.grid", "Y.grid", "Disparity", "Frame.No")
  final_output <- rbind(output, data_of_interest)
  return(final_output)
}

# Merges frame description with Disparity frames
disp_marge <- function(disp_data, frame_data)
{
  output <- merge(frame_data, disp_data, by.x = c("X.grid", "Y.grid", "Frame.No"),
                  by.y = c("X.grid", "Y.grid", "Frame.No"))
  return(output)
}

# 
rename_disp <- function(disp_delay, data)
{
  disp.name <- paste0("Disparity", disp_delay)
  names <- c(names(data)[c(1:(length(data)-1))], disp.name)
  return(names)
}

#----------------------------------------------------------------------------------
# I discovered dplyr, plyr and data.table at this point
# Creates data.frame of disparity readings with (x,y) location
ozymandias <- function(data)
{
  names(data) <- 1:length(data)
  data_on_coord <- melt(as.matrix(data))
  names(data_on_coord) <- c("Y", "X", "Disparity")
  data_on_coord <- data_on_coord[, c("X", "Y", "Disparity")]
  return(data_on_coord)
}

# With disparity not all of either 2D image is included.
# Only the overlap is; thus the grid position of a value
# is skewed by the amount cut off. For instance, if the
# depth map covers 1060*1900 then it is 20 pixels shorter
# in both length and height. This means (10,10) is the
# same location as (20,20) in 2D frame (cameras overlap
# in a symmetric fashion). This is the reason for the
# x_diff and y_diff variables.
break_some_heads <- function(data)
{
  x_diff <- (1920 - max(data[["X"]]))/2
  y_diff <- (1080 - max(data[["Y"]]))/2
  
  output <- mutate(data, X.grid = ceiling((X + x_diff)/16))
  output <- mutate(output, Y.grid = ceiling((Y + y_diff)/(1080/67)))
  return(output)
}

# This outputs a frame corresponding to the 67*120 grid.
# The disparity in each square is the mean across the square.
finally_lad <- function(data, names_to_keep)
{
  data_for_play <- as.data.table(data[, names_to_keep])
  keys <- colnames(data_for_play)[!grepl("Disparity", colnames(data_for_play))]
  output <- data_for_play[, list(Disp.grid = mean(Disparity)), keys]
  return(output)
}

# Does the lot and adds a frame no
across_list <- function(data, names_to_keep, frame_no)
{
  local_data <- ozymandias(data)
  local_data <- break_some_heads(local_data)
  output <- finally_lad(local_data, names_to_keep)
  output[["Frame.No"]] <- frame_no
  return(output)
}

# At this point everything was coming together. Please see
# Pippa Passes by Robert Browning.
the_larks_on_the_wing <- function(list_data, names_to_keep, min_frame)
{
  loc_list <- list()
  for(i in 1:length(list_data))
  {
    
    loc_list[[i]] <- across_list(list_data[[i]], names_to_keep, (min_frame + i - 1))
  }
  return(loc_list)
}

# Again, Pippa Passes.
the_snail_is_on_the_thorn <- function(list_data_name, pieces, names_to_keep)
{
  result <- list()
  for(i in 1:(length(pieces)- 1))
  {
    min_frame <- pieces[i] + 1
    max_frame <- pieces[i + 1]
    map_list <- info_list(list_data_name, min_frame, max_frame)
    info_list <- the_larks_on_the_wing(map_list, names_to_keep, min_frame)
    result[[i]] <- rbind.fill(info_list)
    
    # The lines commented out were used in case the final result should
    # exceed my RAM.
#     file_name <- paste0("disparityframe/disparityframe", min_frame, "to", max_frame, ".csv")
#     write.csv(result[[i]], file = file_name, row.names = FALSE)
  }
  result <- rbind.fill(result)
  
  return(result)
}

#---------------------------------------------------------------------------------- 
list_data_name <- "disparityMap"
names_to_keep <- c("X.grid", "Y.grid", "Disparity")
# pieces <- c(0, 3,7)

pieces <- c(0, 12, 23, 35, 46, 58, 69, 81, 92, 104, 115,
            127, 138, 150, 161, 173, 184, 196, 207, 219,
            230, 242, 253, 265, 276, 288, 299, 311, 322,
            334, 345, 357, 368, 380, 391, 403, 414, 426,
            437, 449, 460)


disp_cat <- the_snail_is_on_the_thorn("disparityMap", pieces, names_to_keep)
write.csv(disp_cat, file = "disparityFrame.csv", row.names = FALSE)

# pieces <- c(0:2)
# pieces <- c(0:460)
# the_whole_regime("disparityMap", pieces)

#----------------------------------------------------------------------------------

# disparityframe <- combine_disp(pieces)
BIG_MAN <- read.csv("frame_description_sal.csv")

# BIG_MAN <- merge(BIG_MAN, disparityframe, by.x = c("X.grid", "Y.grid", "Frame.No"), 
         # by.y = c("X.grid", "Y.grid", "Frame.No"), all.x = T)

BIG_MAN <- merge(BIG_MAN, disp_cat, by.x = c("X.grid", "Y.grid", "Frame.No"), 
           by.y = c("X.grid", "Y.grid", "Frame.No"), all.x = F)

names(BIG_MAN)[17] <- "Disparity"

write.csv(BIG_MAN, file = "frame_description_disp.csv", row.names = FALSE)
# rm(disparityframe)
#----------------------------------------------------------------------------------
disp_10 <- disp_change(BIG_MAN, 10)
disp_25 <- disp_change(BIG_MAN, 25)
disp_50 <- disp_change(BIG_MAN, 50)
disp_100 <- disp_change(BIG_MAN, 100)

#----------------------------------------------------------------------------------
outcat <- disp_marge(disp_10, BIG_MAN)
names(outcat) <- rename_disp(10, outcat)
outcat <- disp_marge(disp_25, outcat)
names(outcat) <- rename_disp(25, outcat)
outcat <- disp_marge(disp_50, outcat)
names(outcat) <- rename_disp(50, outcat)
outcat <- disp_marge(disp_100, outcat)
names(outcat) <- rename_disp(100, outcat)

names_new <- c(names(outcat)[c(1:(length(BIG_MAN) - 1))],
               "Disparity", names(outcat)[c((length(BIG_MAN)  + 1):length(outcat))])

names(outcat) <- names_new
#----------------------------------------------------------------------------------
outcat <- mutate(outcat,
                 Disp.Change.10 = Disparity - Disparity10,
                 Disp.Change.25 = Disparity - Disparity25,
                 Disp.Change.50 = Disparity - Disparity50,
                 Disp.Change.100 = Disparity - Disparity100,
       
                 Sal.Change.10 = Saliency - Saliency10,
                 Sal.Change.25 = Saliency - Saliency25,
                 Sal.Change.50 = Saliency - Saliency50,
                 Sal.Change.100 = Saliency - Saliency100)

names2D <- names(outcat)[c(1:16, 26:29)]

write.csv(outcat,  file = "frame_description_3D.csv",row.names=FALSE)
write.csv(outcat[, names2D],  file = "frame_description_2D.csv",row.names=FALSE)
