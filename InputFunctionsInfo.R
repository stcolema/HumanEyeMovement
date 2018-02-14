# Author: Stephen Coleman
# The purpose of this code is to read in the features of a
# video and create a dataframe containing these feature descriptions.
# The features were extracted using MatLab


setwd("C:/Users/Captain Bluebear/Desktop/TP/Year 4/Semester 2 - Hillary/MA4492 - Project/R")

# Inputs the local feature csv files allowing for empty inputs
info_list <- function(folder_name)
{
  my_list <- list()
  for(i in 1:460)
  {
    file_name <- paste0(folder_name, "/", folder_name, "_", i, ".csv")
    try(my_list[[i]] <- read.csv(file_name, header = F), silent = T)
  }
  return(my_list)
}

#----------------------------------------------------------------------------------
#function that returns list of two vectors
#first vector is the x grid position of inputted data
#second is the y grid position
grid_position <- function(data, x.pos.name, y.pos.name)
{
  fix <- list()
  fix[[1]] <- ceiling(data[[x.pos.name]]/16)
  fix[[2]] <- ceiling(data[[y.pos.name]]/(1080/67))
  return(fix)
}


#----------------------------------------------------------------------------------
# The following functions prep the fixation data list
prep_fixations <- function(data)
{
  data_grid <- grid_position(data, "X.value", "Y.value")
  data[["X.grid"]] <- data_grid[[1]]
  data[["Y.grid"]] <- data_grid[[2]]
  data[["Index"]] <- c(1:nrow(data))
  return(data)
}


fix_final <- function(list_data)
{
  for(i in 1:length(list_data))
  {
    names(list_data[[i]]) <- c("Y.value", "X.value")
    list_data[[i]] <- prep_fixations(list_data[[i]])
  }
  return(list_data)
}

#----------------------------------------------------------------------------------

# images are of resolution 1080x1920x3
# salMap breaks this into a grid of 67x120
# (1,1) is the top left corner of the image! Huzzah.
# note that there are 120 patches in the X-direction

#----------------------------------------------------------------------------------
# This function creates a data frame relating to the 
# 120*67 grid sal map uses
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

# frame_cov <- create_frame_of_covariates()

fix2frame <- function(data, fix_data)
{
  output <- data
  for(j in 1:nrow(fix_data))
  {
    output[["Fixation"]][output[["X.grid"]] == fix_data[["X.grid"]][j] &
                output[["Y.grid"]] == fix_data[["Y.grid"]][j]] <- fix_data[["Index"]][j]
  }
  output[["Fixation"]] <- factor(output[["Fixation"]])
  return(output)
}

#----------------------------------------------------------------------------------

# Turns the saliency entries into a data frame of 1 column
saliency_column <- function(data)
{
  output <- data.frame(double())
  list_local <- list()
  for(i in 1:length(data))
  {
    list_local[[i]] <- data[,i]
    output <- rbind(output, as.data.frame(list_local[[i]]))
  }
  output <- as.data.frame(output)
  output[is.na(output),] <- 0   # The only values salMap is NaN for is (0,0) is 0
  names(output) <- "Saliency"
  return(output)
}

#----------------------------------------------------------------------------------
# Creates the data frame in which frame feature info will be stored
# Stores saliency info and Walther's prediction of a fixation
frame_info_frame <- function(frame_no)
{
  grid_frame <- create_frame_of_covariates()
  output <- data.frame("X.grid" = grid_frame[,1], "Y.grid" = grid_frame[,2],
                       "Saliency" = rep(0,nrow(grid_frame)),
                       "Fixation" = rep(0,nrow(grid_frame)))
  output <- fix2frame(output, fixations[[frame_no]])
  output[["Saliency"]] <- saliency_column(salMap[[frame_no]])[,1]
  return(output)
}

#----------------------------------------------------------------------------------
#The next section of code is dedicated to cleaning and prepping face data

#Sets up the list of data frames FACE for following steps
prep_face <- function(list_frame)
{
  for(i in 1:length(list_frame))
  {
    if (length(list_frame[[i]]) == 0)
    {
      list_frame[[i]] <- data.frame("Y.ltc" = double(), "X.ltc" = double(),
                              "Y.length" = double(),
                              "X.length" = double())
    }
    else
    {
      names(list_frame[[i]]) <- c("Y.ltc", "X.ltc",
                            "Y.length","X.length")
    }
  }
  return(list_frame)
}

# Identifies the top left corner of the face box in terms of a
# 120*67 grid
grid_identify_left_top_corner <- function(list_data)
{
  list_data_fix_list <- list()
  for(i in 1:length(list_data))
  {
    list_data_fix_list[[i]] <- grid_position(list_data[[i]], "X.ltc", "Y.ltc")  
    list_data[[i]][["X.grid.ltc"]] <- list_data_fix_list[[i]][[1]]
    list_data[[i]][["Y.grid.ltc"]] <- list_data_fix_list[[i]][[2]]
  }
  return(list_data)
}

# For the face box data, returns the (x,y) locaiton of each corner
# (x,y) coordinates for left top corner (ltc), right bottom corner (rbc)
# and right top corner (rtc) for each face box.
# Again remember MatLab inverts the Y-axis
grid_col <- function(list_data)
{
  for(i in 1:length(list_data))
  {
    list_data[[i]][["Y.lbc"]] <- list_data[[i]][,1] + list_data[[i]][,3]
    list_data[[i]][["X.lbc"]] <- list_data[[i]][,2]
    list_data[[i]][["Y.rtc"]] <- list_data[[i]][,1]
    list_data[[i]][["X.rtc"]] <- list_data[[i]][,2] + list_data[[i]][,4]
    list_data[[i]][["Y.rbc"]] <- list_data[[i]][,1] + list_data[[i]][,3]
    list_data[[i]][["X.rbc"]] <- list_data[[i]][,2] + list_data[[i]][,4]
  }
  return(list_data)
}
 
# Identifies on a 120*67 grid the location of each corner of the input
# Thus converts an input on a scale of 1820*1080 to 120*67
grid_identify_each_corner_global <- function(list_data)
{
  list_data_grid <- list()

  for(i in 1:length(list_data))
  {
#     print(i)
#     if(nrow(list_data[[i]]) != 0)
#     {
      list_data_grid[[i]] <- list()
      list_data_grid[[i]][[1]] <- grid_position(list_data[[i]], "X.ltc", "Y.ltc")
      list_data_grid[[i]][[2]] <- grid_position(list_data[[i]], "X.lbc", "Y.lbc")
      list_data_grid[[i]][[3]] <- grid_position(list_data[[i]], "X.rtc", "Y.rtc")
      list_data_grid[[i]][[4]] <- grid_position(list_data[[i]], "X.rbc", "Y.rbc")
#     }
  }
  return(list_data_grid)
}

# Returns the (x,y) of each corner on grid for a data frame
grid_identify_each_corner_local <- function(data)
{
  data_grid <- list()
  data_grid[[1]] <- grid_position(data, "X.ltc", "Y.ltc")
  data_grid[[2]] <- grid_position(data, "X.lbc", "Y.lbc")
  data_grid[[3]] <- grid_position(data, "X.rtc", "Y.rtc")
  data_grid[[4]] <- grid_position(data, "X.rbc", "Y.rbc")
  return(data_grid)
}

# creates lsit of information about each face
# data is a data frame
# list_data_grid is a list of information about the grid
# index is the element of which list we're looking at
grid_data <- function(data)
{
  data_grid <- grid_identify_each_corner_local(data)
  grid <- list()
  if(nrow(data))
  {
    for(k in 1:nrow(data)) #in basic version k is face number
    {
      grid[[k]] <- data.frame("X.grid" = double(),
                        "Y.grid" = double(),
                        "Face.Area" = double(),
                        "Face.Order" = double())

      for(i in data_grid[[1]][[1]][k]:data_grid[[4]][[1]][k])
      {
        for(j in data_grid[[1]][[2]][k]:data_grid[[4]][[2]][k])
        { 
          if(i == data_grid[[1]][[1]][k] | i == data_grid[[4]][[1]][k] |
               j == data_grid[[1]][[2]][k] | j ==data_grid[[4]][[2]][k])
          {
            grid[[k]] <- rbind(grid[[k]], c(i,j,0,k))
          }
          else
          {
            grid[[k]] <- rbind(grid[[k]], c(i,j,1,k))
          }
        }
      }
      names(grid[[k]]) <- c("X.grid", "Y.grid", "Face.Area", "Face.Order")
    }
  }
  return(grid)
}

# Calculates the area of each grid square containing the object of interest
# This was built with faces in mind; it calculates how much of the square contains a face
data_frame_ind_face_info <- function(data, list_element, face_number)
  {
    for(i in 1:nrow(data))
    {
    if(data[i,1] == min(data[,1]) & data[i,2] != min(data[,2]) 
       & data[i,2] != max(data[,2]))
    {
      area <- ceiling(list_element[["X.ltc"]][face_number]/16) - list_element[["X.ltc"]][face_number]/16    
    }
    else if(data[i,1] == min(data[,1]) & data[i,2] == min(data[,2]))
    {
      area <- ((ceiling(list_element[["X.ltc"]][face_number]/16) - list_element[["X.ltc"]][face_number]/16))*
        (ceiling(list_element[["Y.ltc"]][face_number]/(1080/67)) - list_element[["Y.ltc"]][face_number]/(1080/67))
    }
    else if(data[i,1] == min(data[,1]) & data[i,2] == max(data[,2]))
    {
      area <- (ceiling(list_element[["X.ltc"]][face_number]/16) - list_element[["X.ltc"]][face_number]/16)*
        (list_element[["Y.ltc"]][face_number]/(1080/67) - floor(list_element[["Y.ltc"]][face_number]/(1080/67)))
    }
    else if(data[i,1] == max(data[,1]) & data[i,2] != min(data[,2]) 
            & data[i,2] != max(data[,2]))
    {
      area <- list_element[["X.rbc"]][face_number]/16 - floor(list_element[["X.rbc"]][face_number]/16)
    }
    else if(data[i,1] == max(data[,1]) & data[i,2] == min(data[,2]))
    {
      area <- (list_element[["X.rbc"]][face_number]/16 - floor(list_element[["X.rbc"]][face_number]/16))*
        (ceiling(list_element[["Y.ltc"]][face_number]/(1080/67)) - list_element[["Y.ltc"]][face_number]/(1080/67))
    }
    else if(data[i,1] == max(data[,1]) & data[i,2] == max(data[,2]))
    {
      area <- (list_element[["X.rbc"]][face_number]/16 - floor(list_element[["X.rbc"]][face_number]/16))*
        (list_element[["Y.rbc"]][face_number]/(1080/67) - floor(list_element[["Y.rbc"]][face_number]/(1080/67)))
    }
    else if(data[i,2] == min(data[,2]) & data[i,1] != min(data[,1]) 
            & data[i,1] != max(data[,1]))
    {
      area <- ceiling(list_element[["Y.ltc"]][face_number]/(1080/67)) - list_element[["Y.ltc"]][face_number]/(1080/67)
    }
    else if(data[i,2] == max(data[,2]) & data[i,1] != min(data[,1]) 
       & data[i,1] != max(data[,1]))
    {
      area <- list_element[["Y.rbc"]][face_number]/(1080/67) - floor(list_element[["Y.rbc"]][face_number]/(1080/67))
    }
    else
    {
      area <- 1
    }
#     print(i)
    data[["Face.Area"]][i] <- area
  }
  return(data)
}

# Categorises faces based on size and orders data frame accordingly
categorise_face_size <- function(data)
{
  data_new <- data[with(data, order(-X.length)),]
  data_new[["Index"]] <- c(1:nrow(data))
  data_new <- data_new[, c(3,4,1,2,5,6,7,8,9,10,11)]
  return(data_new)
}

# Indexes the input (solely face as now) based on the size of
# the face across data frame and orders columns
categorise_face_size_global <- function(list_data)
{
  list_data_new <- list_data
  for(i in 1:length(list_data_new))
  {
    if(nrow(list_data_new[[i]]))
    {
      list_data_new[[i]] <- categorise_face_size(list_data_new[[i]]) 
    }
    else
    {
      list_data_new[[i]][["Index"]] <- double()
      names(list_data_new[[i]]) <- names(list_data_new[[1]])
    }
  }
  return(list_data_new)
}

# For each frame carries out grid creation
grid_across_list <- function(list_data)
{
  list_of_grids <- list()
  phony_list_of_data <- list()
  list_data_new <- categorise_face_size_global(list_data)
  for(i in 1:length(list_data))
  {
    list_of_grids[[i]] <- grid_data(list_data_new[[i]])
  }
  return(list_of_grids)
}

# Outputs list of lists of data frames, each data frame
# corresponding to one face in a frame
# The final data frame contains infomration about each grid square
# covered by the face
create_face_grid_frame <- function(list_data)
{
  list_data_grid <- grid_across_list(list_data)
  list_data_output <- list()
  for(i in 1:length(list_data))
  {
    if(nrow(list_data[[i]]))
    {
      list_data_output[[i]] <- list()
      for(j in 1:length(list_data_grid[[i]]))
      {
        list_data_output[[i]][[j]] <- data_frame_ind_face_info(list_data_grid[[i]][[j]],
                                        list_data[[i]], j)
      }
    }
  }
  return(list_data_output)
}


# This function calls the necessary functions to produce 
# a list (level 1) of lists (level 2) of data frames (level 3).
# Level 1 is across the movie.
# The index of the lists at level 2 corresponds to frame number
# The index at level 3 is the face in the frame
face_function <- function(list_data)
{
  output <- prep_face(list_data)
  output <- grid_col(output)
  output <- create_face_grid_frame(output)
  return(output)
}

# Input should be the output of face_function
# e.g. face_list_of_list_of_frames <- face_function(face)
# input of type face_list_of_list_of_frames[[i]]
# COnverts the list of data frames into a single data frame
create_face_frame_local <- function(list_data)
{
  output <- data.frame("X.grid" = double(), "Y.grid" = double(),
                       "Face.Area" = double(), "Face.Order" = double())
  for(i in 1:length(list_data))
  {
    output <- rbind(output, list_data[[i]])
  }
  return(output)
}

# Merges the face info data frame with the more general description of the
# frame
merge_face_info <- function(data_frame, data_face)
{
  output <- merge(data_frame, data_face, by.x = c("X.grid", "Y.grid"), by.y = c("X.grid","Y.grid")
                  , all.x = T)
  output[["Face.Order"]][is.na(output[["Face.Order"]])] <- 0
  output[["Face.Order"]] <- factor(output[["Face.Order"]])
  
  output[["Face.Area"]][is.na(output[["Face.Area"]])] <- 0
  
  return(output)
}

# Creates a data frame describing the video frame's features
prep_info_frame_local <- function(frame_no, list_of_list_of_frames)
{
  face_local_frame <- create_face_frame_local(list_of_list_of_frames[[frame_no]])
  
  #I noticed I had swapped the X and Y coordinates in face_frames
  names(face_local_frame) <- c("Y.grid", "X.grid", 
                               "Face.Area", "Face.Order")
  
  info_frame <- frame_info_frame(frame_no)
  
  # This next step can output a frame of > 8040 entries.
  # This is if two face overlaps we obtain two entries for
  # each grid square they overlap over
  output <- merge_face_info(info_frame, face_local_frame)
  output[["Frame.No"]] <- frame_no
  return(output)
}

# Creates a list of data frames, each data frame describing the 
# characteristics of a frame
prep_info_list <- function()
{
  face_list_of_list_of_frames <- face_function(face)
  fixations <- fix_final(fixations)
  info_list <- list()
  for(i in 1:length(fixations))
  {
    info_list[[i]] <- prep_info_frame_local(i, face_list_of_list_of_frames)
  }
  return(info_list)
}  

# Creates a single dataframe describing all the frames of the movie
prep_info_frame_global <- function(info_list)
{
  output <- data.frame("X.grid" = double(), "Y.grid" = double(),
                       "Saliency" = double(), "Fixation" = double(),
                       "Face.Area" = double(), "Face.Order" = double(),
                       "Frame_no" = double())
  for(i in 1:length(info_list))
  {
    output <- rbind(output, info_list[[i]])
  }
  return(output)
}


#----------------------------------------------------------------------------------
#Input the local features of each frame

list_centroid <- info_list("FaceCentroid") # I don't actually use this
salMap <- info_list("salMap")
fixations <- info_list("fixation")
face_size <- info_list("FaceBoxSize") # This information is contained in FaceBox
face <- info_list("FaceBox")

#----------------------------------------------------------------------------------
# Create the desired output data frame and write to csv file
info_list <- prep_info_list()
BIG_MAN <- prep_info_frame_global(info_list)

BIG_MAN[["Centrality"]] <- sqrt((BIG_MAN[["X.grid"]] - 60)^2 + (BIG_MAN[["Y.grid"]] - 34)^2)


BIG_MAN[["Log.Centrality"]] <- log(BIG_MAN[["Centrality"]])
BIG_MAN[["Sqrt.Centrality"]] <- sqrt(BIG_MAN[["Centrality"]])
BIG_MAN[["Centrality^2"]] <- (BIG_MAN[["Centrality"]])^2
BIG_MAN[["Exp.Centrality"]] <- exp(BIG_MAN[["Centrality"]])


write.csv(BIG_MAN, file = "frame_description_new.csv",row.names=FALSE)