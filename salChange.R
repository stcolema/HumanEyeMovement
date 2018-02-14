setwd("C:/Users/Captain Bluebear/Desktop/TP/Year 4/Semester 2 - Hillary/MA4492 - Project/R")


require(dplyr)
require(plyr)
require(data.table)

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

# Removes undesired columns from saliency frame
remove_undesired_columns <- function(data)
{
  len <- length(data)
  for(i in 1:length(data))
  {
    if(!(names(data)[len + 1 - i] %in% c("X.grid", "Y.grid", "Frame.No", "Saliency")))
    {
      data <- data[, -(len + 1 - i)]
    }
  }
  return(data)
}

# Outputs data frame of saliency from <frame_diff> number
# of frame ago
sal_change <- function(frame_data, frame_diff)
{
  data <- frame_data
  data[["Frame.No"]] <- data[["Frame.No"]] + frame_diff
  saliency_median <- median(data[["Saliency"]])

  local_data <- create_frame_of_covariates()
  local_data[["Saliency"]] <- saliency_median
  top_rows <- list()
  output <- data.frame("X.grid" = double(), "Y.grid" = double(),
                       "Saliency" = double(), "Frame.No" = double())
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
  names(output) <- c("X.grid", "Y.grid", "Saliency", "Frame.No")
  final_output <- rbind(output, data_of_interest)
  return(final_output)
}

# Merges frame description with saliency frames
sal_marge <- function(sal_data, frame_data)
{
  output <- merge(frame_data, sal_data, by.x = c("X.grid", "Y.grid", "Frame.No"),
                  by.y = c("X.grid", "Y.grid", "Frame.No"))
  return(output)
}

# 
rename_sal <- function(sal_delay, data)
{
  sal.name <- paste0("Saliency", sal_delay)
  names <- c(names(data)[c(1:(length(data)-1))], sal.name)
  return(names)
}

BIG_MAN <- read.csv("frame_description2.csv")

sal_10 <- sal_change(BIG_MAN, 10)
sal_25 <- sal_change(BIG_MAN, 25)
sal_50 <- sal_change(BIG_MAN, 50)
sal_100 <- sal_change(BIG_MAN, 100)

outcat <- sal_marge(sal_10, BIG_MAN)
# names(outcat) <- c(names(outcat)[c(1:12)], "Saliency.10")
names(outcat) <- rename_sal(10, outcat)

outcat <- sal_marge(sal_25, outcat)
names(outcat) <- rename_sal(25, outcat)
outcat <- sal_marge(sal_50, outcat)
names(outcat) <- rename_sal(50, outcat)
outcat <- sal_marge(sal_100, outcat)
names(outcat) <- rename_sal(100, outcat)

names_new <- c(names(outcat)[c(1:3)], "Saliency", names(outcat)[c(5:16)])
names(outcat) <- names_new


outcat <- mutate(outcat,
                 Sal.Change.10 = Saliency - Saliency10,
                 Sal.Change.25 = Saliency - Saliency25,
                 Sal.Change.50 = Saliency - Saliency50,
                 Sal.Change.100 = Saliency - Saliency100)


write.csv(outcat,  file = "frame_description_2D.csv",row.names=FALSE)
