#### R Group: For Loops ####

# Anna Kurtin 9/8/2024
# https://gge-ucd.github.io/R-DAVIS/lesson_14_iteration.html 

#### Basics of Loops #####
# Loops are helpful when you want to do the same process to multiple objects, observations in a dataframe, etc. 

# Generally it's not best practice to copy and paste your code then change the inputs

# R makes doing the same thing many times easy for us because it is a vectorized language, meaning that if you have a vector and apply some function to the vector the function will automatically be applied to every element

# Review:
x <- 1:10
log(x)

# But sometimes you want to do more complicated processes, for these you might need a for loop

#### Examples #####
# In for loops, i is commonly used as the "iteration value", meaning an object that will change in each iteration of the loop

# Even if you assign it outside the loop, it will get changed within the loop

non_iter <- 1
i <- 2
for(i in 1:10){
  print(i)
  print(non_iter)
}
# Notice how i changes but not a 

# You can also use the iteration value more than once in a loop but you generally don't want to reassign it

# Good
for (i in 1:10){
  print(i+1)
  print(i^2)
}
# you want to make sure its easy to follow what i is

# You can also use i to iterate through dataframes as we've learned
for (i in 1:10) {
  print(letters[i])
  print(mtcars$wt[i])
}
# R is faster with lists rather than dataframes but unless you're working with really large data you won't notice a difference

# Nested for loops
mat <- matrix(NA, ncol = 5, nrow = 8)

# Want to simulate data where each column is drawn from a normal distribution with a different standard deviation
# First iterate through each column and choose a random value for the standard deviation
for (i in 1:ncol(mat)){
  # Put in a print statement to follow where we are
  print(paste0("Column is: ", i))
  # Create a value for the standard deviation
  stdev <- sample(c(1:5), 1, replace = FALSE)
  
  for (j in 1:nrow(mat)){
    # Print statement to track where we are
    print(paste0("Row is: ", j))
    # Assign a value to the matrix
    mat[j, i] <- rnorm(1, mean = 0, sd = stdev)
  }
}


#### In Practice: Renaming Files #####

# you only need to change this, then run the rest of the code
input_path <- "F:/Cuckoo_Acoustic_Data/2023/2023_UMBEL_Data/2023_UMBEL_Audio/"


# Functions
# Create a function to add a prefix to the name
add_prefix <- function(file_name, prefix){
  new_name <- paste(prefix,file_name,sep="_")
  return(new_name)
}
# Create a function to remove any existing prefixes from the name
trim_name <- function(file_name){
  new_name <- str_extract(file_name,"([[:digit:]]{8})_([[:digit:]]{6}).WAV|([[:digit:]]{8})_([[:digit:]]{6}).wav")
  return(new_name)
}


# Establish the base directory
base_dir <- input_path

# Pull out the directories within this one
all_dirs <- list.dirs(base_dir, full.names = TRUE)

# Filter out the base directory itself
sub_dirs <- all_dirs[all_dirs != base_dir]


for (folder in sub_dirs) {
  # Get the last folder name from the path
  last_folder <- basename(folder)
  print(last_folder)
  # Get a list of file names in the current subdirectory
  files_in_folder <- list.files(folder, full.names = TRUE)
  #print(files_in_folder)
  # Create the prefix using the last folder name
  prefix <- last_folder
  print(paste("prefix is",prefix))
  
  
  # Iterate through the list of file names in the subdirectory
  for (file_name in files_in_folder) {
    # Call the trim_names function 
    trimmed_name <- trim_name(file_name)
    #print(trimmed_name)
    
    # Call the add_prefix function to rename the file
    new_name <- add_prefix(basename(trimmed_name), prefix)
    
    print(new_name)
    # Rename the file using file.rename
    file.rename(file_name, file.path(dirname(file_name), new_name))
  }
}