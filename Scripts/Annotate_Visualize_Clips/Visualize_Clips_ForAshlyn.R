##### Reading Audio Clips into R #####


#### Setup ####
# For reading in tab delimited files
library(readr)
# For creating spectrograms
library(seewave)
# For reading in AudioFiles
library(tuneR)
# To make column names easy to work with
library(janitor)
library(tidyverse)


# Establish which audio file you're working with (reduce human error)
site <- "MI_GO_CC3"
file <- "20230607_075000"
# Establish how many top clips to look at
num_top <- 3


# Read in your selection output from BirdNet
## using dynamic coding, where rather than changing the file path itself here I just have to specify which site and audio file I want to work with above and then can run the rest of the script without changing anything
clips_tab <- read_tsv(paste0("./examples/ah_dat/tables/",site,"_",file,".BirdNET.selection.table.txt")) %>% clean_names()
# Sort by species and take the top three 
clips_top <- clips_tab %>% group_by(species_code) %>% slice_max(order_by = confidence, n = num_top, with_ties = FALSE)

# Initialize a dataframe
df <- data.frame(
  site = character(),
  audio_file = character(),
  begin_clip = numeric(),
  end_clip = numeric(),
  confidence = numeric(),
  annotation = character(),
  comments = character(),
  stringsAsFactors = FALSE  # Keeps columns as characters, not factors
)

# Loop through your clips and annotate a new dataframe from them 
for(i in 1:nrow(clips_top)){
  current_clip <- clips_top[i,]
  # write in metadata to dataframe 
  df[i,"site"] <- site
  df[i,"audio_file"] <- file
  df[i, "begin_clip"] <- current_clip$begin_time_s
  df[i, "end_clip"] <- current_clip$end_time_s
  df[i, "confidence"] <- current_clip$confidence
  
  # Read in the audio file
  audio_file <- readWave(filename = paste0("./examples/ah_dat/audio/",file,".WAV"), 
                         from = current_clip$begin_time_s, 
                         to = current_clip$end_time_s, 
                         units = "seconds")
  
  # Create a spectrogram
  spectro(audio_file,
          wl = 512, 
          ovlp = 40,
          collevels = seq(-80, 0, 2), 
          flim = c(.5,8),  
          scale = FALSE,
          fastdisp = TRUE,
          tlab = "Time (s)",
          flab = "Frequency (kHz)",
          main = paste0(current_clip$species_code,", clip start: ",current_clip$begin_time_s),
          listen = TRUE) 
  
  df[i, "annotation"] <- readline("Annotation:")
  df[i, "comments"] <- readline("Comments:")
  
  # Add something that saves df here or writes it as a csv?  orjust only run this on small files
}

# Write the csv you made where you want it to go
#write.csv(df, paste0("./examples/ah_dat/",site,file,".csv"), row.names = FALSE)








# Old code that may or may not be helpful ####

# When sorting the dataframe, could also just take one species and sort by confidence score and take the top three
clips_sorted <- clips_sorted %>% filter(species_code == "vesspa")
clips_sorted <- clips_tab[order(-clips_tab$confidence),]
clips_top <- clips_tab[1:num_top,]



# Play around with spectrogram parameters
# Read in the spectrogram based on the first column in the clip
current_clip <- clips_top[1,]
# Read in your audio file, specifying where to start and where to end 
audio_file <- readWave(filename = paste0("./examples/ah_dat/audio/",audio_file,".WAV"), # again flexible coding for file path
                       from = current_clip$begin_time_s, # can subtract .25 seconds to picture the start better
                       to = current_clip$end_time_s, # end of clip
                       units = "seconds")
# Create a spectrogram
spectro(audio_file,
        wl = 512, 
        ovlp = 80,
        collevels = seq(-80, 0, 0.5), 
        flim = c(.5,8), # Frequency limits of the sound you want ot display 
        scale = FALSE,
        fastdisp = TRUE,
        tlab = "Time (s)",
        flab = "Frequency (kHz)")
#listen = TRUE)


# take input from the user
annotation <- readline("Annotation: ")
# Trying to get it to only accept valid inputs, but can't get this to work
# Try 1
# while(!(annotation %in% c("1", "0", "u"))){
#   cat("Invalid entry! Please enter either 1, 0, or u.\n")
#   annotation <- readline("Annotation: ")
# }
# Try 2
# valid <- annotation %in% c("1","0","u")
# while(valid == FALSE){
#   print("Invalid entry! Please enter either 1, 0, or u.")
#   annotation <- readline("Annotation: ")
#   valid <- annotation %in% c("1","0","u")
# }
# Try 3
# if(!annotation %in% c("1","0","u")){
#   print("Invalid entry! Please enter either 1, 0, or u.")
#   annotation <- readline("Annotation: ")
#   } else {
#     print("Annotation completed.")
#   }
# comment <- readline("Comment: ")