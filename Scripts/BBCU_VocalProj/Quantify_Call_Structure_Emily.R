#### Quantifying Acoustic Structure #####
# Info: https://marce10.github.io/PR_BIR_2024/measure_acoustic_structure.html 
# if you don't have the packages installed, run install.pacakges("package") ex. install.pacakges("warbleR")
library(warbleR)
library(Rraven)
library(tidyverse)

## YOU JUST NEED TO CHANGE THESE NEXT FEW LINES
# You will be able to run the rest of the code without changing anything
# Change this to be the directory where the selection tables are stored
st_directory <- "D:/TestFiles_Emily/selection_tables/"
# Change this to be the directory where the audio is stored
audio_directory <- "D:/TestFiles_Emily/audio"



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Run Spectrographic Analysis
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
selection_files <- list.files(path = st_directory, pattern = "\\.txt$")
st <- data.frame()
for (file in selection_files){
  # Read in the file of interest using read.delim
  df <- read.delim(paste0(file_directory,file), header = TRUE, stringsAsFactors = FALSE)
  # Create a file name string  - pull out from the file any digits following an underscore then append .wav
  file_string <- sub(".*_(\\d+)\\.txt$", "\\1.wav", file) 
  # Create a new column in the dataframe and populate each row with the file name string
  df$sound.files <- file_string
  # Rename and reorder the columns
  df <- df %>% rename("channel" = "Channel", 
                      "selec" = "Selection", 
                      "start" = "Begin.Time..s.", 
                      "end" = "End.Time..s.",
                      "bottom.freq" = "Low.Freq..Hz.",
                      "top.freq" = "High.Freq..Hz.")
  # Convert top and bottom freq to kHz
  df$bottom.freq <- df$bottom.freq/1000
  df$top.freq <- df$top.freq/1000
  df$channel = as.numeric(df$channel)
  # Check if end is less than or equal to start and if so remove this row from the dataset
  df <- df %>% filter(end > start)
  df <- df %>% select(sound.files, channel, selec, start, end, bottom.freq, top.freq)
  # Add on to dataset to combine selection tables
  st <- rbind(st, df)
}
# Run spectro_analysis
st_spectro <- spectro_analysis(st, path = audio_directory, bp = "frange")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Run Principal Components Analysis 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run principal components analysis on the spectrographic analysis output
#Info on prcomp vs princomp: https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
pca <- prcomp(st_spectro[, -c(1, 2)], scale = TRUE)

# extract first 2 PCs
sp_pcs <- data.frame(st_spectro[, 1:2], pca$x[, 1:2])

# Now we'll be evaluating these results
#### How much of the variation is explained by each PC axis?
# Compute variance
pca.var <- pca$sdev ^ 2
# Proportion of variance for a scree plot
propve <- pca.var / sum(pca.var)
# Plot variance explained for each principal component
plot(propve, xlab = "principal component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b",
     main = "Scree Plot")
#### Where does the amount of variance explained by each PC axis taper off?
#### Ideally you want to have 90% accounted for by first two or three PCs

# Now we'll look at the loadings, or which spectral properties influence each PC axis
pca_loads <- as.data.frame(pca$rotation[,1:2])
# First we'll just look at the spectral properties that are important to the first PC axis
pca_loads %>% arrange(desc(PC1))
# Next we'll just look at the spectral properties that are important to the second PC axis
pca_loads %>% arrange(desc(PC2))
# Second blue box at this site describes what these spectral properties mean: https://marce10.github.io/PR_BIR_2024/measure_acoustic_structure.html


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Visualize Clusters
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(sp_pcs,
       aes(
         x = PC1,
         y = PC2,
         color = sound.files,
         shape =  sound.files
       )) +
  geom_point(size = 5) +
  scale_color_viridis_d(option = "G",
                        end = 0.9,
                        direction = -1) +
  theme_classic() +
  labs(x = "PC1", y = "PC2") +
  theme(legend.position = "right")
# How many groups can you pick out visually?
# To further explore this data, you can add a geom_ellipse function



#### Graveyard - Ignore######
# # Troubleshooting
# warbleR_options(wav.path = "D:/TestFiles_Emily/audio")
# st_formatted <- selection_table(X = st, pb = FALSE)
# st_checked <- check_sels(X = st, pb = FALSE, fix.selec = TRUE)

# WarbleR options removd: , flim = c(1, 10), wl = 200, ovlp = 90, pb = FALSE

# Read in one selection table as a dataframe
#t1 <- read.delim("D:/TestFiles_Emily/selection_tables/for_analysis/selection_table_16525.txt")
# the selection tables as they are don't have a file column 
#rvn.dat <- imp_raven(path = "D:/TestFiles_Emily/selection_tables/for_analysis", sound.file.col = 'file_name')
#Error: No column containing sound file names was found in any selection table file

# Check if there are any missing files in this location
#file_paths <- file.path("D:/TestFiles_Emily/audio", selection_tables_all$sound.files)
#missing_files <- selection_tables_all$sound.files[!file.exists(file_paths)]
# Nothing looks like it's missing - ??????
#list.files(path = "D:/TestFiles_Emily/audio")
#info_sound_files("D:/TestFiles_Emily/audio")

# Error: Error in m[(fl[1]:fl[2]) + 1, ] : subscript out of bounds. Try googling this
# st %>% group_by(sound.files) %>% summarize(max(end), max(top.freq))
# file_info <- info_sound_files("D:/TestFiles_Emily/audio")
# # Merge duration info with selection table
# st_info <- st %>% 
#   left_join(file_info %>% select(sound.files, duration), by = "sound.files")
# list.files("D:/TestFiles_Emily/audio")
# # Identify where end time exceeds duration
# problem_rows <- st_info %>% filter(end > duration)
# print(problem_rows)
# # Filter out selections with end time exceeding the file duration
# st <- st_info %>% filter(end <= duration)
# test <- st %>% filter(bottom.freq < top.freq)
# st %>% mutate(duration = end - start) %>% filter(duration < 0.01)
# 
# test <- spectro_analysis(st[1:10,], path = "D:/TestFiles_Emily/audio")
# test <- spectro_analysis(st[11:49,], path = "D:/TestFiles_Emily/audio")
# test <- spectro_analysis(st[11,], path = "D:/TestFiles_Emily/audio")
# #Nothing working

# Example
# Load in your selection table
# # what format do these need to be in?
# data("lbh_selec_table")
# # run spectro_analysis on this selection table and associated audio files - I think this takes a selection table where it's combined with sound files (refresh on this)
# sp <- spectro_analysis(lbh_selec_table)