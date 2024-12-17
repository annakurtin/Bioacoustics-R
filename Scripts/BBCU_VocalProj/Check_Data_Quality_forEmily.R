#### Checking Data Quality - for Emily #####
# Source and info: https://marce10.github.io/PR_BIR_2024/quality_checks.html

# Setup 
library(warbleR)
library(seewave)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: make sure they're in the correct format
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Convert any mp3 files to wav
#list.files("./examples/bbcu_xc_mp3")
#sox "C:\Users\ak201255\Documents\Bioacoustics-R\examples\bbcu_xc_mp3\16525.mp3" "C:\Users\ak201255\Documents\Bioacoustics-R\examples\16525.wav"
#list.files(path = "./examples/bbcu_xc_mp3", pattern = "\\.mp3$", full.names = TRUE)
# Issues wtih running sox on this bc the version I downloaded doesn't have mp3 support?????
mp32wav(path = "./examples/bbcu_xc_mp3", dest.path = "./examples/test_wav")
# Error is saying all .mp3 files have been converted?
# 
# # Can get around this with this code if needed:
# library(tuneR)
# r <- readMP3("./examples/bbcu_xc_mp3/16525.mp3")  ## MP3 file in working directory
# writeWave(r,"./examples/bbcu_xc_wav/16525.wav",extensible=FALSE)

# Use this loop
folder <- "./examples/bbcu_xc_mp3"
files <- list.files(folder)
folder_wav <- "./examples/bbcu_xc_wav"

for(i in files){
  # Read mp3 
  r <- readMP3(paste0(folder,"/",i))
  name <- sub("\\.mp3$","",i)
  # write wav
  writeWave(r,paste0(folder_wav,"/",name,".wav"),extensible=FALSE)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Standardize parameters of files
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Let's check out the properties of the .wav sound files
info_sound_files(path = "./examples/bbcu_xc_wav")
# You want the same sampling rate, dynamic interval and number of channels

# Let's make them all the same parameters so that we can compare them 
fix_wavs(path = "./examples/bbcu_xc_wav", samp.rate = 44.1, bit.depth = 24)
# Check that this worked
info_sound_files(path = "./examples/bbcu_xc_wav/converted_sound_files")
# Yay this works!!!!
check_sound_files()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Annotate in Raven
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Once you've created the selection tables, combine them into one 