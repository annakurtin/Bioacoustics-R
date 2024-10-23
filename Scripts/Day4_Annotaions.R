##### Day 4: Annotationsin R ####

## When creating annotations in raven
## Begin file, begin path, and file offset are good ones to include so that you know which file you had and where it was located
# https://marce10.github.io/PR_BIR_2024/annotations.html

# Package for interfacing with raven from R
library(Rraven)
library(warbleR)
# Designed to facilitate data exchange between R andraven
## R can simplify complex steps in analysis, and packages in R can expand the analysis capability

# Look at the text files
list.files(path = "./examples", pattern = "\\.txt$")
# Let's import annotations
rvn.dat <- imp_raven(all.data = TRUE, path = "./examples")
# you can change it to all.data = FALSE and it will read in less columns of the data??
# note that the default behavior is to import ALL text files in directory
head(rvn.dat)
# if there are any text files that aren't compatable with Raven, they won't be read in

# you can also save the rows for the annotations in the waveform
rvn.dat <- imp_raven(all.data = TRUE, waveform = TRUE, path = "./examples")
head(rvn.dat)

# Another useful argument: specify to load it in warblR fromat
rvn.dat <- imp_raven(all.data = FALSE, freq.cols = TRUE, path = "./examples", warbler.format = TRUE, all.data = FALSE)

head(rvn.dat)
# This will change column headings to those expected by warbleR and change frquency measurements to kHz (instead of default Hz)





# Can also relabel the columns back and forth from Raven fomat to warbleR format
# para simplificar solo las primeras 7 columnas
st1 <- rvn.dat[ ,1:7]
st1
relabel_colms(st1)
relabel_colms(st1, extra.cols.name = "View",
              extra.cols.new.name = "Raven view")

# Can also export data to raven
data(lbh_selec_table)
st1 <- lbh_selec_table[lbh_selec_table$sound.files == "Phae.long1.wav",]
# has to be in the warbleR format
exp_raven(st1, file.name = "Phaethornis 1", khz.to.hz = TRUE)
# exports it to the working directory
# Can also specify the location to export to
st1 <- lbh_selec_table[lbh_selec_table$sound.files == "Phae.long1.wav",]

exp_raven(st1, file.name = "Phaethornis 1", khz.to.hz = TRUE, path = "./examples")



# Multiple selection tables in one
# Use the single.file = TRUE argument
exp_raven(X = lbh_selec_table, file.name = "Phaethornis multiple sound files", 
          sound.file.path = "./examples", single.file = TRUE)
# when opening one selection tble with multiple files, make sure the name of the file is in the selection table



#### Exercise ####
# Try importing my own data
gcwa_tab <- imp_raven(all.data = TRUE, path = "./examples/ak_dat/GCWA_proj/selection_tables")
# mp3 isn't supported with this when you add name.from.file
# someone had problems reading their data in, they used name.from.file = TRUE and it solved it
# how do I import separate tables?
gcwa_tab1 <- imp_raven(all.data = TRUE, path = "./examples/ak_dat/GCWA_proj/selection_tables", files = "Setophaga_chrysoparia-XC13294-selections.txt")



#### warbleR formats ####
# most tools with warbleR are taken from seewave
# efficient bc it works on selection tables

# selection_table()
# takes dataframes containing selection data
# selection tables must containe
## sound files
## selection
## start
## end
library(warbleR)
data("lbh_selec_table")
lbh_selec_table
# You can convert it to a selection table format like this 
# This is a data frame
# global parameters
warbleR_options(wav.path = "./examples")
# Note that the path to the sound files has been provided. This is necessary in order to verify that the data provided conforms to the characteristics of the audio files.
# x is provided as a dataframe
st <- selection_table(X = lbh_selec_table, pb = FALSE)

# You can also check if the selection tables are formatted properly (end higher than start, no negative numbers in frequency, etc)
st1 <- lbh_selec_table
st1$start[1] <- 100
st1$top.freq[2] <- 1000000
cs <- check_sels(st1)


## Extended selection tables
## will transform the selection table into a self contained object of acoustic data - will contain the clips of those sounds as well 
## Will check the annotations, make sure they're fine, then go through the sound files and clip out the sound so that everything is in a single object
#  global parameters
warbleR_options(wav.path = "./examples")

ext_st <- selection_table(X = lbh_selec_table, pb = FALSE, 
                          extended = TRUE)
ext_st
# Notice when you print this it says it has 11 wave objects as attributes
## let's take a look at these
attr(ext_st, "wave.objects")
wo <- attr(ext_st, "wave.objects")
wo
wo$Phae.long1.wav_1

# extract the first wav file
## Index indicates the row of the selection that will be read
w1 <- read_sound_file(ext_st, index = 1)

w1

# This way, the annotations and the sound files are in the same place, making things more reproducable

# You can save extended selection tables as an RDS object
saveRDS(ext_st, "./examples/Export_ExtSelectionTable.RDS")
# And you can read these back in 
y <- readRDS("./examples/Export_ExtSelectionTable.RDS")
## Can only open these within R
# They're working on expanding these capabilities within R


# You can even pull in data from github
URL <- "https://github.com/maRce10/OTS_BIR_2024/raw/master/data/extended.selection.table.araya-salas.et.al.2017.bioacoustics.100.sels.rds"

dat <- readRDS(gzcon(url(URL)))

nrow(dat)
str(dat)
# now let's look at one spectrogram
w1 <- read_sound_file(dat, index = 1)
spectro(w1, flim = c(0,10))

# Now that we have only the data that we want to work with, we can reduce the size of the data by quite a bit
format(object.size(dat), units = "auto")
# THe original data was 1.1 GB
# Note however that this clips out quite a bit of data

# PUt this into a loop - read each one in and create a spectrogram in a multipanel figure
par(mfrow = c(3, 2), mar = rep(0, 4))

for(i in 1:6){
  
  wv <- read_sound_file(X = dat, index = i, from = 0.17, to = 0.4)
  
  spectro(
    wv,
    wl = 250,
    grid = FALSE,
    scale = FALSE,
    axisX = FALSE,
    axisY = FALSE,
    ovlp = 90,
    flim = c(0, 12),
    palette = viridis::viridis,
    collevels = seq(-120, 0, 5)
    
  )
}

##### Exercise #####
# Run the example code in the selection_table() function documentation
{
  data(list = c(
    "Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4",
    "lbh_selec_table"
  ))
  # save the sound files as temporary directory 
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
  
  # make selection table
  st <- selection_table(X = lbh_selec_table, path = tempdir())
  
  is_selection_table(st)
  
  #' # make extended selection table
  st <- selection_table(
    X = lbh_selec_table, extended = TRUE,
    path = tempdir()
  )
  
  is_extended_selection_table(st)
  
  ### make extended selection by song
  # create a song variable - quick and dirty way to just say each annotation in the same sound file is in the same song
  lbh_selec_table$song <- as.numeric(as.factor(lbh_selec_table$sound.files))
  
  st <- selection_table(
    X = lbh_selec_table, extended = TRUE,
    by.song = "song", path = tempdir()
  )
}

# What do the arguments “mar” and “by.song” from selection_table() do?
## mar specifies the margins in seconds adjacent to the start and end points of the selections when creating extended selection tables, with a default of 0.1
ext_st <- selection_table(X = lbh_selec_table, pb = FALSE, 
                          extended = TRUE, mar = .2)
t1 <- attr(ext_st, "wave.object")$Phae.long1.wav_1
ext_st <- selection_table(X = lbh_selec_table, pb = FALSE, 
                          extended = TRUE, mar = .7)
t2 <- attr(ext_st, "wave.object")$Phae.long1.wav_1
# notice the difference in duration
library(viridis)
# Some analyses need extra space around the signal to calculate SNR or cross correlation or stuff like that
spectro(t2, flim = c(0,10), wl = 200, collevels = seq(-120,0,1), palette = viridis)
# doesn't affect the precision of the annotation, just adds room to either side
# annotation will relate to the start and end in the original file 

## by.song an a wav object containing all selections belonging to a single song is saved in the extended selection table
## sometimes you have notes into syllables, syllables into phrases, phrases into songs, and songs into singing bouts
# songs are these higher levels of organization
# if you want to keep these together in a single clip, that's when you use by.song so that the extended selection table won't break them up and will keep them in a single .wav file
# "songs" are a categorical variable that labels which of the subunits belong to the higher level of organization
# when we create it by.song, we have less wave objects (just four here)
st
w2 <- attr(st, "wave.object")$"Phae.long1.wav-song_1"
# also
w2 <- read_sound_file(st, 1) # but this will still read the whole sound file???
spectro(w2, flim = c(0,10), wl = 200, collevels = seq(-120, 0,1), palette = viridis)

# Measure the peak frequency of the 8th selection in dat() (hint: use seewave’s fpeaks())
sel8 <- read_sound_file(st, index = 8)
# create a spectral analysis of the file using meanspec
sel8_m <- meanspec(sel8, f = sel8@samp.rate, wl = 512)
# can also use spec
sel8_m <- spec(sel8, f = sel8@samp.rate, wl = 512)
# dmeasure the peak of the frequency spectrum
sel8_peak <- fpeaks(sel8_m, nmax = 1)


### Remove a selection that's not ok
# rvn.dat[-4,1]
