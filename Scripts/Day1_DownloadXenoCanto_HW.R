#### Homework Day 1 #####

# 1. Use the function query_xenocanto() from the suwo package to check the availability of recordings for any bird species (do not download at this step)
### Do this based on what the instructor sends us
# Tutorial https://marce10.github.io/PR_BIR_2024/get_online_recordings.html
library(suwo)
library(tuneR)
library(seewave)
#install.packages("suwo")
# 2. Subset the data frame returned by the function to get a subset of subspecies/populations or recordings from a specific country and for certain vocalization type (using base R subsetting tools)
phae_his <- query_xenocanto(term = 'Phaethornis hispidus')
# ex. pull cuckoo data
coc_ery <- query_xenocanto(term = 'Coccyzus erythropthalmus')
# Painted bunting
pas_cir <- query_xenocanto(term = 'Passerina ciris')
# Golden-cheeked warbler
gcwa <- query_xenocanto(term = 'Setophaga chrysoparia')
# Pull out just the song
song_gcwa <- gcwa[grep("song", ignore.case = TRUE, gcwa$vocalization.type), ]
# Map the locations
map_locations(song_gcwa)

# 3. Download the associated recordings using query_xenocanto() again
download_media(metadata = gcwa, 
               path = "./examples/ak_dat/GCWA_proj")

# 4. Explore the recordings with any spectrogram creating GUI program

# Try making a spectrogram here
# Someone mentioned this doesn't work for them in R? Mine is working fine tho
xc1 <- readMP3("./examples/ak_dat/GCWA_proj/audio/Setophaga_chrysoparia-XC127127.mp3")
spectro(xc1, 
        f = 32000, 
        wl = 512, 
        tlim = c(0.5,2.5),
        ovlp = 80,
        collevels = seq(-80, 0, 0.5),
        flim = c(2,6), 
        scale = FALSE,
        listen = TRUE,
        main = "GCWA XC127127")
