#### Day 5: Quality Checks ####
# https://marce10.github.io/PR_BIR_2024/quality_checks.html


library(warbleR)

#### Convert mp3 to wav ####
warbleR_options(wav.path = "./examples", ovlp = 90)

list.files(path = "./examples/mp3", pattern = "mp3$")

mp32wav(path = "./examples/mp3", dest.path = "./examples/mp3")
# also need sox package
list.files(path = "./examples/mp3", pattern = "mp3$|wav$")


#### Modify sampling rate or dynamic range 
mp32wav(path = "./examples/mp3", samp.rate = 48, bit.depth = 24, overwrite = TRUE,
        dest.path = "./examples/mp3")

list.files(path = "./examples/mp3")


# WarbleR expects all sound files to be in the same directory
# check the properties of the .wav files using this function:
info_sound_files(path = "./examples/mp3")
# using this, we can see that we have two different sampling rates


# You can also use fix_wavs() function to homogenize the sampling rate, dynamic interval, and number of channels
# generally you want all files to have the same recording parameters before any acoustic analysis
# sox not available for my version of R??????????????
mp32wav(path = "./examples/mp3", overwrite = TRUE, dest.path = "./examples/mp3")
info_sound_files(path = "./examples/mp3")

fix_wavs(path = mp3.pth, samp.rate = 44.1, bit.depth = 24)
info_sound_files(path = "./examples/mp3/converted_sound_files")

#### Checking the recordings #####
# check_sound_files() should be the first function used before running warbleR analysis
# checks if sound files in .wav format in working directory can be read in R
check_sound_files()


##### Spectrogram settings #####
# use the same spectrogram parameters to analyze all signals in a project 
# tweak_spectro() simas to simplify the selection of parameterrs through the disply of spectrograms
# function plots a mosaic of spectrograms with different display parameters
tweak_spectro(X = lbh_selec_table, 
              wl = c(100, 1000), # specify which range of iwndow lengths you want to try
              wn = c("hanning"), 
              length.out = 16, 
              flim = c(0,2000),
              nrow = 8, 
              ncol = 6,
              width = 15, # specify the output image parameters
              height = 20, 
              rm.axes = TRUE, 
              cex = 1, 
              box = F)
# why is this not visualizing?????
# looking at this, we can tell when the features of interest start to disappear
# Note that the length.out argument defines the number of values to interpolate within the numerical ranges. wl = 220 seems to produce clearer spectrograms
# hanning or hamming give you a similar answer either way 

# Add a color palette to differentiate the levels of one of the parameters

library(RColorBrewer)

# crear paleta
cmc <- function(n) if (n > 5) rep(adjustcolor(brewer.pal(5, "Spectral"),
                                              alpha.f = 0.6), ceiling(n/4))[1:n] else adjustcolor(brewer.pal(n,
                                                                                                             "Spectral"), alpha.f = 0.6)

tweak_spectro(X = lbh_selec_table, wl = c(100, 1000), wn = c("hanning",
                                                             "hamming", "rectangle"), length.out = 16, nrow = 8, ncol = 6,
              width = 15, height = 20, rm.axes = TRUE, cex = 1, box = F, group.tag = "wn",
              tag.pal = list(cmc))

# Can also use this to choose the color palette and minimum amplitude for plotting collev
tweak_spectro(X = lbh_selec_table, wl = 220, flim = c(0,2), collev.min = c(-20, -100),
              pal = c("reverse.gray.colors.2", "reverse.topo.colors", "reverse.terrain.colors"),
              length.out = 16, nrow = 8, ncol = 6, width = 15, height = 20,
              rm.axes = TRUE, cex = 1, box = F, group.tag = "pal", tag.pal = list(cmc))
# Why am I getting this error????????





#### Double-check selections #####
# check_sels()
# checks a large number of possible errors in the selection information
# used internally when creating selection tables and extended selection tables
cs <- check_sels(lbh_selec_table)

# Let’s modified a selection table to see how the function work
# copiar las primeras 6 filas
st2 <- lbh_selec_table[1:6, ]
# hacer caracter
st2$sound.files <- as.character(st2$sound.files)
# cambiar nombre de archivo de sonido en sel 1
st2$sound.files[1] <- "aaa.wav"
# modificar fin en sel 3
st2$end[3] <- 100
# hacer top.freq igual q bottom freq en sel 3
st2$top.freq[3] <- st2$bottom.freq[3]
# modificar top freq en sel 5
st2$top.freq[5] <- 200
# modificar channes en sel 6
st2$channel[6] <- 3
# revisar
cs <- check_sels(st2)
# when we look at cs, we can see all the issues with these files
cs[, c(1:7, 10)]



# Visual inspection of spectrograms
# use spectrograms() to create a spectrogram of each selection to ensure they contain accurate information about the location of the signals of interest
# using default parameters tweak_spectro()
warbleR_options(wav.path = "./examples", wl = 220, wn = "hanning",
                ovlp = 90, pal = reverse.topo.colors)
# this code will create a jpg for each of the selections
# how do you combine this into a gif like marcelo did?
spectrograms(lbh_selec_table, collevels = seq(-100, 0, 5))
# Add a heading to each of these plots
spectrograms(lbh_selec_table, collevels = seq(-100, 0, 5), sel.labels = "sel.comment")



# Full spectrograms
#If the X argument is not given, the function will create the spectrograms for all the files in the working directory. Otherwise, the function generates spectrograms for sound files in X and highlights selections with transparent rectangles similar to those ofspectrograms()
# load package with color palettes
library(viridis)
# create directory
dir.create("./examples/hermit")
# download sound file
phae.stri <- query_xc(qword = "nr:154074", download = TRUE, path = "./examples/hermit")
# Convert mp3 to wav format
mp32wav(path = "./examples/hermit/", pb = FALSE)
# plot full spec
full_spectrograms(sxrow = 1, rows = 10, pal = magma, wl = 200, flim = c(3,
                                                                        10), collevels = seq(-140, 0, 5), path = "./examples/hermit/")
# You can add an argument X = to specify a selection taable to show on the spectrograms
# Sox again??????? Idk what's going on 
# Gives you a page with ten rows, one second per row
# Generally is better to do this in Raven




# Catalogs 
# read bat inquiry data
inq <- readRDS(file = "ext_sel_tab_inquiry.RDS")

catalog(X = inq[1:100, ], flim = c(10, 50), nrow = 10, ncol = 10,
        same.time.scale = T, mar = 0.01, # margin in seconds - how long before and after the spectrogra
        gr = FALSE, img.suffix = "inquiry",
        labels = c("sound.files", "selec"), legend = 0, rm.axes = TRUE,
        box = F, group.tag = "sound.files", tag.pal = list(magma), width = 20,
        height = 20, pal = viridis, collevels = seq(-100, 0, 5))
# allows you to see if anything is weirdly annotated or if the boundaries or off at a glance


# Tailoring spectrograms
# can modify selections and fix annotations in specgrograms in R (can be faster than raven)
tailor_sels(X = lbh_selec_table[1:4, ], auto.next = TRUE)
# Error in z[(fl1:fl2) + 1, , drop = FALSE] : subscript out of bounds???????
# returns the corrected data as a data frame in R and saves a .csv file in the directory where the sound files are located
## Can also be used to modify the frequency contours such as those produced by dfDTW() or ffDTW() function
cntours <- freq_ts(X = lbh_selec_table[1:5, ])
tail.cntours <- tailor_sels(X = lbh_selec_table[1:5, ], ts.df = cntours,
                            auto.contour = TRUE)
### This is a way you can measure it automatically and then fix the ones that look weird

