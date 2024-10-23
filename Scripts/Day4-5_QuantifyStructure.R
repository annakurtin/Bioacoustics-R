#### Day 4: Quantifying Structure ####

# warbleR has four methods for measuring structure of acoustic signals
## these are widely used
# Two are absolute measures of the structure
## spectrographic parameters
## statistical descriptors of cepstral coefficients (uses Mel frequency scale)

# The other two provide a relative similarity of value between the signals
## Spectrographic cross-correlation
## Dynamic time warping

##### Load example data ######

library(warbleR)
library(viridis)
library(ggplot2)
library(ggalign)
library(PhenotypeSpace) # can't install this?

# Set global options
warbleR_options(wav.path = "./examples/", flim = c(1, 10), wl = 200, ovlp = 90, pb = FALSE)
# load in data
data(lbh_selec_table)
# take a look
lbh_selec_table

# make a color pallete for tagging spectrograms
tag_pal <- function(x)
  mako(x,
       alpha = 0.6,
       begin = 0.1,
       end = 0.9)

# plot all annotation spectrograms in a catalog
catalog(
  lbh_selec_table,
  flim = c(1.5, 10),
  wl = 200,
  ovlp = 90,
  nrow = 4,
  ncol = 3,
  width = 12,
  height = 8,
  pb = FALSE,
  same.time.scale = TRUE,
  mar = 0.02,
  pal = mako,
  collevels = seq(-120, 0, 1),
  group.tag = "sound.files",
  tag.pal = list(tag_pal),
  spec.mar = 0.6,
  box = FALSE,
  res = 200
)



#### Spectrographic parameters #####
# function: spectro_analysis()
# measures parameters related to amplitude distributions in time and frequency, descriptions of the fundamental and dominatn frequency contours and descriptors of harmonic content
### Reference the link above for a description of what these are
# these are analogous to some measurements that you can generate in raven
# you can pull these together into a descriptor of the acoustic space of the signals

# We can easily generate these measurements in R
library(warbleR)
library(ggplot2)
# recommended global parameters
warbleR_options(wav.path = "./examples", flim = c(1,10), w1 = 200, ovlp = 90, pb = FALSE)

# Load the data 
data("lbh_selec_table")
# Conduct the spectral analysis
sp <- spectro_analysis(lbh_selec_table)
# take a look at the output
sp

# let's reduce the dimensionality using a principal component analysis
# run principal components
pca <- prcomp(sp[, -c(1, 2)], scale = TRUE)
# extract first 2 PCs
sp_pcs <- data.frame(sp[, 1:2], pca$x[, 1:2])
sp_pcs

# Acoustic space described by this data can be easily visualized with a scatterplot
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
# the way to read this: things that are closer together in acoustic space are more similar

#### Exercise ####
# 1. The parameters related to harmonic content were not calculated. How can we do that?
## Add in harmonics
sp_h <- spectro_analysis(lbh_selec_table, harmonicity = TRUE)

# 2. How does measuring harmonic content affect performance?
## slows it down a lot

# Let's see how it performs
pca_h <- prcomp(sp_h[, -c(1, 2)], scale = TRUE)
## Error in svd(x, nu = 0, nv = k) : infinite or missing values in 'x' ????????????????
# Notice that some signals don't have enough information to calculate harmonics, which is why we're getting this error
# sp_pcsh <- data.frame(sp_h[, 1:2], pca_h$x[, 1:2])
# ggplot(sp_pcsh,
#        aes(
#          x = PC1,
#          y = PC2,
#          color = sound.files,
#          shape =  sound.files
#        )) +
#   geom_point(size = 5) +
#   scale_color_viridis_d(option = "G",
#                         end = 0.9,
#                         direction = -1) +
#   theme_classic() +
#   labs(x = "PC1", y = "PC2") +
#   theme(legend.position = "right")

# How many harmonics do you keep?
## marcelo said this is the hardest part - to define the top boundary 
## The way to approach this is to define a top frequency - basically use a band pass filter so you're capturing the same frequency for each signal
## in R, you can add a certain number to the bottom frequency to define the top frequency 
## The time this takes to run depends on nharmonics, wl (window length) - finer resolution in frequency domain will take longer to run

# 3. What does the argument ‘threshold’ do?
# defines the amplitude threshold for fundamental frequency and dominant frequency detection 
# default is 15
# you can optimize that value using the visualizations we talked about in day 2

# example: Marcelo looked at vocalizations of lab rats
# flats, trills, and complex calls
# even if you don't have a perfect estimation of frequency contours (bc measuring fundamental frequency is challenging), you can use these to gain inference on call types




#### Statistical descriptors of cepstral coefficients #####
# These coefficients deompose the sounds in a similar way that the human auditory system does in order to facilitate speech recognition (in a logarithmic scale ratehr than a linear scale)
# Done by using the Mel logarithmic scale
#The descriptive statistics that are extracted from the cepstral coefficients are: minimum, maximum, average, median, asymmetry, kurtosis and variance. It also returns the mean and variance for the first and second derivatives of the coefficients. These parameters are commonly used in the processing and detection of acoustic signals (e.g. Salamon et al 2014). 
# widely used for human voice analysis and it's use has extended to mammalian bioacoustics 

# recommended global parameters
warbleR_options(wav.path = "./examples", flim = c(1,10), w1 = 200, ovlp = 90, pb = FALSE)
# Calculate descriptors
cc <- mfcc_stats(X = lbh_selec_table)

## Marcelo says that these measurements have performed well for him, even though they're harder for us to wrap our head around
# captures variation in the structure of the parameters that the spectral parameters don't catch
# marcelo said it's good to catch dialects and differences in calls between juvniles and adults
## interpreting the result
# use dimensionality reduction methods 
# run principal components
pca <- prcomp(cc[, -c(1, 2)], scale = TRUE)

# extract first 2 PCs
cc_pcs <- data.frame(cc[, 1:2], pca$x[, 1:2])

cc_pcs

ggplot(cc_pcs,
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


### How to analyze these pcas?
# Fixed effect regression with individual as an effect, a manova also
# ANy mode where you can represent variation in individuals 

# the reduced version of the data with a PCA is more manaageable with stats


#### Relative measurements of similarity ######
#### Spectrographic cross correlation ####
# This analysis correlates the amplitude values in the frequency and time space pairwise for all signals in a selection table. The correlation represents a measure of spectrographic similarity of the signals

# Run the code
xcor <- cross_correlation(X = lbh_selec_table)
# this gives a triangular matrix with pearson's correlation coefficient
xcor
# here, we can see that phae.long1 all have a high correlation coefficient, this is because the underlying data is from the same individual. We can calulcate the similarity between signals from the same group and signals from different groups
## Good way to check the results of the PCA?
# Menthal test is a way to test these correlation outputs
## To do this you have to convert from similarities to dissimiliarities 
1 - xcor


# Visualizing this 
# present xcor as a heatmap using ggplot2
ggheatmap(xcor) +
  scale_fill_viridis_c(
    option = "G",
    direction = 1,
    begin = 0.1,
    end = 0.8
  ) +
  theme(axis.text.x = element_text(angle = 90))

# The acoustic space defined by the pairwise similarities can be projected into two axis using multidimensional scaling
# convert into distances
xcor_dist <- 1 - xcor
# multidimensional scaling
mds <- cmdscale(xcor_dist, k = 2)
# extract first 2 vectors
xcor_mds <- data.frame(lbh_selec_table[, 1:2], mds = mds[, 1:2])
# print
xcor_mds
#This simplified acoustic space can also be easily visualized with a scatterplot
ggplot(xcor_mds,
       aes(
         x = mds.1,
         y = mds.2,
         color = sound.files,
         shape =  sound.files
       )) +
  geom_point(size = 5) +
  scale_color_viridis_d(option = "G",
                        end = 0.9,
                        direction = -1) +
  theme_classic() +
  labs(x = "MDS1", y = "MDS2") +
  theme(legend.position = "right")

##### Exercise ###
#1. What does the argument type do and how does it affect the performance of the function?

#2. What does the pb argument do?



#### Dynamic Time Warping ######
# Works on the frequency contours
# Best explanation of this is on the github https://marce10.github.io/PR_BIR_2024/measure_acoustic_structure.html
# euclidean matching doesn't work well fror signals at different speeds or different start times
# The freq_DTW() function extracts the dominant frequency values as a time series and then calculates the acoustic dissimilarity using dynamic time warping

dtw_dist <- freq_DTW(lbh_selec_table)
# Gives you a distance matrix - the diagonal is zero 


# Cross-correlation is better when you have signals with a range of fequencies 
# when you have more like pure tones, dynamic time warping could be the better approach

# You can set the img = TRUE to produce image files with the spectrograms of the signals listed in the input data frame that show the location of the dominant frequneices
freq_DTW(lbh_selec_table, img = TRUE, col = "red", pch = 21, line = FALSE)
library(ggheatmap)
# present xcor as a heatmap using ggplot2
ggheatmap(dtw_dist) +
  scale_fill_viridis_c(
    option = "G",
    direction = -1,
    begin = 0.1,
    end = 0.8
  ) +
  theme(axis.text.x = element_text(angle = 90))


#### How would you analyze these matrices?
## First convert it into vectors
# Then handle it the same way - use the same statistics as you would with the PCA output

 


### Exercise #####
#1. What do the length.out argument in freq_DTW()?
# This is a numeric vector that gives the number of measurements of frequency desired (the length of the time series)

#2. Calculate spectrographic cross-correlation for the bat inquiry calls from these individuals: c("206433", "279470", "279533", "279820"). 
download.file(url = "https://ndownloader.figshare.com/files/21167052", 
              destfile = "./examples/iniquiry_calls.RDS")

inquiry_calls_new <- readRDS("~/UM/Research/Coding_Workspace/Biacoustics_R/examples/inquiry_calls_new.RDS")

iq_red <- inquiry_calls_new[inquiry_calls_new$indiv %in% c("206433", "279470", "279533", "279820")]
# Some of these don't have the same sampling rate
iq2 <- resample_est(iq_red, samp.rate = 375) # need sox for this, it doesn't work bc of installation stuff
# can we just remove the ones?
cr <- attr(inquiry_calls_new, "check.res")
cr$indiv <- inquiry_calls_new$indiv
aggregate(sample.rate ~ indiv, cr, mean)
# Pick new indiv
iq_red <- inquiry_calls_new[inquiry_calls_new$indiv %in% c("206433", "279545", "279817", "279820")]
# Run cross correlation
iq_xcor <- cross_correlation(X = iq_red)
# IF this takes a while to run you can reduce the overlap with the ovlp argument

#3. Compare dissimilarity from cross-correlation (1 - correlation matrix) with individual call membership matrix using Mantel test - are calls that belong to the same individual more similar than calls that belong to different individuals?
# gives you distance between things - used for genetics and for phenology
## can't find this??? it looks like phenotypespace isn't available for R 4.4.1 (current version??)
#remotes::install_github("maRce10/PhenotypeSpace")
library(PhenotypeSpace)
bi_mat <- binary_triangular_matrix(iq_red$indiv)

library(vegan)
mantel(xdis = bi_mat, ydis = 1 - iq_xcor)
# output indicates it is not significant - this tells us there is no signal at the individual level
## Could also look at this across age classes, etc.
## Notice that this is different from a regression, which would just tell you if there is a difference between the reference individual and the other individuals

#4. Do the same test but this time using cepstral coefficient cross-correlation (hint: see argument “type”)

xci_cc <- cross_correlation(iq_red, type = "mfcc") # mfcc for mel frequency instead of fourier
mantel(xdis = bi_mat, ydis = 1 - xci_cc)

#5. Do the same test using dynamic time warping distances
dtw_dist <- freq_DTW(iq_red, img = FALSE)
# Again, do the mentel test
mantel(xdis = bi_mat, ydis = dtw_dist)





#### Day 5: Addtional Measurements with warbleR ####

#1. Signal-to-noise ratio
# ratio of the power in the amplitude envelope of the signal to the adjacent noise
# sig2noise()
# you must provide the duration of the margin around the singal in which to measure the background noise
# higher numbers means you have more power in your signal relative to the background noise
snr <- sig2noise(X = lbh_selec_table, mar = 0.06)

#2. Inflections
# calculates how many times the slope changes in the signal (inflections) - can also get this in raven
# used as a measure of frequency modulation 
cntrs <- freq_ts(X = lbh_selec_table)
inflcts <- inflections(cntrs)

#3. Calculate parameters at higher levels of organization
# vocalizations can be organized above basic signal units like in long repertoire songs or multi-syllable calls
# can calculate the average of extreme values of acoustic parameters in the sub-units for these higher levels of organization 
# add a 'song' column
lbh_selec_table$song <- rep(1:4, each = 3)[1:11]

# measure default parameters
song_analysis(X = lbh_selec_table, song_colm = "song", parallel = 1, pb = TRUE)
# grouping things by song will first group by file, then by song 

data("lbh_selec_table")
# Can also be done on other parameters extracted from other functions
# measure acoustic parameters
sp <- spectro_analysis(lbh_selec_table[1:8, ], bp = c(1, 11), 300, fast = TRUE)
# Error in spectro_analysis(lbh_selec_table[1:8, ], bp = c(1, 11), 300,  : 
#The sound files are not in the working directory
# need to load this an an extended selection table I think 

sp <- merge(sp, lbh_selec_table[1:8, ], by = c("sound.files", "selec"))

# caculate song-level parameters for all numeric parameters
song_analysis(X = sp, song_colm = "song", parallel = 1, pb = TRUE)

# caculate song-level parameters selecting parameters with mean_colm
song_analysis(X = sp, song_colm = "song",mean_colm = c("dfrange", "duration"), parallel = 1, pb = TRUE)

song_analysis(X = sp, weight = "duration", song_colm = "song",
              mean_colm =  c("dfrange", "duration"), min_colm =  "mindom", max_colm = "maxdom", 
              parallel = 1, pb = TRUE)


#### Exercise ####
download.file(url = "https://github.com/maRce10/PR_BIR_2024/raw/master/examples/response_calls.RDS", 
              destfile = "./examples/response_calls.RDS")
response_calls <- readRDS("./examples/response_calls.RDS")
# Download directly 

#1. Calculate spectrographic parameters (spectro_analysis()) for the Spix’s disc-winged bat response calls.
sp <- spectro_analysis(X = response_calls, 
                       parallel = 3, # use parallel for faster processing
                       wl = 512) # adjust window length
sp$start <- response_calls$start
sp$end <- response_calls$end

#2. Summarize parameters by call (song_analysis()). To do that you should add the column ‘start’, ‘end’ and ‘call’ to the output of spectro_analysis()
# caculate song-level parameters for all numeric parameters
sa <- song_analysis(X = sp, song_colm = "sound.files", parallel = 1, pb = TRUE)
# notice for files where there is only one unit within the song, there is no measurements of spacing between units, etc