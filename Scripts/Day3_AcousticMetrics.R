#### Day 3: Acoustic Metrics #####

# seewave has capacity to create small modules and tools that you can combine into an efficient workflow
# warbleR comes in handy with this - batch process annotations


#### Tools in seewave #####
library(seewave)
# wave object class comes from tuneR which seewave adopted
# Marcelo recommends using wave objects
# Load in a couple of audio examples from packages
## Bird
data(tico)
## Cicadas
data(orni)
## Sheep
data(sheep)

# Review: creating oscillograms 
## Reminder: don't need to provide sampling frequency since this is a wave objecta and it's already built in 
oscillo(tico)
# Oscillograms are more for visualizations, you rarely visualize them like thie

# Oscillogram customization
oscillo(tico, f = 22050, k = 4 , j = 1,
        title = TRUE,
        colwave = "black", 
        coltitle = "yellow",
        collab = "red",
        colline = "white",
        colaxis = "blue",
        coly0 = "grey50")

# Amplitude envelope
#generate representations of amplitude vs time
env(tico, f = 22050, colwave = "#8fcc78")
# helps with measuring degradation

# envelopes can help you with automatic detection
oscillo(tico, f = 22050)
par(new=TRUE)
env(tico, f = 22050, colwave = "#8fcc78")
# Works well when you have a good signal to noise ratio

# You often have to smooth out the envelopes to help with this, which is done with a sliding window
## Sliding windows allow you to smooth out the contours of a time series by calculating an average value around the “neighborhood” of values for a given value. In the case of amplitude envelope the size of the “neighborhood” is given by the length of the window (“wl”). The larger the window length, the greater the smoothing of the curve

# Can use timer() from seewave to demonstrate this
tmr <- timer(orni, 
             f = 22050, 
             threshold = 5, # threshold in percentage, highest amplitude is 100% so you set anything above 5% of the highest amplitude to be part of the signal
             ssmooth = 40, # amount of smoothing to use
             bty = "l", colval = "#51c724")
# notice how the last sound in this amplitude envelope is split into two as a result of the variation amplitude within the signal 

tmr
# s is start times of the signals
# p isthe start tie of the pauses
# r is ratio of time in the recording with signal vs with just noise
# s.start signal state
# s.end signal end (look for these two with automatic detection)

#### Exercise: use timer() after smoothing the envelope to fix the issue with the last signal
# first try increasing the smoothing
tmr2 <- timer(orni, 
             f = 22050, 
             threshold = 5, 
             ssmooth = 100, 
             bty = "l", colval = "#51c724")
tmr2
# still getting 9, let's try increasing the threshold
tmr3 <- timer(orni, 
              f = 22050, 
              threshold = 10, 
              ssmooth = 100, 
              bty = "l", colval = "#51c724")
tmr3
# This is nearly perfect, try smoothing it a bit more
tmr4 <- timer(orni, 
              f = 22050, 
              threshold = 10, 
              ssmooth = 150, 
              bty = "l", colval = "#51c724")
tmr4
# Yay!

# Visualize the amplitude in the frequency domain using power spectra
# meanspec() function in seewave
# Calculates avg dist of energy in frequency range

mspc <- meanspec(orni, f = 22050, wl = 512, col = "#d1e7dd")
# look at just the first frequency value?
polygon(rbind(c(0, 0), mspc), col = "#d1e7dd")
nrow(mspc)
# Why is this? This is the size of one window

# spc() calculates spectrum for entire signal
spc <- spec(orni, f=22050, wl=512, col = "#8fcc78")

# The result of spec()or meanspec() can be input into the fpeaks() function to calculate amplitude peaks
pks <- fpeaks(spc, nmax = 1)
# print this out to see where exactly this peak is
pks


# We can cut oscillograms
tico2 <- cutw(tico, to = 1, output = "Wave")
oscillo(tico2)

# Can combine segments
tico3 <- pastew(tico, tico2, output = "Wave")
oscillo(tico3)

# You can also remove segments
# remove what we added before 
tico4 <- deletew(tico3, output = "Wave", from = duration(tico), to = duration(tico3))
oscillo(tico4)
# NOTE this just deletes and also modifies the time

#You can add silence as well 
# have to be explicit about output format
tico5 <- addsilw(tico, at = "end", d = 1, output = "Wave")
# Check the 
duration(tico)
duration(tico5)

# Can also reverse the order of a wav object

tico_rev <- rev(tico@left)
#spectro(tico)
spectro(tico, 
        f = 22050, 
        wl = 512, 
        ovlp = 80,
        collevels = seq(-40, 0, 0.5),
        scale = FALSE,
        main = "Tico Orig")

spectro(tico_rev, 
        f = 22050, 
        wl = 1024, 
        ovlp = 80,
        collevels = seq(-80, 0, 0.5),
        flim = c(2,6), 
        scale = FALSE,
        main = "Tico Reversed")
# These functions make manipulations fairly simple, since part of a wave object is a vector so you can do all the vector math on it
# Can also add silence
tico_sil <- c(tico@left, rep(0,tico@samp.rate))
spectro(tico_sil, 
        f = 22050, 
        wl = 512, 
        ovlp = 80,
        collevels = seq(-40, 0, 0.5),
        scale = FALSE,
        main = "Tico With Silence")
# Can also add noise
tico_noise <- c(tico@left, sample(tico@left, tico@samp.rate, replace = TRUE))
spectro(tico_noise, 
        f = 22050, 
        wl = 512, 
        ovlp = 80,
        collevels = seq(-40, 0, 0.5),
        scale = FALSE,
        main = "Tico With Noise")
# Can simulate background noise 
tico_back <- tico@left+ sample(tico@left)/3
spectro(tico_back, 
        f = 22050, 
        wl = 512, 
        ovlp = 80,
        collevels = seq(-40, 0, 0.5),
        scale = FALSE,
        main = "Tico With Background")
# They used this when they wanted to play back sounds with various levels of backround noise


#### Bandpass filters ####
# function ffilter()
spectro(ffilter(tico, 
                from = 4000, 
                to = 6500, 
                output = "Wave"),
        scale = FALSE, 
        #collevels = c(-80,0,0.5),
        grid = FALSE, 
        flim = c(2, 6))

# Set bandpass = FALSE to do this?

#### Change Pitch####
# function fs = frequency shift
# cut the first
tico6 <- cutw(tico, from = 0, to = 0.5, output = "Wave")

# increase frec
tico.lfs <- lfs(tico6, shift = 1000, output = "Wave")

# decrease frec
tico.lfs.neg <- lfs(tico6, shift = -1000, output = "Wave")

# 3 column graph
opar <- par()
par(mfrow = c(1, 3))

# original
spectro(tico6, scale = FALSE, grid = FALSE, flim = c(1, 8), main = "original")

# modified
spectro(tico.lfs, scale = FALSE, grid = FALSE, flim = c(1, 8), main = "1 kHz up")

spectro(tico.lfs.neg, scale = FALSE, grid = FALSE, flim = c(1, 8), main = "1 kHz down")

# Use case: how do seabirds find their offspring? They took the audio of a chick's call and edited it higher/lower to see what was important
# fpeaks gives you peak frequency
# timer gives you duration
# Load in a focaliation with a lot of harmonics
spectro(sheep, scale = FALSE, grid = FALSE)

par(new=TRUE)
# extract the fundamental frequency
# lowest frequency in the harmonic stack
# Lots of ways to do this
## fund() uses cepstral transformation
ff <- fund(sheep, fmax = 300, # frequency below which to search 
           ann = FALSE, 
           threshold=6, # amplitude threshold
           col = "green")
head(ff)
# for each time window in the spectrogram it gives you the fundamental frequency

# autoc() measures the fundamental frequency only using autocorrelation
# this tends to be noisy
## not many good ways of doing this

#Now add on dominant frequency
# Dominant frequency is the frequency with the highest amplitude
par(new=TRUE)

df <- dfreq(sheep, f = 8000, fmax = 300, type = "p", pch = 24, ann = FALSE, threshold = 6, col = "red")

head(df)
# dominant frequency seems to be easier to calculate since the math is easier - less noise in this output

# generally there are more tools for dealing for harmonics in the phonetics literature
## Harmonics are produced by the resonance of our vocal chords or other sound producing organ
## ex speech, different vowels are formed by changing the shape of our mouth to emphasize different harmonics 
## ex: you can take a hawk vocalization and modify the relative amplitude of different harmonics andchange the sound of the calls
## Birds have organs to filter out the harmonics so that they only produce the fundamental frequency
## Harmonics get degraded faster than the fundamental frequency
## harmonics are very information rich - can inform dialects, regional variations etc
## Methods for describing and analyzing calls ignore harmonics - Marcelo thinks we should be paying more attention to it



# Measure statistical descriptors of the amplitude distribution in frequency and time
# Just like how we use statistical descriptors to measure distirbutions, we can do this for acoustics too
# cut
note2 <- cutw(tico, from=0.6, to=0.9, output="Wave")
# use acoustat() function in seewave
n2.as <- acoustat(note2)
# can help you describe which attributes of a call vary the most between species
# Produces these two graphs
names(n2.as)
# measurements about the distribution of energy within the signal
# gives quartiles (labeled as P for percentile) and interqartile range (labeled as IPR)
# was originally developed in matlab
as.data.frame(n2.as[3:8])
# returns time contour, frequency controu
# measure power spectrum
n2.sp <- meanspec(note2, plot = FALSE)
# specprop only works on the frequency spectrum
n2.spcp <- specprop(n2.sp, f = note2@samp.rate)

as.data.frame(n2.spcp)
# returns mean, median, asymmetry (skewedness) and peakiness (kurtosis)



#### Exercise ####
# Measure the statistical descriptors of the frequency spectra (function specprop()) on the 3 notes (hint: you must cut each note first)
## Step 1: cut out the separate notes of tico
# Find where they are
spectro(tico, 
        f = 22050, 
        wl = 512, 
        ovlp = 80,
        collevels = seq(-40, 0, 0.5),
        scale = FALSE,
        main = "Tico Orig")
tico_n1 <- cutw(tico, from=0.06, to=0.4, output="Wave")
# Step 1: measure power spectrum
tico_n1_sp <- meanspec(tico_n1, plot = FALSE)
# specprop only works on the frequency spectrum
#step 2: calculate values
tico_n1_sprp <- specprop(tico_n1_sp, f = tico_n1@samp.rate)
as.data.frame(tico_n1_sprp)
# check the shared code for the rest


# Can use this on annotations from raven?? or maybe we can just export these
# Check shared code for this

# can put this functionality into a loop to deal with multiple selections then export them into a dataframe
## Check the shared code for what this looks like *************

# Do we need to standardize the sound from xeno-canto and other sources with different sampling rates?
## Yes standardize sample rate 
## Yes use frequency filter and band passes on all of them
## Use the same window length on the spectrograms
## Any parameters that affect the way the spectrogram is being represented or built you want to standardize
# Many measurements in statistical description that we do are robust to variation in the signal to noise ratio - just looking at the power distribution so it will give you the same answer regardless of the scale
# The power distributions that we measured today are robust to that
# As long as the signal isn't significantly degraded by the background noise and signal to noise ratio
# Some people are saying that they want a tutorial to standardize this
# Marcelo only standardizes the amplitude when he's doing playback experiments