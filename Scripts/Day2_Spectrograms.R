#### Spectrograms #####
# https://marce10.github.io/PR_BIR_2024/spectrograms.html
# Using the seewave package
library(seewave)

# Load example wav objects
data(tico)
data(orni)

#### Fourier Transformation ####
# TO create a spectrogram, we use the Fourier transformation - a mathematical transformation that detects the periodicity in time series, idenifying the different frequencies that compose them and their relative energy
## Originally developed to understand heat transfer


# Let's create a time series with three different frequencies

f <- 11025 # define the sampling rate 

# time sequence
t <- seq(1/f, 1, length.out = f)

# period
pr <- 1/440 
w0 <- 2 * pi/pr

# frec 1
h1 <- 5 * cos(w0 * t) # 5 tells us amplitude
plot(h1[1:75], type = "l", col = "blue", xlab = "Time (samples)", ylab = "Amplitude (no units)", main = "Frequency 1")

# frec 2
h2 <- 10 * cos(2 * w0 * t) # 10 gives us amplitude
plot(h2[1:75], type = "l", col = "blue", xlab = "Time (samples)", ylab = "Amplitude (no units)", main = "Frequency 2")

# frec 3
h3 <- 15 * sin(3 * w0 * t) # 15 gives us amplitude
plot(h3[1:75], type = "l", col = "blue", xlab = "Time (samples)", ylab = "Amplitude (no units)", main = "Frequency 3")

# Put them together in a single complex sound wave
H0 <- 0.5 + h1 + h2 + h3
plot(H0[1:75], type = "l", col = "blue", xlab = "Time (samples)", ylab = "Amplitude (no units)", main = "Combined Frequncies")



# Apply a fourier transformation using fft()
fspc <- Mod(fft(H0))

plot(fspc, type = "h", col = "blue", xlab = "Frecuency (Hz)", ylab = "Amplitude (no units)")
abline(v = f/2, lty = 2)
text(x = (f/2) + 1650, y = 8000, "Nyquist Frequency")

# We see this gives the relative amplitude of the frequency domain (time is gone)
## It has recovered our original inputs
## We can also see a reflection of the image above the nyquist frequency 


# Let's zoom in to just samples below the nyquist frequency
plot(fspc[1:(length(fspc)/2)], type = "h", col = "blue", xlab = "Frecuency (Hz)",
     ylab = "Amplitude (no units)")
# This periodogram is the output from the fourier transformation
## We can manipulate this to get a spectrogram


# So how does this work?
# Since the fourier transformation gets rid of the time domain, we can split the audio signal into discrete segments and calculate a fourier transformation for each. Then, we combine the results of each fourier transformation and visualize it over time again

# Overlap of 50% means that half of the values in the previous time window will be used in the next time signal. Greater overlap means smoother contours.

# However, the more windows you have, the longer it take to calculate the spectrogram
## At about 80% overlap, you start exponentially increasing the number of windows you have to calculate
## You want to take this into account when producing spectrograms of long sound files
# 512 window size with 10% overlap means that window 1 goes from 1-512, next goes from 51 to 563, etc


# Time-frequency tradeoff 
# As the time windows increase, the resolution of the time window increases (gets smaller)
# But as time resolution increases, the resolution of values in the frequency domain decreases
# Why? The Fourier transformation is more accurate the more data you provide to it. 
# The larger the amplitude values you give, the more precise the frequecncy estimates
# No single value to define where you land on this tradeoff

# There are other methods for calculating spectrograms (Something called Mel?) but Fourier transformation is by far the most common




#### Creating Spectrograms in R ####
# Lots of different ways to create spectrograms, but we'll be using spectro() from seewave() - Marcelo says this is the most customizable
# warblR creates a spectrogram with seewave()

# Crop the audio
tico2 <- cutw(tico, from = 0.55, to = 0.9, output = "Wave")
# Since tico is a wave object, we do't need to provide f = 
#spectro(tico2, f = 22050, wl = 512, ovlp = 90, collevels = seq(-40, 0, 0.5), flim = c(2,6), scale = FALSE)
spectro(tico2, wl = 512, # window size - length of amplitude vector used for each fourier transformation
        ovlp = 90, # overlap between windows
        collevels = seq(-40, 0, 0.5), 
        flim = c(2,6), # adjust the frequency represented
        scale = FALSE) # if TRUE, it will add a legend with amplitude values (not calibrated in this case so it's relative to the highest sound in this file)


#### Exercise 1 #####

# 1. How can I increase the overlap between time windows?
spectro(tico2, f = 22050, wl = 512, ovlp = 80, collevels = seq(-40, 0, 0.5), flim = c(2,6), scale = FALSE)
## Change ovlp argument

# 2. How much longer it takes to create a 99%-overlap spectrogram compare to a 5%-overlap spectrogram?
system.time(spectro(tico2, f = 22050, wl = 512, ovlp = 5, collevels = seq(-40, 0, 0.5), flim = c(2,6), scale = FALSE))
system.time(spectro(tico2, f = 22050, wl = 512, ovlp = 90, collevels = seq(-40, 0, 0.5), flim = c(2,6), scale = FALSE))
# it takes 600% longer(6x longer)

# 3. What does the argument ‘collevels’ do? Increase the range and look at the spectrogram.
spectro(tico2, f = 22050, wl = 512, ovlp = 80, collevels = seq(-80, 0, 0.5), flim = c(2,6), scale = FALSE)
# changes the range of amplitude values in the spectrogram
# calibrates the range of amplitude values in the spectrogram
# you are matching certain amplitude values to certain colors and defining how low the amplitude goes 
# 0 is the loudest sound in the spectrogram, -40 is how low you go relative to the loudest sound
# acts like a bit of a "noise filter"
# 0.5 is the size of the interval across which color is assigned - how many colors do you want?
spectro(tico2, f = 22050, wl = 512, ovlp = 80, collevels = seq(-80, 0, 10), flim = c(2,6), scale = FALSE)
# Using a calibrated value for amplitude
# unless you are picking up signals from different parts of the spectrogram and putting them together then you might want to calibrate them, but a spectrogram isn't the best way to reprsent absolute amplitude. 
# you would use dBref within the spectro() arguments


# 4. What do the ‘flim’ and ‘tlim’ arguments determine?
# flim is frequency limites, tlim is time limits
# zoom in on time 
spectro(tico2, f = 22050, wl = 512, ovlp = 80, collevels = seq(-40, 0, 0.5), flim = c(2,6),tlim = c(0.1,0.25), scale = FALSE)
# Zoom in on frequency
spectro(tico2, f = 22050, wl = 512, ovlp = 80, collevels = seq(-40, 0, 0.5), flim = c(3,5.5), scale = FALSE)

# 5. Run the examples that come in the spectro() function documentation
# change the dB scale by setting a different dB reference value (20microPa)
spectro(tico2,f=22050, dBref=2*10e-5)





# Try this with my own data too
ak_coo <- readMP3("./examples/ak_dat/XC547273-BBCU.mp3")
spectro(ak_coo,
        ovlp = 90,
        flim = c(0.7,1.75),
        tlim = c(1.5,1.9),
        collevels = seq(-20, 0, 0.5))
# Try different visualization parameters
spectro(ak_coo,
        ovlp = 90,
        flim = c(0.7,1.75),
        tlim = c(2.3,4),
        collevels = seq(-25, 0, 0.5),
        scale = FALSE)
# Testing different palettes
spectro(ak_coo,
        ovlp = 90,
        flim = c(0.7,1.75),
        tlim = c(2.3,4),
        collevels = seq(-25, 0, 0.5),
        scale = FALSE,
        palette = reverse.gray.colors.1)
custom_pal <- colorRampPalette( c("#2d2d86", "#2d2d86", reverse.terrain.colors(10)[5:10]))
spectro(ak_coo,
        ovlp = 90,
        flim = c(0.7,1.75),
        tlim = c(2.3,4),
        collevels = seq(-25, 0, 0.5),
        scale = FALSE,
        grid = FALSE,
        osc = TRUE,
        colbg = "black",
        colaxis = "white",
        collab = "white",
        heights = 1, # change the heigh tof the oscillogram
        #cont = TRUE,
        #colcont = "white",
        palette=magma) # add the oscillogram
library(viridis)
spectro(ak_coo,
        ovlp = 90,
        flim = c(0.7,1.75),
        tlim = c(2.3,4),
        collevels = seq(-25, 0, 0.5),
        scale = FALSE,
        grid = FALSE,
        osc = TRUE,
        listen = TRUE,
        heights = c(2,1), # change the heigh tof the oscillogram
        #cont = TRUE,
        #colcont = "white",
        palette=magma) # add the oscillogram
# try with a different vocalization - make a rattle for visualization in the manuscript


library(viridis)
# palette magma




#### Day 3: Playing around with zero padding ####

# zero-padding is a trick to increase the number of freuquency samples while leaving the time samples unchanged
# Adds zeros on to the window size to artificially increase the amount of data you have for the fourier transform
# improves your visualization
# marcelo says he very rarely sees a significant improvement in visualizations
# Don't use it for measurements, just for visualization
spectro(tico2, 
        f = 22050, 
        wl = 512, 
        ovlp = 80,
        zp = 100,
        collevels = seq(-80, 0, 0.5),
        flim = c(2,6), 
        scale = FALSE,
        main = "Zero-padding spectro")


spectro(orni, 
        f = 22050, 
        wl = 1024, 
        ovlp = 80,
        collevels = seq(-80, 0, 0.5),
        flim = c(2,6), 
        scale = FALSE,
        #listen = TRUE,
        main = "Orni With Large Window Size")
# Going up in the window size decreases the time resolution and increases the frequency resolution
spectro(orni, 
        f = 22050, 
        wl = 256, 
        ovlp = 80,
        collevels = seq(-80, 0, 0.5),
        flim = c(2,6), 
        scale = FALSE,
        main = "Orni with Smaller Window Size")
# window size changes the frequency windows

# If youwant to have the whole background colored, make the amplitude range for collevels very low
spectro(orni, 
        f = 22050, 
        wl = 256, 
        ovlp = 80,
        collevels = seq(-120, 0, 0.5),
        flim = c(2,6), 
        scale = FALSE,
        main = "Orni Viridis Palette",
        palette = mako)





#### Dynamic spectrogram ######
# Play around a bit with dynaSpec package: https://cran.r-project.org/web/packages/dynaSpec/readme/README.html
#install.packages("dynaSpec")
library(dynaSpec)
# Will visualize the spectrogram in a way that scrolls over the sound file 
# Don't do this with several minutes of recordings!!! He said downsample or trim them first 

ak_coo_red <- readWave("./examples/ak_dat/XC16526-BBCUCadenceCoo.wav")
# Tried with ak_coo at first and it was too long, now I'm getting an error so need to work on this more later

scrolling_spectro(wave = ak_coo_red,
                  wl = 300, 
                  t.display = 1.7, pal = viridis, 
                  grid = FALSE, flim = c(1, 9), 
                  width = 1000, height = 500, 
                  res = 120, file.name = "./examples/ak_data/coo_vis.mp4")

