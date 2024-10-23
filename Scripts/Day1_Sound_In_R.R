#### Sound in R ######
library(seewave)
library(tuneR)
library(warbleR)
library(phonTools)
library(audio)
# All code on website: https://marce10.github.io/PR_BIR_2024/sound.html

##### Sound represented in: ####

# Common classes
## non-specific vectors (simplest way to encode it)
s1 <- sin(2 * pi * 440 * seq(0,1, length.out = 8000))
# length out is equal to sampling frequency
## visualize middle c
#s1 <- sin(2 * pi * 261.6 * seq(0,1, length.out = 8000))
is.vector(s1)
mode(s1)

# Look at the sine wave we just created using seawave
oscillo(s1, 
        f = 8000, # sampling frequency - if you don't provide this you will get an error
        from = 0, # starting time point
        to = 0.01) # ending time point (can adjust this to visualize different parts of the wave)

## matrices
s2 <- as.matrix(s1)
dim(s2)
# 1 column, 8000 rows
# When you have a matrix, each channel can be put into a different column
oscillo(s2, f = 8000, from = 0, to = 0.01)
# If the matrix has more than one column, only the first column will be considered
## Simulate noise
x <- rnorm(8000)
s3 <- cbind(s2, x)
oscillo(s3, f = 8000, from = 0, to = 0.01)


## Time series
# generate a series of time is shown corresponding to a 440 Hz sinusoidal sound sampled at 8000 Hz for one second
s4 <- ts(data = s1, start = 0, frequency = 8000)
# default is 1 if not provided
## Can also replace s1 with the data-generating code sin(2 * pi * 440 * seq(0,1, length.out = 8000))
str(s4)
oscillo(s4, f = 8000, from = 0, to = 0.01)

# The functions frequency() and deltat() return sampling frequency and time resolution 
frequency(s4)
deltat(s4)
# Time resolution is the wavelength?

# simulate noise
s5 <- ts(data = runif(4000, min = -1, max = 1), start = 0, end = 0.5, frequency = 8000)

# since frequency is incorporated into ts objects, it is not necessary to specify it when used within functions dedicated to audio
oscillo(s5, from = 0, to = 0.01)

# It's easier to use ts() for sound so that the data about how you sampled the sound is included 

#### Dedicated R classes for sound #####
# Most commonly, we will use the Wave class from tuneR (most popular to use)
# sound class of package phonTools
# AudioSample class of package audio

# wave class
## save class is an s4 class that includes different "slots" with amplitude data (L or R channel), sampling frequency, number of bits, and type of sound (mono/stereo)
# you can also simulate data within this class ******figure out later******
# The function to import .wav files is readWave
s6 <- readWave("./examples/Phae.long1.wav")
class(s6)
# look at the metadat - use @ to access each slot
str(s6)
# default with mono audio is the left channel
# pcm means the method to digitally represent sampled analog signals - it's the standard form of digital audio. in PCM streams, amplitude is sampled regularly at uniform intervals and each sample is quantized to the nearest value within a range of digital steps. (You won't use any other method)
# using the fourier transformation and other mathematical devices, we can transform this single vector of amplitudes into separate frequency bands
# Sampling rate
s6@samp.rate
# pull out the first 40 samples (amplitude values) from the left (only) channel
s6@left[1:40]
# Units for this? amplitude (?)
## Calibrate recordings by playing a standardized sound 1m from the microphone to calibrate it to spl - converts unitless amplitude values to something you can put units on
# if not calibrated this will be relative to the loudest sound in the recording

#### Exercise: ####
# Calculate the duration of the wave object using information in object? (number of samples, sampling rate)
# duration 
num_samps <- length(s6@left)
samp_rate <- s6@samp.rate
secs <- num_samps/samp_rate
# here we have 2.5 seconds of audio

# Extract the first second of audio from the object s6 using indexing and square brackets
sec_1 <- s6@left[1:samp_rate]
# or extract it as a wave object still
sec_1 <- s6[1:s6@samp.rate]
# you can also just print a wave object and see the duration
s6
# Notice how the durations are different
sec_1

# read in my own data to work with
ex_ak <- readWave("./examples/ak_dat/XC16526-BBCUCadenceCoo.wav")
ex_ak

#### Advantages of using wave classes #####
# An advantage of using readWave() is the ability to read specific segments of sound files, especially useful with long files
s7 <- readWave("./examples/Phae.long1.wav", from = 1, to = 2, units = "seconds")
s7
# This is helpful with large acoustic recordings
# You can also read in mp3 data with readMP3() function
s7 <- readMP3("./examples/Phae.long1.mp3")

# Using wave objects allows us to use the same data class for different objects, but at a loss of some information
# readWave does not normalize the sound. You lost a bit of the data included in mp3 files, for example a quiet sound right after a loud sound is missed to the human ear so is dropped from wave objects but is present in mp3 files. The values that describe the sound will be included between +- 2^bit - 1
range(s7@left)
# total amplitude intervals
2^16 - 1
(2^16 - 1)/2

#### Exercise ####
# Use the wave function to create wave objects
# constructing a Wave object (1 sec.) containing sinus sound with 440Hz:
x <- seq(0, 2*pi, length = 44100)
channel <- round(32000 * sin(440 * x))
wobj <- Wave(left = channel)
wobj
# Plot the oscillogram
oscillo(wobj, f = 44100, from = 0 , to = 0.01)

# Function sine provides a shortcut that can be used to create wave object with a sine wave
# Generate a sine wave signal - these are the most similar to what we see in nature
wobj1 <- sine(freq = 440, duration = 1000)
wobj1
# can also use base plot
plot(wobj1)
oscillo(wobj1, f = 44100, from = 0 , to = 0.01)
# create a signal with random noise (scattered waveform)
wobj2 <- noise(duration = 1000)
oscillo(wobj2, f = 44100, from = 0 , to = 0.01)
# Create a pulsating noise
wobj3 <- pulse(220, duration = 1000)
oscillo(wobj3, f = 44100, from = 0 , to = 0.01)


#### read_sound_files() from warbleR ####
library(warbleR)
# Can handle a lot of different types of files as wave objects
#.wav files
rsf1 <- read_sound_file("Phaethornis-eurynome-15607.wav", path = "./examples")
class(rsf1)

# mp3
rsf2 <- read_sound_file("Phaethornis-striigularis-154074.mp3", path = "./examples")
class(rsf2)

# flac
rsf3 <- read_sound_file("Phae.long1.flac", path = "./examples")
class(rsf3)

# wac
rsf4 <- read_sound_file("recording_20170716_230503.wac", path = "./examples")
class(rsf4)

# Can also read recordings hosted in an online repository
rsf5 <- read_sound_file(X = "https://xeno-canto.org/35340/download")
class(rsf5)
# Sub this in with a cuckoo file *********

#### sound object with phonTools ####
# package for phonetic analysis
# only works with files of a bit depth of 8 or 16
# can also only open whole files
# only takes .wav files
library(phonTools)

s8 <- loadsound("./examples/Phae.long1.wav")
s8
str(s8)
# stores the sound as a list, not an s4 object (extract elements with $)
# fs is sampling frequency


##### audioSample with audio package #####
# less user friendly
# also can't read in a subset of the audio file
library(audio)

s9 <- load.wave("./examples/Phae.long1.wav")
head(s9)
# stores it as a audioSample class which is essentially a vector of numbers
attr(s9, "rate")


### Recording in R ####
# with the audio package you can also use record() to directly record into R
# create a vector with empty values
# define the length of the vector as the sampling rate * the amount of time you want to record for 
s11 <- rep(NA_real_, 16000 * 2)
# record for two seconds
record(where = s11, rate = 16000, channels = 1)
save.wave(s11, "./examples/ak_dat/rec_test.wav")
# can be controlled with pause(), rewind(), and resume()


#### Export sound #####
# For maximum compatability with other sound programs, it's useful to save as a .txt file
data(tico) # 2-second file of rufous-collared sparrow
#export(tico, f = 22050)
# marcelo doesn't use this very often

# Exporting a .wav file
# tuneR: writeWave() # recommended one
# audio: save.wave()
# seewave savewav() # marcelo said this one has had issues
writeWave(object = tico, filename = "./examples/ak_dat/tico.wav")

# Can also use .flac format
# reduces bandwidth and storage requirements (from generally 40-50%) without sacrificing audio quality 
# .flac can't be used with R but you can use wav2flac()
wav2flac(file = "./examples/Phae.long1.wav", overwrite = FALSE)
# ex
wav2flac("Phae.long1.flac", reverse = TRUE)
# can also use read_sound_files() in warbleR to read .flac
## This converts flac to wav then reads the wav file


#### Exercise ####
# how does the sampling rate affect the size of an audio file?
# small audio file - low sampling rate
t1 <- sine(freq = 440, duration = 1000, xunit = "time", samp.rate = 4410)
t2 <- sine(freq = 440, duration = 1000, xunit = "time", samp.rate = 2205)
object.size(t1)
object.size(t2)
# Save object
#writeWave(object = t1, filename = "./examples/ak_dat/test_highsamprate.wav")
#writeWave(object = t2, filename = "./examples/ak_dat/test_lowsamprate.wav")
# Doubles the size of the audio file

# How does the dynamic range affect the size of an audio file
t4 <- sine(freq = 440, duration = 1000, xunit = "time", samp.rate = 4410, bit = 32)
t5 <- sine(freq = 440, duration = 1000, xunit = "time", samp.rate = 4410, bit = 64)
object.size(t4)
# unique numbers stored: 
2^32
object.size(t5)
# unique numbers stored: 
2^64
# Save object
# writeWave(object = t4, filename = "./examples/ak_dat/test_lowbitdepth.wav")
# writeWave(object = t5, filename = "./examples/ak_dat/test_highbitdepth.wav")
# Doubles the size of the audio file as well 
# there are more numbers needed to represent amplitude values measured with a greater bit depth (more digits)
# generally only if you go to very low bit depth is this going to be an issue


#Use the system.time() function to compare the performance of the different functions to import audio files in R. For this use the file “LBH.374.SUR.wav” (Long-billed hermit songs) which lasts about 2 min
tim1 <- Sys.time()
# Import method one: audio package
test <- load.wave("./examples/LBH.374.SUR.wav")
tim2 <- Sys.time()
print(paste0("Run time load.wave() (s):", round(tim2-tim1,3)))

tim1 <- Sys.time()
# Import method one: read_sound_file
test <- read_sound_file("LBH.374.SUR.wav", path = "./examples")
tim2 <- Sys.time()
print(paste0("Run time read_sound_files() (s):", round(tim2-tim1,3)))

tim1 <- Sys.time()
# Import method one: readWave
test <- readWave("./examples/LBH.374.SUR.wav")
tim2 <- Sys.time()
print(paste0("Run time readWave() (s):", round(tim2-tim1,3)))

system.time(test <- readWave("./examples/LBH.374.SUR.wav"))
# Load.wave is the fastest but is more limited in what you can do 

# Demonstrate the nyquist frequency
# generate sine wave
wav <- sine(freq = 440, duration = 500, xunit = "samples", samp.rate = 44100)
oscillo(wav, f = 44100, from = 0, to = 0.01)
# Nice smooth curve
# plot
plot(wav@left)
wav_d1 <- downsample(object = wav,samp.rate = 3200)
plot(wav_d1@left)
# push this below the nyquist frequency
wav_d2 <- downsample(object = wav,samp.rate = 2000)
plot(wav_d2@left)
# now we're starting to see the results of sampling below the nyquist frequency
oscillo(wav_d2, f = 2200, from = 0, to = 0.01)

# Frequency 
wav <- sine(freq = 440, duration = 500, xunit = "samples", samp.rate = 44100)
# freq is how often the sound wave oscillates per second
# Marcelo's code
samp_rates <- c(4000, 3200, 2000)
for (i in samp_rates){
  new_wav <- downsample(object = wav,samp.rate = i)
  plot(new_wav@left)
  # add in a pause of 1 second after each loop iteration
  Sys.sleep(1)
}

## Nyquist frequency is half the sampling rate, not half the signal
# wav <- sine(freq = 440, duration = 0.01, xunit = "time", samp.rate = 44100)
# plot(wav@left)
# wav <- sine(freq = 440, duration = 0.01, xunit = "time", samp.rate = 44100/2)
# plot(wav@left)
# wav <- sine(freq = 440, duration = 0.01, xunit = "time", samp.rate = 220)
# 
# wav <- sine(freq = 440, duration = 0.01, xunit = "time", samp.rate = 1000)
# wav <- sine(freq = 440, duration = 0.01, xunit = "time", samp.rate = 440)
# # this isn't the nyquist frequency????
# plot(wav@left)
# # nyquist frequency is below the signal
