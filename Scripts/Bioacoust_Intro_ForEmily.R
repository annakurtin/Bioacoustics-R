#### What is bioacoustics? For Emily #####

#### What is sound? #####

# People always say that sound is a wave, but what does this mean?
# Sound is the compression and rarefaction of particles in a medium that spread in three dimensions
# We can see the propagaion of these sound waves with the naked eye in nature: https://www.youtube.com/watch?v=CCR5J1YYCiE

# So if sound is constantly varying pressure in a three dimensional space, how do we quantify it or capture it in a way that we can measure and understand?
## We take samples of pressure as measured at a fixed point in space, which allows us to reconstruct a sound wave
# the pressure is measured via amplitude, and the rate at which peaks in amplitude pass is called the frequency, which we percieve as the pitch of the sound

library(seewave)
#library(tuneR)

# let's create one second of a pure tone
# let's make a vector of amplitude samples that vary as a sinusoid
# the frequency of this simulated sound signal is 440 Hz, which corresponds to the A above middle C
s1 <- sin(2 * pi * 440 * seq(0,1, length.out = 8000))
# now let's just look at the very first part of this sound as a waveform also called an oscillo gram
oscillo(s1, 
        f = 8000, # sampling frequency - if you don't provide this you will get an error
        from = 0, # starting time point
        to = 0.005) # ending time point (can adjust this to visualize different parts of the wave)

# In reality, we have a much more complex signal than just a pure tone like this
# Sound waves overlap in time and affect each other with constructive/destructive interference
# Let's create another sound wave as if we have two pure tones going at the same time, this time an octave higher
s2 <- sin(2 * pi * 880 * seq(0,1, length.out = 8000))
oscillo(s2, 
        f = 8000, 
        from = 0, 
        to = 0.005)
# Now, if we add these sound signals together as if they are being produced at the same time, we see something weird 
s3 <- s1 + s2
oscillo(s3, 
        f = 8000, 
        from = 0, 
        to = 0.005)

# This is an example of a combination of constructive and destructive interference
# plot the oscillograms on top of each other
par(mfrow = c(3,1))
oscillo(s1, 
        f = 8000, 
        from = 0, 
        to = 0.005)
oscillo(s2, 
        f = 8000, 
        from = 0, 
        to = 0.005)
oscillo(s3, 
        f = 8000, 
        from = 0, 
        to = 0.005)

# destructive interference
s4 <- -sin(2 * pi * 440 * seq(0,1, length.out = 8000))
destruct <- s1 + s4
par(mfrow = c(3,1))
oscillo(s1, 
        f = 8000, 
        from = 0, 
        to = 0.005)
oscillo(s4, 
        f = 8000, 
        from = 0, 
        to = 0.005)
oscillo(destruct, 
        f = 8000, 
        from = 0, 
        to = 0.005)
# nothing?! these sounds have canceled each other out
# Constructive interference
s5 <- sin(2 * pi * 440 * seq(0,1, length.out = 8000))
construct <- s1 + s5
par(mfrow = c(3,1))
oscillo(s1, 
        f = 8000, 
        from = 0, 
        to = 0.005)
oscillo(s5, 
        f = 8000, 
        from = 0, 
        to = 0.005)
oscillo(construct, 
        f = 8000, 
        from = 0, 
        to = 0.005)


# So this is a complex, constantly varying signal, how do we measure and record it? 
# Take samples
# generate sine wave
par(mfrow = c(2,1))
wav <- sine(freq = 440, duration = 500, xunit = "samples", samp.rate = 44100)
plot(wav@left)
oscillo(wav, f = 44100, from = 0, to = 0.01)

# Nyquist frequency ex - highest frequency you can observe in your data
# generate sine wave
# par(mfrow = c(4,1))
# wav <- sine(freq = 440, duration = 500, xunit = "samples", samp.rate = 44100)
# plot(wav@left)
# oscillo(wav, f = 44100, from = 0, to = 0.01)
# # push this below the nyquist frequency
# ## Why is this looking like double the frequency??
# wav2 <- sine(freq = 440, duration = 500, xunit = "samples", samp.rate = 22000)
# plot(wav2@left)
# oscillo(wav, f = 22000, from = 0, to = 0.01)

# wav_d2 <- downsample(object = wav,samp.rate = 2000)
# plot(wav_d2@left)
# # now we're starting to see the results of sampling below the nyquist frequency
# oscillo(wav_d2, f = 2200, from = 0, to = 0.01)




# What does this data actually look like?
# read in my own data to work with
ex_ak <- readWave("./examples/ak_dat/XC16526-BBCUCadenceCoo.wav", from = 0, to = 2, units = "seconds")
ex_ak <- readMP3("./examples/ak_dat/XC547273-BBCU.mp3")
ex_ak@left
par(mfrow = c(1,1))
oscillo(ex_ak, 
        f = 8000, 
        from = , 
        to = 0.05)

# This is one weird looking wave, is there something going on with the nyquist frequency here?
# No, we are just seeing a combination of lots of different sounds! 
# When we visualize sound, we rarely use an oscillogram/waveform. Instead, we'll be looking at a spectrogram, which uses math called a fourier transformation to decompose the relative contributions of each frequency
spectro(ex_ak,
        f = 8000,
        wl = 512, 
        collevels = seq(-40, 0, 0.5), 
        ovlp = 70)


# As we start to look at bird calls, let's take a look at how bird calls are different from mammalian vocalizations
# Humans and other mammals produce sound by passing air through muscle tissue called the vocal cords, which then vibrate to produce sound. Think about how you blow air through a blade of grass - you can stretch the grass to change the pitch. 
# Relics of this method of sound production are: you can only produce one sound at a time and you can only easily produce sound on an exhalation
# Birds, however, produce sound by adjusting the tension on a membrane called the syrinx. This method of sound production is more akin to letting air out of a balloon and stretching the balloon to make different pitches. 
# Air passes over these membranes in the two bronchi of the birds lungs
# The results of this different system of producing sound is that birds can produce two different sounds at the same time (think hermit thrush song) and they can sing on both the inhalation and exhalation (skylark https://www.youtube.com/watch?v=rBp1Q_bHe0Y)
# Birds can also cancel out the harmonics and create a pure tone more easily than humans


# Can we see this?
### Recording in R ####
# with the audio package you can also use record() to directly record into R
# create a vector with empty values
# define the length of the vector as the sampling rate * the amount of time you want to record for 
library(audio)
library(seewave)
library(tuneR)
s11 <- rep(NA_real_, 16000 * 2)
# record for two seconds
record(where = s11, rate = 16000, channels = 1)
save.wave(s11, "./examples/ak_dat/rec_test.wav")
# can be controlled with pause(), rewind(), and resume()
s11 <- readWave("./examples/ak_dat/rec_test.wav")
spectro(s11, wl = 512, # window size - length of amplitude vector used for each fourier transformation
        ovlp = 90, # overlap between windows
        collevels = seq(-40, 0, 0.5), 
        flim = c(2,6), # adjust the frequency represented
        scale = FALSE)



#### Raven Intro For Emily #####
#https://www.ravensoundsoftware.com/video-tutorials/english/01-introduction-to-the-raven-pro-interface/


# Will want to annotate things like this:
# carfully find the minimum frquency - we will add the max on late
# mark which "song" the calls belong to - how to separate these out? Should we separate these out?
