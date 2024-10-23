##### Day 5: Automatic Detection #####

# https://marce10.github.io/PR_BIR_2024/ohun.html
library(ohun)
library(tuneR)
library(seewave)
library(warbleR)

# Uses: 
## energy based detection
## template based detection

# Automatic sound even tdetection
## First: do you have stereotyped signals?
## If yes, use template based detection (template_detector())

## If you don't have stereotyped signals, and a high SNR, use energy-based detectiion (energy_detector())

## If you don't have stereotyped signals and a low signal to noise ratio, use machine learning approaches

## Metrics that make use of true negatives aren't easily applied in the context of sound event detections ince noise can't always be partitioned into discrete units

# load example data
data("lbh1", "lbh2", "lbh_reference")

lbh_reference

# save sound file
writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))

# save sound file
writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))

# print spectrogram
label_spectro(
  wave = lbh1,
  reference = lbh_reference[lbh_reference$sound.files == "lbh1.wav",],
  hop.size = 10,
  ovlp = 50,
  flim = c(1, 10)
)

# print spectrogram
label_spectro(
  wave = lbh2,
  reference = lbh_reference[lbh_reference$sound.files == "lbh2.wav",],
  hop.size = 10,
  ovlp = 50,
  flim = c(1, 10)
)

# use diagnose_detection to evaluate performance of a detector by comparing it to a reference table
# ex: perfect reference is comparing lbh_reference to itself
lbh1_reference <-
  lbh_reference[lbh_reference$sound.files == "lbh1.wav",]

# diagnose
diagnose_detection(reference = lbh1_reference, 
                   detection = lbh1_reference)[, c(1:3, 7:9)]
# overlap tells you the temporal overlap of the detections vs the actual data



# create new table that only contains some of the annotations
lbh1_detection <- lbh1_reference[3:9, ]

# print spectrogram
label_spectro(
  wave = lbh1,
  reference = lbh1_reference,
  detection = lbh1_detection,
  hop.size = 10,
  ovlp = 50,
  flim = c(1, 10)
)

# diagnose - we can see that some is missing
diagnose_detection(reference = lbh1_reference, 
                   detection = lbh1_detection)[, c(1:3, 7:9)]
# we see that the recall is lower


# having additional sound events will affect precision but not recall
# print spectrogram
label_spectro(
  wave = lbh1,
  detection = lbh1_reference,
  reference = lbh1_detection,
  hop.size = 10,
  ovlp = 50,
  flim = c(1, 10)
)
# diagnose
diagnose_detection(reference = lbh1_detection, 
                   detection = lbh1_reference)[, c(1:3, 7:9)]
# Can also add the argument by.sound.file = TRUE to generate these metrics for each sound file so that one or two files with specific diagnostics don't overly bias the output

# You can use this with birdnet - birdnet doesn't have a method to diagnose these 
# birdnetR is a package to interface birdnet with R


## Detecting sound events with ohun
# Uses an energy based detection
# plot spectrogram and envelope
label_spectro(
  wave = cutw(
    lbh1,
    from = 0,
    to = 1.5,
    output = "Wave"
  ),
  ovlp = 90,
  hop.size = 10,
  flim = c(0, 10),
  envelope = TRUE
)


# simulate a recording with 10 sounds with two different frequency ranges and durations

# install this package first if not installed
#install.packages("Sim.DiffProc")
library(Sim.DiffProc)
#Creating vector for duration 
durs <- rep(c(0.3, 1), 5)

#Creating simulated song
set.seed(12)
simulated_1 <-
  warbleR::simulate_songs(
    n = 10,
    durs = durs,
    freqs = 5,
    sig2 = 0.01,
    gaps = 0.5,
    harms = 1,
    bgn = 0.1,
    path = tempdir(),
    file.name = "simulated_1",
    selec.table = TRUE,
    shape = "cos",
    fin = 0.3,
    fout = 0.35,
    samp.rate = 18
  )$wave

# plot spectrogram and envelope
label_spectro(wave = simulated_1,
              env = TRUE,
              fastdisp = TRUE)

# run detection
detection <-
  energy_detector(
    files = "simulated_1.wav",
    bp = c(2, 8),
    threshold = 50,
    smooth = 150,
    path = tempdir()
  )

# This output is a selection table
# plot spectrogram and envelope
label_spectro(
  wave = simulated_1,
  envelope = TRUE,
  detection = detection,
  threshold = 50
)
# This way we're running it is on the amplitude threshold

# You can also change arguments such as minimum duration to screen out short events
# run detection
detection <-
  energy_detector(
    files = "simulated_1.wav",
    bp = c(1, 8),
    threshold = 50,
    min.duration = 500,
    smooth = 150,
    path = tempdir()
  )

# plot spectrogram
label_spectro(wave = simulated_1, detection = detection)

# Max duration allows you to screen out long events in a similar way 
# run detection
detection <-
  energy_detector(
    files = "simulated_1.wav",
    bp = c(1, 8),
    threshold = 50,
    smooth = 150,
    max.duration = 500,
    path = tempdir()
  )

# plot spectrogram
label_spectro(wave = simulated_1,  detection = detection)


# Can also use the bandpass filter
# By setting bp = c(5, 8) only those sound events found within that frequency range (5-8 kHz) will be detected, which excludes sound events below 5 kHz 
# Detecting
detection <-
  energy_detector(
    files = "simulated_1.wav",
    bp = c(5, 8),
    threshold = 50,
    smooth = 150,
    path = tempdir()
  )

# plot spectrogram
label_spectro(wave = simulated_1,  detection = detection)

# The idea of an energy detector is to pick up things above a certain amplitude - further refined with min freq, max freq, bp


# Optimizing energy-based detection 
# use function optimize_energy_detection()
optim_detection <-
  optimize_energy_detector(
    reference = sim2_sel_table,
    files = "simulated_2.wav",
    threshold = 50, # amplitude threshold
    min.duration = 1, # duration threshold
    path = tempdir(),
    smooth = c(100, 250, 350) # specify the range of smoothing values to try 
  )
# output will tell you performance metrics at each combination of parameters
# hold time: if there are two events and the distance is smaller than the hold time, they will be combined into one 


# Additional dianostic metrics
# Split positives
# merged positives
# proportional overlap of true positives





#### Template based detection ####
# better suited for highly stereotyped sound events
# doesn't depend on signal-to-noise ratio so more robust than higher levels of background noise

# Three steps: choosing the right template (get_templates())
# Estimate the cross-correlation scores of templates along sound files (templated_correlator())
# Detecting sound events by applying a correlation threshold (template_detector())


# get mean structure template based on your own data
template <-
  get_templates(reference = lbh1_reference, path = tempdir())
# Will find the annotation that is the closest to the center of the acoustic space (most representative of the calls)
# can also specify more than one 

# get 3 templates
get_templates(reference = lbh_reference, 
              n.sub.spaces = 3, path = tempdir())

# get correlations
correlations <-
  template_correlator(templates = template,
                      files = "lbh1.wav",
                      path = tempdir())

# run detection
detection <-
  template_detector(template.correlations = correlations, threshold = 0.4)
# same amplitude threshold used here - this is the only argument to optimimze in this detector
detection



# plot spectrogram
label_spectro(
  wave = lbh1,
  detection = detection,
  template.correlation = correlations[[1]],
  flim = c(0, 10),
  threshold = 0.4,
  hop.size = 10, ovlp = 50)

#diagnose performance
diagnose_detection(reference = lbh1_reference, detection = detection)

# run optimization
optimization <-
  optimize_template_detector(
    template.correlations = correlations,
    reference = lbh1_reference,
    threshold = seq(0.1, 0.5, 0.1)
  )
#Additional threshold values can be evaluated without having to run it all over again. We just need to supplied the output from the previous run with the argument previous.output (the same trick can be done when optimizing an energy-based detection):
# run optimization
optimize_template_detector(
  template.correlations = correlations,
  reference = lbh1_reference,
  threshold = c(0.6, 0.7),
  previous.output = optimization
)