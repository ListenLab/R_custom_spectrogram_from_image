# Goal: make a sound whose spectrogram reproduces a specified image 

# Overview: 
# 1) make a random-sine tone complex carrier
#     (the number of carrier components is X)
# 2) divide the image into X rows
#     and extract the lightness/ darkness as a vector
# 3) resample that vector so that it equals the number of points in the sinewaves
# 4) use the vector as a modulator for each sinewave carrier component
# 5) add all the carriers 
# 6) save as a sound
library(tidyr)
library(dplyr)
library(signal)
library(audio)
library(scales)
library(imager)
# https://dahtah.github.io/imager/imager.html

rm(list = ls())

# This is the path for your image
# setwd("C:\\Users\\Matt\\Documents\\R\\Custom_spectrogram_from_image\\Demo")
setwd("C:\\Type\\Your\\File\\Path\\Here")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Load the image
img_m <- load.image("UMN.jpg") %>% grayscale()

# carrier frequencies in the output sound
num_channels <- 96
freqs <- seq(100, 5000, length.out = num_channels)
sound_duration <- 1
time <- seq(0, sound_duration, by = 1/44100)

#===================================================#
# Split the image into horizontal slices
img_rows <- imager::imsplit(img_m,"y",num_channels)

img_rows_df <- as.data.frame(img_rows)

# right after that DF is made, the im row categories are ordered. 
# Lock that order into a variable here...
ordered_ims <- img_rows_df$im %>% unique()

img_rows_df <- img_rows_df %>%
  # and here's where you ensure that that variable
  # preserves its true ordering,
  # rather than alphabetic ordering
  mutate(im = factor(im, levels = ordered_ims)) %>%
  mutate(channel = as.numeric(as.factor(im))) %>%
  mutate(channel = max(channel) - channel + 1)

#===================================================#
# Summarise power within each channel
# (this is the image color "envelope")
img_rows_df_sum <- img_rows_df %>%
  group_by(channel, x) %>%
  # invert the value so that dark is higher
  summarise(value = 1 - mean(value)) 

# Convert to wide-data format
img_rows_df_sum_wide <- tidyr::spread(img_rows_df_sum, key = channel, value = value) %>%
  # omit the x column, since we're about to resample anyway
  mutate(x = NULL)

#===================================================#
# Resample those vectors to match the length of the sound
resample <- function(values){
  values_resampled <- signal::resample(values, p = length(time), q = length(values)) 
  
  # correct for clipping resulting from filtering
  values_resampled <- ifelse(values_resampled > 1, 1, 
                             ifelse(values_resampled < 0, 0, 
                                    values_resampled))
  
  # Interactive: verify match with plot
  # plot(values, type = 'l')
  # plot(values_resampled, type = 'l')
  return(values_resampled)
}
#===================================================#
# resample each column of img_rows_df_sum_wide 
# this can take a little while. 
img_rows_df_sum_wide_resampled <- 
  lapply(1:num_channels, function(channel_index){
    resample(img_rows_df_sum_wide[[channel_index]])}) 
#===================================================#
# Those image-based color envelopes will be the modulators of each carrier frequency
modulators <- img_rows_df_sum_wide_resampled %>% bind_cols()
# column names are V1, V2, V3 for channels 1, 2, 3, etc. 
#===================================================#
# Function to make carrier sinewave with randomized phase 
make_carrier <- function(carrier_index){
  # carrier_index = 1
  carrier <- sin(2*pi*(freqs[carrier_index]*time + runif(1, 0,2*pi)))/sqrt(num_channels)
  return(carrier)
}
#===================================================#
# Generate the carrier tones
carriers <- lapply(1:num_channels, make_carrier) %>% bind_cols()
#===================================================#
# Now multiply the carriers by the modulators 
signals <- lapply(1:num_channels, 
                  function(channel_index){
                    output <- data.frame(modulators[[channel_index]] * carriers[[channel_index]])
                    names(output) <- paste0("C",channel_index)
                    return(output)}) %>%
  # and put them together into  single data frame
  bind_cols()
#===================================================#
# Add up all the components together, and scale so it's a neat waveform
full_signal <- rowSums(signals) %>% scales::rescale(., to = c(-0.99, 0.99))
#===================================================#
# Save that audio file!
audio::save.wave(full_signal, "UMN_wave.wav")
