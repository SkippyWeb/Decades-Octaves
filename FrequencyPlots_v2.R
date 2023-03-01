# Skippy Studio
# www.skippyweb.eu
# 
# 22 May 2021
# v2: UPDATE 5 JUNE 2021
#   - save to png
#   - added general function plotFigures()
#
# Decades, Octaves, third-octaves
#
# In this script I show one approach towards plotting different
# frequency scales.
#   -Plots on log and linear scale
#   -For Decade plot, I also plot intermediate frequencies
#
# All frequency tables are written to an text output file
# All frequency tables are written to an PNG file
# All plots are written to a hi-res tiff and png file
#

library(ggplot2)
library(scales)
library(gt)      # making tables
library(webshot) # saving tables as png


options(scipen=999) # No scientific notation
plotToFile = TRUE   # If true then do not plot on screen

#Parameters for saving tiff files
#
#This will make a file that is 2400 x 2400 pixels, with an 300
#resolution gives you 2400/300 = 8 inches.
#
pwidth=1200
pheight=1200
punits="px"
psize=12
pres=150 #dpi

#################
# DECADE
#################

# Define freqencies: Decades
#
# Every decade gives an increase in frequency by 10 (ten fold)
# If we start at f=1 we will get 10, 100, 100, etc

# Audio spectrum from 20Hz to 20kHz can be divided in about 4 decade bands
# Define center of 7th Octave band as f4 = 1000Hz

# Lower center octave bands: divide frequency by 10
# Upper center octave bands: multiply frequency by 10

# For each center frequency, the half-decade low/high frequency of
# each decade band are 
#f_low  = fc/sqrt(10)
#f_high = fc*sqrt(10)

# This results in a constant percent fractional bandwidth
# BW = 100*(f_high-f_low)/fc

# We can generate the complete decade band table


#
# General function to plot figures to png and tiff file, and/or to screen
#
plotFigures <- function(p,fn,plotToFile) {
  if(plotToFile) {
    #save tiff file
    tiff(paste(fn,"tiff",sep="."), res=pres,width = pwidth, height = pheight, units = punits, pointsize = psize)
    plot(p)
    dev.off()
    
    #save png file
    png(paste(fn,"png",sep="."), width = 480, height = 480, units = "px", pointsize = 12,res = NA)
    plot(p)
    dev.off()
    
    #Plot graph to screen
    plot(p)
  } else {plot(p)} #plot graph to screen
}

df     <- data.frame(matrix(vector(), 0, 4), stringsAsFactors=F)
CNames <- c('f_low','f_center','f_high','BW')

fc   <- 1000
MultiplicationFactor <- 10
s    <- seq(from=-3,to=1,by=1)

for (i in s) {
  f    <- fc * MultiplicationFactor^i
  fl   <- f / sqrt(MultiplicationFactor)
  fh   <- f * sqrt(MultiplicationFactor)
  BW   <- 100*(fh-fl)/f
  r    <- round(c(fl,f,fh,BW),1)
  df   <- rbind(df, setNames(r,names(df)))
}
colnames(df) <- CNames
df

write.table(df,file="Decade.txt",sep="\t")

gt_tbl <- gt(data=df)
gt_tbl <- gt_tbl %>%  
  tab_header(title = md("**Decades**")) %>%
  tab_source_note(source_note = "low/high frequency; center frequency; bandwidth")
gt_tbl
gtsave(gt_tbl,file="Decades-Table.png")


freq = c()
for (i in 1:dim(df)[1]) {
  FreqStart    = df$f_center[i]
  FreqInterval = df$f_center[i]
  s = seq(from=FreqStart,to=MultiplicationFactor*FreqStart,by=FreqInterval)
  freq=c(freq,s)
}
freq = unique(freq) #remove the frequencies that occur twice
df2  = data.frame(freq=as.integer(freq))


#
# Decade plot of frequencies
#
p<-ggplot(df, aes(x = f_center,l=f_low)) +
  #geom_point() +
  scale_y_continuous(labels=comma_format(accuracy=1))+
  geom_vline(xintercept=df$f_center, linetype="solid", # center frequencies
             color = "black", size=0.5)+
  geom_vline(xintercept=df$f_low, linetype="dashed",   # start of band
             color = "blue", size=0.5)+
  labs(title="Decade plot", x ="Frequency", y = "a.u.") +
  theme(plot.title = element_text(hjust = 0.5)) + #left alugb 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(trans='log10',labels = df$f_center, breaks = df$f_center) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 


plotFigures(p=p, fn='Decade',plotToFile=plotToFile)
#plotFigures(p=p, fn='Decade',plotToFile=FALSE)



#
# Decade plot of frequencies on linear scale
#
p<-ggplot(df, aes(x = f_center,l=f_low)) +
  #geom_point() +
  #scale_x_continuous(labels=comma_format(accuracy=1), labels = df$f_center, breaks = df$f_center)+
  scale_y_continuous(labels=comma_format(accuracy=1)) +
  geom_vline(xintercept=df$f_center, linetype="solid", # center frequencies
             color = "black", size=0.5)+
  geom_vline(xintercept=df$f_low, linetype="dashed",   # start of band
             color = "blue", size=0.5) +
  labs(title="Decade plot on linear scale", x ="Frequency", y = "a.u.") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(labels = df$f_center, breaks = df$f_center) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

plotFigures(p=p, fn='Decade-Linear',plotToFile=plotToFile)

# Decade plot of frequencies with intermediate frequencies
p<-ggplot(df2, aes(x = freq)) +
  #geom_point() +
  #scale_x_continuous(trans='log10', # Log10 transformation
  #                   labels=comma_format(accuracy=1))+
  geom_vline(xintercept=df2$freq, linetype="solid", 
             color = "black", size=0.5)+
  labs(title="Decade plot", x ="Frequency", y = "a.u.") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(trans='log10',labels = df2$freq, breaks = df2$freq) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

plotFigures(p=p, fn='Decade-Intermediate',plotToFile=plotToFile)



#################
# OCTAVES
#################

# Define freqencies: Octaves
#
# Every octave gives an increase in frequency by 2 (doubling)
# If we start at f=1 we will get 1, 2, 4, 16, 32, 64 etc

# Audio spectrum from 20Hz to 20kHz can be divided in about 11 octave bands
# Define center of 7th Octave band as f7 = 1000Hz

# Lower center octave bands: divide frequency by 2
# Upper center octave bands: multiply frequency by 2

# For each center frequency, the half-octave low/high frequency of
# each octave band are 
# f_low  = fc/sqrt(2)
# f_high = fc*sqrt(2)

# This results in a constant percent fractional bandwidth
# BW = 100*(f_high-f_low)/fc

# We can generate the complete octave band table


df   <- data.frame(matrix(vector(), 0, 4), stringsAsFactors=F)
CNames <- c('f_low','f_center','f_high','BW')

fc   <- 1000
MultiplicationFactor <- 2
s    <- seq(from=-6,to=5,by=1)

for (i in s) {
  f    <- fc * MultiplicationFactor^i
  fl   <- f / sqrt(MultiplicationFactor)
  fh   <- f * sqrt(MultiplicationFactor)
  BW   <- 100*(fh-fl)/f
  r    <- round(c(fl,f,fh,BW),1)
  df <- rbind(df, setNames(r,names(df)))
}
colnames(df) <- CNames
df

write.table(df,file="Octave.txt",sep="\t")

gt_tbl <- gt(data=df)
gt_tbl <- gt_tbl %>%  
  tab_header(title = md("**Octaves**")) %>%
  tab_source_note(source_note = "low/high frequency; center frequency; bandwidth")
gt_tbl
gtsave(gt_tbl,file="Octaves_Table.png")


#
# Octave plot of frequencies
#
p<-ggplot(df, aes(x = f_center,l=f_low)) +
  #geom_point() +
  scale_y_continuous(labels=comma_format(accuracy=1))+
  geom_vline(xintercept=df$f_center, linetype="solid", # center frequencies
             color = "black", size=0.5)+
  geom_vline(xintercept=df$f_low, linetype="dashed",   # start of band
             color = "blue", size=0.5)+
  labs(title="Octave plot", x ="Frequency", y = "a.u.") +
  theme(plot.title = element_text(hjust = 0.5)) + #left alugb 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(trans='log2',labels = df$f_center, breaks = df$f_center) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

plotFigures(p=p, fn='Octave',plotToFile=plotToFile)



#
# Octave plot of frequencies on linear scale
#
p<-ggplot(df, aes(x = f_center,l=f_low)) +
  #geom_point() +
  #scale_x_continuous(labels=comma_format(accuracy=1), labels = df$f_center, breaks = df$f_center)+
  scale_y_continuous(labels=comma_format(accuracy=1)) +
  geom_vline(xintercept=df$f_center, linetype="solid", # center frequencies
             color = "black", size=0.5)+
  geom_vline(xintercept=df$f_low, linetype="dashed",   # start of band
             color = "blue", size=0.5) +
  labs(title="Octave plot on linear scale", x ="Frequency", y = "a.u.") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(labels = df$f_center, breaks = df$f_center) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

plotFigures(p=p, fn='Octave-Linear',plotToFile=plotToFile)



######################
# One-thrid OCTAVES
######################

# Define freqencies: One-thrid Octaves
#
# Every octave gives an increase in frequency by 2 (doubling)
# If we start at f=1 we will get 1, 2, 4, 16, 32, 64 etc

# Audio spectrum from 20Hz to 20kHz can be divided in about 31 one-third octave bands
# Define center of 19th one-third octave band as f19 = 1000Hz

# Lower center octave bands: divide frequency by 2^(1/3)
# Upper center octave bands: multiply frequency by 2^(1/3)

# For each center frequency, the 1/6-octave low/high frequency of
# each octave band are 
# f_low  = fc/2^(1/6)
# f_high = fc*2^(1/6)

# This results in a constant percent fractional bandwidth
# BW = 100*(f_high-f_low)/fc

# We can generate the complete octave band table


df   <- data.frame(matrix(vector(), 0, 4), stringsAsFactors=F)
CNames <- c('f_low','f_center','f_high','BW')

fc   <- 1000
MultiplicationFactor <- 2^(1/3)
s    <- seq(from=-18,to=14,by=1)

for (i in s) {
  f    <- fc * MultiplicationFactor^i
  fl   <- f / sqrt(MultiplicationFactor)
  fh   <- f * sqrt(MultiplicationFactor)
  BW   <- 100*(fh-fl)/f
  r    <- round(c(fl,f,fh,BW),1)
  df <- rbind(df, setNames(r,names(df)))
}
colnames(df) <- CNames
df

write.table(df,file="OneThirdOctave.txt",sep="\t")

gt_tbl <- gt(data=df)
gt_tbl <- gt_tbl %>%  
  tab_header(title = md("**Third-Octaves**")) %>%
  tab_source_note(source_note = "low/high frequency; center frequency; bandwidth")
gt_tbl
gtsave(gt_tbl,file="OneThirdOctaves-Table.png")


#
# One-third Octave plot of frequencies
#
p<-ggplot(df, aes(x = f_center,l=f_low)) +
  #geom_point() +
  scale_y_continuous(labels=comma_format(accuracy=1))+
  geom_vline(xintercept=df$f_center, linetype="solid", # center frequencies
             color = "black", size=0.5)+
  geom_vline(xintercept=df$f_low, linetype="dashed",   # start of band
             color = "blue", size=0.5)+
  labs(title="One-thrid Octave plot", x ="Frequency", y = "a.u.") +
  theme(plot.title = element_text(hjust = 0.5)) + #left alugb 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(trans='log2',labels = df$f_center, breaks = df$f_center) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

plotFigures(p=p, fn='One-third-octave',plotToFile=plotToFile)


#
# One-third Octave plot of frequencies on linear scale
#
p<-ggplot(df, aes(x = f_center,l=f_low)) +
  #geom_point() +
  #scale_x_continuous(labels=comma_format(accuracy=1), labels = df$f_center, breaks = df$f_center)+
  scale_y_continuous(labels=comma_format(accuracy=1)) +
  geom_vline(xintercept=df$f_center, linetype="solid", # center frequencies
             color = "black", size=0.5)+
  geom_vline(xintercept=df$f_low, linetype="dashed",   # start of band
             color = "blue", size=0.5) +
  labs(title="One-thrid Octave plot on linear scale", x ="Frequency", y = "a.u.") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(labels = df$f_center, breaks = df$f_center) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

plotFigures(p=p, fn='One-third-octave-linear',plotToFile=plotToFile)


          