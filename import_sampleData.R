# import Ternus apparent motion data
importTData <- function(fn) {
  path <- file.path('.','examples','ternus-data') # locate the folder
  raw <- readMat(file.path(path,fn))
  dat <- data.table(raw$Trial)
  names(dat) <- c('trlno','direction','soa','resp','rt')
  dat$resp <- (dat$resp -1)/2 # convert to 0, 1
  dat$soa <- dat$soa*30+20 # convert to ms. 
  dat$sub <- gsub('.mat','',fn) # add subject name
  dat
}

subfiles <- dir(file.path('examples', 'ternus-data'))
ternus_data <- do.call(rbind, lapply(subfiles, importTData))

# import RT data
# read data from text file
# 40 trials per block, 30 blocks, target present/absent ratio: 3/1,  1/3, 1/1
# cols: target, block, dimension, target(same as in col1), color, orient, position, key, rt
subjects <- dir(file.path('.','examples','rt-data'))

readExpData <- function(filename){
  fullName <- file.path('.','examples','rt-data',filename)
  varnames <-  c('target','block','dimension','t0','color','orientation','position','key','rt')
  raw <- fread(fullName, col.names = varnames )
  # some preliminary process
  raw$blkNo <- rep(1:(nrow(raw)/40),each=40) # add block number
  raw$sub <- gsub('.txt','',filename) # add subject name
  # exclude rt outside 3*sigma, and first trial of each block
  raw$outlier <- abs(raw$rt-mean(raw$rt))> 3*sd(raw$rt) | raw$blkNo%%40==1 # outside of 3sigma and first trial
  # error trials: target 0, 1, key response: 1, 3 --> target*2+1
  raw$error <- raw$t0*2+1 != raw$key
  raw
}

rt_data <- do.call(rbind,lapply(subjects,readExpData))

# further preliminary data processing
rt_data <- within(rt_data,{
  target <- factor(target,labels = c('Absent','Present'))
  dimension <- factor(dimension,labels=c('Color','Orientation'))
  color <- factor(color,labels=c('red','turg'))
  orientation <- factor(orientation,labels=c('left','right'))
  block <- factor(block, labels = c('3:1','1:3','1:1'))
  sub <- factor(sub)
})