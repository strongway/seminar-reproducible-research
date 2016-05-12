# ---- load necessary packages ----
source('loadPackages.R')

fn <- file.path('examples', 'rt-data','aleg.txt')

# ---- import text file ----
subnames <- dir(file.path('examples','rt-data'))
importRTData <- function(sname) {
  varnames <-  c('target','block','dimension',
                 't0','color','orientation',
                 'position','key','rt')
  fn <- file.path('examples', 'rt-data',sname)
  dat <- fread(fn,col.names = varnames)
  dat$sub <- gsub('.txt','',sname)
  dat$outlier <- (dat$rt > mean(dat$rt) + 3* sd(dat$rt)) |
    (dat$rt < mean(dat$rt)-3*sd(dat$rt))
  dat
}

rt_data <- do.call(rbind, lapply(subnames,importRTData))

fn <- file.path('examples','rat1.txt')


initTime <- proc.time()
dat <- read.csv(fn, header=FALSE)
proc.time()-initTime

initTime <- proc.time()
dat <- fread(fn, drop = 2)
proc.time()-initTime

dat_url <- c('http://www.census.gov/popest/data/national/totals/2015/files/NA-EST2015-01.csv')
dat <- fread(dat_url, skip = 2, nrows = 88)

# ---- import matlab file ----

path <- file.path('examples', 'ternus-data')
subfiles <- dir(path)

readMatFile <- function(subname){
  fullName <- file.path('examples', 'ternus-data',subname)
  sub <- readMat(fullName)
  dat <- data.table(sub$Trial)
  names(dat) <- c('trlno','direction','soa','resp','rt')
  dat$sub <- gsub('.mat','',subname)
  dat
}

dat <- do.call(rbind,lapply(subfiles, readMatFile))


## ---- data manipulation -----

rt_data %>% filter(error == FALSE & outlier == FALSE) %>%
  group_by(sub, target, block) %>% 
  summarise(mRT = mean(rt)) %>% 
  unite(., condition, target, block) %>% 
  spread(., condition, mRT) ->mSub


