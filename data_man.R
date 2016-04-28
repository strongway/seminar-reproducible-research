source('loadPackages.R')

# ---- 1. import text file ----
filename <- paste0(file.path('examples','rt-data','aleg'),'.txt')

varnames <-  c('target','block','dimension','t0','color','orientation','position','key','rt')
subdata <- read.csv(filename) # error: first line as header
subdata <- read.csv(filename, header = FALSE, col.name = varnames)
subdata <- read.table(filename, col.names = varnames, sep = ',' )

# fread from data.table
subdata <- fread(filename, col.name = varnames ) # recognize header or not
subdata <- fread(filename, drop = 4) # drop one column
subdata <- fread(filename, select = c(1,8,9)) # select some columns

# read rat data
filename = file.path('examples','rat1.txt')

initTime <- proc.time()
subdata <- fread(filename)
proc.time() - initTime

# read.csv, way slower
initTime <- proc.time()
subdata <- read.csv(filename)
proc.time() - initTime

subdata <- fread(filename, drop = 2)

# read internet data
dat_url <- c('http://www.census.gov/popest/data/national/totals/2015/files/NA-EST2015-01.csv')
odata <- fread(dat_url)
# skip rows
odata <- fread(dat_url, skip = 2, nrows = 88)

# import excel
# requires readxl package, 
# main function: read.xls()

# import matlab file
# library(R.matlab)
filename <- paste0(file.path('examples','ternus-data','wenhao'),'.mat')
sub <- readMat(filename)
subdata <- data.table(sub$Trial)
names(subdata) <- c('trlno','direction','soa','resp','rt')


# ---- writing a function ----

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
t_data <- do.call(rbind, lapply(subfiles, importTData))

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

## ---- data manipulation with dplyr ----
# show error rates
rt_data %>% group_by(sub) %>% summarise(mError = mean(error), mOutlier = mean(outlier)) 

# check individual blocks
rt_data %>% group_by(sub, block) %>% summarise(mError = mean(error)) %>% arrange(sub, block) %>%
  spread(., block, mError)

# calculate individual mean RTs
sub.RTs <- rt_data %>% filter(!outlier & !error) %>% group_by(sub,block, dimension, target) %>%
  summarise(mRT = mean(rt), sRT = sd(rt))

# some time you may want to reshape the data into SPSS like format
sub.RTs %>% select(-sRT) %>% unite(., condition, block, dimension, target) %>% spread(., condition, mRT)

# mean RT plots
mRTs <- sub.RTs %>% group_by(block,dimension, target) %>% 
  summarise(mmRT = mean(mRT), se = sd(mRT)/sqrt(nlevels(sub.RTs$sub)-1))

fig1 <- ggplot(mRTs, aes(x=block,y=mmRT,fill=target)) + 
  geom_bar(stat='identity',position='dodge', width=0.8) + facet_grid(.~dimension) + 
  coord_cartesian(ylim=c(0.3,0.6)) +
  geom_errorbar(aes(ymax=mmRT+se, ymin=mmRT-se, color=target), position='dodge', width=0.8)

## ---- psychometric function estimation -----
# estimate PSEs and sigma from psychometric functions
t_data %>% group_by(sub,soa) %>%  summarise(m = sum(resp), c = length(resp)) %>% # average mean responses
  do(tidy (glm(cbind(m,c-m) ~ soa, family = binomial(probit), data=.))) %>% # curve estimation
  select(., one_of(c( 'term','estimate'))) %>% # preseve only the estimated parameters
  spread(.,term, estimate) %>% rename(., b=soa, a = `(Intercept)`) %>% # change to a and b
  mutate(., pse = -a/b, sig = 1/b) # estiamte pse and sigma 


t_data %>% group_by(sub,soa)  %>% summarise(m = mean(resp)) %>%
    ggplot(., aes(x=soa, y=m)) + geom_point() + # plot points
    geom_smooth(method=glm, method.args= list(family = binomial(probit)), se = FALSE) +
    xlab('SOA (ms)') + ylab('Prop. of group motion')+ legend_pos(c(0.2,0.8)) + facet_wrap(~sub)

