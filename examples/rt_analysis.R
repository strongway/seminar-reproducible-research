source('loadPackages.R')

# read data from text file
# 40 trials per block, 30 blocks, target present/absent ratio: 3/1,  1/3, 1/1
# cols: target, block, dimension, target(same as in col1), color, orient, position, key, rt
subjects <- dir(file.path('.','examples','rt-data'))

readExpData <- function(filename){
  fullName <- file.path('.','examples','rt-data',filename)
  raw <- read.csv(fullName,header=FALSE)
  names(raw) <- c('target','block','dimension','t0','color','orientation','position','key','rt')
  raw$blkNo <- rep(1:(nrow(raw)/40),each=40)
  raw$sub <- gsub('.txt','',filename)
  # exclude rt outside 3*sigma, and first trial of each block
  raw$outlier <- abs(raw$rt-mean(raw$rt))> 3*sd(raw$rt) | raw$blkNo%%40==1
  # error trials: target 0, 1, key response: 1, 3 --> target*2+1
  raw$error <- raw$t0*2+1 != raw$key
  raw
}

df <- do.call(rbind,lapply(subjects,readExpData))

# preliminary data process: set names, factorize, etc
df <- within(df,{
  target <- factor(target,labels = c('Absent','Present'))
  dimension <- factor(dimension,labels=c('Color','Orientation'))
  color <- factor(color,labels=c('red','turg'))
  orientation <- factor(orientation,labels=c('left','right'))
  block <- factor(block, labels = c('3:1','1:3','1:1'))
  sub <- factor(sub)
})

# show error rates
df %>% group_by(sub) %>% summarise(mError = mean(error), mOutlier = mean(outlier)) 

# calculate individual mean RTs
sub.RTs <- df %>% filter(!outlier & !error) %>% group_by(sub,block, dimension, target) %>%
  summarise(mRT = mean(rt), sRT = sd(rt))

# mean RT plots
mRTs <- sub.RTs %>% group_by(block,dimension, target) %>% 
  summarise(mmRT = mean(mRT), se = sd(mRT)/sqrt(nlevels(sub.RTs$sub)-1))

fig1 <- ggplot(mRTs, aes(x=block,y=mmRT,fill=target)) + 
  geom_bar(stat='identity',position='dodge', width=0.8) + facet_grid(.~dimension) + 
  coord_cartesian(ylim=c(0.3,0.6)) +
  geom_errorbar(aes(ymax=mmRT+se, ymin=mmRT-se, color=target), position='dodge', width=0.8)
