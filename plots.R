source('loadPackages.R')
source('import_sampleData.R')

mRTData <- rt_data %>% group_by(sub, block, target) %>%
  summarise(mRT = mean(rt)) %>% # mean of subject-wise
  group_by(block, target) %>% 
  summarise(gmRT = mean(mRT), 
            seRT = sd(mRT)/sqrt(nlevels(sub)-1)) 

fig <- ggplot(mRTData, aes(x = block, y= gmRT, fill = target, color = target)) + 
  geom_bar(stat='identity', position = 'dodge', width=0.75)+ 
geom_errorbar(aes(ymin=gmRT-seRT, ymax = gmRT+seRT),
                position = 'dodge', width = 0.75) + 
  xlab('Block Target/Absent Ratio') 

# experiment 1
fig +  coord_cartesian(ylim=c(0.3, 0.6)) +
  ylab('Mean RTs (+/-SE) secs')

# experiment 2
mRTData2 <- mRTData %>% mutate(gmRT =gmRT*1000, seRT = seRT*1000)

fig2 <- fig %+% mRTData2 

ggplot(mRTData,
    aes(x = block, 
        y= gmRT, group = target, color = target, shape = target)) + 
  geom_point(size = 3) + geom_line() +
  scale_shape(solid = FALSE) + 
  geom_errorbar(aes(ymin=gmRT-seRT, ymax = gmRT+seRT),
                 width = 0.25)  +
  coord_cartesian(ylim=c(0.3, 0.6)) +
  ylab('Mean RTs (+/-SE) secs') + 
  xlab('Target/Absent Ratio')

fig <- ternus_data %>% group_by(sub, soa) %>% 
  summarise(m = mean(resp)) %>% 
  ggplot(., aes(x = soa, y=m)) + geom_point() +
  geom_smooth(method = 'glm', 
      method.args= list(family = binomial(logit)),
      se = FALSE)
fig + facet_wrap(~sub)

fig + geom_smooth(method = 'lm') +
  xlab('SOA (ms)') + ylab('Mean Prop. Responses')



  



