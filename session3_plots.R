# ---- loadData ----
source('loadPackages.R')
source('import_sampleData.R')

## ggplot basic grammars
# data, coordinate system, geom_<elements>

# ---- hist ---- 
ggplot(rt_data, aes(x=rt)) + geom_histogram()

# ---- rt_bars ----
# combination with dplyr 
rt_data %>% filter(outlier == FALSE) %>% 
  group_by(block, target) %>% 
  summarize(mRT = mean(rt), seRT = sd(rt)/sqrt(nlevels(sub)-1)) -> mRTData

ggplot(mRTData, aes(x=block, y = mRT, fill = target, color = target)) + 
  geom_bar(stat='identity', position='dodge', width=0.5) + 
  geom_errorbar(aes(ymin = mRT -seRT , ymax = mRT + seRT), position = 'dodge', width = 0.5) + 
  coord_cartesian(ylim=c(0.3,0.6)) + 
  xlab('Block Target Absent:Present Ratio') + 
  ylab('Mean RTs +/- SE (ms) ') +
  theme_bw()

# ---- rt_errorbars ----
ggplot(mRTData, aes(x=as.numeric(block), y = mRT, shape = target, color = target)) + 
  geom_line() + geom_point(size = 3) +   
  geom_errorbar(aes(ymin = mRT -seRT , ymax = mRT + seRT),  width = 0.2) +
  scale_shape(solid = FALSE) + 
  coord_cartesian(ylim=c(0.3,0.6)) + 
  xlab('Block Target Absent:Present Ratio') + 
  ylab('Mean RTs +/- SE (ms) ') 

# ---- anova_rt ----
anova_rt_table <- rt_data %>% filter(outlier == FALSE & error == FALSE) %>%
  group_by(sub, block, target, dimension) %>% 
  summarise(mRT = mean(rt)) %>%
  ezANOVA(., dv = mRT, wid = sub, within = .(block, target, dimension))
#library('papaja')
#apa_table(anova_rt_table$ANOVA,
#          caption = 'ANOVA table for RT analysis')

xtable(anova_rt_table$ANOVA)

# ---- additional RT analysis ----
# reuse plots

p <- ggplot(mRTData)
p1 <- p + aes(x = block, y=mRT) + geom_bar(stat='identity')
p2 <- p1 + geom_errorbar(aes(ymin=mRT -seRT, ymax = mRT + seRT)) + facet_wrap(~target)
p2

p2 %+% (rt_data %>% filter(error) %>% group_by(target, block) %>% summarize(mRT = mean(rt), seRT = sd(rt)))

# geom_smooth curve estimation

# ---- ternus_curve ----
ternus_data %>% group_by(soa) %>% summarise(m = mean(resp)) -> mResp

fig <- mResp %>% ggplot(aes(x=soa, y=m)) + geom_point()

fig + geom_smooth(method = lm)

fig + geom_smooth(method=glm, method.args= list(family = binomial(probit)), se = FALSE) 


