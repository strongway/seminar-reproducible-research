 # ---- initialization ----
source('./data/loadPackages.R')

# ---- preprocess ----
readData <- function(fn) {
  raw <- read.csv(paste0('./data/',fn,'.csv'))
  raw$mIntv <- factor(raw$mIntv, labels=c('-70', '0','70'))
  raw$sub <- factor(raw$sub)
  raw
}
# experiments: regular, irregular, ari_vs_geo_mean, variance, last_interval manipulations. 
exp_files <- c('exp1','exp2','exp3')
exp_names <- c('Regular','Irregular','Geometry')
raw_data <- lapply(exp_files,readData) #read all data
names(raw_data) <- exp_names
# change levels for Exp3 and Exp5. 
levels(raw_data$Geometry$mIntv) <-  c('AriM','GeoM','Equal')

# estimate PSEs and SIGs.
psyfun <- function(data) { # define a general psych curve estimation
  data %>% 
  group_by(sub, mIntv, soa) %>%
  summarise(m = sum(resp), c = length(resp)) %>%
  do(tidy (glm(cbind(m,c-m) ~ soa, family = binomial(probit), data=.))) %>%
  select(., one_of(c('sub','mIntv', 'term','estimate'))) %>%
  spread(.,term, estimate) %>% rename(., b=soa, a = `(Intercept)`) %>%
  mutate(., pse = -a/b, sig = 1/b)
}
# Psychometric curves. 
psycurve <- function(data){ # plot psychometric functions 
  data %>% group_by(mIntv,soa) %>% summarise(m = mean(resp)) %>%
  ggplot(., aes(x=soa, y=m, color=mIntv)) + geom_point(aes()) + 
  geom_point(aes(shape = mIntv), size=3) + 
  geom_smooth(method=glm, method.args= list(family = binomial(probit)), se = FALSE, aes(linetype=mIntv)) +
  xlab('SOA (ms)') + ylab('Prop. of group motion') + 
#  theme(legend.title=element_blank())
  legend_pos(c(0.2,0.8))
}

psebar <- function(data){
  data %>% group_by(mIntv) %>% summarise(m = mean(pse), se = sd(pse)/sqrt(nlevels(sub)-1)) %>%
    ggplot(.,aes(x=mIntv, y=m, color=mIntv, fill=mIntv)) + 
    geom_bar(stat='identity', width=0.7) + 
    geom_errorbar(aes(ymin=m-se, ymax=m+se, color=mIntv), stat='identity', width=0.3) + 
    coord_cartesian(ylim=c(100,175)) + xlab('Relative mean auditory interval (ms)') + ylab('PSEs (ms)') + 
    theme(legend.position="none")
}

# calculate PSEs, Curves, bars
pses <-  lapply(raw_data,psyfun)
figs.psy <- lapply(raw_data, psycurve)
figs.pse <- lapply(pses,psebar)


# anovas
pses.aov <- lapply(pses, function(x) ezANOVA(data=data.frame(x), dv = pse, wid = sub, within=mIntv))

pses$Regular$exp = 'Regular'
pses$Irregular$exp = 'Irregular'

# ---- reg_irreg ----
plot_grid(figs.psy$Regular,figs.psy$Irregular, figs.pse$Regular, figs.pse$Irregular, 
          nrow=2,ncol=2, labels=c('A','B','C','D'))

# ---- anova_tables ----
xtable(pses.aov$Regular$ANOVA, caption = 'ANOVA for Regular sequence', digits = 3)
xtable(pses.aov$Irregular$ANOVA, caption = 'ANOVA for Irregular sequence', digits = 3)
