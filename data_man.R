source('loadPackages.R')

# import text file
filename <- paste0(file.path('examples','rt-data','aleg'),'.txt')

varnames <-  c('target','block','dimension','t0','color','orientation','position','key','rt')
subdata <- read.csv(filename) # error: first line as header
subdata <- read.csv(filename, header = FALSE, col.name = varnames)
subdata <- read.table(filename, col.names = varnames, sep = ',' )

# fread from data.table
subdata <- fread(filename, col.name = varnames ) # recognize header or not
subdata <- fread(filename, drop = 4) # drop one column
subdata <- fread(filename, select = c(1,8,9)) # select some columns

# import excel
# requires readxl package, 
# main function: read.xls()

# import matlab file
# library(R.matlab)
filename <- paste0(file.path('examples','ternus-data','wenhao'),'.mat')
sub <- readMat(filename)
subdata <- data.table(sub$Trial)
names(subdata) <- c('trlno','direction','soa','resp','rt')

library(readr)
# read online table
dat_url <- c('http://www.census.gov/popest/data/national/totals/2015/files/NA-EST2015-01.csv')
odata <- read_csv(dat_url)
# skip rows
odata <- read_csv(dat_url, skip = 2, n_max = 88)
