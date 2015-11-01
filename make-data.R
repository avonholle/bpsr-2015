# make-data.R

require(knitr)
library(reshape2)
library(ggplot2)
library(car)
library(plyr)
library(tables)
library(data.table)
library(htmlTable)

library(ztable)
options(ztable.type="html")

## @knitr part1

# Read in data
setwd("C:/Users/vonholle/Dropbox/unc.grad.school/misc/practicum/bpsr/backup")

dat.1 = read.csv(file="full-text-review-form-08022015-revision-2015-10-29-rev.csv", # this particular file has hashtags instead of | to delimit values within one entry item
         head=T,
         sep=",")

dim(dat.1)

# alter names from gravity form field labels -- too long

rownames(dat.1)
length(colnames(dat.1))

cnames = colnames(dat.1)
cnames.short = sapply(cnames, function(x) {substr(x,0,40)})
cnames.short.2 = paste("field", 1:length(cnames.short), sep="")

cnames.short.2[1:10]

colnames(dat.1) = cnames.short.2

sapply(dat.1, class) # look at class values for each field

# list of col names and field description from gravity form
var.vals = cbind(cnames.short, cnames.short.2)
rownames(var.vals)=NULL
var.vals[1:25,]

