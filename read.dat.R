# read.dat.R
# Read in data

setwd("C:/Users/vonholle/Dropbox/unc.grad.school/misc/practicum/bpsr/backup")

dat.1 = read.csv(file="full-text-review-form-08022015-revision-2015-10-29.csv",
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

sapply(dat.1, class)

# list of col names and field description from gravity form

var.vals = cbind(cnames.short, cnames.short.2)
rownames(var.vals)=NULL
var.vals[1:25,]


# Look at fields for Table 1: first author, year, study population,
# comparison groups, Sample size, mean age (sd), age range, % women, % AA
# ....................................

# Notes: author name field (4)
# race (23) needs cleaning
# Need to go back and see which ones are 'include' from time before
# addition of the include field (quite a few were done before that
# field was added.

dat.1.t1 = dat.1[,colnames(dat.1) %in% c("field4", "field6", 
                                         paste("field", c(10:14), sep=""),
                                         paste("field", c(19:23), sep=""))]

colnames(dat.1.t1) = c("author", "year", 
                       "clinic.amb", "clinic.home", "home.amb", 
                       "clinic.aoffice", "aoffice.amb",
                       "mean.age", "min.age", "max.age",
                       "female", "race")
# 
# # make comparison fields binary
# dat.1.t1[,3:7] = apply(dat.1.t1[,3:7], 2, 
#                        function(x) {ifelse(x=="",0,1)} )

dat.1.t1$comparison.groups = with(dat.1.t1,
                                  paste(clinic.amb,
                                        clinic.home,
                                        home.amb,
                                        clinic.aoffice,
                                        aoffice.amb,
                                        sep="\n"))
dat.1.t1$comparison.groups

dat.1.t1[,-c(3:7)]

