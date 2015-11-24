# make-data.R

require(knitr)
library(reshape)
library(reshape2)
library(ggplot2)
library(car)
library(plyr)
library(tables)
library(data.table)
library(htmlTable)
require(stringr)

library(ztable)
options(ztable.type="html")

require(data.table)

## @knitr part1

# Read in data
setwd("C:/Users/vonholle/Dropbox/unc.grad.school/misc/practicum/bpsr/backup")

dat.1 = read.csv(file="full-text-review-form-08022015-revision-2015-11-05.csv", # this particular file has hashtags instead of | to delimit values within one entry item
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
length(colnames(dat.1)) # verify 111.

sapply(dat.1, class) # look at class values for each field

# list of col names and field description from gravity form
var.vals = cbind(cnames.short, cnames.short.2)
rownames(var.vals)=NULL
var.vals[1:25,]


# Subset data for table 2 and data handling
# ...........................................

comparisons = c("field11", # clinic to amb
                "field12", # clinic to home
                "field13", # home to amb
                "field14", # clinic to auto office
                "field15") # auto office to amb

# LEFT OFF HERE .................... NEED to fix fields since I have changed the data base since end of october
# Added a field to indicate a re-entry.
# ..............................................................................

dim(dat.1)
# only include those that were marked as include
dat.ts = dat.1[dat.1$field8=="No",
               colnames(dat.1) %in% c("field2", "field3", "field4",
                                      "field5", "field7", "field8", "field10",
                                      "field16",
                                      comparisons, "field28",
                                      "field50", "field56")]

head(dat.ts)

# Note: daytime and nighttime abpm means were not on form.
#       Need to (add field?) go back and get that information
colnames(dat.ts)
colnames(dat.ts) = c("date", "revision", "id", "author", "year",
                     "exclude", "reproduc",
                     "clinic.amb", "clinic.home", "home.amb",
                     "clinic.auto", "auto.amb", 
                     "far.apart", "setting",
                     "num.meas", "mean.meas")
head(dat.ts)
table(dat.ts$reproduc) # 6 reproducibility studies
table(dat.ts$revision) # 8 revisions so far (2015/11/20)

# prep dat.ts so I can extract out delimited fields.
# there are some values that have no responses and no delimiters.
# will add delimiters so the strsplit function works properly.
# .............................................................
head(dat.ts[dat.ts$id %in% c(10569),])

dat.ts = within(dat.ts, {
  author.year = paste(author, year, sep=", ")
   
  rep.table = ifelse(substr(reproduc,1,1)=="R",1,0)
  
  # number of office bp measurements
  # order of entries should be clinic|home|ambulatory|other
  # see bpsr-full-review-preview-online-form-20151101-p3.pdf, a copy of the online form
  
  # see http://stackoverflow.com/questions/7069076/split-column-at-delimiter-in-data-frame
  # means
  mean.meas.2 = ifelse(mean.meas=="", "|||", as.character(mean.meas))
  #mean.2 <- data.frame(do.call('rbind', strsplit(mean.meas.2, '|', fixed=TRUE)))
  sbt.1 = strsplit(mean.meas.2, '|', fixed=TRUE)
  n.1 <- max(sapply(sbt.1, length))
  l.1 <- lapply(sbt.1, function(X) c(X, rep(NA, n.1 - length(X))))
  mean.2 = data.frame(t(do.call(cbind, l.1)))
  clinic.mean = mean.2$X1; home.mean = mean.2$X2; 
  amb.mean = mean.2$X3; other.mean = mean.2$X4
 # amb.mean = mean.2$X1; other.mean = mean.2$X4

  # number of measurements
  # see http://stackoverflow.com/questions/12946883/strsplit-by-row-and-distribute-results-by-column-in-data-frame
  
  num.meas.2 = ifelse(num.meas=="", "|||", as.character(num.meas))
  sbt = strsplit(num.meas.2, '|', fixed=TRUE)
  n <- max(sapply(sbt, length))
  l <- lapply(sbt, function(X) c(X, rep(NA, n - length(X))))
  num.2 = data.frame(t(do.call(cbind, l)))
  clinic.num = num.2$X1; home.num = num.2$X2; 
  amb.num = num.2$X3; other.num = num.2$X4

  # indicator variable for type of comparison
  clinic.amb.yes = ifelse(clinic.amb=="",0,1)
  clinic.home.yes = ifelse(clinic.home=="",0,1)
  home.amb.yes = ifelse(home.amb=="",0,1)
  clinic.auto.yes = ifelse(clinic.auto=="",0,1)
  auto.amb.yes = ifelse(auto.amb=="",0,1)
  
})

table(dat.ts$rep.table) # 6 reproducibility studies

colnames(dat.ts)
names(dat.ts$num.2)

dim(dat.ts)
names(dat.ts)

# Convert data frame from wide to long for tables 2, 4, and 6
# ......................................
dat.ts.sub1 = dat.ts[,!(colnames(dat.ts) %in% c("author", "year", "setting",
                                                "exclude", "clinic.amb",
                                                "reproduc",
                                                "clinic.home", "home.amb",
                                                "clinic.auto", "auto.amb",
                                                "mean.meas", "num.meas",
                                                "mean.2", "mean.meas.2",
                                                "l", "n", "sbt", "l.1", "n.1", "sbt.1",
                                                "num.2", "num.meas.2"))] # remove original variables
colnames(dat.ts.sub1)


head(dat.ts.sub1[dat.ts.sub1$id %in% c(10569),])

# Take the last entered revision for a paper if marked, dat.ts$revision==1
table(dat.ts.sub1$revision)
dat.ts.sub1[order(dat.ts.sub1$id, 
                  dat.ts.sub1$revision, 
                  dat.ts.sub1$date),]
dat.ts.sub1[,c(1:4)]
table(dat.ts.sub1$id)[table(dat.ts.sub1$id)==2] # Note: 10657 is the duplicate

dt1 = data.table(dat.ts.sub1, key="id")

# see http://stats.stackexchange.com/questions/7884/fast-ways-in-r-to-get-the-first-row-of-a-data-frame-grouped-by-an-identifier
dat.ts.sub2 = dt1[J(unique(id)),mult="last"]
nrow(dat.ts.sub2); nrow(dat.ts.sub1) #  now no duplicate entries.
dat.ts.sub2[,c(1:4), with=F] # Note: picked id=10657 with date

id.vars.t = c("id", "date",
              "revision",
              "rep.table",
              "author.year",
              "auto.amb.yes",
              "clinic.auto.yes",
              "home.amb.yes",
              "clinic.home.yes",
              "clinic.amb.yes")


head(dat.ts.sub2[dat.ts.sub2$id %in% c(10569)])

dat.ts.long = melt(dat.ts.sub2, id.vars=c(id.vars.t, "far.apart"))
dat.t9.long = melt(dat.ts.sub2, id.vars=c(id.vars.t))

colnames(dat.ts.long)
table(dat.ts.long$variable)
table(dat.ts.long$author.year)

# more data handling to make indicator variables for type of measurements (other, amb, home and clinic)
# and mean or count 

dat.ts.long = within(dat.ts.long, {
  type.measure = ifelse(grepl("num", variable)==T, "count", "mean")
  type.descrip = ifelse(grepl("other", variable)==T, "other",
                        ifelse(grepl("amb", variable)==T, "amb",
                               ifelse(grepl("home",  variable)==T, "home",
                                      ifelse(grepl("clinic", variable)==T, "clinic", NA))))
})

head(dat.ts.long[dat.ts.long$id %in% c(10569)])
table(dat.ts.long$variable)
table(dat.ts.long$type.measure) #check that it works
table(dat.ts.long$type.descrip) #check

# .........................................
# Note: for subsequent analyses will need to select out the appropriate values
# by selecting mean/count from type.measure variable
# and other/amb/home/clinic from type.descrip variable

# example:
# Look at n/mean for the clinic.home comparison
# ..............................................

compare.1 = dat.ts.long[dat.ts.long$clinic.home.yes==1 & 
                          dat.ts.long$type.descrip %in% c("home", "clinic"), ]
dim(compare.1)
head(compare.1)
table(compare.1$author.year)

compare.1[,colnames(compare.1) %in% c("author.year", 
                                      "type.measure",
                                      "type.descrip",
                                      "value")]

dcast(compare.1, author.year ~ type.measure + type.descrip, 
      value.var="value", na.rm=T)

# compare.1$value = as.numeric(compare.1$value)
# cast(compare.1, author.year ~ type.descrip + type.measure)
