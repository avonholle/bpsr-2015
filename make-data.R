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

library(xlsx)
library(compare)


## @knitr part1

# Read in data obtained from the wordpress form at https://bpsr.web.unc.edu/full-text-review-form/
# and handle the data to export to the google sheet at https://docs.google.com/spreadsheets/d/11vikKzexfVU3hAoyXaUB4MO4gHUPJxT-nlw6j2azVYY/edit?usp=sharing
#setwd("C:/Users/vonholle/Dropbox/unc.grad.school/misc/practicum/bpsr/backup")
setwd("~/../Dropbox/unc.grad.school/misc/practicum/bpsr/backup")
dat.orig = read.csv(file="full-text-review-form-08022015-revision-2017-10-02-full.csv", # this particular file has hashtags instead of | to delimit values within one entry item
                    head=T,
                    sep=",")

dim(dat.orig)
dat.orig[1:10,1:8]

# Alter names from gravity form field labels -- too long

rownames(dat.orig)
length(colnames(dat.orig))

cnames = colnames(dat.orig)
cnames.short = sapply(cnames, function(x) {substr(x,0,40)})
cnames.short.2 = paste("field", 1:length(cnames.short), sep="")

colnames(dat.orig) = cnames.short.2
length(colnames(dat.orig)) # verify 111.

sapply(dat.orig, class) # look at class values for each field

# list of col names and field description from gravity form
var.vals = cbind(cnames.short, cnames.short.2)
rownames(var.vals)=NULL

# make a file with revised names to output for double checking work below
names.out = data.frame( gravity.revised.name = cnames.short.2,
                        gravity.short.name = cnames.short,
                        gravity.full.name = cnames)
rownames(names.out) = NULL
head(names.out)
write.csv(names.out, file="~/../Dropbox/unc.grad.school/misc/practicum/bpsr/programs/bpsr-2015/dat/gravity-form/variable-crosswalk.csv")


# Subset data for table 2 and data handling
# ...........................................

# NOTE: look at variable-crosswalk.csv to double check these values

comparisons = c("field11", # clinic to amb
                "field12", # clinic to home
                "field13", # home to amb
                "field14", # clinic to auto office
                "field15") # auto office to amb

# Revise the exclude variable to include those reviews done prior to 10/13/2015.
# this date was before the exclude field was added to the gravity form.
# for the entries with a missing "exclude" variable I will assign one given
# the availability of information in the key fields:  "clinic.amb", "clinic.home", 
# "home.amb", "clinic.auto", "auto.amb", "far.apart", "setting", 
# "num.meas", "mean.meas"
# ......................................................................

key.fields = c( "field16", "field50", "field56")
dat.orig[dat.orig$field8=="", c("field8", key.fields)] # look at fields
dat.orig$miss = apply(dat.orig[,key.fields], 1, function(x) all(x==""|is.na(x))) # make an indicator of 1 if all fields are missing
table(dat.orig$miss)
table(dat.orig$field8)

length(dat.orig[dat.orig$field8=="",]$field8)

dat.orig$exclude=dat.orig$field8
dat.orig$exclude[dat.orig$field8 == ""] = ifelse(dat.orig$miss[dat.orig$field8 == ""] == F, "No", "Yes") # fix exclude for those entries made before 10/13/2015.
with(dat.orig, table(exclude, miss, field8)) # check
dim(dat.orig)

dat.orig[1:20,1:8]



# If the entry is a revision then take only the most recent revision and discard the rest
# .......................................
dat.orig = dat.orig[order(dat.orig$field4, dat.orig$field3),] # sort by paper id and whether it is revised (last date should be last)
dat.orig[1:30, 1:4]
dat.orig[dat.orig$field1=="Ann Von Holle",1:5]
dt = data.table(dat.orig)
dtu = data.frame(dt[, .SD[.N], by = c("field4")]) # select last row of each Author -- last entry 

dtu[dtu$field1=="Ann Von Holle",1:5]
dat.orig.sub = dtu

# make an indicator for those entries that have an updated exclude variable.
dat.orig.sub$exclude.update = with(dat.orig.sub, ifelse(dat.orig.sub$field8=="Yes", 1, 0))


table(duplicated(dat.orig$field4)) # Check duplicated paper id? As of 11/24/2015 there are 9 duplicate ids
as.data.frame(dat.orig.sub)[,colnames(dat.orig.sub) %in% c("field1","field3", "field4")] #check


head(dat.orig$field100) #add this field to google sheet, per Jonathan's request 12/13/2016

# Create a file for export to google docs for future revisions (post-2015/11/28)
# ..............................................................................

vars.to.keep = c("field1", "field2", "field3", "field4", "field5", "field7", 
                 "field10", "exclude.update",
                 comparisons, 
                 "field16", "field28",
                 "field50", "field56",
                 
                 "field8", "field9",
                 
                 "field68", "field69", "field70",
                 "field71", 
                 "exclude",
                 
                 "field74", "field75",
                 "field76", "field77",
                 
                 "field80", "field81", 
                 "field82", 
                 paste("field", c(19:24), sep=""),
                 "field100")


dat.tofile = dat.orig.sub[, vars.to.keep]

dim(dat.tofile) # currently 75 by 38


# Note: to make the matching variable names \unc.grad.school\misc\practicum\bpsr\documentation\online-forms\bpsr-full-review-preview-online-form-20151101-p4.pdf, ..-p3.pdf, etc...
# Also used my data dictionary, titled, "var.vals.html".
# Lastly, referred to a scanned copy of the full text review: full_text_review_form_051915.docx
# ...................................................................

colnames(dat.tofile)
dat.tofile = rename(dat.tofile, 
                    c("field1"="reviewer",
                      "field2"="date of review",
                      "field3"="revised entry?",
                      "field4"="id", 
                      "field5"="first author",
                      "field7"="year", 
                      "field10"="reproducibility",
                      "exclude.update"="revised exclude status",
                      "field11"="comparison clinic to ambulatory",
                      "field12"="comparison clinic to home",
                      "field13"="comparison of home to ambulatory",
                      "field14"="comparison of clinic to automated office",
                      "field15"="comparison of automat. office to amb.",
                      "field16"="reproducibility. how far apart?",
                      "field28"="8. setting for monitoring office clinic setting",
                      "field50"="f1. no. of measurements: clinic|home|amb|other",
                      "field56"="f7. mean of measurements: clinic|home|amb|other",
                      "field8"="original. decision to exclude?", 
                      "field9"="original. reason to exclude",
                      "field68"="g2. clinic vs home counts: yes/no | yes/yes | no/no | no/yes",
                      "field69"="g2. clinic threshold sys|dia (1)",
                      "field70"="g2. clinic threshold sys|dia (2)", # note: there is an option on the form to add more entries. This happened with this field at least once.
                      "field71"="g2. home threshold sys|dia", 
                      "exclude"= "updated. decision to exclude?",
                      "field74"="g3. clinic|abpm counts: yes/no | yes/yes | no/no | no/yes",
                      "field75"="g3. clinic threshold sys|dia",
                      "field76"="g3. abpm threshold sys|dia (1)", 
                      "field77"="g3. abpm threshold sys|dia (2)",
                      "field80"="g4. home vs abpm counts: yes/no | yes/yes | no/no | no/yes",
                      "field81"="g4. home threshold sys|dia", 
                      "field82"= "g4. abpm threshold sys|dia",
                      "field19" = "Country of study",
                      "field20" = "2. Age (mean)",
                      "field21" = "2. Age (min)",
                      "field22" =  "2. Age (max)",  
                      "field23" = "Sex (% female)",
                      "field24" = "Race/ethnicity",
                      "field100" = "Additional comments"))

rownames(dat.tofile) = NULL

dat.tofile$"g2. sensitivity" = ''
dat.tofile$"g2. specificity" = ''

dat.tofile$"g3. sensitivity" = ''
dat.tofile$"g3. specificity" = ''

dat.tofile$"g4. sensitivity" = ''
dat.tofile$"g4. specificity" = ''

last.vars = c("g2. sensitivity", "g2. specificity",
              "g3. sensitivity", "g3. specificity",
              "g4. sensitivity", "g4. specificity")


# Write data to file that can be edited in excel
# This file is written to a google sheet that can be edited and followed with 'track revisions'
# at https://docs.google.com/spreadsheets/d/11vikKzexfVU3hAoyXaUB4MO4gHUPJxT-nlw6j2azVYY/edit?usp=sharing (note this link is for viewing only)
#............................................................
colnames(dat.tofile)
write.csv(dat.tofile, "~/../Dropbox/unc.grad.school/misc/practicum/bpsr/programs/bpsr-2015/dat/dat.tofile.csv")

# IMPORTANT: read edited file back in for tables
# ....................................................

# NOTE: when I try to read in directly from excel, via .xlsx structure,
# I get very different fields that are hard to reconcile with original file.
# If I use excel, I need to convert back to csv to make this work for rest of
# programs that follow.
# This file is from the google doc at https://docs.google.com/spreadsheets/d/11vikKzexfVU3hAoyXaUB4MO4gHUPJxT-nlw6j2azVYY/edit?usp=sharing, 
# which has been edited (from the doc in the write.csv directly above)
# In google docs export as .csv file and place in folder listed below.

dat.1 <- read.csv("~/../Dropbox/unc.grad.school/misc/practicum/bpsr/programs/bpsr-2015/dat/dat.20151128-3.csv")
# NOTE: the dat.20151128-2.csv file has updated entries from 2017
dim(dat.1) # currently 75 by 45: 2016/12/14
# now 75 by 45: 2017/08/28
colnames(dat.1)

colnames(dat.1) = c("row.num", vars.to.keep, c("field113", "field114",
                                               "field115", "field116", "field117", "field118"))

# make sure that the original file (pre-edits) that I read back in
# has the same responses

test1 = as.matrix(dat.1[,4:(ncol(dat.1)-6)])
test2 = as.matrix( dat.orig.sub[, vars.to.keep[3:length(vars.to.keep)]])

colnames(test1)
colnames(test2)
ncol(test1)
ncol(test2)

compare(test1, test2) # these data frames match on everything.

# Make a data frame only including those that were marked as include
# (now including updated exclude variable. Should check for these entries.)
# these are for tables 2, 4, and 6
# ....................................................................

dat.ts = dat.1[dat.1$exclude=="No",
               colnames(dat.1) %in% c("field1", "field2", "field3", "field4", "field5", "field7", 
                                      "field10", "exclude.update",
                                      comparisons, 
                                      "field16", "field28",
                                      "field50", "field56")]
colnames(dat.ts)

# Note: daytime and nighttime abpm means were not on form.
#       Need to (add field?) go back and get that information

colnames(dat.ts) = c("reviewer", 
                     "date", "revision", "id", "author", "year",
                     "reproduc",  "exclude.update",
                     "clinic.amb", "clinic.home", "home.amb",
                     "clinic.auto", "auto.amb", 
                     "far.apart", "setting",
                     "num.meas", "mean.meas")

head(dat.ts)
table(dat.ts$reproduc) # 6 reproducibility studies
table(dat.ts$revision) # 8 revisions so far (2015/11/20)

# Prep dat.ts so I can extract out delimited fields.
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
dat.ts.sub1 = dat.ts[,!(colnames(dat.ts) %in% c("reviewer", 
                                                "author", "year", "setting",
                                                "exclude", "clinic.amb",
                                                "reproduc",
                                                "clinic.home", "home.amb",
                                                "clinic.auto", "auto.amb",
                                                "mean.meas", "num.meas",
                                                "mean.2", "mean.meas.2",
                                                "l", "n", "sbt", "l.1", "n.1", "sbt.1",
                                                "num.2", "num.meas.2",
                                                "exclude.update"))] # remove original variables
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
