# Blood pressure systematic review
---

## Project Description

This repository contains (mostly R) files to create tables based on data entered in the [wordpress form entry site](https://bpsr.web.unc.edu/).

## Programs
The entered data are handled to create tables that can be found [here](https://avonholle.github.io/bpsr-2015/tables.html)

To create these [tables](https://avonholle.github.io/bpsr-2015/tables.html) I ran the source program titled [tables.Rmd](tables.Rmd). This program references many 'child' programs, which contain separate code for each table. These tables were based loosely on [table shells from late 2015](JAMA tables.docx).

## Comments regarding data flow

[Data handling](make-data.R) involved obtaining data from the [google sheet](https://docs.google.com/spreadsheets/d/11vikKzexfVU3hAoyXaUB4MO4gHUPJxT-nlw6j2azVYY/edit?usp=sharing) containing a sanitized version of the data as entered in the [Wordpress forms](https://bpsr.web.unc.edu/).

## Comments regarding Gravity form that holds the original data entered in 2015 onwards

The full text reviews from 2015 were entered [here](https://bpsr.web.unc.edu/). To convert data entered into this form to a data file in R I did the following:
  1. Export the form entries in Wordpress to a .csv file. Not posted here but the filename is 'full-text-review-form-08022015-revision-2015-12-11.csv' on local drive.
  2. Take the .csv form from part 1 above and read it into R when [handling the data](make-data.R).
  3. To double check that the fields are correct I triangulated between 
      - the form preview files I printed off from the Wordpress form [p1](/dat/gravity-form/full text review form p1.pdf), [p2](/dat/gravity-form/full text review form p2.pdf), [p3](/dat/gravity-form/full text review form p3.pdf), [p4](/dat/gravity-form/full text review form p4.pdf)
      - the [crosswalk of variable names](/dat/gravity-form/variable-crosswalk.csv) I made that corresponds to the columns names from the Wordpress form .csv download and more user-friendly columns names.
      - the original data file mentioned in part 1 above.
  

