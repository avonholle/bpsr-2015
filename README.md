# Blood pressure systematic review
---

## Project Description

This repository contains (mostly R) files to create tables based on data entered in the [wordpress form entry site](https://bpsr.web.unc.edu/).

## Programs
The entered data are handled to create tables that can be found [here](https://avonholle.github.io/bpsr-2015/tables.html)

To create these [tables](https://avonholle.github.io/bpsr-2015/tables.html) I ran the source program titled [tables.Rmd](tables.Rmd). This program references many 'child' programs, which contain separate code for each table. These tables were based loosely on table shells from late 2015.

## Comments regarding data flow

[Data handling](make-data.R) involved obtaining data from the [google sheet](https://docs.google.com/spreadsheets/d/11vikKzexfVU3hAoyXaUB4MO4gHUPJxT-nlw6j2azVYY/edit?usp=sharing) containing a sanitized version of the data as entered in the [Wordpress forms](https://bpsr.web.unc.edu/).

