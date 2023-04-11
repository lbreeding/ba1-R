# Tsa01: Time series analysis script 1, GEOS 585A
#
# Read file of time series in either xlsx, csv or tsv 
# Row 1 should be column heading, starting with the time variable (e.g., "Year") for column 1
# Row 2 should be units for the variables, with "Null" for the year column
# Remaining rows should be the data. File should follow rules:
#   Missing data only leading and trailing -- not internal, and as blank, NaN or NA
#   Column heading for data columns should concise codes or names
#     -begin with a letter and contain only letters or numbers; no special characters, 
#      such as dashes, underscores, dollar signs
#     -no spaces in the headings
#     -length of no more than 12 characters
# Time variable should increment by 1 row by row (e.g., 1 year, 1 month one second) and be 
# the same time variable for all series.
#
# Refer to Notes01.pdf for information about types of times series suitable for the course,
# suggested minimum overlap of series, etc.
#
# Data read is stored in a list tsa as follows:
#   Time series matrix as data matrix tsa$X
#   Year (or other time variable) as numeric vector tsa$time
#   Name of time variable as tsa$timeVar
#   Ids of time series (header of input file) as vector of strings tsa$Id

#  $$$$  First task will be writing code that can read the input files in any of the three
# forms.  See https://stackoverflow.com/questions/17797840/reading-two-line-headers-in-r
#
# This script will save the list tsa in a file called xxx.RData, where xxx is your last name,
# which should have just regular letters (no special characters)
#
# Idea is to write this script such that it read the name of the input file and student's
# last name from a .json input file init01Tsa.json
