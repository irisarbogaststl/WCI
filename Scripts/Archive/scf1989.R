#############################################################################################
# Project: FOMC 2019
# Economist: Ravi
# RA: Brian Reinbold
# Last Edited: 11/13/2019
#

#############################################################################################

library(tidyverse)
library(readxl)


rm(list=ls())

# directories
path <- "G:/Research_Analyst/Reinbold/Ravi/FOMC 2019"
input_dir <- "/Data/SCF/"
output_dir <- "/Output/"

raw_imp <- haven::read_dta(file = paste0(path, input_dir, "p89i6.dta"))
raw_rw <- haven::read_dta(file = paste0(path, input_dir, "p89_rw.dta"))

df <- raw_imp %>%
  select(., X1, XX1,
         # Checking Account
         X3501,                                # do you have a checking account?
         paste0("X", seq(3506, 3526, by = 4)), # amount in each account
         paste0("X", seq(3507, 3527, by = 4)), # is money market account?
         X3529,                                # amount in remaining accounts
         # Savings Account
         X3801,                                # do you have a savings account
         paste0("X", seq(3504, 3516, by = 4)), # amount in each account
         X3818,                                # amount in remaining accounts
         # Money Market
         X3701,                                # do you have money market account?
         paste0("X", seq(3706, 3516, by = 5)), # amount in each account
         X3718                                # 
  )






