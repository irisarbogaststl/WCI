# This script is the primary resource for analysis of the Survey of Consumer
# Finances conducted by the Center for Household Financial Stability at the
# Federal Reserve Bank of St. Louis. This script was written by Lowell
# Ricketts, Lead Analyst at the Center towards the beginning of his 
# tenure in early 2016. Much of the SCF analysis done by the Center since then
# stems from this script and most of that work can be reproduced in the
# script sections below. I would like to acknowledge the work of Anthony Damico
# of asdfree. This script uses much of the same methodology and supporting
# scripts and packages.

# Last Modified: 11/13/2019
rm(list=ls())

source.dir <- "C:/Users/h1bjr02/Desktop/SCF"
out.dir <- "C:/Users/h1bjr02/Desktop/SCF"
path <- "G:/Research_Analyst/Reinbold/Ravi/FOMC 2019/Output"
output_dir <- "/MulliganSalaimartin2000/"

# load two svyttest functions (one to conduct a df-adjusted t-test and one to 
# conduct a multiply-imputed t-test)
source(paste0(source.dir, "/scripts/asdfree/scf.survey.R"))



library(mitools)               # allows analysis of multiply-imputed survey data
library(survey)          # load survey package (analyzes complex design surveys)
library(xlsx)
library(Hmisc)
library(tidyverse)


footnote <- "Source: Federal Reserve Board's Survey of Consumer Finances"

# years.to.loop <- c(1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016)
years.to.loop <- 1989


#### Import Data, Define Variables ####
results <- NULL; full_imp1 <- NULL; full_imp2 <- NULL; full_imp3 <- NULL
full_imp4 <- NULL; full_imp5 <- NULL; full_rw <- NULL

# CPI-U Annual Series sourced from Haver Analytics, vintage as of 9/27/17
cpi <- read.csv(paste0(source.dir, "/R/input/cpi_u_annual.csv"))

deflate <- function(x) {
  x <- mutate(x,
              x5804 = ifelse(x5804 < 0, 0, x5804),
              x5809 = ifelse(x5809 < 0, 0, x5809),
              x5814 = ifelse(x5814 < 0, 0, x5814))
  gift1 <- select(x, c(y1, yy1, year, x5804, x5805)) %>%
    left_join(cpi, by = c("x5805" = "cpi_year")) %>%
    mutate(gift1 = x5804/deflator) %>%
    select(y1, yy1, year, gift1)
  gift2 <- select(x, c(y1, yy1, year, x5809, x5810)) %>%
    left_join(cpi, by = c("x5810" = "cpi_year")) %>%
    mutate(gift2 = x5809/deflator) %>%
    select(gift2)
  gift3 <- select(x, c(y1, yy1, year, x5814, x5815)) %>%
    left_join(cpi, by = c("x5815" = "cpi_year")) %>%
    mutate(gift3 = x5814/deflator) %>%
    select(gift3)
  
  adj_gift <- cbind(gift1, gift2, gift3)
  
  adj_gift <- mutate(adj_gift, 
                     gift1 = ifelse(is.na(gift1), 0, gift1),
                     gift2 = ifelse(is.na(gift2), 0, gift2),
                     gift3 = ifelse(is.na(gift3), 0, gift3))
  
  adj_gift$real_gift = rowSums(adj_gift[4:6])
  
  adj_gift <- select(adj_gift, c(y1, yy1, year, real_gift))
  x <- left_join(x, adj_gift, by = c("y1", "yy1", "year"))
  
  
}

create.variables <- function(x) {
  x <- mutate(x,
              
              ## RACE/ETHNICITY VARIABLES ##
              
              # Recode race variable 
              race = case_when(
                race == 1 ~ 1,
                race == 2 ~ 2,
                race == 3 ~ 3, 
                T ~ 4),
              
              # Race binaries
              iswhite = ifelse(race == 1, 1, 0),
              isblack = ifelse(race == 2, 1, 0),
              ishispanic = ifelse(race == 3, 1, 0),
              isother = ifelse(race == 4, 1, 0),
              
              
              ## AGE VARIABLES ##
              
              # Age binaries
              age_young = ifelse(age <= 39, 1, 0),
              age_middle = ifelse(age >= 40 & age <= 61, 1, 0),
              age_old = ifelse(age >= 62, 1, 0),
              agecl = case_when(
                age <= 39 ~ 1, 
                age >= 40 & age <= 61 ~ 2,
                age >= 62 ~ 3),
              
              ## SURVEY YEAR BINARIES ##
              
              y1989 = ifelse(year == 1989, 1, 0), 
              y1992 = ifelse(year == 1992, 1, 0),
              y1995 = ifelse(year == 1995, 1, 0),
              y1998 = ifelse(year == 1998, 1, 0),
              y2001 = ifelse(year == 2001, 1, 0),
              y2004 = ifelse(year == 2004, 1, 0),
              y2007 = ifelse(year == 2007, 1, 0),
              y2010 = ifelse(year == 2010, 1, 0),
              y2013 = ifelse(year == 2013, 1, 0),
              y2016 = ifelse(year == 2016, 1, 0)
              
  )
}


for (i in seq( length( years.to.loop ) ) ) {
  load(paste0(source.dir, "/R/input/scf", years.to.loop[i], ".rda"))
  
  # define which variables from the five imputed iterations to keep
  vars.to.keep <- c( 'y1', 'yy1', 'year', 'wgt', 'x42001', 'one', 'five',
                     'x101', 'x407', 'x409', 'x716', 'x2710', 'x2723', 
                     'x2727', 'x2740', 'x2810', 'x2823', 'x2827', 'x2840',
                     'x2910', 'x2923', 'x2927', 'x2940',
                     'x3004', 'x3006', 'x3015', 'x3016', 'x3017', 'x3018',
                     'x3019', 'x3020', 'x3023', 'x3031', 'x4001', 'x5701', 
                     'x5702', 'x5708', 'x5715', 'x5734',
                     'x5801', 'x5804', 'x5805', 'x5809', 'x5810', 'x5814',
                     'x5815', 'x5819', 'x5821', 'x5823', 'x5824', 'x5825', 
                     'x5901', 'x5902', 'x5904', 'x5905', 'x5931', 'x5932',
                     'x6030', 'x6032', 'x6033', 'x6111', 'x6112', 'x6132',
                     'x6133', 'x6357', 'x6443', 'x6772', 'x6773', 'x6788', 
                     'x6809', 'x7004', 'x7052', 'x7100', 
                     'x7111', 'x7186', 'x7187', 'x7362', 'x7507', 'x7508', 
                     'x7509', 'x7510', 'x7556', 'x7557', 'x7558', 'x7559', 
                     'x7560', 'x7561', 'x7562', 'x7575', 'x7577', 'x7650', 
                     'x7179', 'x7775', 'x7802', 'x7824', 
                     'x7847', 'x7870', 'x7879', 'x7883', 'x7884', 'x7888',
                     'x7889', 'x7893', 'x7894', 'x7898', 'x7899', 'x7924', 
                     'x7947', 'x7970', 'x7978', 'x7993', 'x7994', 'x8023',
                     #' Board Variables (see bulletin.macro.sas)
                     'networth', 'income', 'houses', 'debt', 'asset', 
                     'vehic','othnfin', 'oresre', 'resdbt', 'liq', 'cds', 
                     'savbnd', 'bond', 'fin', 'nfin', 'nnresre',  'bus', 'install',
                     'othloc',  'ccbal','odebt', 'mrthel', 'race', 'age', 
                     'married', 'kids', 'pir40', 'debt2inc', 'bussefarminc', 
                     'intdivinc', 'kginc', 'wageinc', 'famstruct', 'late', 
                     'late60', 'retqliq', 'homeeq', 'levratio', 'stocks',
                     'nmmf', 'othma', 'equity', 'prepaid', 'hccbal', 
                     'edn_inst', 'veh_inst', 'oth_inst', 'tpay', 'pirtotal',
                     'cashli', 'othfin',
                     # Brian added these vars
                     'checking', 'saving', 
                     'mmmf',                 # mutual funds
                     'mmda',                 # money market deposit accounts
                     "mma",                  # money market accounts: sum(mmda,mmfa)
                     'x5721',                # recieve social security or pension 1:yes 5: no
                     'x310',                 # distance from finacial institution
                     'edcl'                  # highest degee attained: 4 = college
  )
  
  
  # Add SCF year as variable
  imp1 <- mutate(imp1, year = years.to.loop[i])
  imp2 <- mutate(imp2, year = years.to.loop[i])
  imp3 <- mutate(imp3, year = years.to.loop[i])
  imp4 <- mutate(imp4, year = years.to.loop[i])
  imp5 <- mutate(imp5, year = years.to.loop[i])
  #rw <- mutate(rw, year = years.to.loop[i])
  
  # restrict each `imp#` data frame to only those variables
  imp1 <- imp1[ , vars.to.keep ]
  imp2 <- imp2[ , vars.to.keep ]
  imp3 <- imp3[ , vars.to.keep ]
  imp4 <- imp4[ , vars.to.keep ]
  imp5 <- imp5[ , vars.to.keep ]
  
  # clear up RAM
  gc()
  
  # Adjust several variables for inflation which aren't already adjusted
  # by the Board's bulletin SAS script. All values are expressed in 2016
  # dollars.
  imp1 <- deflate(imp1)
  imp2 <- deflate(imp2)
  imp3 <- deflate(imp3)
  imp4 <- deflate(imp4)
  imp5 <- deflate(imp5)
  
  # Create all other variables for analysis
  imp1 <- create.variables(imp1)
  imp2 <- create.variables(imp2)
  imp3 <- create.variables(imp3)
  imp4 <- create.variables(imp4)
  imp5 <- create.variables(imp5)
  
  # Append new year to combined data set
  full_imp1 <- rbind(full_imp1, imp1)
  full_imp2 <- rbind(full_imp2, imp2)
  full_imp3 <- rbind(full_imp3, imp3)
  full_imp4 <- rbind(full_imp4, imp4)
  full_imp5 <- rbind(full_imp5, imp5)
  full_rw <- rbind(full_rw, rw)
  
}

rm(imp1, imp2, imp3, imp4, imp5, rw)


# Select implicate want to use
#raw <- bind_rows(full_imp1, full_imp2, full_imp3, full_imp4, full_imp5)
raw <- full_imp1


### do initial data cleaning
df <- raw %>%
  filter(., year == 1989) %>%
  select(., year, age, checking, saving, cds, mmmf, stocks, bond, mma, income, x5721, x310, edcl, wgt) %>%
  mutate(., m                  = checking + saving,
         interestBearingAssets = cds + mmmf + stocks + bond + mma,
         matchStata            = mma + checking
  ) %>%
  mutate(., finAssets = m + interestBearingAssets) %>%
  mutate(., dist = x310,
         retired = if_else(x5721 < 5,
                           1,
                           0
         ),
         college = if_else(edcl == 4,
                           1,
                           0
         )
  ) %>%
  mutate(., dist = if_else(dist == -1,                      # less than a mile
                           0,
                           if_else(dist == 0 | dist == -7,  # basically doesn't have one
                                   as.numeric(NA),
                                   dist
                           )
  ) 
  ) %>%
  filter(., income > 0 & finAssets > 0) %>%
  mutate(., log_income = log(income),
         log_A      = log(finAssets),
         m_a        = m / finAssets
  ) %>%
  # indicator if they have a checking account
  mutate(., hasChecking = if_else(checking > 0,
                                  1,
                                  0
  ),
  # indicator if they have interest bearing assets
  hasInterestBearing = if_else(interestBearingAssets > 0,
                               1,
                               0
  )
  ) 

###  verify variables with stata code
var <- c("checking", "saving", "cds", "mmmf", "stocks", "bond", "mma", "income", "m", "finAssets", "interestBearingAssets", "matchStata")
df_1989 <- filter(df, year == 1989)
set_names(
  lapply(var, function(v) {summary(df_1989[[v]]) * 0.51665160}), 
  var
)
summary(df[["m_a"]])


### match table in paper
match_table1 <- df %>%
  mutate(., hasChecking = factor(hasChecking,
                                 levels = c(1, 0),
                                 labels = c("Yes Checking", "No Checking")
  ),
  hasInterestBearing = factor(hasInterestBearing,
                              levels = c(1, 0),
                              labels = c("Yes Interest-Bearing", "No Interest-Bearing")
  )
  )
match_table1 <- split(match_table1, f = match_table1$year)

table1 <- lapply(match_table1, function(t) {
  table(t$hasChecking, t$hasInterestBearing)
})
table1_prop <- lapply(table1, function(t) {
  prop.table(t)
})  


# export table 1
# for (y in names(table1)) {
#   write.xlsx(as.data.frame(table1[[y]]),
#              file      = paste0(path, output_dir, "Table 1.xlsx"),
#              showNA    = FALSE,
#              row.names = FALSE,
#              append    = TRUE,
#              sheet     = y
#   )
#   write.xlsx(as.data.frame(table1_prop[[y]]),
#              file = paste0(path, output_dir, "Table 1 Proportions.xlsx"),
#              showNA    = FALSE,
#              row.names = FALSE,
#              append    = TRUE,
#              sheet     = y
#   )
# }

### match table 1 by doing weighted sum
df %>%
  mutate(., hasChecking = if_else(checking > 0,
                                  1,
                                  0
  ),
  hasInterestBearing = if_else(interestBearingAssets > 0,
                               1,
                               0
  )
  ) %>%
  group_by(., year, hasChecking, hasInterestBearing) %>%
  summarise(., weight = sum(wgt)) %>%
  ungroup(.) %>%
  group_by(., year) %>%
  mutate(., totalWeight = sum(weight)) %>%
  mutate(., proportion = weight / totalWeight) %>%
  #summarise(., sum(proportion)) %>%                # just a check to makes sure each year adds to one
  ungroup(.)


# number of obs
nrow(df)

# drop those without a checking account and remove NAs for regressions
df <- df %>%
  filter(., checking > 0) %>%
  filter(., complete.cases(.))


### match summary statistcs in table 2
table2_df <- df %>%
  #filter(., year == 1989) %>%
  select(., year, m_a, finAssets, log_A, m, age, income, log_income, dist) %>%
  gather(., key = "variable", value = "value", -year) %>%
  group_by(., year, variable) %>%
  summarise(., Mean   = mean(value),
            SD     = sd(value),
            Median = median(value),
            Min    = min(value),
            Max    = max(value)
  ) %>%
  left_join(., select(cpi, -cpi_u), by = c("year" = "cpi_year")) %>%
  mutate(., deflator = if_else(variable %in% c("finAssets", "m", "income"),
                               deflator,
                               1
                               )
  )

# export table 2
for (y in unique(table2_df$year)) {
  write.xlsx(as.data.frame(filter(table2_df, year == y)),
             file      = paste0(path, output_dir, "Table 2.xlsx"),
             showNA    = FALSE,
             row.names = FALSE,
             append    = TRUE,
             sheet     = as.character(y)
  )
}
table2_df_1989 <- filter(table2_df, year == 1989)
match_table2 <- map_dfc(table2_df_1989[3:(ncol(table2_df_1989)-1)],
                        function(c) {c * table2_df_1989[ncol(table2_df_1989)]}
)
rownames(match_table2) <- table2_df_1989[[2]]
colnames(match_table2) <- colnames(table2_df_1989)[3:(ncol(table2_df_1989)-1)]
print(match_table2)



### Divide sample into 100 equally sized bins
centile_df <- df %>%
  arrange(., year, finAssets) %>%                # sort on financial assets
  group_by(., year) %>%
  mutate(., centile = seq(1:n())) %>%            # row number by year
  mutate(., centile = cut(centile,               # define 100 bins by year
                          breaks = 100,
                          labels = seq(1, 100)
  )
  ) %>%
  group_by(., year, centile) %>%                    
  summarize(., finAssets        = mean(finAssets),
            fractionAdopting = sum(hasInterestBearing) / n(),
            m_a              = mean(m_a)
  )

# fit logistic equation to data

library(minpack.lm)                        # use for non-linear least squares
### Examples from 'nls' doc ###

# centile_df <- filter(centile_df, year == 1989)

centile_list <- split(centile_df, f = centile_df$year)
centile_df <- map_dfr(centile_list, function(cdf) {
  
  # Model for fraction adopting
  m_adopting <- nlsLM(data = cdf,
                      fractionAdopting ~ Asym/(1 + exp((xmid - log(finAssets))/scal)), 
                      start = list(Asym = 1, xmid = log(1e4), scal = 1)
  )
  cdf$logistic_adopting <- fitted(m_adopting)
  
  # Model for money holdings
  m_m_a <- nlsLM(data = cdf,
                 m_a ~ Asym/(1 + exp(-(xmid - log(finAssets))/scal)), 
                 start = list(Asym = 0, xmid = log(1e4), scal = 1))
  cdf$logistic_m_a <- fitted(m_m_a)
  
  return(cdf)
}
)



logGraph <- function(v, fitv, ylab, y) {  
  
  # v = "fractionAdopting"
  # fitv = "logistic_adopting"
  # y = 1989
  # ylab = "Fraction Adopting"
  # 
  # v = "m_a"
  # fitv = "logistic_m_a"
  # y = 1989
  # ylab = ""
  
  d <- filter(centile_df, year == y)
  
  ggplot(data = d) +
    theme_bw() +
    geom_point(aes(x = finAssets, y = !!sym(v))) +
    scale_x_continuous(trans = "log10",
                       breaks = c(1,
                                  10,
                                  100,
                                  1000,
                                  10000,
                                  100000,
                                  1000000,
                                  10000000,
                                  100000000,
                                  1000000000
                       ),
                       #expand = c(0, 0),
                       limits = c(1, 1e8),
                       labels = scales::comma
    ) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
    geom_line(aes(x = finAssets, y = !!sym(fitv)), color = "blue") +
    labs(title = y,
         x     = "Financial Assets",
         y     = ylab
    )
  
  # Export
  # ggsave(filename = paste0(path, output_dir, ylab, "/", y, ".png"),
  #        width    = 12,
  #        height   = 8,
  #        units    = "in"
  # )
  
}
for (y in unique(centile_df$year)) {
  logGraph(v    = "fractionAdopting",
           fitv = "logistic_adopting",
           ylab = "Fraction Adopting",
           y    = y
  )
  logGraph(v    = "m_a",
           fitv = "logistic_m_a",
           ylab = "Money as a Fraction of Assets",
           y    = y
  )
}


logisticGraph <- function(v, ylab) {  
  
  ggplot(data = centile_df) +
    theme_bw() +
    scale_x_continuous(trans = "log10",
                       breaks = c(1,
                                  10,
                                  100,
                                  1000,
                                  10000,
                                  100000,
                                  1000000,
                                  10000000,
                                  100000000,
                                  1000000000
                       ),
                       #expand = c(0, 0),
                       limits = c(1, 1e8),
                       labels = scales::comma
    ) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
    geom_line(aes(x = finAssets, y = !!sym(v), color = as.factor(year))) +
    labs(title = "",
         x     = "Financial Assets",
         y     = ylab
    ) +
    theme(legend.title = element_blank())
  
  # Export
  ggsave(filename = paste0(path, output_dir, ylab, "/Logistic - ", ylab, ".png"),
         width    = 12,
         height   = 8,
         units    = "in"
  )
  
}

logisticGraph(v    = "logistic_adopting",
              ylab = "Fraction Adopting"
)
logisticGraph(v    = "logistic_m_a",
              ylab = "Money as a Fraction of Assets"
)




ggplot(data = df) +
  theme_bw() +
  geom_point(aes(x = finAssets, y = m_a)) +
  scale_x_continuous(trans = "log10",
                     breaks = c(1,
                                10,
                                100,
                                1000,
                                10000,
                                100000,
                                1000000,
                                10000000,
                                100000000,
                                1000000000
                     ),
                     #expand = c(0, 0),
                     limits = c(1, 1e8),
                     labels = scales::comma
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  #geom_line(aes(x = finAssets, y = !!sym(fitv)), color = "blue") +
  labs(title = y,
       x     = "Financial Assets",
       y     = "Money as a Fraction of Assets"
  )




adoptionCost_m <- lm(data = df,
                     age + college + dist + retired)

probit_m <- glm(data = df,
                hasInterestBearing ~ log_A + age + college + dist + retired,
                family = binomial(link = "probit")
)
summary(probit_m)



logdf <- data.frame(x = seq(0,10, by = 0.1))
logdf$y = 10/(1 + exp((5 + log(logdf[["x"]]))/1))
ggplot(data = logdf, aes(x = x, y = y)) + geom_line()

# #### Create Complex Survey Design Object ####
# # For basic statistics and NOT standard errors, just ignore replicate weights. 
# # Replicate design requires much more computational resources.
# scf.design <-
#         svydesign(
#                 ids = ~0,
#                 # use the main weight within each of the imp# objects
#                 weights = ~wgt,
#                 # read the data directly from the five implicates
#                 data = imputationList( list( full_imp1, full_imp2, full_imp3, 
#                                              full_imp4, full_imp5 ) )
#         )
# 
# 
# #### Create Replicate Survey Design Object (Resource Intensive) ####
# # Use this if you need standard errors
# scf.repdesign <-
#         svrepdesign(
#                 ids = ~0,
#                 # use the main weight within each of the imp# objects
#                 weights = ~wgt,
#                 #weights = ~x42001, NOTE: Found no difference using original weight versus
#                 # the weight/5
#                 # use the 999 replicate weights stored in the separate replicate weights file
#                 repweights = full_rw[ , -1 ],
#                 #repweights = full_rw[ , 991:1000 ],
#                 # read the data directly from the five implicates
#                 data = imputationList( list( full_imp1, full_imp2, full_imp3, 
#                                              full_imp4, full_imp5 ) ),
#                 scale = 1,
#                 rscales = rep( 1 / 998, 999 ),
#                 #rscales = rep( 1 / 9, 10 ),
#                 # use the mean of the replicate statistics as the center
#                 # when calculating the variance, as opposed to the main weight's statistic
#                 mse = T,
#                 type = "other",
#                 combined.weights = T
#         )
