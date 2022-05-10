#############################################################################################
# Project: FOMC 2019
# Economist: Ravi
# RA: Brian Reinbold
# Last Edited: 11/12/2019
#
# Replicate Lucas 2000, update data, and look at various subsets of the data
#############################################################################################
library(tidyverse)
library(readxl)


rm(list=ls())

# directories
path <- "G:/Research_Analyst/Reinbold/Ravi/FOMC 2019"
input_dir <- "/Data/"
output_dir <- "/Output/Lucas 2000/"

### i used this initially to create folders for each parametrization
# for (p in 1:length(params_list)) {
#   dir.create(path = paste0(path, output_dir, "Parametrization ", p))
# }

#### Parametrizations ###

# for Log-Log Demand Function
eta <- c(0.3, 0.5, 0.7)

# for semi-log demand functions
xi <- c(5, 7, 9)


params_list <- list(
  # 1) Match up Lucas Original Paper
  list(yBeg        = 1900,
       yEnd        = 1994,
       filterProbs = FALSE,
       freq        = "Annual"
       ),
  # 2) Extend Data of Lucas Paper
  list(yBeg        = 1900,
       yEnd        = 2018,
       filterProbs = FALSE,
       freq        = "Annual"
  ),
  # 3) Match up Lucas Original Paper but take out problem areas identified in Ireland 2009
  list(yBeg        = 1900,
       yEnd        = 1994,
       filterProbs = TRUE,
       freq        = "Annual"
  ),
  # 4) Extend Data of Lucas Paper but take out problem areas identified in Ireland 2009
  list(yBeg        = 1900,
       yEnd        = 2018,
       filterProbs = TRUE,
       freq        = "Annual"
  ),
  # 5) Takes out first problem 
  list(yBeg        = 1950,
       yEnd        = 2018,
       filterProbs = FALSE,
       freq        = "Annual"
  ),
  # 6) Takes out second problem
  list(yBeg        = 1980,
       yEnd        = 2018,
       filterProbs = FALSE,
       freq        = "Annual"
  ),
  # 7) Takes out second problem
  list(yBeg        = 1980,
       yEnd        = 2019,
       filterProbs = FALSE,
       freq        = "Quarterly"
  )
)

# assing empty list to appends results to
results_list <- list()



### read in data ###

# initialize empty list to store data
df_list <- list()


### Annual data to match Lucas 2000 data
raw <- read_excel(paste0(path, input_dir, "Lucas Data.xlsx"),
                  sheet = "Data"
)
colnames(raw) <- c("year", "gdp", "gdpDef", "m1", "rate")
df <- raw %>%
  mutate(., gdp = gdp / 1e3)                   # convert from millions to billions
  
df_list$Annual <- df                           # store annual data in list
rm(df, raw)                                    # clear up workspace


### Quarterly Data 1980Q1 - 2019Q3 to match Ireland 2009
raw <- read_excel(paste0(path, input_dir, "Quarterly Data.xlsx"),
                  skip      = 3,
                  col_names = FALSE
)
# drop first variable
raw[1] <- NULL
colnames(raw) <- c("date", "rate", "gdp", "m1")

# clean data
df <- raw %>%
  mutate(., year = lubridate::year(date),
            date = as.Date(date),
            rate = rate / 100
         )

df_list$Quarterly <- df                      # store annual data              
rm(df, raw)                                  # clear up workspace





for (p in 1:length(params_list)) {

  # debugging
  #p = 2
  
  
  # find list of parameters for specification
  params <- params_list[[p]]
  
  
  # frequency of data
  freq <- params[["freq"]]
  
  ### last year of subset ###
  # Lucas 2000 ends in 1994
  yBeg <- params[["yBeg"]]
  yEnd <- params[["yEnd"]]
  
  # filter problem areas identified in Ireland 2009
  filterProbs = params[["filterProbs"]]
  
  # retrieve data based on whether parametrization is annual or monthly
  df <- df_list[[freq]]
  
  
  ### Clean Data ###
  df <- df %>%
    mutate(., m1_gdp = m1 / gdp) %>%             # ratio of M1 to nominal GDP
    filter(., year <= yEnd & year >= yBeg) %>%   # select range
    # identify problem areas in Ireland 2009
    mutate(., dotColor = if_else(year >= 1945 & year <= 1949,     
                                 "1945-1949",
                                 if_else(year >= 1979 & year <= 1982,
                                         "1979-1982",
                                         "Data"
                                 )
                                 )
    )
  
  if (filterProbs == TRUE) {
    df <- df %>%
      filter(., dotColor == "Data")
  }
  
  
  # calculate geometric means
  geomMean_m <- exp(mean(log(df$m1_gdp)))
  geomMean_r <- exp(mean(log(df$rate)))
  
  
  
  ### Figure 1: Line Plot ###
  
  if (filterProbs == FALSE) {
    
    if (freq == "Annual") {
      g <- ggplot(data = df, aes(x = year)) +
        theme_bw(20) +
        geom_line(aes(y = m1_gdp, color = "M1 to GDP Ratio"), size = 1.5) +
        geom_line(aes(y = rate * 4, color = "Interest Rate"), size = 1.5) +
        scale_y_continuous(sec.axis = sec_axis(~./4, 
                                               name = "Interest Rate",
                                               breaks = seq(0, 0.16, by = 0.02)
        ),
        breaks = seq(0, 0.6, by = 0.1)
        ) +
        scale_x_continuous(breaks = seq(yBeg, 2010, by = 10),
                           expand = c(0, 1)
        ) +
        labs(x = "",
             y = "M1 to GDP Ratio"
        ) +
        theme(legend.title = element_blank(),
              legend.position = c(0.2, 0.9)
        )
    } else {
      g <- ggplot(data = df, aes(x = date)) +
        theme_bw(20) +
        geom_line(aes(y = m1_gdp, color = "M1 to GDP Ratio"), size = 1.5) +
        geom_line(aes(y = rate * 4, color = "Interest Rate"), size = 1.5) +
        scale_y_continuous(sec.axis = sec_axis(~./4, 
                                               name = "Interest Rate",
                                               breaks = seq(0, 0.16, by = 0.02)
        ),
        breaks = seq(0, 0.6, by = 0.1)
        ) +
        # scale_x_continuous(breaks = seq(yBeg, 2010, by = 10),
        #                    expand = c(0, 1)
        # ) +
        labs(x = "",
             y = "M1 to GDP Ratio"
        ) +
        theme(legend.title = element_blank(),
              legend.position = c(0.8, 0.9)
        )
    }
    
    print(g)
    
    # Export
    ggsave(filename = paste0(path, output_dir, "Parametrization ", p, "/",
                             "Figure 1_", yEnd, ".png"),
           plot     = g,
           width    = 12,
           height   = 8,
           units    = "in"
           )
    
  }
  
  
  ### log-log Demand Function Calculations ###
  
  ### select parameter A such that curve passes thorough geometric mean of two series
  A <- rep(as.numeric(NA), times = 3)         # initialze empty matrix to hold values
  
  # find value of A for each eta
  for (i in 1:length(eta)) {
    A[i] <- geomMean_m / (geomMean_r ^ (-eta[i]))
  }
  
  ### Calc values for log-log demand for each eta
  loglogCurves_df <- data.frame(rate = seq(0, 0.16, by = 0.001)) # intialize x-value for curves
  for (i in 1:length(eta)) {
    
    # function of log-log demand to be overlaid on plots
    loglogCurves_df[[paste0("logLogDemand_eta_", eta[i])]] <- (loglogCurves_df$rate ^ (-eta[i])) * A[i]
    
    # fitted log-log demand curve of data
    df[[paste0("fitted_logLogDemand_eta_", eta[i])]] <- (df$rate ^ (-eta[i])) * A[i]
  }
  
  # dataframe for log-log demand curves reshaped and relabeled for ggplot
  loglogCurves_gdf <- loglogCurves_df %>%
    arrange(., rate)
  colnames(loglogCurves_gdf)[2:4] <- paste0("Eta = ", eta)
  loglogCurves_gdf <- gather(loglogCurves_gdf, key = "parameter", value = "demand", -rate)
  
  
  
  ### semi-log Demand Function Calculations ###
  
  
  ### select parameter B such that curve passes thorough geometric mean of two series
  B <- rep(as.numeric(NA), times = 3) 
  
  # find values of B for each xi
  for (i in 1:length(xi)) {
    B[i] <- geomMean_m / exp(geomMean_r * -xi[i])
  }
  
  ### Calc values for semi-log demand curves for each xi
  semiLogCurves_df <- data.frame(rate = seq(0, 0.16, by = 0.001))            # intialize x-value for curves
  for (i in 1:length(xi)) {
    
    # function of log-log demand to be overlaid on plots
    semiLogCurves_df[[paste0("semiLogDemand_xi_", xi[i])]] <- exp(semiLogCurves_df$rate * -xi[i]) * B[i]
    
    # fitted log-log demand curve of data
    df[[paste0("fitted_semiLogDemand_xi_", xi[i])]] <- exp(df$rate * -xi[i]) * B[i]
  }
  
  # dataframe for semi-log demand curves for ggplot
  semiLogCurves_gdf <- semiLogCurves_df %>%
    arrange(., rate)
  colnames(semiLogCurves_gdf)[2:4] <- paste0("Xi = ", xi)
  semiLogCurves_gdf <- gather(semiLogCurves_gdf, key = "parameter", value = "demand", -rate)
  
  
  
  graph_scatter <- function(data_df, curves_df, title) {
    
    g <- ggplot() +
      theme_bw(20) + 
      geom_point(data = data_df, aes(x = rate, y = m1_gdp)) +
      # this add color to problem areas
      # geom_point(data = data_df, aes(x = rate, y = m1_gdp, fill = dotColor), shape = 21) +
      geom_line(data = curves_df, aes(x = rate, y = demand, color = parameter)) +
      scale_x_continuous(breaks = seq(0, 0.16, by = 0.02), 
                         limits = c(0, 0.16),
                         expand = c(0, 0.0025)
      ) +
      scale_y_continuous(breaks = seq(0.05, 0.55, by = 0.05), 
                         limits = c(0.05, 0.55),
                         expand = c(0, 0.01)
      ) +
      # scale_fill_manual(values = c("1945-1949" = "red",
      #                              "1979-1982" = "green",
      #                              "Data"      = "black"),
      #                   name = "",
      #                   aesthetics = "fill"
      #                   ) +
      # scale_fill_manual(values = c("red", "green", "black"),
      #               name = ""
      #               ) +
      theme(legend.title = element_blank()
      ) +
      ggtitle(title) + 
      labs(x       = "Interest Rate",
           y       = "M1 to GDP Ratio"
      )
  
    
    # Export
    ggsave(filename = paste0(path, output_dir, "Parametrization ", p, "/",
                             title, ".png"
                             ),
           plot     = g,
           width    = 12,
           height   = 8,
           units    = "in"
    )
    
    return(g)
  }
  
  
  
  graph_scatter(data_df   = df, 
                curves_df = loglogCurves_gdf,
                title     = paste0("Log-Log Money Demand ", yBeg, " - ", yEnd)
  ) 
  graph_scatter(data_df   = df, 
                curves_df = semiLogCurves_gdf,
                title     = paste0("Semi-Log Money Demand ", yBeg, " - ", yEnd)
  )
  
  ### Sum of Residuals^2 of Demand Curves ###
  resid_df <- df %>%
    mutate_at(., .vars = vars(contains("fitted")),
              .funs = list(
                residual = ~(. - m1_gdp)
              )
    ) %>%
    mutate_at(., .vars = vars(contains("residual")),
              .funs = list(
                sq = ~(. ^ 2)
              )
    ) %>%
    summarize_at(., .vars = vars(contains("residual_sq")),
                 .funs = sum
    ) %>%
    gather(key = "variable", value = "value") %>%
    separate(., col  = variable, 
             into = c("fit", "Demand", "Parameter", "elasticity", "resid", "sq"),
             sep  = "_"
    ) %>%
    select(., Demand, Parameter, elasticity, value)
  
  
  ### Calculate Welfare Cost of Autarky ###
  
  # find index of elasticity of best fitting demand curve
  idx_df <- resid_df %>%
    group_by(., Parameter) %>%
    summarise(., idx = which.min(value)) 
  
  etaidx <- idx_df[[1, 2]]
  xiidx <- idx_df[[2, 2]]
  
  # find demand curve with smallest residual
  idx_df <- resid_df %>%
    summarise(., idx = which.min(value))
 bestCurve <- resid_df[[idx_df[[1]], "Demand"]]
  
  
  # Log-Log Demand
  a <- A[[etaidx]]
  e <- eta[[etaidx]]
  # a <- A[[2]]
  # e <- eta[[2]]
  # make welfare function per parametrization
  welfare_loglog <- function(r) {
    a * (e / (1-e)) * r^(1-e)
  }
  welfare_loglog(.06)
  
  # Semi-Log Demand
  b <- B[[xiidx]]
  x <- xi[[xiidx]]
  # b <- B[[2]]
  # x <- xi[[2]]
  # make welfare function per parametrization
  welfare_semilog <- function(r) {
    (b / x)*(1 - (1 + (x*r))*exp(-x*r))
  }
  welfare_semilog(.06)
  
  
  # log-log
  welfareCost_loglog_df <- data.frame(rate = seq(0, 0.16, by = 0.001)) 
  welfareCost_loglog_df$parameter <- e
  welfareCost_loglog_df$type <- "Log-Log Demand"
  welfareCost_loglog_df$wc<- welfare_loglog(r = welfareCost_loglog_df[["rate"]])
  welfareCost_loglog_df$wc_minus3 <- 
    welfare_loglog(r = welfareCost_loglog_df[["rate"]]) - welfare_loglog(r = 0.03)
  
  # semi-log
  welfareCost_semilog_df <- data.frame(rate = seq(0, 0.16, by = 0.001)) 
  welfareCost_semilog_df$parameter <- x
  welfareCost_semilog_df$type <- "Semi-Log Demand"
  welfareCost_semilog_df$wc<- welfare_semilog(r = welfareCost_semilog_df[["rate"]])
  welfareCost_semilog_df$wc_minus3 <- 
    welfare_semilog(r = welfareCost_semilog_df[["rate"]]) - welfare_semilog(r = 0.03)
  
  welfare_cost_df <- bind_rows(welfareCost_loglog_df, welfareCost_semilog_df)
  rm(welfareCost_loglog_df, welfareCost_semilog_df)
  
  graphWC <- function(y) {
    
    # assign title
    if (y == "wc") {
      title = "Welfare Cost Functions"
    } else {
      title = "Welfare Cost Relative to r = 0.03"
    }
    
    
    g <- ggplot() +
      theme_bw(20) + 
      geom_line(data = welfare_cost_df, 
                aes(x = rate, y = !!sym(y), color = type),
                size = 1.5
                ) +
      scale_x_continuous(breaks = seq(0, 0.16, by = 0.02), 
                         limits = c(0, 0.16),
                         expand = c(0, 0.0025)
      ) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 8)
      ) +
      theme(legend.title    = element_blank(),
            legend.position = c(0.75, 0.2)
      ) +
      ggtitle("Inflation and Welfare") +
      labs(x = "Rate",
           y = "Fraction of Income",
           caption = paste0("Eta = ", e, " and xi = ", x)
      ) 
    
    # Export
    ggsave(filename = paste0(path, output_dir, "Parametrization ", p, "/",
                             title, ".png"
            ),
            plot     = g,
            width    = 12,
            height   = 8,
            units    = "in"
            )
    
    return(g)
  }
  
  graphWC(y = "wc")
  graphWC(y = "wc_minus3")
  
  
  wc_table <- welfare_cost_df %>%
    filter(., rate == 0.01 | 
              rate == 0.02 |
              rate == 0.03 |
              rate == 0.04 |
              rate == 0.05 |
              rate == 0.06 |
              rate == 0.07 |
              rate == 0.08 |
              rate == 0.09 |
              rate == 0.1 
             )
  
  # gather results into a list
  results <- list(
    "Geometric Mean r" = geomMean_r,
    "Geometric Mean m" = geomMean_m,
    "A"                = A,
    "B"                = B,
    "Best Eta"         = e,
    "Best Xi"          = x,
    "Best Demand Curve"= bestCurve,
    "Log-Log Curves"   = loglogCurves_df,
    "Semi-Log Curves"  = semiLogCurves_df,
    "Residuals"        = resid_df,
    "Welfare Cost"     = welfare_cost_df,
    "WC Table"         = wc_table
      )
  
  # append this parametrizations results to results_list
  results_list[[p]] <- results

}

### retrieve results for welfare cost of inflation for Ravi

# for (i in 1:length(results_list)) {
# 
#   # i = 1
# 
#   r <- results_list[[i]]
# 
#   wc_tab <- r[["WC Table"]]
#   wc_tab <- wc_tab %>%
#     select(., rate, type, wc) %>%
#     spread(., key   = type,
#               value = wc
#            )
# 
#   write.xlsx(as.data.frame(wc_tab),
#              row.names = FALSE,
#              sheet     = as.character(i),
#              file      = paste0(path, output_dir, "Welfare Cost Results.xlsx"),
#              append    = TRUE
#              )
# 
# }

rbind(lapply(results_list, function(r) {
  r[["Best Eta"]]
})
)

rbind(lapply(results_list, function(r) {
  r[["Best Xi"]]
})
)

rbind(lapply(results_list, function(r) {
  r[["Best Demand Curve"]]
})
)
lapply(results_list, function(r) {
  r[["Residuals"]]
})
