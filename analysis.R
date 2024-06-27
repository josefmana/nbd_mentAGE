# This script runs and summarise regression analyses comparing MR to HC groups with matched mental ages

rm( list = ls() ) # clear environment

# load packages
library(here)
library(tidyverse)
library(performance)


# IN-HOUSE FUNCTIONS ----

rprint <- function(x, d = 2) sprintf( paste0("%.",d,"f"), round(x, d) )
zerolead <- function(x, d = 3) ifelse( x < .001, "< .001", sub("0.", ".", rprint(x, 3), fixed = T) )
msd <- function(x, d = 2) paste0( rprint( mean(x, na.rm = T), d ), " ± ", rprint( sd(x, na.rm = T), d ) )
mse <- function(x, d = 2) paste0( rprint( x["Estimate"], d ), " (", rprint( x["Std. Error"], d ), ")" )


# DATA READ ----

vars <- read.csv(here("_data","vars.csv"), sep =";") # variables helper
d1 <- read.csv(here("_data","long_df.csv"), sep = ",") # full data set
wisc_tests <- sub( "_MV", "", names(d1)[ grepl( "_MV", names(d1) ) ] ) # extract WISC subtests names

# matched data sets
d2 <- lapply(
  
  setNames(wisc_tests, wisc_tests),
  function(i)
    
    read.csv( here( "_data", paste0("wide_df_matched_by_",i,".csv") ), sep = "," )
  
)


# DESCRIPTION ----

# print a descriptive table of ages
write.table(
  
  x = cbind.data.frame(
    
    sapply( unique(d1$group), function(i) sapply( wisc_tests, function(j) msd( subset(d2[[j]], group == i)$mental_age, d = 2) ) ),
    MR_cage = sapply( wisc_tests, function(i) msd( subset(d2[[i]], group == "MR")$age ) )
    
  ) %>%
    
    rename( "HC_age" = "HC", "MR_mage" = "MR") %>%
    rownames_to_column("matched_by"),
  
  file = here("tabs","matched_ages.csv"),
  sep = ",",
  row.names = F,
  quote = F
  
)

# extract and save mean test scores per group & matching
write.table(
  
  x = sapply(
    
    vars$index,
    function(i)
      
      c( MR = msd( subset(d2$DO, group == "MR")[ , i], d = 2 ),
         sapply( wisc_tests, function(j) msd( subset(d2[[j]], group == "HC")[ , i], d = 2 ) )
        )
    
    
  ) %>%
    
    t() %>%
    as.data.frame() %>%
    rename_at( -1, ~ paste0("HC_",.x) ) %>%
    rownames_to_column("index") %>%
    mutate( test = unlist( sapply( 1:nrow(.), function(i) vars[ vars$index == index[i], "test"] ) , use.names = F ), .before = 1 ),
  
  file = here("tabs","matched_test_scores.csv"),
  sep = ",",
  row.names = F,
  quote = F
  
)


# VISUALISATION ----

# loop through all outcomes of interest
for ( i in vars$index ) {
  
  # plot it
  lapply( names(d2), function(j) d2[[j]] %>% mutate(match = j) ) %>%
    do.call( rbind.data.frame, . ) %>%
    ggplot() +
    aes( x = mental_age, y = get(i), colour = group, fill = group ) +
    geom_point(size = 2) +
    geom_smooth(linewidth = 1.25, method = "lm", alpha = .2) +
    labs(y = i) +
    facet_wrap( ~match, nrow = 3 ) +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom")
  
  # save it
  ggsave(
    plot = last_plot(),
    filename = here( "figs", paste0("_",i,".jpg") ), # begin each one with an underscore to keep them out of GitHub (too big)
    dpi = 300,
    width = 10,
    height = 10
  )
  
}

# zip the figures (add the propensity matching figures as well to the zip)
zip(
  zipfile = here("figs","_interaction_plots.zip"),
  files = c( "figs/propensity_score_matching.jpg", paste0( "figs/", list.files("figs")[ grepl( "X", list.files("figs") ) ] ) )
)

# remove the plots from the zip (apart from the propensity matching one)
file.remove( here( "figs", list.files("figs")[ grepl( "X", list.files("figs") ) ] ) )


# REGRESSIONS ----

## fitting ----

# fit regressions for each pair test score/matching
fit <- lapply(
  
  with( vars, setNames(index, index) ),
  function(i)
    
    lapply(
      
      setNames(wisc_tests, wisc_tests),
      function(j)
        
        lm( formula = as.formula( paste0(i," ~ 1 + MR * mental_age") ), data = d2[[j]] )
      
    )
)


## fit diagnosis ----

write.table(
  
  x = lapply(
    
    vars$index,
    function(i)
      
      sapply(
        
        wisc_tests,
        function(j)
          
          c( p_breusch_pagan = c( check_heteroscedasticity(fit[[i]][[j]]) ),
             n_cook = sum( check_outliers(fit[[i]][[j]]) ),
             p_shapiro_wilk = c( check_normality(fit[[i]][[j]]) ),
             p_durbin_watson = c( check_autocorrelation(fit[[i]][[j]]) )
             )
        
      ) %>%
      
      t() %>%
      as.data.frame() %>%
      rownames_to_column("match") %>%
      mutate(outcome = i, .before = 1)

  ) %>%
    
    do.call( rbind.data.frame, . ) %>%
    mutate( heteroscedasticity = ifelse(p_breusch_pagan < .05, "*", NA), .after = p_breusch_pagan ) %>%
    mutate( nonnormality = ifelse(p_shapiro_wilk < .05, "*", NA), .after = p_shapiro_wilk ) %>%
    mutate( autocorrelation = ifelse(p_durbin_watson < .05, "*", NA), .after = p_durbin_watson ) %>%
    mutate( across( starts_with("p_"), zerolead ) ),
  
  file = here("tabs","matched_regression_diagnosis.csv"),
  sep = ",",
  row.names = F,
  quote = F,
  na = ""
  
)

## results extraction ----

# extract and save regression coefficients
tabreg <- lapply(
  
  vars$index,
  function(i)
    
    sapply(
      
      c("(Intercept)","MR","mental_age","MR:mental_age"),
      function(x)
        
        sapply(
          
          wisc_tests,
          function(j)
            mse( summary( fit[[i]][[j]] )$coefficients[x, ] )
          
        )
      
    ) %>%
    
    as.data.frame() %>%
    rownames_to_column("match") %>%
    mutate(outcome = i, .before = 1)
  
) %>%
  
  do.call( rbind.data.frame, . )

# save it
write.table( x = tabreg, file = here("tabs","matched_regression_coefficients.csv"), sep = ",", row.names = F, quote = F )

# add more sophistacet table including statistical tests as well
write.table(
  
  x =
    tabreg %>%
    pivot_longer( cols = c("(Intercept)","MR","mental_age","MR:mental_age"), names_to = "parameter", values_to = "b (SE)" ) %>%
    mutate(
      t = unlist( sapply( 1:nrow(.), function(i) ifelse( parameter[i] == "(Intercept)", NA, rprint( summary( fit[[outcome[i]]][[match[i]]] )$coefficients[ parameter[i], "t value" ], d = 3 ) ) ), use.names = F ),
      p = unlist( sapply( 1:nrow(.), function(i) ifelse( parameter[i] == "(Intercept)", NA, summary( fit[[outcome[i]]][[match[i]]] )$coefficients[ parameter[i], "Pr(>|t|)" ] ) ), use.names = F ),
      sig = ifelse( p < .05, "*", NA ),
      sig_fwer = ifelse( p < .05/sum( !is.na(p) ), "*", NA ),
      p = zerolead(p)
    ),
  
  file = here("tabs","matched_regression_summaries.csv"),
  sep = ",",
  row.names = F,
  quote = F,
  na = ""
  
)

