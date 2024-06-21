# This script serves for purposes of data pre-processing.

rm( list = ls() ) # clear environment

# load packages
library(here)
library(tidyverse)
library(MatchIt)

# prepare data folders for the outcomes
sapply( "_data", function(i) if( !dir.exists(i) ) dir.create(i) )


# DATA READ ----

# variables helper
vars <- read.csv( here("_raw","vars.csv"), sep =";" )

# read data sets
d0 <- lapply(
  
  setNames( c("HC","MR"), c("HC","MR") ),
  function(i)
    
    lapply(
     
      setNames( list.files( here("_raw",i) ), sub( ".csv", "", list.files( here("_raw",i) ) ) ),
      function(j)
        read.csv( here("_raw",i,j), sep = "\t" )

    )
)

# manually sort the data where needed
for ( i in names(d0$HC)[ grepl("wasi|WASI",names(d0$HC)) ] ) d0$HC[[i]] <- NULL

# drop unusable rows in MR data sets
for ( i in names(d0$MR) ) d0$MR[[i]] <- d0$MR[[i]][ -( rownames( d0$MR[[i]][ d0$MR[[i]]$kod_ditete == "LMR32", ] ) : nrow( d0$MR[[i]] ) ) , ]

# rename columns where needed
d0$HC$`6_trideni` <- d0$HC$`6_trideni` %>% rename( "n_new_rules" = "n_new_rules...2", "n_correct_rules" = "n_correct_rules...3" )
d0$MR$`0_anamneza` <- d0$MR$`0_anamneza` %>% rename( "vek_roky" = "vek_roky_NBD" )
d0$MR$`2_pamet_na_pribehy_OKAMZITE` <- d0$MR$`2_pamet_na_pribehy_OKAMZITE` %>% mutate( sum_1_2 = pribeh1_ok_sum + pribeh2_ok_sum )
d0$MR$`2_pamet_na_pribehy_ODDALENE` <- d0$MR$`2_pamet_na_pribehy_ODDALENE` %>% mutate( sum_1_2 = pribeh1_odd_sum1 + pribeh2_odd_sum )
d0$MR$`8_pracovni_pamet` <- d0$MR$`8_pracovni_pamet` %>% rename( "sumaHS" = "suma_HS" )
for ( i in c("16_zrakove_motoricka_presnost","17_zrakove_vnimani","19_orientace_v_prostoru") ) d0$MR[[i]] <- d0$MR[[i]] %>% rename("cas_adm_sek" = "cas_adm")
for ( i in names(d0) ) d0[[i]]$`9_verbalni_fluence` <- d0[[i]]$`9_verbalni_fluence` %>% mutate( KV_sum = K_sum + V_sum )

# calculate nonverbal memory scores for controls
d0$HC$`3_neverbalni_pamet` <- d0$HC$`3_neverbalni_pamet` %>%
  
  mutate(
    hs1 = rowSums( across( starts_with("pok1_pol"), ~ case_when( grepl(0,.x) ~ 2, grepl(1,.x) ~ 1, .default = 0 ) ) ),
    hs2 = rowSums( across( starts_with("pok2_pol"), ~ case_when( grepl(0,.x) ~ 2, grepl(1,.x) ~ 1, .default = 0 ) ) ),
    hs3 = rowSums( across( starts_with("pok3_pol"), ~ case_when( grepl(0,.x) ~ 2, grepl(1,.x) ~ 1, .default = 0 ) ) ),
    hs4 = rowSums( across( starts_with("pok4_pol"), ~ case_when( grepl(0,.x) ~ 2, grepl(1,.x) ~ 1, .default = 0 ) ) ),
    odd_sum = rowSums( across( starts_with("odd_pol"), ~ case_when( grepl(0,.x) ~ 2, grepl(1,.x) ~ 1, .default = 0 ) ) ),
    P1.4_HS = hs1 + hs2 + hs3 + hs4
  )

# calculate nonverbal memory scores for MRs
d0$MR$`3_neverbalni_pamet` <- d0$MR$`3_neverbalni_pamet` %>%
  
  mutate( across( starts_with("pok"), as.numeric ) ) %>%
  mutate( across( starts_with("odd"), as.numeric ) ) %>%
  mutate(
    hs1 = rowSums( across( starts_with("pok1_pol"), ~ case_when( .x == 0 ~ 2, .x == 1 ~ 1, .default = 0 ) ) ),
    hs2 = rowSums( across( starts_with("pok2_pol"), ~ case_when( .x == 0 ~ 2, .x == 1 ~ 1, .default = 0 ) ) ),
    hs3 = rowSums( across( starts_with("pok3_pol"), ~ case_when( .x == 0 ~ 2, .x == 1 ~ 1, .default = 0 ) ) ),
    hs4 = rowSums( across( starts_with("pok4_pol"), ~ case_when( .x == 0 ~ 2, .x == 1 ~ 1, .default = 0 ) ) ),
    odd_sum = rowSums( across( starts_with("odd_pol"), ~ case_when( .x == 0 ~ 2, .x == 1 ~ 1, .default = 0 ) ) ),
    P1.4_HS = hs1 + hs2 + hs3 + hs4
  )

# change variables file appropriately
vars <- vars %>%
  
  mutate(
    variable = case_when(
      variable == "n_new_rules...2" ~ "n_new_rules",
      variable == "n_correct_rules...3" ~ "n_correct_rules",
      variable == "cas_adm (s)" ~ "cas_adm_sek",
      .default = variable
    ),
    type = ifelse( variable == "KV_sum", "cont", type ),
    test = ifelse( variable == "KV_sum", "9_verbalni_fluence", test )
  ) %>%
  
  filter( !grepl("err_",variable) ) %>% # emotion labelling errors
  filter( test != "3_neverbalni_pamet" ) %>% # get rid of original nonverbal memory variables
  add_row( variable = "P1.4_HS", type = "cont", test = "3_neverbalni_pamet", name = NA ) %>% # return nonverbal memory
  add_row( variable = "odd_sum", type = "cont", test = "3_neverbalni_pamet", name = NA ) %>% # return nonverbal memory delayed recall
  filter( !( variable == "HS" & test == "5_zrakova_pozornost" ) ) %>%
  filter( !( type == "cat" & variable != "gender" ) )

# check which variables do not match
lapply(
  
  setNames( names(d0), names(d0) ),
  function(i)
    
    lapply(
      
      setNames( names(d0[[i]]), names(d0[[i]]) ),
      function(j)
        
        cbind(
          c( na.omit( with( vars, variable[ test == j ] ) ) ),
          na.omit( with( vars, variable[ test == j ] ) ) %in% colnames(d0[[i]][[j]])
        )
      
    ) %>% do.call( rbind.data.frame, . )
  
) %>%
  
  do.call( rbind.data.frame, . ) %>%
  filter( V2 == FALSE )

## for now drop "6_trideni" data sets until I have raw scores for MR ----
for ( i in names(d0) ) d0[[i]][["6_trideni"]] <- NULL

# reformat where needed
d0$MR$`1_verbalni_pamet_a_uceni` <- d0$MR$`1_verbalni_pamet_a_uceni` %>% mutate( across( all_of( with( vars, variable[test == "1_verbalni_pamet_a_uceni"] ) ), as.numeric ) )
d0$MR$`16_zrakove_motoricka_presnost` <- d0$MR$`16_zrakove_motoricka_presnost` %>% mutate(cas_adm_sek = as.numeric(cas_adm_sek) )

# prepare a full long data set
d1 <- lapply(
  
  setNames( names(d0), names(d0) ),
  function(i)
    
    lapply(
      
      unique(vars$test)[-c(1,7)],
      function(j) {
        
        print( paste0("pre-processing ", j, " in ", i, " ...") ) # printing to diagnose mistakes
        
        d0[[i]][[j]] %>%
          select( all_of( c( "kod_ditete", with( vars, variable[test == j] ) ) ) ) %>%
          filter( !is.na(kod_ditete) ) %>%
          filter( !( kod_ditete %in% names( table(d0[[i]][[j]]$kod_ditete) )[ table(d0[[i]][[j]]$kod_ditete) > 1 ] ) ) %>% # drop duplicated records
          mutate( test = j, group = i ) %>%
          pivot_longer(
            cols = all_of( with( vars, variable[test == j] ) ),
            names_to = "index",
            values_to = "score"
          )
        
      }
    ) %>%
    
    do.call( rbind.data.frame, . ) %>%
    left_join( d0[[i]]$`0_anamneza`[ , c("kod_ditete","vek_roky","gender") ], by = "kod_ditete" ) # add demographics
    
) %>%
  
  # put it all together
  do.call( rbind.data.frame, . ) %>%
  left_join( d0$MR$WISC %>% select( kod_ditete, ends_with("IQ"), ends_with("_MV") ), by = "kod_ditete" ) %>%
  
  # format it
  rename(
    "id" = "kod_ditete",
    "age_years" = "vek_roky",
    "sex" = "gender"
  ) %>%
  
  # finish it
  mutate( sex = case_when( sex == "1=zena" ~ "female", sex == "0=muz" ~ "male" ) ) %>%
  mutate( index = paste(test, index, sep = "_") )

# pivot the data set to wider for propensity matching
d2 <- d1 %>%

  select(-test) %>%
  pivot_wider(
    names_from = index,
    values_from = score
  )

# do the matching
d3 <- lapply(
  
  setNames( colnames(d2)[ grepl( "MV", colnames(d2) ) ], sub( "_MV", "", colnames(d2)[ grepl( "MV", colnames(d2) ) ] ) ),
  function(i)
    
    d2 %>%
    mutate(
      MR = ifelse(group == "HC", 0, 1),
      mental_age = ifelse( group == "HC", age_years, get(i) )
    ) %>%
    filter( complete.cases(mental_age) ) %>%
    matchit(
      MR ~ mental_age,
      data = . ,
      method = "nearest",
      distance = "glm"
    ) %>%
    match.data()
  
) 

# show density plots of matched data
lapply( names(d3), function(i) d3[[i]] %>% mutate(matching_var = i) ) %>%
  do.call( rbind.data.frame, . ) %>%
  ggplot() +
  aes(x = mental_age, colour = group) +
  geom_density(linewidth = 1.5) +
  labs(y = NULL, x = "Mental age (years)") +
  scale_colour_manual( values = c("grey","black") ) +
  facet_wrap( ~ matching_var, scales = "free" ) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom", panel.grid = element_blank() )

