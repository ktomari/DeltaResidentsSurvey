
source("~/Documents/Delta_Resident_Survey/helper_functions.R")

cool_colors <- c( "#FFFFBF", "#E6F598","#ABDDA4", "#66C2A5", "#C8F3FFFF","#E3D7F4FF" )
cool_colors2 <- c("#ABDDA4", "#66C2A5", "#C8F3FFFF","#E3D7F4FF" )

###
zips <- read_csv("/Users/sonra/Documents/Delta_Resident_Survey/Crosswalk-ZIP_to_TRACT_032021 - ZIP_TRACT_032021.csv")
stockton_zips = zips %>%
  filter(USPS_ZIP_PREF_STATE=='CA' & USPS_ZIP_PREF_CITY=='STOCKTON') %>%
  select(ZIP) %>% distinct()

survey_path = "/Users/sonra/Documents/Delta_Resident_Survey/collaborator_generic/"

### read in data

drs_data <- cdrs::cdrs_read(survey_path, 
                            relevel_ = "default")

### specific demographic groups that were statistically significant for certain Q7 responses
drs_data$data$over45 <- ifelse(drs_data$data$AGE_P %in% c("65+", "55-64", "45-54"), "over 45", ifelse(!is.na(drs_data$data$AGE_P),"under 45", NA))
drs_data$data$over35 <- ifelse(drs_data$data$AGE_P %in% c("65+", "55-64", "45-54", "35-44"), "over 35", ifelse(!is.na(drs_data$data$AGE_P),"under 35", NA))
drs_data$data$between25_44 <- ifelse(drs_data$data$AGE_P %in% c("25-34", "35-44"), "between 25 and 44", ifelse(!is.na(drs_data$data$AGE_P),"outside 25 and 44", NA)) 
drs_data$data$notwhite_white <- ifelse(drs_data$data$RACE_P=="White", "white", "notwhite")
drs_data$data$white_and_asain_PI <- ifelse(drs_data$data$RACE_P%in%c("White", "Asian/PI"), "White or Asian/PI", drs_data$data$RACE_P)

###make sure the age counts worked
drs_data$data %>% group_by(AGE_P, over45,over35, between25_44) %>% count()
drs_data$data %>% group_by(RACE_P, notwhite_white,white_and_asain_PI) %>% count()



### separate data by rural and urban/suburban and plot demos
rural_respondents = drs_data$data %>% 
  filter(Zone == 1)
nrow(rural_respondents)
rural_respondents %>% count(SEX_P) 
rural_respondents %>% 
  filter(!is.na(Q2) & Q2 != "<I don't know>") %>%
  count(Q2) %>%  mutate(percent = (n / sum(n)) * 100)

plot_demos(rural_respondents, "RACE_P", cool_colors2)
plot_demos(rural_respondents, "INCOME_P", cool_colors)
plot_demos(rural_respondents, "AGE_P", cool_colors)


urban_respondents = drs_data$data %>% 
  filter(Zone != 1)
nrow(urban_respondents)
urban_respondents %>% count(SEX_P)
plot_demos(urban_respondents, "RACE_P", cool_colors2)
plot_demos(urban_respondents, "INCOME_P", cool_colors)
plot_demos(urban_respondents, "AGE_P", cool_colors)

urban_respondents=urban_respondents %>%
  mutate(city = case_when(
    geoid.county %in% c("067","113") ~ "Sacramento",
    geoid.county == "013" ~ "East Bay",
    Zip %in% stockton_zips$ZIP ~ "Stockton",
    
    TRUE ~ "Other"
  ))

urban_respondents %>%
  count(city) %>%  mutate(percent = (n / sum(n)) * 100)

### specific city groups that were statistically significant for certain identity questions
urban_respondents$East_Bay <- ifelse(urban_respondents$city=="East Bay", "East Bay", "Other")
urban_respondents$Three_Cities <- ifelse(urban_respondents$city=="Other", "Other", "Three Cities")
urban_respondents$East_Bay_Stockton <- ifelse(urban_respondents$city%in% c("East Bay", "Stockton"), "East Bay and Stockton", "Other")
urban_respondents %>% group_by(city, East_Bay,Three_Cities,East_Bay_Stockton ) %>% count()


#Which of the following statements describe why YOU feel the Delta is important? 
svymean_yes_reponses(urban_respondents, "Q4", 1:5)
#Which factors, if any, do you personally value most about living in the Delta area?
svymean_yes_reponses(urban_respondents, "Q6", 1:9)
#Are you involved with any of the following groups or communities in the Delta? 
svymean_yes_reponses(urban_respondents, "Q40", 1:16)
#Do you engage in any of the following activities in the Delta? 
svymean_yes_reponses(urban_respondents, "Q8", 1:6)
#which factors... largest challenges to your personal well-being as a Delta resident. 
svymean_yes_reponses(urban_respondents, "Q7", 1:11)

####demographic differences for Q7
fn_chisq1("Q7_7", "notwhite_white", custom_data = urban_respondents)$chisq
calculate_proportions(urban_respondents, "Q7_7", "notwhite_white")

fn_chisq1("Q7_7", "between25_44", custom_data = urban_respondents)$chisq
calculate_proportions(urban_respondents, "Q7_7", "between25_44")

fn_chisq1("Q7_1", "RACE_P", custom_data = urban_respondents)$chisq
calculate_proportions(urban_respondents, "Q7_1", "RACE_P")
fn_chisq1("Q7_1", "white_and_asain_PI", custom_data = urban_respondents)$chisq
calculate_proportions(urban_respondents, "Q7_1", "white_and_asain_PI")
fn_chisq1("Q7_1", "ETHNICITY_P", custom_data = urban_respondents)$chisq
calculate_proportions(urban_respondents, "Q7_1", "ETHNICITY_P")


fn_chisq1("Q7_1", "over35", custom_data = urban_respondents)$chisq
calculate_proportions(urban_respondents, "Q7_1", "over35")


#Which of the following resources do you currently have access to? 
svymean_yes_reponses(urban_respondents, "Q24", 1:15)
#Have you or anyone in your home experienced any of the following impacts while living in the Delta? 
q12 <- svymean_yes_reponses(urban_respondents, "Q12", 1:7)


##5 excessive heat
create_circle_with_arc(round(100*q12[rownames(q12)=="Q12_5Yes","mean"]), 'indianred4') 
##6 air quality
create_circle_with_arc(round(100*q12[rownames(q12)=="Q12_6Yes","mean"]), 'indianred1') 
##7 water quality
create_circle_with_arc(round(100*q12[rownames(q12)=="Q12_7Yes","mean"]), 'indianred') 
##3 natural disaster
create_circle_with_arc(round(100*q12[rownames(q12)=="Q12_3Yes","mean"]), 'indianred2') 
##1 flooded property
create_circle_with_arc(round(100*q12[rownames(q12)=="Q12_1Yes","mean"]), 'indianred3') 

#How concerned are you about each of the following environmental changes affecting the Delta over the next 25 years?
urban_respondents_obj <- cdrs::cdrs_design(urban_respondents, set_fpc = TRUE)

response <- data.frame(survey::svymean(
  x =  as.formula(paste0("~",paste0("Q13", letters[1:7], collapse = "+"))),
  design = urban_respondents_obj,
  na.rm = TRUE
)) 
response <- response[grep("Very|Moderat", rownames(response)),]

response <- response %>%
  tibble::rownames_to_column(var = "rowname") %>%
  mutate(substring = substr(rowname, start = 1, stop = 4)) %>%
  select(-rowname) %>%
  group_by(substring) %>%
  summarize(sum_of_means = sum(mean))
response <-  data.frame(response)
response[order(response$sum_of_means, decreasing = T),]
#drought
create_circle_with_arc(round(response[response$substring=="Q13c", "sum_of_means"]*100), 'dodgerblue4') 
#air
create_circle_with_arc(round(response[response$substring=="Q13f", "sum_of_means"]*100), 'deepskyblue4') 
#wter
create_circle_with_arc(round(response[response$substring=="Q13g", "sum_of_means"]*100), 'dodgerblue3') 
#heat
create_circle_with_arc(round(response[response$substring=="Q13b", "sum_of_means"]*100), 'deepskyblue3') 
##floods
create_circle_with_arc(round(response[response$substring=="Q13d", "sum_of_means"]*100), 'dodgerblue2') 
##wildfire smoke



####city differences
fn_chisq1("Q3_1", "East_Bay_Stockton", custom_data = urban_respondents)$chisq
calculate_proportions(urban_respondents, "Q3_1", "East_Bay_Stockton")

fn_chisq1("Q3_2", "East_Bay", custom_data = urban_respondents)$chisq
calculate_proportions(urban_respondents, "Q3_2", "East_Bay")

fn_chisq1("Q3_8", "Three_Cities", custom_data = urban_respondents)$chisq
calculate_proportions(urban_respondents, "Q3_8", "Three_Cities")
calculate_proportions(urban_respondents, "Q3_8", "city")



######################################################################################
#### RURAL


#Which of the following statements describe why YOU feel the Delta is important? 
svymean_yes_reponses(rural_respondents, "Q4", 1:5)
#Which factors, if any, do you personally value most about living in the Delta area?
svymean_yes_reponses(rural_respondents, "Q6", 1:9)
#Are you involved with any of the following groups or communities in the Delta? 
svymean_yes_reponses(rural_respondents, "Q40", 1:16)
#Do you engage in any of the following activities in the Delta? 
svymean_yes_reponses(rural_respondents, "Q8", 1:6)
#he following... potential challenges to the well-being of Delta residents… 
#which factors... largest challenges to your personal well-being as a Delta resident. 
svymean_yes_reponses(rural_respondents, "Q7", 1:11)
# Which of the following resources do you currently have access to?
svymean_yes_reponses(rural_respondents, "Q24", 1:14)

####demographic differences
calculate_proportions(rural_respondents, "Q7_7", "RACE_P")

calculate_proportions(rural_respondents, "Q7_7", "AGE_P")

calculate_proportions(rural_respondents, "Q7_1", "RACE_P")

calculate_proportions(rural_respondents, "Q7_1", "AGE_P")

####demographic differences
fn_chisq1("Q7_2", "notwhite_white", custom_data = rural_respondents)$chisq
calculate_proportions(rural_respondents, "Q7_2", "notwhite_white")

fn_chisq1("Q7_6", "over45", custom_data = rural_respondents)$chisq
calculate_proportions(rural_respondents, "Q7_6", "over45")

fn_chisq1("Q7_3", "RACE_P", custom_data = rural_respondents)$chisq
calculate_proportions(rural_respondents, "Q7_3", "RACE_P")

fn_chisq1("Q7_1", "over35", custom_data = urban_respondents)$chisq
calculate_proportions(urban_respondents, "Q7_1", "over35")



#Which of the following resources do you currently have access to? 
svymean_yes_reponses(rural_respondents, "Q24", 1:15)



#Have you or anyone in your home experienced any of the following impacts while living in the Delta? 

q12 <-svymean_yes_reponses(rural_respondents, "Q12", 1:7)


##5 excessive heat
create_circle_with_arc(round(100*q12[rownames(q12)=="Q12_5Yes","mean"]), 'indianred4') 
##1 flooded property
create_circle_with_arc(round(100*q12[rownames(q12)=="Q12_1Yes","mean"]), 'indianred1') 
##7 water quality
create_circle_with_arc(round(100*q12[rownames(q12)=="Q12_7Yes","mean"]), 'indianred') 
##3 natural disaster
# create_circle_with_arc(round(100*q12[rownames(q12)=="Q12_3Yes","mean"]), 'indianred2') 
##6 air quality
create_circle_with_arc(round(100*q12[rownames(q12)=="Q12_6Yes","mean"]), 'indianred3') 

#How concerned are you about each of the following environmental changes affecting the Delta over the next 25 years?
rural_respondents_obj <- cdrs::cdrs_design(rural_respondents, set_fpc = TRUE)

response <- data.frame(survey::svymean(
  x =  as.formula(paste0("~",paste0("Q13", letters[1:7], collapse = "+"))),
  design = rural_respondents_obj,
  na.rm = TRUE
)) 
response <- response[grep("Very|Moderat", rownames(response)),]

response <- response %>%
  tibble::rownames_to_column(var = "rowname") %>%
  mutate(substring = substr(rowname, start = 1, stop = 4)) %>%
  select(-rowname) %>%
  group_by(substring) %>%
  summarize(sum_of_means = sum(mean))
response <- data.frame(response)
response[order(response$sum_of_means, decreasing = T),]

##water quality
create_circle_with_arc(round(response[response$substring=="Q13g", "sum_of_means"]*100), 'dodgerblue4') 
##droughts
create_circle_with_arc(round(response[response$substring=="Q13c", "sum_of_means"]*100), 'deepskyblue4') 
##floods
create_circle_with_arc(round(response[response$substring=="Q13d", "sum_of_means"]*100), 'dodgerblue3') 
##air quality
create_circle_with_arc(round(response[response$substring=="Q13f", "sum_of_means"]*100), 'deepskyblue3') 
##heat waves
create_circle_with_arc(round(response[response$substring=="Q13b", "sum_of_means"]*100), 'dodgerblue2') 
##wildfire smoke



### legacy towns
calculate_svymean(rural_respondents, "Zone", "==", 1, "Q2")
rural_legacy <- rural_respondents %>% filter(Q2 %in% c( "Rural (outside of town)","Historic or Delta \"legacy\" town"))


calculate_proportions(rural_legacy, "Q4_2", "Q2")
calculate_proportions(rural_legacy, "Q4_3", "Q2")
calculate_proportions(rural_legacy, "Q3_4", "Q2")

