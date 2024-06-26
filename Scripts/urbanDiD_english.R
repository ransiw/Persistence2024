#library(devtools)
#devtools::install_github("ransiw/didwrappers")
library(didwrappers)
library(tidyverse)

filepath = "~/Dropbox/ColoradoSmokeGrades/ColoradoSmokeGit/Data/GradeDataforGitHub"
schoolspath = "~/Dropbox/ColoradoSmokeGrades/ColoradoSmokeGit/Data/GradeDataforGitHub"
didpath = "~/Dropbox/ColoradoSmokeGrades/ColoradoSmokeGit/DIDresults"

#### Participation Rate ####

panelEnglish = read.csv(file.path(filepath, "PanelEnglish_all.csv"))

obsname = "Pct_Ptcp_"

panelEnglish = panelEnglish %>%
  mutate(unitid = paste0(District_Code,"_",School_Code_New,"_",Test)) %>%
  mutate(unitid = as.numeric(as.factor(unitid)))

# change to missing if obs_name is zero
panelEnglish <- panelEnglish %>%
  mutate(!!obsname := if_else(!!sym(obsname)==0 , NA, !!sym(obsname)))

panelEnglish_dropNAs = panelEnglish %>% 
  filter(!is.na(get(obsname))) %>%
  filter(System_Code=="Public")


panelEnglish_dropNAs = panelEnglish_dropNAs %>%
  group_by(District_Code,School_Code_New,Test) %>%
  mutate(has_2019 = sum(if_else(Year==2019,1,0),na.rm = TRUE),
         has_2021 = sum(if_else(Year==2021,1,0),na.rm = TRUE),
         has_2022 = sum(if_else(Year==2022,1,0),na.rm = TRUE),
         has_2023 = sum(if_else(Year==2023,1,0),na.rm = TRUE),
         has_2018 = sum(if_else(Year==2018,1,0),na.rm = TRUE),) %>%
  ungroup()

panelEnglish_dropNAs = panelEnglish_dropNAs %>%
  filter(has_2018==1 & has_2019==1 & has_2022==1 & has_2023==1)

# choose the panelEnglish settings
panelEnglish_dropNAs = panelEnglish_dropNAs %>%
  filter(Setting %in% c("Denver Metro", "Urban-Suburban"))

smokedf2021 = unique(panelEnglish_dropNAs[,c("District_Code", "School_Code", "all_smoke_2021")])
hist(smokedf2021$all_smoke_2021, n=20)

smokecontrol = 11

smokedf2022 = unique(panelEnglish_dropNAs[panelEnglish_dropNAs$all_smoke_2021<smokecontrol,c("District_Code", "School_Code", "all_smoke_2022")])
hist(smokedf2022$all_smoke_2022, n=20)

panelEnglish_dropNAs = panelEnglish_dropNAs %>%
  mutate(smoke_group = if_else(all_smoke_2021<smokecontrol & all_smoke_2022<smokecontrol,2024,NA))
panelEnglish_dropNAs = panelEnglish_dropNAs %>%
  mutate(smoke_group = if_else(is.na(smoke_group) & all_smoke_2021>=smokecontrol,2021,smoke_group))
panelEnglish_dropNAs = panelEnglish_dropNAs %>%
  mutate(smoke_group = if_else(is.na(smoke_group) & all_smoke_2022>=smokecontrol,2022,smoke_group))
table(panelEnglish_dropNAs$smoke_group, useNA = "ifany")

panelEnglish_dropNAs = panelEnglish_dropNAs %>%
  mutate(smoke_intensity_2021 = case_when(all_smoke_2021>20 ~ 20,
                                          all_smoke_2021>18 ~ 18,
                                          all_smoke_2021>16 ~ 16,
                                          all_smoke_2021>14 ~ 14,
                                          all_smoke_2021>12 ~ 12,
                                          all_smoke_2021>10 ~ 10,
                                          all_smoke_2021>8 ~ 8))

panelEnglish_dropNAs = panelEnglish_dropNAs %>%
  mutate(smoke_intensity_2022 = case_when(all_smoke_2022>20 ~ 20,
                                          all_smoke_2022>18 ~ 18,
                                          all_smoke_2022>16 ~ 16,
                                          all_smoke_2022>14 ~ 14,
                                          all_smoke_2022>12 ~ 12,
                                          all_smoke_2022>10 ~ 10,
                                          all_smoke_2022>8 ~ 8))


panelEnglish_dropNAs = panelEnglish_dropNAs %>%
  mutate(smoke_intensity = if_else(smoke_group==2021,smoke_intensity_2021,NA),
         smoke_intensity = if_else(smoke_group==2022,smoke_intensity_2022,smoke_intensity),
         smoke_intensity = if_else(smoke_group==2024,0,smoke_intensity))

table(panelEnglish_dropNAs$smoke_intensity, useNA = "ifany")

# drop if treated in 2022
panelEnglish_dropNAs = panelEnglish_dropNAs %>% 
  filter(smoke_group!=2022)

panelEnglish_dropNAs = panelEnglish_dropNAs %>% select(unitid,Year,Test,!!obsname,smoke_group,smoke_intensity,num20schools,Num_Records_2019)
panelEnglish_dropNAs = panelEnglish_dropNAs %>% arrange(unitid,Year)

# remove any observations before 2018
panelEnglish_dropNAs = panelEnglish_dropNAs %>% filter(Year>=2018)
panelEnglish_dropNAs = panelEnglish_dropNAs %>% arrange(unitid)

attobject = att_it(yname=obsname, tname="Year",idname= "unitid", gname = "smoke_group", xformla = ~ Num_Records_2019 + Test,
                   cohortnames = c("smoke_intensity", "Test"), data = panelEnglish_dropNAs, panel=FALSE, est_method = "ipw")


attdf = attit_table(attobject)
attdf = attdf %>% left_join(distinct(panelEnglish_dropNAs,unitid,smoke_group,Test,smoke_intensity,num20schools,Num_Records_2019) %>% rename(id=unitid))
#write.csv(attdf,file.path(didpath,paste0("English_","All_",obsname,"Urban.csv")))

agtobject = aggite(attobject, type = "dynamic", na.rm = TRUE)
agtobject$overall.att
agtobject$overall.se
agtdynamic = aggite_table(agtobject)
write.csv(agtdynamic,file.path(didpath,paste0("English_","All_",obsname,"Urban_dynamic.csv")), row.names = FALSE)

# Grade level

## Test 3
panelEnglish_grade = panelEnglish_dropNAs %>% filter(Test==3)

attobject_grade = att_it(yname=obsname, tname="Year",idname= "unitid", gname = "smoke_group", xformla = ~ Num_Records_2019,
                         cohortnames = c("smoke_intensity"), data = panelEnglish_grade, panel=FALSE, est_method = "ipw")

attdf_grade = attit_table(attobject_grade)
attdf_grade = attdf_grade %>% left_join(distinct(panelEnglish_grade,unitid,smoke_group,Test,smoke_intensity,num20schools,Num_Records_2019) %>% rename(id=unitid))

agtobject = aggite(attobject_grade, type = "dynamic", na.rm = TRUE)
agtobject$overall.att
agtobject$overall.se
agtdynamic = aggite_table(agtobject)
write.csv(agtdynamic,file.path(didpath,paste0("English_","All_",obsname,"Urban_Test3_dynamic.csv")), row.names = FALSE)


## Test 4
panelEnglish_grade = panelEnglish_dropNAs %>% filter(Test==4)

attobject_grade = att_it(yname=obsname, tname="Year",idname= "unitid", gname = "smoke_group", xformla = ~ Num_Records_2019,
                         cohortnames = c("smoke_intensity"), data = panelEnglish_grade, panel=FALSE, est_method = "ipw")

attdf_grade = attit_table(attobject_grade)
attdf_grade = attdf_grade %>% left_join(distinct(panelEnglish_grade,unitid,smoke_group,Test,smoke_intensity,num20schools,Num_Records_2019) %>% rename(id=unitid))

agtobject = aggite(attobject_grade, type = "dynamic", na.rm = TRUE)
agtobject$overall.att
agtobject$overall.se
agtdynamic = aggite_table(agtobject)
write.csv(agtdynamic,file.path(didpath,paste0("English_","All_",obsname,"Urban_Test4_dynamic.csv")), row.names = FALSE)


## Test 5
panelEnglish_grade = panelEnglish_dropNAs %>% filter(Test==5)

attobject_grade = att_it(yname=obsname, tname="Year",idname= "unitid", gname = "smoke_group", xformla = ~ Num_Records_2019,
                         cohortnames = c("smoke_intensity"), data = panelEnglish_grade, panel=FALSE, est_method = "ipw")

attdf_grade = attit_table(attobject_grade)
attdf_grade = attdf_grade %>% left_join(distinct(panelEnglish_grade,unitid,smoke_group,Test,smoke_intensity,num20schools,Num_Records_2019) %>% rename(id=unitid))

agtobject = aggite(attobject_grade, type = "dynamic", na.rm = TRUE)
agtobject$overall.att
agtobject$overall.se
agtdynamic = aggite_table(agtobject)
write.csv(agtdynamic,file.path(didpath,paste0("English_","All_",obsname,"Urban_Test5_dynamic.csv")), row.names = FALSE)


## Test 6
panelEnglish_grade = panelEnglish_dropNAs %>% filter(Test==6)

attobject_grade = att_it(yname=obsname, tname="Year",idname= "unitid", gname = "smoke_group", xformla = ~ Num_Records_2019,
                         cohortnames = c("smoke_intensity"), data = panelEnglish_grade, panel=FALSE, est_method = "ipw")

attdf_grade = attit_table(attobject_grade)
attdf_grade = attdf_grade %>% left_join(distinct(panelEnglish_grade,unitid,smoke_group,Test,smoke_intensity,num20schools,Num_Records_2019) %>% rename(id=unitid))

agtobject = aggite(attobject_grade, type = "dynamic", na.rm = TRUE)
agtobject$overall.att
agtobject$overall.se
agtdynamic = aggite_table(agtobject)
write.csv(agtdynamic,file.path(didpath,paste0("English_","All_",obsname,"Urban_Test6_dynamic.csv")), row.names = FALSE)



## Test 7
panelEnglish_grade = panelEnglish_dropNAs %>% filter(Test==7)

attobject_grade = att_it(yname=obsname, tname="Year",idname= "unitid", gname = "smoke_group", xformla = ~ Num_Records_2019,
                         cohortnames = c("smoke_intensity"), data = panelEnglish_grade, panel=FALSE, est_method = "ipw")

attdf_grade = attit_table(attobject_grade)
attdf_grade = attdf_grade %>% left_join(distinct(panelEnglish_grade,unitid,smoke_group,Test,smoke_intensity,num20schools,Num_Records_2019) %>% rename(id=unitid))

agtobject = aggite(attobject_grade, type = "dynamic", na.rm = TRUE)
agtobject$overall.att
agtobject$overall.se
agtdynamic = aggite_table(agtobject)
write.csv(agtdynamic,file.path(didpath,paste0("English_","All_",obsname,"Urban_Test7_dynamic.csv")), row.names = FALSE)


## Test 8
panelEnglish_grade = panelEnglish_dropNAs %>% filter(Test==8)

attobject_grade = att_it(yname=obsname, tname="Year",idname= "unitid", gname = "smoke_group", xformla = ~ Num_Records_2019,
                         cohortnames = c("smoke_intensity"), data = panelEnglish_grade, panel=FALSE, est_method = "ipw")

attdf_grade = attit_table(attobject_grade)
attdf_grade = attdf_grade %>% left_join(distinct(panelEnglish_grade,unitid,smoke_group,Test,smoke_intensity,num20schools,Num_Records_2019) %>% rename(id=unitid))

agtobject = aggite(attobject_grade, type = "dynamic", na.rm = TRUE)
agtobject$overall.att
agtobject$overall.se
agtdynamic = aggite_table(agtobject)
write.csv(agtdynamic,file.path(didpath,paste0("English_","All_",obsname,"Urban_Test8_dynamic.csv")), row.names = FALSE)


#### Competency Rate ####

panelEnglish = read.csv(file.path(filepath, "PanelEnglish_all.csv"))

obsname = "Pct_AorB_"

panelEnglish = panelEnglish %>%
  mutate(unitid = paste0(District_Code,"_",School_Code_New,"_",Test)) %>%
  mutate(unitid = as.numeric(as.factor(unitid)))

# change to missing if obs_name is zero
panelEnglish <- panelEnglish %>%
  mutate(!!obsname := if_else(!!sym(obsname)==0 , NA, !!sym(obsname)))

panelEnglish_dropNAs = panelEnglish %>% 
  filter(!is.na(get(obsname))) %>%
  filter(System_Code=="Public")


panelEnglish_dropNAs = panelEnglish_dropNAs %>%
  group_by(District_Code,School_Code_New,Test) %>%
  mutate(has_2019 = sum(if_else(Year==2019,1,0),na.rm = TRUE),
         has_2021 = sum(if_else(Year==2021,1,0),na.rm = TRUE),
         has_2022 = sum(if_else(Year==2022,1,0),na.rm = TRUE),
         has_2023 = sum(if_else(Year==2023,1,0),na.rm = TRUE),
         has_2018 = sum(if_else(Year==2018,1,0),na.rm = TRUE),) %>%
  ungroup()

panelEnglish_dropNAs = panelEnglish_dropNAs %>%
  filter(has_2018==1 & has_2019==1 & has_2022==1 & has_2023==1)

# choose the panelEnglish settings
panelEnglish_dropNAs = panelEnglish_dropNAs %>%
  filter(Setting %in% c("Denver Metro", "Urban-Suburban"))

smokedf2021 = unique(panelEnglish_dropNAs[,c("District_Code", "School_Code", "all_smoke_2021")])
hist(smokedf2021$all_smoke_2021, n=20)

smokecontrol = 11

smokedf2022 = unique(panelEnglish_dropNAs[panelEnglish_dropNAs$all_smoke_2021<smokecontrol,c("District_Code", "School_Code", "all_smoke_2022")])
hist(smokedf2022$all_smoke_2022, n=20)

panelEnglish_dropNAs = panelEnglish_dropNAs %>%
  mutate(smoke_group = if_else(all_smoke_2021<smokecontrol & all_smoke_2022<smokecontrol,2024,NA))
panelEnglish_dropNAs = panelEnglish_dropNAs %>%
  mutate(smoke_group = if_else(is.na(smoke_group) & all_smoke_2021>=smokecontrol,2021,smoke_group))
panelEnglish_dropNAs = panelEnglish_dropNAs %>%
  mutate(smoke_group = if_else(is.na(smoke_group) & all_smoke_2022>=smokecontrol,2022,smoke_group))
table(panelEnglish_dropNAs$smoke_group, useNA = "ifany")

panelEnglish_dropNAs = panelEnglish_dropNAs %>%
  mutate(smoke_intensity_2021 = case_when(all_smoke_2021>20 ~ 20,
                                          all_smoke_2021>18 ~ 18,
                                          all_smoke_2021>16 ~ 16,
                                          all_smoke_2021>14 ~ 14,
                                          all_smoke_2021>12 ~ 12,
                                          all_smoke_2021>10 ~ 10,
                                          all_smoke_2021>8 ~ 8))

panelEnglish_dropNAs = panelEnglish_dropNAs %>%
  mutate(smoke_intensity_2022 = case_when(all_smoke_2022>20 ~ 20,
                                          all_smoke_2022>18 ~ 18,
                                          all_smoke_2022>16 ~ 16,
                                          all_smoke_2022>14 ~ 14,
                                          all_smoke_2022>12 ~ 12,
                                          all_smoke_2022>10 ~ 10,
                                          all_smoke_2022>8 ~ 8))


panelEnglish_dropNAs = panelEnglish_dropNAs %>%
  mutate(smoke_intensity = if_else(smoke_group==2021,smoke_intensity_2021,NA),
         smoke_intensity = if_else(smoke_group==2022,smoke_intensity_2022,smoke_intensity),
         smoke_intensity = if_else(smoke_group==2024,0,smoke_intensity))

table(panelEnglish_dropNAs$smoke_intensity, useNA = "ifany")

# drop if treated in 2022
panelEnglish_dropNAs = panelEnglish_dropNAs %>% 
  filter(smoke_group!=2022)

panelEnglish_dropNAs = panelEnglish_dropNAs %>% select(unitid,Year,Test,!!obsname,smoke_group,smoke_intensity,num20schools,Num_Records_2019)
panelEnglish_dropNAs = panelEnglish_dropNAs %>% arrange(unitid,Year)

# remove any observations before 2018
panelEnglish_dropNAs = panelEnglish_dropNAs %>% filter(Year>=2018)
panelEnglish_dropNAs = panelEnglish_dropNAs %>% arrange(unitid)

# the unit 1421 causes standard errors to blow out of proportion
panelEnglish_dropNAs = panelEnglish_dropNAs %>% filter(unitid!=1421)
# The school name is Peak Expeditionary - Pennington

attobject = att_it(yname=obsname, tname="Year",idname= "unitid", gname = "smoke_group", xformla = ~ Num_Records_2019 + Test,
                   cohortnames = c("smoke_intensity", "Test"), data = panelEnglish_dropNAs, panel=FALSE, est_method = "ipw")


attdf = attit_table(attobject)
attdf = attdf %>% left_join(distinct(panelEnglish_dropNAs,unitid,smoke_group,Test,smoke_intensity,num20schools,Num_Records_2019) %>% rename(id=unitid))
#write.csv(attdf,file.path(didpath,paste0("English_","All_",obsname,"Urban.csv")))

agtobject = aggite(attobject, type = "dynamic", na.rm = TRUE)
agtobject$overall.att
agtobject$overall.se
agtdynamic = aggite_table(agtobject)
write.csv(agtdynamic,file.path(didpath,paste0("English_","All_",obsname,"Urban_dynamic.csv")), row.names = FALSE)

# Grade level

## Test 3
panelEnglish_grade = panelEnglish_dropNAs %>% filter(Test==3)

attobject_grade = att_it(yname=obsname, tname="Year",idname= "unitid", gname = "smoke_group", xformla = ~ Num_Records_2019,
                         cohortnames = c("smoke_intensity"), data = panelEnglish_grade, panel=FALSE, est_method = "ipw")

attdf_grade = attit_table(attobject_grade)
attdf_grade = attdf_grade %>% left_join(distinct(panelEnglish_grade,unitid,smoke_group,Test,smoke_intensity,num20schools,Num_Records_2019) %>% rename(id=unitid))

agtobject = aggite(attobject_grade, type = "dynamic", na.rm = TRUE)
agtobject$overall.att
agtobject$overall.se
agtdynamic = aggite_table(agtobject)
write.csv(agtdynamic,file.path(didpath,paste0("English_","All_",obsname,"Urban_Test3_dynamic.csv")), row.names = FALSE)


## Test 4
panelEnglish_grade = panelEnglish_dropNAs %>% filter(Test==4)

attobject_grade = att_it(yname=obsname, tname="Year",idname= "unitid", gname = "smoke_group", xformla = ~ Num_Records_2019,
                         cohortnames = c("smoke_intensity"), data = panelEnglish_grade, panel=FALSE, est_method = "ipw")

attdf_grade = attit_table(attobject_grade)
attdf_grade = attdf_grade %>% left_join(distinct(panelEnglish_grade,unitid,smoke_group,Test,smoke_intensity,num20schools,Num_Records_2019) %>% rename(id=unitid))

agtobject = aggite(attobject_grade, type = "dynamic", na.rm = TRUE)
agtobject$overall.att
agtobject$overall.se
agtdynamic = aggite_table(agtobject)
write.csv(agtdynamic,file.path(didpath,paste0("English_","All_",obsname,"Urban_Test4_dynamic.csv")), row.names = FALSE)


## Test 5
panelEnglish_grade = panelEnglish_dropNAs %>% filter(Test==5)

attobject_grade = att_it(yname=obsname, tname="Year",idname= "unitid", gname = "smoke_group", xformla = ~ Num_Records_2019,
                         cohortnames = c("smoke_intensity"), data = panelEnglish_grade, panel=FALSE, est_method = "ipw")

attdf_grade = attit_table(attobject_grade)
attdf_grade = attdf_grade %>% left_join(distinct(panelEnglish_grade,unitid,smoke_group,Test,smoke_intensity,num20schools,Num_Records_2019) %>% rename(id=unitid))

agtobject = aggite(attobject_grade, type = "dynamic", na.rm = TRUE)
agtobject$overall.att
agtobject$overall.se
agtdynamic = aggite_table(agtobject)
write.csv(agtdynamic,file.path(didpath,paste0("English_","All_",obsname,"Urban_Test5_dynamic.csv")), row.names = FALSE)


## Test 6
panelEnglish_grade = panelEnglish_dropNAs %>% filter(Test==6)

attobject_grade = att_it(yname=obsname, tname="Year",idname= "unitid", gname = "smoke_group", xformla = ~ Num_Records_2019,
                         cohortnames = c("smoke_intensity"), data = panelEnglish_grade, panel=FALSE, est_method = "ipw")

attdf_grade = attit_table(attobject_grade)
attdf_grade = attdf_grade %>% left_join(distinct(panelEnglish_grade,unitid,smoke_group,Test,smoke_intensity,num20schools,Num_Records_2019) %>% rename(id=unitid))

agtobject = aggite(attobject_grade, type = "dynamic", na.rm = TRUE)
agtobject$overall.att
agtobject$overall.se
agtdynamic = aggite_table(agtobject)
write.csv(agtdynamic,file.path(didpath,paste0("English_","All_",obsname,"Urban_Test6_dynamic.csv")), row.names = FALSE)



## Test 7
panelEnglish_grade = panelEnglish_dropNAs %>% filter(Test==7)

attobject_grade = att_it(yname=obsname, tname="Year",idname= "unitid", gname = "smoke_group", xformla = ~ Num_Records_2019,
                         cohortnames = c("smoke_intensity"), data = panelEnglish_grade, panel=FALSE, est_method = "ipw")

attdf_grade = attit_table(attobject_grade)
attdf_grade = attdf_grade %>% left_join(distinct(panelEnglish_grade,unitid,smoke_group,Test,smoke_intensity,num20schools,Num_Records_2019) %>% rename(id=unitid))

agtobject = aggite(attobject_grade, type = "dynamic", na.rm = TRUE)
agtobject$overall.att
agtobject$overall.se
agtdynamic = aggite_table(agtobject)
write.csv(agtdynamic,file.path(didpath,paste0("English_","All_",obsname,"Urban_Test7_dynamic.csv")), row.names = FALSE)


## Test 8
panelEnglish_grade = panelEnglish_dropNAs %>% filter(Test==8)

attobject_grade = att_it(yname=obsname, tname="Year",idname= "unitid", gname = "smoke_group", xformla = ~ Num_Records_2019,
                         cohortnames = c("smoke_intensity"), data = panelEnglish_grade, panel=FALSE, est_method = "ipw")

attdf_grade = attit_table(attobject_grade)
attdf_grade = attdf_grade %>% left_join(distinct(panelEnglish_grade,unitid,smoke_group,Test,smoke_intensity,num20schools,Num_Records_2019) %>% rename(id=unitid))

agtobject = aggite(attobject_grade, type = "dynamic", na.rm = TRUE)
agtobject$overall.att
agtobject$overall.se
agtdynamic = aggite_table(agtobject)
write.csv(agtdynamic,file.path(didpath,paste0("English_","All_",obsname,"Urban_Test8_dynamic.csv")), row.names = FALSE)


