#library(devtools)
#devtools::install_github("ransiw/didwrappers")
library(didwrappers)
library(tidyverse)

filepath = "~/Dropbox/ColoradoSmokeGrades/ColoradoSmokeGit/Data/GradeDataforGitHub"
schoolspath = "~/Dropbox/ColoradoSmokeGrades/ColoradoSmokeGit/Data/GradeDataforGitHub"
didpath = "~/Dropbox/ColoradoSmokeGrades/ColoradoSmokeGit/DIDresults"

panelMath = read.csv(file.path(filepath, "PanelMath_all.csv"))

obsname = "Pct_Ptcp_"

panelMath = panelMath %>%
  mutate(unitid = paste0(District_Code,"_",School_Code_New,"_",Test)) %>%
  mutate(unitid = as.numeric(as.factor(unitid)))

# change to missing if obs_name is zero
panelMath <- panelMath %>%
  mutate(!!obsname := if_else(!!sym(obsname)==0 , NA, !!sym(obsname)))

panelMath_dropNAs = panelMath %>% 
  filter(!is.na(get(obsname))) %>%
  filter(System_Code=="Public")


panelMath_dropNAs = panelMath_dropNAs %>%
  group_by(District_Code,School_Code_New,Test) %>%
  mutate(has_2019 = sum(if_else(Year==2019,1,0),na.rm = TRUE),
         has_2021 = sum(if_else(Year==2021,1,0),na.rm = TRUE),
         has_2022 = sum(if_else(Year==2022,1,0),na.rm = TRUE),
         has_2023 = sum(if_else(Year==2023,1,0),na.rm = TRUE),
         has_2018 = sum(if_else(Year==2018,1,0),na.rm = TRUE),) %>%
  ungroup()

panelMath_dropNAs = panelMath_dropNAs %>%
  filter(has_2018==1 & has_2019==1 & has_2022==1 & has_2023==1)

# choose the panelMath settings
panelMath_dropNAs = panelMath_dropNAs %>%
  filter(Setting %in% c("Denver Metro", "Urban-Suburban"))

smokedf2021 = unique(panelMath_dropNAs[,c("District_Code", "School_Code", "all_smoke_2021")])
hist(smokedf2021$all_smoke_2021, n=20)

smokecontrol = 11

smokedf2022 = unique(panelMath_dropNAs[panelMath_dropNAs$all_smoke_2021<smokecontrol,c("District_Code", "School_Code", "all_smoke_2022")])
hist(smokedf2022$all_smoke_2022, n=20)

panelMath_dropNAs = panelMath_dropNAs %>%
  mutate(smoke_group = if_else(all_smoke_2021<smokecontrol & all_smoke_2022<smokecontrol,2024,NA))
panelMath_dropNAs = panelMath_dropNAs %>%
  mutate(smoke_group = if_else(is.na(smoke_group) & all_smoke_2021>=smokecontrol,2021,smoke_group))
panelMath_dropNAs = panelMath_dropNAs %>%
  mutate(smoke_group = if_else(is.na(smoke_group) & all_smoke_2022>=smokecontrol,2022,smoke_group))
table(panelMath_dropNAs$smoke_group, useNA = "ifany")

panelMath_dropNAs = panelMath_dropNAs %>%
  mutate(smoke_intensity_2021 = case_when(all_smoke_2021>20 ~ 20,
                                          all_smoke_2021>18 ~ 18,
                                          all_smoke_2021>16 ~ 16,
                                          all_smoke_2021>14 ~ 14,
                                          all_smoke_2021>12 ~ 12,
                                          all_smoke_2021>10 ~ 10,
                                          all_smoke_2021>8 ~ 8))

panelMath_dropNAs = panelMath_dropNAs %>%
  mutate(smoke_intensity_2022 = case_when(all_smoke_2022>20 ~ 20,
                                          all_smoke_2022>18 ~ 18,
                                          all_smoke_2022>16 ~ 16,
                                          all_smoke_2022>14 ~ 14,
                                          all_smoke_2022>12 ~ 12,
                                          all_smoke_2022>10 ~ 10,
                                          all_smoke_2022>8 ~ 8))


panelMath_dropNAs = panelMath_dropNAs %>%
  mutate(smoke_intensity = if_else(smoke_group==2021,smoke_intensity_2021,NA),
         smoke_intensity = if_else(smoke_group==2022,smoke_intensity_2022,smoke_intensity),
         smoke_intensity = if_else(smoke_group==2024,0,smoke_intensity))

table(panelMath_dropNAs$smoke_intensity, useNA = "ifany")

# drop if treated in 2022
panelMath_dropNAs = panelMath_dropNAs %>% 
  filter(smoke_group!=2022)

panelMath_dropNAs = panelMath_dropNAs %>% select(unitid,Year,Test,!!obsname,smoke_group,smoke_intensity,num20schools,Num_Records_2019)
panelMath_dropNAs = panelMath_dropNAs %>% arrange(unitid,Year)

# remove any observations before 2018
panelMath_dropNAs = panelMath_dropNAs %>% filter(Year>=2018)
panelMath_dropNAs = panelMath_dropNAs %>% arrange(unitid)

attobject = att_it(yname=obsname, tname="Year",idname= "unitid", gname = "smoke_group", xformla = ~ Num_Records_2019 + Test,
                   cohortnames = c("smoke_intensity", "Test"), data = panelMath_dropNAs, panel=FALSE, est_method = "ipw")


saveRDS(attobject, file.path(didpath, paste0("Math_","All_",obsname,"Urban.rds")))

attdf = attit_table(attobject)
attdf = attdf %>% left_join(distinct(panelMath_dropNAs,unitid,smoke_group,smoke_intensity,num20schools,Num_Records_2019) %>% rename(id=unitid))
write.csv(attdf,file.path(plotpath,paste0("Math_","All_",obsname,"_Urban.csv")))



attobject = att_it(yname=obsname, tname="Year",idname= "unitid", gname = "smoke_group", xformla = ~ Num_Records_2019,
                   cohortnames = c("smoke_intensity"), data = panelMath_dropNAs, panel=FALSE, est_method = "ipw")


agtobject = aggite(attobject, type = "unit", na.rm = TRUE)
agtunit = aggite_table(agtobject)
agtobject$overall.att
agtobject$overall.se
agtunit = agtunit %>% left_join(distinct(panelMath_dropNAs,unitid,smoke_group,smoke_intensity,num20schools,Num_Records_2019) %>% rename(egt=unitid))

agtobject = aggite(attobject, type = "smoke_intensity", na.rm = TRUE)
agtobject$overall.att
agtobject$overall.se
agtintensity = aggite_table(agtobject)

agtobject = aggite(attobject, type = "Test", na.rm = TRUE)
agtobject$overall.att
agtobject$overall.se
agttest = aggite_table(agtobject)

agtobject = aggite(attobject, type = "dynamic", na.rm = TRUE)
agtobject$overall.att
agtobject$overall.se
agtdynamic = aggite_table(agtobject)
write.csv(agtdynamic,file.path(plotpath,paste0("Math_","All_",obsname,"Urban_dynamic.csv")))
