

dep_struct <- read_csv("~/Policy/CAMG/Dynamic Pricing/DEPABMMD.csv")
zet_survey <- readxl::read_xlsx("~/Policy/SurveyDataAndAnalysis/Data/ZET_survey_2024_values.xlsx",sheet=1)
zet_labels <- readxl::read_xlsx("~/Policy/SurveyDataAndAnalysis/Data/ZET_survey_2024_data_labels.xlsx",sheet=1)



dep_questions <- read_csv("~/Policy/CAMG/Dynamic Pricing/dep_survey_questions.csv")
dep_qanda <- read_csv("~/Policy/SurveyDataAndAnalysis/Data/ZET_survey_2024_qanda.csv")
dep_qanda <- dep_qanda %>% filter(question_code %in% dep_questions$question_code) %>% mutate(across(where(is.numeric), ~ na_if(na_if(.x, -99), -98)))


dep_survey <- zet_survey %>% select(all_of(dep_questions$question_code)) %>% mutate(across(where(is.numeric), ~ na_if(na_if(.x, -99), -98)))


dep_survey <- zet_labels %>% select(all_of(dep_questions$question_code)) %>% mutate(across(where(is.numeric), ~ na_if(na_if(.x, -99), -98)))
###
# dep_society
####

dep_society <- dep_survey %>% select(any_of(c("serial",homophily$code)))
homophily_lookup <- deframe(homophily %>% select(code,variable))

# Rename the columns
dep_society <- dep_society %>% rename_with(~ homophily_lookup[.x], .cols = any_of(names(homophily_lookup)))

#
degrees <- dep_society %>% dplyr::filter(!is.na(degree) & degree != 0) %>% pull(degree) %>% table() %>% as.data.frame()
degrees <- degrees %>% as_tibble()
names(degrees) <- c("degree","n")
degrees <- degrees %>% mutate(f=n/sum(n))

#####################
# remove pay as you go
###############
dep_qanda %>% filter(question_code=="q41")
dep_survey %>% dplyr::filter(!is.na(Q41_oth))
dep_survey %>% dplyr::filter(q41==4) %>% dim()
prepays <- dep_survey %>% dplyr::filter(str_detect(Q41_oth,"Pay|pay")) %>% pull(serial)
weekend <- dep_survey %>% dplyr::filter(str_detect(Q41_oth,"Week|week|Sunday|Saturday|saturday|sunday")) %>% pull(serial)

dep_survey %>% dplyr::filter(q41==4 & !(serial %in% c(prepays,weekend))) %>% select(serial,Q41_oth)


