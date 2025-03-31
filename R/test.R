library(readr)
res <- read_csv("//cdc.gov/project/CGH_OD_ADPD/Strategic/NCD Global Partners/Bloomberg/D4H NCD Risk Factor Surveillance/Countries/Sri Lanka/NCD/T2/Data/Data Release Package/Sri Lanka T2 MPS PUBLIC_4366 Dataset 22AUG2022.CSV")
path = "C:/Users/qno4/Downloads/438-Sri_Lanka_Questionnaire_Repeat_10_21_2021_duplicate/manifest.json"
codebook = get_codebook(path)

res_lab <- label_data(x = res, codebook = codebook)

res_lab <- recode(res_lab, vars=c("alcohol1", "alcohol2"), current_alcohol)
res_lab <- recode(res_lab, vars=c("smoke_covid"), smoke_covid)


desc <- set_descriptions(path=path, x=res)
survey_variables <- survey_variables(desc=desc)

mps <- mps_design(df = res, ids = respondent_id, strata = c(agecat, sex), weight = weight)
