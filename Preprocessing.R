library(dplyr)
library(sva)

#### Filtering missing in fMRI, age ,sex ####

df_raw = read.csv("./Data/abcd_resilient_Longitudinal_v5.1_20250903.csv")|>
  mutate(SubID = sub(pattern = "_",
                     x = src_subject_id,
                     replacement = ""))

df_imaging = read.csv("/Users/luchen/Documents/Xi/SuStaIn/Data/ABCD_3T1_betnet_baseline_ACE(complete)_participants_CBCL_withQC_merged.csv")|>
  filter(if_all(c(age, sex), ~ !is.na(.)))|>
  filter(if_all(starts_with("rsfmri_"), ~ !is.na(.)))

df_raw |> 
  group_by(eventname)|>
  summarise(n())

df_base = df_raw |> 
  filter(eventname == "baseline_year_1_arm_1")|>
  inner_join(df_imaging, by = "SubID")
df_base|>
  group_by(Resilience_Group)|>
  summarise(cnt = n())
## need to double check the NA (n = 4)
## "NDARINV9PVR76W7" "NDARINVJHJDGEFN" "NDARINVL9NUBDAN" "NDARINVTRG5GX9T"

df_base = df_base |>
  filter(!is.na(Resilience_Group))
#### Combat for age and sex  #####
feature_matrix = df_base|>select(starts_with("rsfmri"))

combat_output = ComBat(
  dat = t(feature_matrix),  # Transpose: features x subjects
  batch = df_base|>pull(site),
  mod = model.matrix(~ age + sex, data = df_base)
)

harmonized_data = t(combat_output)  # Transpose back: subjects x features
df_base = df_base|>
  select(-starts_with("rsfmri_"))|>
  cbind(harmonized_data)|>
  select(SubID,Resilience_Group,
         matches("^(cbcl_|nihtbx|rsfmri_)"))

save(df_base,
     file = "./Data/ABCD_Resilience_preprocessed_1012.RData")
