
# import data from VL Surveyer website
source("uiVars.R")

vl_data <- vroom("VL surveyor with PKDL addition2023-01-13.csv",
                 show_col_types = FALSE)

## there are issues with the VL Surveyer data
# all the trial designs listed as 'RCT' are PKDL studies
# Last updated in 2021
# Two trials listed as in India have discordant regions (Mediterranean, East Africa)
# Some trials listed as drug dosing not in patients (ie Phase I trials)


# create unique study identifier (which unique publication title and hiv_co_infection defines)
vl_data <- vl_data %>% 
  group_by(pub_title, hiv_co_infection) %>% 
  mutate(study_id = cur_group_id()) %>% 
  ungroup() %>% 
  arrange(study_no, drugs)
write_csv(vl_data, "vl_data.csv")

# studyID 188: study arms for SR pubs 130 and 131 have been merged (remove for now)
vl_data <- vl_data %>% filter(study_no != 161)

# 64, 70, 99, 116, 145, 146, 164 multiple registry IDs
# 64, 70, 99, 145, 146, 164 multiple pubMedIDs
# 64, 70, 99, 146, 164 multiple URLs
vl_data <- vl_data %>% mutate(
  across(c(pubMedId, url, registry_ID), 
         function(x) if_else(study_id %in% c(64, 70, 99, 116, 145, 146, 164), NA, x))
)

# studyID 6 (study_no 240); should be 48 enrolment
vl_data <- vl_data %>% mutate(
  enrolment = if_else(study_no==240, 48, enrolment)
)

# studyID 126 (study_no 182, 183): status should just be 'published'
vl_data <- vl_data %>% mutate(
  status = if_else(study_no == 183, "Published", status)
)

# studyID 3 (study_no 187, 188, 189, 190) have a different author per row
vl_data <- vl_data %>% mutate(
  authors = if_else(study_id == 3, NA, authors)
)

vl_table <- vl_data

# # check all columns are unique per 'study_no' (which is location level): all except drugs as expected
# for (names in names(vl_data)) {
#   no_distinct <- vl_data %>% group_by(study_no) %>% summarise(sum = n_distinct(.data[[names]])) %>% ungroup() %>% filter(sum > 1) %>% nrow()
#   message(names, ":", no_distinct)
# }
# 
# # check all columns are unique per 'study_id' (which is study level): see below
# for (names in names(vl_data)) {
#   no_distinct <- vl_data %>% 
#     group_by(study_id) %>% 
#     summarise(sum = n_distinct(.data[[names]]))%>% 
#     ungroup() %>% 
#     filter(sum > 1) %>% 
#     nrow()
#   message(names, ":", no_distinct)
# }

vl_map <- vl_data %>% 
  select(-drugs) %>% 
  mutate(popup = str_c("<b>", if_else(is.na(pub_title), "title not available", pub_title), "</b>",
                       "<hr>",
                       "Study number: ", as.character(study_no),
                       "<br>",
                       "Status: ", status, 
                       "<br>",
                       "Design: ", t_design, 
                       "<br>",
                       "Publication year: ", if_else(is.na(pub_year), "not available", as.character(pub_year))))

