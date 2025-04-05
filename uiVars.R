# uiVars

# create an inline tibble to allow functional programming of UI and server functions
# check_na assumes the id is the vl_data column name with _na at the end
uiVars <- tribble(
  ~ type,      ~ id,                ~ label,                                           ~ sep, ~ ticks,  ~step,
  "select",    "study_no",          "Study ID",                                        NA,    NA,       NA,
  "select",    "disease",           "Select disease",                                  NA,    NA,       NA,
  "select",    "status",            "Study status",                                    NA,    NA,       NA,
  "select",    "drugs",             "Drug used",                                       NA,    NA,       NA,
  "select",    "t_design",          "Select study design",                             NA,    NA,       NA,
  "select",    "hiv_co_infection",  "HIV co-infection",                                NA,    NA,       NA,
  "select",    "region",            "Select region",                                   NA,    NA,       NA,
  "select",    "country",           "Select country",                                  NA,    NA,       NA,
  "slider",    "enrolment",         "Number of participants",                          ",",   FALSE,    50,
  "check_na",  "enrolment",         "Missing particpant numbers",                      NA,    NA,       NA,
  "slider",    "pub_year",          "Publication year",                                "",    FALSE,    NA,
  "check_na",  "pub_year",          "Missing publication year",                        NA,    NA,       NA,
)