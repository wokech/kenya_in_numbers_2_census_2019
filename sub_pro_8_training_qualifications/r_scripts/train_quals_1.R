# Distribution of Population Age 15 years and Above by 
# Sex and Main Training Acquired and Qualified for
# Census (2019)

# Data: rKenyaCensus

# Inspo: Data Story - Check Links

# Load libraries

library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)

# Load data

train_quals <- V4_T2.7 

train_quals <- train_quals |>
  clean_names() |>
  mutate(m_f_ratio_100 = round(male*100/female)) |>
  rename(training_qualification = areaof_training)

train_quals_renamed <- train_quals |>
  mutate(training_qualification = recode(training_qualification, 
                                         "Total" = "Total population above 15 yrs of age"),
         training_qualification = recode(training_qualification, 
                                         "Agriculture (Horticulture, Agribusiness Agric Economics Crop Sciences etc)" = "Agriculture"),
         training_qualification = recode(training_qualification, 
                                         "Air craft Maintenance Pilot Steewardship Aviation (Air Traffic Controllers)" = "Aviation-Related Courses"),
         training_qualification = recode(training_qualification, 
                                         "Animal Health and Vetenary Sciences" = "Animal Health and Vet Science"),
         training_qualification = recode(training_qualification, 
                                         "Architecture and design (Quantity Surveying Structural Engineering etc)" = "Architecture and Design"),
         training_qualification = recode(training_qualification, 
                                         "Arts (Performing and Visual Arts)-Literature Performing arts Visual arts etc" = "Performing and Visual Arts"),
         training_qualification = recode(training_qualification, 
                                         "Business (Commerce Accounting Finance Marketing Administration Management Insurance Hotel\nManagement Tourism etc.)" = "Business Studies"),
         training_qualification = recode(training_qualification, 
                                         "Divinity and Pastoral Related (Theological Studies)" = "Theological Studies"),
         training_qualification = recode(training_qualification, 
                                         "Earth Sciences (Geology and Meteorology)" = "Earth Sciences"),
         training_qualification = recode(training_qualification, 
                                         "Education (Science Arts Special Management Economics etc)" = "Education (All Fields)"),
         training_qualification = recode(training_qualification, 
                                         "Engineering and technology (Civil Electrical and Electronics Mechanical Chemical Aeronautical Biosystems\netc)" = "Engineering and Technology"),
         training_qualification = recode(training_qualification, 
                                         "Environmental Studies Marine Ecology and Forestry" = "Environmental Studies and Forestry"),
         training_qualification = recode(training_qualification, 
                                         "Family and Consumer Sciences-Home Economics Applied Human Nutrition Diatetrics etc" = "Family and Consumer Sciences"),
         training_qualification = recode(training_qualification, 
                                         "Health and Human Medicine (Clinical Officers Nurses Physiotherapist etc)" = "Nurses, Clinical Officers, and Physiotherapists"),
         training_qualification = recode(training_qualification, 
                                         "Health and Human Medicine (Dentists and Pharmacist)" = "Dentists and Pharmacists"),
         training_qualification = recode(training_qualification, 
                                         "Health and Human Medicine (Doctors)" = "Doctors"),
         training_qualification = recode(training_qualification, 
                                         "Health and Human Medicine (Laboratory Technologist Radiologists Anaesthesists etc )" = "Allied Health Careers"),
         training_qualification = recode(training_qualification, 
                                         "Human Physical Performance and recreation (Sports Science Sports Economics etc)" = "Physical Education and Recreation"),
         training_qualification = recode(training_qualification, 
                                         "Interdisciplinary Studies (Cultural Studies Gender Studies Organizational Studies-Project management\nplanning human resource mgt Development Studies) Demography Urban Planning etc" = "Interdisciplinary Studies"),
         training_qualification = recode(training_qualification, 
                                         "Journalism Media Studies and Communication (Public Relations Mass Communication Speech and Rhetoric\netc.)" = "Journalism, Media Studies, and Communication"),
         training_qualification = recode(training_qualification, 
                                         "Library Information Science and Museum Studies" = "Library Science and Museum Studies"),
         training_qualification = recode(training_qualification, 
                                         "Mathematics (Pure, Applied and Logic)" = "Mathematics"),
         training_qualification = recode(training_qualification, 
                                         "Public Adminstration-Public Policy" = "Public Policy and Administration"),
         training_qualification = recode(training_qualification, 
                                         "Space Sciences-Astronomy" = "Astronomy and Space Sciences"),
         training_qualification = recode(training_qualification, 
                                         "Statistics (Actuarial Biometry Biostatistics Social Statistics Financial Statistics etc)" = "Statistics"),
         training_qualification = recode(training_qualification, 
                                         "Systems Sciences (Computing etc)" = "Computing and Systems Science"),
         training_qualification = recode(training_qualification, 
                                         "Transportation-Logistics Management" = "Transport and Logistics"),
         training_qualification = recode(training_qualification, 
                                         "Vocational Training (Plant Motor Vehicle Plumbing Wiring Masonary Carpentry and Joinary Refrigeration and\nAir Conditioning Hair dressing and related Tailoring Commercial Vehicle Drivers\netc.)" = "TVET and Vocational Training"),
         training_qualification = recode(training_qualification, 
                                         "Others" = "Other Qualifications"),
         training_qualification = recode(training_qualification, 
                                         "None" = "No Qualifications"),
         training_qualification = recode(training_qualification, 
                                         "DK" = "Don't Know"),
         training_qualification = recode(training_qualification, 
                                         "Not Stated" = "No Qualification Stated"))

# Split by CBE pathways

# List of training and qualifications

stem_pathway <- c("Agriculture", "Animal Health and Vet Science", 
                  "Architecture and Design", "Biology", "Chemistry",
                  "Computer Science", "Earth Sciences", "Engineering and Technology",
                  "Environmental Studies and Forestry", "Family and Consumer Sciences",
                  "Nurses, Clinical Officers, and Physiotherapists", 
                  "Dentists and Pharmacists", "Doctors", "Allied Health Careers",
                  "Mathematics", "Military Sciences", "Physics", 
                  "Aviation-Related Courses", "Astronomy and Space Sciences",
                  "Statistics", "Computing and Systems Science",
                  "Transport and Logistics", "TVET and Vocational Training"
       )

social_science_pathway <- c("Anthropology", "Theological Studies", "History", 
                            "Geography", "Business Studies", "Economics", 
                            "Interdisciplinary Studies", 
                            "Journalism, Media Studies, and Communication",
                            "Languages and Linguistics", "Law",
                            "Library Science and Museum Studies",
                            "Philosophy", "Political Science", "Psychology",
                            "Public Policy and Administration", "Religion",
                            "Social Work", "Sociology")

sports_perf_arts_pathway <- c("Performing and Visual Arts", 
                              "Physical Education and Recreation")

interdiscip_pathway <- c("Education (All Fields)")

other_classifications <- c("Total population above 15 yrs of age", 
                           "Other Qualifications", "None ", 
                           "Don't Know", "No Qualification Stated")

# Data frames for various pathways

train_quals_renamed_stem_pathway <- train_quals_renamed |>
  filter(training_qualification %in% stem_pathway)

train_quals_renamed_social_science_pathway <- train_quals_renamed |>
  filter(training_qualification %in% social_science_pathway)

train_quals_renamed_sports_perf_arts_pathway <- train_quals_renamed |>
  filter(training_qualification %in% sports_perf_arts_pathway)

train_quals_renamed_interdiscip_pathway <- train_quals_renamed |>
  filter(training_qualification %in% interdiscip_pathway)

train_quals_renamed_other_classifications <- train_quals_renamed |>
  filter(training_qualification %in% other_classifications)

# Assess male-female differences

train_quals_renamed_all_careers <- train_quals_renamed |>
  filter(!training_qualification %in% other_classifications)
