# Listing of regional economic blocs and segmenting of datasets by bloc

# Maa Economic Bloc (MEB) - 3 counties
meb <- c("Narok", "Kajiado", "Samburu")

meb_ratio <- merged_df |>
  filter(County %in% meb)

# Frontier Counties Development Council (FCDC) - 9 counties
fcdc <- c("Garissa", "Wajir", "Mandera", 
          "Isiolo", "Marsabit", "Tana River", 
          "Lamu", "West Pokot", "Turkana",
          "Samburu")

fcdc_ratio <- merged_df |>
  filter(County %in% fcdc) 

# North Rift Economic Bloc (NOREB) - 8 counties
noreb <- c("Uasin Gishu", "Trans Nzoia", "Nandi", 
           "Elgeyo Marakwet", "West Pokot", "Baringo", 
           "Samburu", "Turkana")

noreb_ratio <- merged_df |>
  filter(County %in% noreb)

# Lake Region Economic Bloc (LREB) - 14 counties
lreb <- c("Migori", "Nyamira", "Siaya", 
          "Vihiga", "Bomet", "Bungoma", 
          "Busia", "Homa Bay", "Kakamega", 
          "Kisii", "Kisumu", "Nandi", 
          "Trans Nzoia", "Kericho")

lreb_ratio <- merged_df |>
  filter(County %in% lreb)

# South Eastern Kenya Economic Bloc (SEKEB) - 3 counties
sekeb <- c("Kitui", "Machakos", "Makueni")

sekeb_ratio <- merged_df |>
  filter(County %in% sekeb)

# Mt Kenya and Aberdare’s Economic Bloc 
mkareb <- c("Nyeri", "Nyandarua", "Meru", 
            "Tharaka Nithi", "Embu", "Kirinyaga", 
            "Murang'a", "Laikipia", "Nakuru", "Kiambu")

mkareb_ratio <- merged_df |>
  filter(County %in% mkareb)

# Jumuiya ya Kaunti za Pwani (JKP) 
jkp <- c("Tana River", "Taita Taveta", "Lamu", 
         "Kilifi", "Kwale", "Mombasa")

jkp_ratio <- merged_df |>
  filter(County %in% jkp)

# Nairobi Metropolitan Counties
namet <- c("Nairobi City", "Kajiado", "Murang'a", 
           "Kiambu", "Machakos")

namet_ratio <- merged_df |>
  filter(County %in% namet)

# Kenyan Cities
kenya_city <- c("Nairobi City", "Mombasa", "Kisumu", 
                "Nakuru", "Uasin Gishu")

kenya_city_ratio <- merged_df |>
  filter(County %in% kenya_city)
