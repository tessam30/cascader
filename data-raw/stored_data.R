
#Object containing all the options for the cascade
plot_name <- c("Standard", "Standard Female", "Standard Male",
               "Pediatric", "Pediatric Female", "Pediatric Male",
               "AYP (15-24 years old)", "AYP Female", "AYP Male",
               "Adults", "Adults Female", "Adults Male", "KP")

usethis::use_data(plot_name, overwrite = TRUE)


# List of indicators to keep
keep_ind <- c("Linkage", "NNT", "VLS", "VLC")
usethis::use_data(keep_ind, overwrite = TRUE)


# List of disags needed for peds
disag_peds <- c("Modality/Age/Sex/Result", "Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus")
usethis::use_data(disag_peds, overwrite = TRUE)

# list of disaggs needed for p
disag_kp <-  c("KeyPop/Result", "KeyPop/HIVStatus", "KeyPop/Indication/HIVStatus")
usethis::use_data(disag_kp, overwrite = TRUE)

# index testing modalities
disag_mods <- c("1:Age/Sex/IndexCasesOffered",
               "2:Age/Sex/IndexCasesAccepted",
               "3:Age Aggregated/Sex/Contacts",
               "4:Age/Sex/Result")
usethis::use_data(disag_mods, overwrite = TRUE)




