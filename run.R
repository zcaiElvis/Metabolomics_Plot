

##### First time run R ########
# install.packages("devtools")
# library("devtools")
# load_all()
###############################

if (!dir.exists("data/")){
  print("Add the data file to the data folder")
  dir.create("data/")
}

if (!dir.exists("output/")){
  dir.create("output/")
}


# file_loc <- "data/Datacorrected_20230419_ET.xlsx"
#
#
# ion_count <- readxl::read_xlsx(file_loc,
#                                range = "A40:Y75")
#
# iso_abun <- readxl::read_xlsx(file_loc,
#                               range = "A78:Z284")


# file_loc <- "data/Datacorrected-version-1.xlsx"

# file_loc <- "data/20230911_3NP_ApA5_media_cell_Datacorrected_ET_.xlsx"

file_loc <- "data/Data(corrected)_2023-09-21.xlsx"

ion_count <- readxl::read_xlsx(file_loc,
                               range = "A43:AE81")

iso_abun <- readxl::read_xlsx(file_loc,
                              range = "A84:AF305")

# offset <- iso_abun[,39, drop = T]

# ion_count <- ion_count[-c(2:19)]
# iso_abun <- iso_abun[-c(2:19)]

# ion_count <- ion_count[c(1, 58:75)]
# iso_abun <- iso_abun[c(1, 58:75, ncol(iso_abun))]


# xtick <- unlist(lapply(1:8, function(x) paste0("Variable_", as.character(x))))


# plot_abundance(ion_count, iso_abun, group_length = 6, total_length = 18,
#                offset = offset, xtick = xtick, removed_cols = c(1, 2, 12, 13), save_loc = "G4/")
#
# plot_multi_abundance(4, ion_count, iso_abun, 6, 18, folder_name = "GN",
#                      plot_offset = list(c(38, 39), c(38, 39), c(76, 77), c(76, 77)))

xtick <- c("VC", "3NP","ApA5","DMM","TTFA","DES","Succ","U13C-VC","U13C-3NP",
           "U13C-ApA5","U13C-DMM","U13C-TTFA","U13C-DES","U13C-Succ")

# xtick <- c("VC", "3NP","ApA5","DMM","TTFA","DES","Succ","VC", "3NP","ApA5","DMM","TTFA","DES","Succ")

plot_abundance(ion_count, iso_abun, group_length = 14, total_length = 28, xtick = xtick,
               removed_cols = c(30, 31), save_loc = "sep25-1/")
