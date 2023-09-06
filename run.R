

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


file_loc <- "data/Datacorrected-version-1.xlsx"

ion_count <- readxl::read_xlsx(file_loc,
                               range = "A40:BE75")

iso_abun <- readxl::read_xlsx(file_loc,
                              range = "A78:BF284")

ion_count <- ion_count[-c(2:24)]
iso_abun <- iso_abun[-c(2:24)]

xtick <- unlist(lapply(1:8, function(x) paste0("Variable_", as.character(x))))

plot_abundance(ion_count, iso_abun, group_length = 8, total_length = 24,
               offset = TRUE, xtick = xtick, removed_cols = c(11))


