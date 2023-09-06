

# First time run

# install.packages("devtools")
# library("devtools")
# load_all()


if (!dir.exists("data/")){
  print("Add the data file to the data folder")
  dir.create("data/")
}

if (!dir.exists("output/")){
  dir.create("output/")
}


file_loc <- "data/Datacorrected_20230419_ET.xlsx"


ion_count <- readxl::read_xlsx(file_loc,
                               range = "A40:Y75")

iso_abun <- readxl::read_xlsx(file_loc,
                              range = "A78:Z284")



plot_abundance(ion_count, iso_abun, group_length = 8, total_length = 24)
