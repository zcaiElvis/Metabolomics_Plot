#### First time run R
install.packages("devtools")
library("devtools")
load_all()


#### Create data and output folder if not exist
if (!dir.exists("data/")){
  print("Add the data file to the data folder")
  dir.create("data/")
}

if (!dir.exists("output/")){
  dir.create("output/")
}

#### File location & processing
file_loc <- "data/Datacorrected_20231110-ET.xlsx" # data location here

ion_table_loc <- "A43:AB81"
iso_table_loc <- "A84:AC305"

ion_count <- readxl::read_xlsx(file_loc,
                               range = ion_table_loc)

iso_abun <- readxl::read_xlsx(file_loc,
                              range = iso_table_loc)

#### Call `View` to check for the dataframe
View(ion_count)
View(iso_abun)


#### Generate and save plots

xtick <- c("10 EV", "10 Cre", "1 EV", "1 Cre", "U13C 10 EV", "U13C 10 Cre", "U13C 1 EV", "U13C 1 Cre")

multiplied_df <- plot_abundance(ion_count, iso_abun, group_length = 8, total_length = 24,
               removed_cols = c(17, 21, 26, 27, 28), save_loc = "Nov19-1/")
