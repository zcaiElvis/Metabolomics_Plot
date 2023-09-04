ion_count <- readxl::read_xlsx("data/Datacorrected_20230419_ET.xlsx",
                               range = "A40:Y75")

iso_abun <- readxl::read_xlsx("data/Datacorrected_20230419_ET.xlsx",
                              range = "A78:Z284")



plot_abundance(ion_count, iso_abun, group_length = 8, total_length = 24)
