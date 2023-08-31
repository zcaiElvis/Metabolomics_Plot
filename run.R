
data <- read.csv("data/20230213_CEDEX-BIO-HT-Archive-2_13_2023-2_13_2023.txt",
                 sep = "\t", header = TRUE)


media_comp_plot(data, analytes = "GLC3B")
