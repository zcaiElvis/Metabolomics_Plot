


# Read data

ion_count <- readxl::read_xlsx("data/Datacorrected_20230419_ET.xlsx",
                          range = "A40:Y75")

iso_abun <- readxl::read_xlsx("data/Datacorrected_20230419_ET.xlsx",
                              range = "A78:Z284")

iso_abun <- na.omit(iso_abun)

# Modify names
ion_count$Fragment_type <- split_string_take_first(ion_count$Fragment, by = " ")

iso_abun$Ion_type <- split_string_take_first(iso_abun$Ion, by = " ")
iso_abun$Ion_type <- unlist(lapply(iso_abun$Ion_type,
                                   function(x) substr(x, 1, nchar(x)-3)))


for(i in 1:nrow(ion_count)){
  frag_count <- ion_count[i,]
  frag_count <- dplyr::select(frag_count, -c("Fragment", "Fragment_type"))
  frag <- ion_count$Fragment_type[i]
  abun_mul_loc <- which(iso_abun$Ion_type == frag)

  for(loc in abun_mul_loc){
    iso_abun[loc, 2:(ncol(iso_abun)-2)] <- # TODO: remove hard coding 2 here
      iso_abun[loc, 2:(ncol(iso_abun)-2)] * frag_count
  }

}





#' Title
#'
#' @param l
#' @param by
#'
#' @return
#' @export
#'
#' @examples
split_string_take_first <- function(l, by = " "){
  l_splitted <- stringr::str_split(l, by)
  unlist(lapply(l_splitted, function(x) x[1]))
}
