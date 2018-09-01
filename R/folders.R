#
# This function creates a data folder if one doesn't already exist.
#
#
#


data_dir <- function() {
  if(!dir.exists("data")) dir.create("data")
}


#
# This function creates an images folder if one doesn't already exist.
#
#
#


images_dir <- function() {
  if(!dir.exists("images")) dir.create("images")
}

#
# This function creates an any folder if one doesn't already exist.
#
#
#

create_dirs <- function(folder) {
  folder %>% purrr::map_chr(dir.create)
}


