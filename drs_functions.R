# This script provides functions for loading data from the 2023 Delta
# Residents Survey. It relies on packages from the Tidyverse.
# Version: 2023-11-27

#' Converts missingness values with angle brackets to standard NA values.
#' 
#' @param .data is a tibble or data.frame of DRS data.
drs_as_NA <- function(
    .data
){
  .data %>%
    mutate(
      # change columns that are of type character.
      across(.cols = where(is.character),
             .fns = ~ case_when(
               # When string with angle brackets detected,
               # assign NA value.
               str_detect(.x, "\\<.+\\>") ~ 
                 NA_character_,
               # Otherwise, return string.
               TRUE ~ .x)
      ),
      # change columns that are of type factor.
      across(.cols = where(is.factor), 
             .fns = ~ case_when(
               # When factor with angle brackets detected,
               # assign NA value.
               str_detect(as.character(.x), "\\<.+\\>") ~ 
                 NA_character_,
               # Otherwise, return factor.
               TRUE ~ as.character(.x)
             ) %>%
               # Then convert to factor, and
               # drop any empty levels, 
               # ie. factors w/ angle brackets.
               factor() %>%
               fct_drop()
               
      )
    )
}

#' Loads, validates, and corrects factor levels in the Delta Residents Survey 
#' 2023 data.
#' 
#' @param .path is a character vector of length 1. Provides the path to the directory containing all the relevant files, including the data dictionary, the data in csv form, and the hash.txt. Although may be incorrect, this argument by default is set to the current working directory.
#' @param convert_to_NA is a logical vector of length 1. Converts the variety of missingness levels (eg. <Decline to answer>) to `NA` values, simplifying calculations. Importantly, if there are columns that were originally factors (due to different missingness levels), but is principally a numeric column, this function will adjust these columns to type numeric.
#' @return tibble of the Delta Residents Survey 2023 data set.
drs_read <- function(
    .path = getwd(),
    convert_to_NA = F
){
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Validate argument: .path
  if(class(.path) != "character"){
    stop("Argument path_ is not a character vector.")
  } else if (length(.path) != 1){
    stop("Argument path_ is not length 1.")
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Conditionally load packages
  lapply(
    X = c(
      # most of the tidyverse, minus ggplot2
      "tibble",
      "dplyr",
      "tidyr",
      "purrr",
      "readr",
      "stringr",
      "forcats",
      "readxl",
      # for data validation
      "digest"
    ),
    function(x) {
      if(!(x %in% .packages())){
        message(paste0("Loading {", x, "}"))
        library(x, character.only = T)
      }
    }
  ) |>
    invisible()
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Validate Files ----
  # Validate that all the necessary files are present.
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load list of files (fl)
  fl <- tibble(
    file = list.files(.path),
    path = list.files(.path, full.names = T)
  )
  
  # Create table of necessary files
  necessary_files <- tibble(
    name = c("dd", "data", "hash"),
    style = c(
      "DRS_data_dictionary_*_.xlsx",
      "DRS_*.csv",
      "DRS_*.hash.txt"
    ),
    regex = c(
      "^DRS(\\_|\\s)data(\\_|\\s)dictionary(\\_|\\s).+\\.xlsx$",
      "^DRS(\\_|\\s).+\\.csv$",
      "^DRS(\\_|\\s).+\\.hash\\.txt$")
  )
  
  # Determine if each file in the file list is a necessary file, 
  # and if so, determine type.
  fl$type <- map_vec(fl$file, function(path_){
    # Obtain location of name in necessary_files
    i <- map_vec(necessary_files$regex, function(regex_){
      str_detect(path_, regex_)
    }) |>
      which()
    
    # if file is not a necessary file.
    if(identical(i, integer(0))){
      return(as.character(NA))
    }
    
    # Return
    necessary_files$name[i]
  })
  
  # Validate that all necessary files present.
  if(length(discard(fl$type, ~is.na(.x))) != nrow(necessary_files)){
    
    # Determine which files are missing.
    missing <- necessary_files$style[
      which(!(necessary_files$name %in% fl$type))
    ] %>%
      paste0(., collapse = ", ")
    
    stop(str_glue("Error: \"{missing}\" file(s) missing."))
    
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Validate Data ----
  # Validate that the data matches the hash checksum.
  # Otherwise, the data may be corrupt.
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Obtain stored hash value.
  hash <- fl %>%
    filter(type == "hash") %>%
    pull(path) %>%
    readLines(.)
  
  if(nchar(hash) != 64){
    stop(".hash.text value is of improper length for SHA256.")
  }
  
  # Obtain path to data
  data <- fl %>%
    filter(type == "data") %>%
    pull(path)
  
  # Obtain SHA256 hash for the data.
  data_hash <- digest(object = data,
         algo = "sha256",
         file = T)
  
  if(!identical(data_hash, hash)){
    stop("Data could not be validated. The SHA 256 hash value obtained for the data does not match the value provided by the DRS team.")
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load Dictionary ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dd <- fl %>%
    filter(type == "dd") %>%
    pull(path) %>%
    read_xlsx(., sheet = "Variables")
  
  # clean up dd (remove empty lines)
  dd <- dd %>%
    drop_na(name) %>%
    fill(Variable, .direction = "down")
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load Data ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Task 1. Determine the column types as specified in the 
  # data dictionary.
  
  # Get column order
  col_order <- tibble(
    vars = fl %>%
      filter(type == "data") %>%
      pull(path) %>%
      readLines(con = ., 
                n = 1) %>% 
      str_split_1(., "\\,")
  )
  
  # Get R class for each variable.
  r_class <- dd %>%
    filter(name == "R Class") %>%
    select(Variable, value) %>%
    # This sets us up for the `read_csv()` argument col_types.
    mutate(readr_type = case_when(
      value == "factor" ~ "f",
      value == "factor - numeric" ~ "f",
      str_detect(value, regex("posix", ignore_case = T)) ~ "T",
      value == "character" ~ "c",
      value == "numeric" ~ "n"
    ))
  
  # Merge col_order with r_class.
  # This helps us keep track of the actual order of columns in the CSV file.
  # Without this, we *could* end up assigning the wrong col_type to a column
  # as we load it using `read_csv()`.
  col_order <- col_order %>%
    left_join(r_class, by = c("vars" = "Variable"))
  
  # Now read data according to readr_type.
  # The argument supplied to col_types looks something like:
  # "ffffffffTffffcffff..." with each character representing a column type.
  data <- read_csv(
    file = data,
    col_types = paste0(col_order$readr_type, 
                       collapse = "")
  )
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set Order of Ordinal Values ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split data dictionary into multiple tibbles (by Variable/column)
  vars_ <- dd %>%
    # Note, this function "experimental" and may change.
    group_split(Variable)
  
  # Obtain metadata of ordinal variables only,
  # otherwise return NULL
  fcts_ <- map(vars_, function(tb){
    # Obtain class of variable.
    cls <- tb %>%
      filter(name == "R Class") %>%
      pull(value)
    
    # If not a factor, return NULL.
    if(cls != "factor"){
      return(NULL)
    }
    
    # Obtain rows with info on factors only.
    fcts <- tb %>%
      filter(name == "factors")
    
    # If no "factors" presented, return NULL.
    if(nrow(fcts) == 0){
      return(NULL)
    }
    
    # If there are no encodings (ie. order is not important),
    # return NULL.
    if(all(map_vec(fcts$encoding, is.na))){
      return(NULL)
    }
    
    # RETURN
    # Sort order of factors by:
    # 1. non-"missing" variables first,
    # 2. encoding value
    fcts %>%
      mutate(std = str_detect(value, "\\<.+\\>", negate = T)) %>%
      arrange(desc(std), encoding) %>%
      # return Variable, value, and encoding only.
      select(Variable, value, encoding)
    
  })   # End of map()
  
  # remove NULL values
  fcts_ <- fcts_[map_vec(fcts_, ~!is.null(.x))]
  
  # Assign names to fcts_
  fcts_ <- fcts_ %>%
    set_names(map_vec(fcts_, function(tb){tb$Variable[1]}))
  
  # Re-order ordinal variables
  data <- map2_dfc(data, names(data), function(col_, nm){
    # Is this column an ordinal factor?
    if(nm %in% names(fcts_)){
      # obtain tibble with Variable, value, encoding.
      fct <- fcts_[[nm]]
      # RETURN
      # Relevel factor.
      fct_relevel(col_, fct$value)
    } else {
      # Not an ordinal factor.
      # RETURN
      col_
    }
  })  # End of map()
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert NAs ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(convert_to_NA){
    # Convert <Missingness> to NA
    data <- drs_as_NA(data)
    
    # Convert Factor - Numeric to just Numeric
    data <- map2_dfc(data, names(data), function(col_, nm){
      cls <- r_class %>%
        filter(Variable == nm) %>%
        pull(value)
      
      # Return
      if(cls == "factor - numeric"){
        as.numeric(col_)
      } else {
        col_
      }
    })   # End of map()
  }  # End of if()
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sort columns in the order as they appear in the data dictionary.
  data <- data %>%
    select(dd %>% pull(Variable) %>% unique())
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return
  data
}

message("Function, drs_read(), created. This function loads and prepares the Delta Residents Survey 2023 data. This function has two arguments: 1) the file path to the directory containing the data dictionary xlsx, data csv, and hash txt; 2) Whether or not to convert specially encoded missing values (eg. Decline to answer) into simple NA values.")
