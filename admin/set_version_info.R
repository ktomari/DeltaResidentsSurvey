# Sets version and packages

# (Handpicked) List of used packages.
libs <- c(
  "tibble",
  "dplyr",
  "tidyr",
  "purrr",
  "readr",
  "stringr",
  "forcats",
  "readxl",
  "digest"
)

# Load packages
lapply(
  X = libs,
  function(x) {
    if(!(x %in% .packages())){
      message(paste0("Loading {", x, "}"))
      library(x, character.only = T)
    }
  }
) |>
  invisible()

# Determine R Version
r_version_ <- sessionInfo()$R.version$version.string

# Get Packages and Detailed info, 
# convert to markdown
pkgs_ <- sessioninfo::package_info(
  pkgs = "attached"
) %>%
  as_tibble() %>%
  select(package, loadedversion, date, source) %>%
  simplermarkdown::md_table(., as_character = T)

pkgs_ <- strsplit(x = pkgs_, split = "\\n")[[1]]

# Get files list
fl <- list.files(getwd())

path_ <- fl[grepl(pattern = "README\\.md", x = fl, perl = T)]

# Load README
md <- readLines(path_)

header <- grep(pattern = "\\#{2} Versions & Dependencies", x = md, perl = T)

# Split README file

top <- md[1:header]
bottom <- md[(header+1):length(md)]

# Cut everything before next header

next_header <- grep(pattern = "^\\#", x = bottom, perl = T)[1]
bottom <- bottom[next_header:length(bottom)]

# Create Versions & Dependencies text.

center <- c(
  "",
  r_version_,
  "",
  pkgs_,
  ""
)

writeLines(
  text = c(top, center, bottom),
  con = path_
)
