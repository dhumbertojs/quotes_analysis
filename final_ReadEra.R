source("scrap_ReadEra.R")

txt <- list.files("data")[2]

data <- parse(txt)
writexl::write_xlsx(data, paste0(gsub("\\.txt$", "",txt), ".xlsx"))