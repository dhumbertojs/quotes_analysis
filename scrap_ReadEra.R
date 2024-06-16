library(tidyverse)
library(stringr)

parse <- function(txt){
  h1_tibble <- txt %>% as_tibble()
  
  h2_prepend <- h1_tibble[4:nrow(h1_tibble),1] %>% 
    filter(value != "") %>% 
    filter(value != fixed("*****")) %>% 
    mutate(
      title = h1_tibble[1,1] %>% 
        pull(),
      author = h1_tibble[2,1] %>% 
        pull()
    ) %>% 
    filter(nchar(value)>2) %>% 
    rename(
      highlight = value
    ) 
  return(h2_prepend)
}



