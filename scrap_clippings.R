library(tidyverse)
library(stringr)
txt <- read_lines("My Clippings.txt")

parse_clippings <- function(txt) {
  
  h1_tibble <- txt %>% as_tibble()
  
  h2_prepend <- bind_rows(tibble(value = "=========="), h1_tibble) 
  
  h3_index <- h2_prepend %>% 
    mutate(index = ifelse(
      str_detect(pattern = "==========", value), 0, NA)) %>% 
    mutate(row = row_number()) %>% 
    mutate(row = ifelse(index == 0, row, NA)) %>% 
    fill(row, .direction = "down") %>% 
    mutate(entry = row %/% 5 + 1) %>% 
    group_by(entry) %>% 
    mutate(meta = row_number())
  
  meta <- tribble(
    ~meta, ~type,
    1, "start",
    2, "book",
    3, "location",
    4, "blank", 
    5, "highlight"
  )
  
  h4_meta <- h3_index %>% 
    left_join(meta, "meta") %>% 
    select(entry, value, type) %>% 
    pivot_wider(
      names_from = type, 
      values_from = value) %>% 
    # spread(type, value) %>% 
    ungroup()
  
  h5_separate <- h4_meta %>% 
    select(book, location, highlight) %>% 
    separate(location, 
             into = c("page", "location", "date"), 
             sep = "\\s\\|\\s", fill = "right") %>% 
    mutate(
      date2 = date,
      date = ifelse(is.na(date), location, date),
      location = ifelse(is.na(date2), NA, location)
    ) %>% 
    separate(book, 
             into = c("book", "author"), 
             sep = "\\s\\(", extra = "merge", fill = "right", 
             remove = F) %>% 
    select(-date2)
  
  h6_replace <- h5_separate %>% 
    mutate(author = str_replace_all(
      author, pattern = "\\)", "")) %>% 
    mutate(page = str_replace_all(
      page, "\\-\\sYour\\sHighlight\\son\\spage\\s", "")) %>% 
    mutate(location = str_replace_all(
      location, "Location\\s", "")) %>% 
    mutate(date = str_replace_all(
      date, "Added\\son\\s", ""))
  
  # h7_format <- h6_replace %>% 
    # drop_na() %>% 
    # mutate(date = as.Date(date, 
    #                       format = "%A, %B %d, %Y %I:%M:%S %p")) %>% 
    # mutate(page = as.integer(page))
  
  return(h6_replace)
}

clippings <- parse_clippings(txt) %>% 
  group_by(book) %>% 
  arrange(location) %>% 
  ungroup()

libros <- levels(as.factor(clippings$book)) %>% 
  as_tibble()

aber <- c(
  "(Contextos) bell hooks - Todo sobre el amor_ Nuevas perspectivas-Ediciones Paidós",
  "Alexandra Stein - Terror, Love and Brainwashing_ Attachment in Cults and Totalitarian Systems-Routledge",
  "For the Love of Men",
  "Mark Fisher - Realismo capitalista. ¿No hay alternativa_-ePubLibre",
  "Nick Couldry_ Ulises Mejias - The Costs of Connection_ How Data Is Colonizing Human Life and Appropriating It for Capitalism-Stanford University Press"
)

filtro <- clippings %>% 
  filter(book %in% aber ) %>% 
  mutate(highlight = unlist(highlight))

writexl::write_xlsx(filtro, "Notas-junio-2024.xlsx")
