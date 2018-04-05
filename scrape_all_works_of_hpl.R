# Scraping and processing and analysing the complete works of H. P. Lovecraft 
library(tidyverse)
library(rvest)

url <- "http://www.hplovecraft.com/writings/texts/"
hpl <- read_html(url)

# Recursively read the links on the homepage
hpl_data <- data_frame(title = hpl %>%  # Use css element names to get titles and links
                               html_nodes("ul ul a") %>%
                               html_text(),
                       link = hpl %>%
                               html_nodes("ul ul a") %>% 
                               html_attr("href") %>% 
                               paste0(url, .)
)

# Read the texts from all links.
# This can take a while
hpl_data <-
    hpl_data %>% 
    rowwise() %>% 
    mutate(text =  map_chr(link, ~read_html(.x) %>%
                            # Find the element on the page and extract it
                            html_nodes(".pagelayout div") %>%
                            # Parse it as text
                            html_text() %>%
                            # Remove redundant whitespaces in the text.
                            str_squish() %>% 
                            # Collapse all text together with linebreak
                            paste(collapse = "\n"))) %>% 
    group_by(title) %>% 
    # Select just one row for each title (because of redundancy)
    slice(1) %>% 
    ungroup()


# Let's save the output, to prevent the lengthy data collection in the future
write_excel_csv(hpl_data, "hpl_all_works.csv")


