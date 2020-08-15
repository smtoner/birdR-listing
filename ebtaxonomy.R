library(tidyverse)
library(stringi)
library(readxl)


# eBird taxonomy
# source: https://support.ebird.org/en/support/solutions/articles/48000837816-the-ebird-taxonomy
# typically updated annually in the late summer
eb_taxonomy2 <- read_xlsx("eBird_Taxonomy_v2019.xlsx") %>%
  set_names(tolower(names(.))) %>%
  select(taxon_order, category, species_code,
         name_common = primary_com_name, name_scientific = sci_name,
         order = order1, family = family, report_as) %>%
  # ascii conversion
  mutate(name_common = stri_trans_general(name_common, "latin-ascii")) %>%
  # fill report_as field to aid joining
  mutate(report_as = if_else(category == "species", species_code, report_as)) %>%
  as_tibble()

eb_taxonomy2 <- eb_taxonomy2 %>%
  filter(!is.na(report_as), category == "species") %>%
  select(report_as,
         species_common = name_common,
         species_scientific = name_scientific) %>%
  left_join(eb_taxonomy2, ., by = "report_as")


