#-- packages ----
source(here::here('01_code/packages.R'))

#-- functions ----

#-- NPI data from ECDC----
NPI_data <- read_csv(here::here('00_data/NPI/response_graphs_data_2021-09-20.csv'))

#-- desired countries
eu_countries <- eurostat::eu_countries %>%
  dplyr::select(name) %>%
  dplyr::pull()

NPI_data %<>%
  dplyr::filter(Country %in% eu_countries)


Npi_try <- NPI_data %>%
#  dplyr::filter(Country == 'Germany') %>%
  dplyr::mutate(Response_measure =  gsub("([a-z])([A-Z])", "\\1 \\2", Response_measure)) %>%
  dplyr::mutate(Response_measure =  gsub("([a-z])(\\d+)", "\\1 \\2", Response_measure)) %>%
  dplyr::mutate(Response_measure =  gsub("(\\d+)([A-Z])", "\\1 \\2", Response_measure)) %>%
  dplyr::mutate(Response_measure =  paste(Response_measure, "\n")) %>%
  dplyr::filter(date_start <= '2020-12-30') %>%
  dplyr::mutate(date_end = dplyr::case_when(
    date_end >= as.Date('2020-12-31') ~ as.Date('2020-12-31'),
    TRUE ~ date_end)) %>%
  dplyr::mutate(date_end = replace_na(date_end, as.Date('2020-12-31')))%>%
  tidyr::pivot_longer(cols = c('date_start', 'date_end'))

#handel NA

Npi_try_G <- Npi_try %>%
  dplyr::filter(Country == 'Germany')

Npi_try_G  %>%
  ggplot2::ggplot(aes(y = fct_rev(fct_inorder(stringr::str_wrap(Response_measure, 30))),  x = value), position = position_dodge(width= 500)) +
  geom_line(size = 1, lineend = "round", color = '#004c93') +
  coord_fixed(ratio = 50) +
  labs(x = '2020', y = '') +
  facet_wrap(~ Country, ncol = 4) +
  own_theme() 


npi_4 <- Npi_try  %>%
  dplyr::filter(Country %in% c('Austria', 'Belgium', 'Bulgaria', 'Germany')) %>%
  ggplot2::ggplot(aes(y = fct_rev(fct_inorder(Response_measure)),  x = value), position = position_dodge(width= 500)) +
#  scale_y_discrete(expand = c(1e-1000, 0.000000001)) +
  geom_line(size = 2, lineend = "round", color = '#004c93') +
  coord_fixed(ratio = 75) +
  labs(x = '2020', y = '') +
  facet_wrap(~ Country, ncol = 4) +
  own_theme() 

tikz(here::here('02_paper/includes/NPI_1.tex'), width = 10, height = 15)
print(npi_4)
dev.off()

npi_5 <- Npi_try  %>%
  dplyr::filter(Country %in% c('Austria', 'Belgium', 'Bulgaria', 'Germany', 'Denmark')) %>%
  ggplot2::ggplot(aes(y = fct_rev(fct_inorder(Response_measure)),  x = value)) +
  geom_line(size = 2, lineend = "round", color = '#004c93') +
  coord_fixed(ratio = 75) +
  labs(x = '', y = '') +
  facet_wrap(~ Country, ncol = 5) +
  own_theme()


#  theme(axis.text.x = element_text(angle = 90,  vjust = 0.5))
npi_5

tikz(here::here('02_paper/includes/NPI_2.tex'), width = 10, height = 15)
print(npi_5)
dev.off()


  
