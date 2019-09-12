Ref_Draft_Scraper <- function(website, ages = c(17, 50), playerStats = "all", goalieStats = "all", Season = "R", sepTeam = F) {
  html <- website %>%
    readLines()
  right_start <- html %>%
    grep('<table(.*)stats_table', .) %>%
    as.numeric()
  right_end <- html %>%
    grep('</table>', .) %>%
    as.numeric()
  links <- html %>%
    .[right_start:right_end] %>%
    paste(collapse = '\n') %>%
    stringr::str_match_all("<a href=\"(.*?)\"") %>% #just grabbing hyperlinks in the html
    .[[1]] %>%
    .[,2] %>%
    .[grep('/players/.', .)] %>% #only want the hyperlinks that link to players
    paste0('https://www.hockey-reference.com', .)
  
  #'https://www.hockey-reference.com/players/f/forfe01.html'
  #'https://www.hockey-reference.com/players/i/inval01.html'
  playerlinks <- character(0)
  
  for(x in links) {
    NHL <- NHL_boolean(x)
    G <- Goalie_boolean(x)
    if(NHL & !G) {
      playerlinks <- append(playerlinks, x)
    }
  }
  
  returnTable <- Ref_Player_Scraper(playerlinks[1], ages, playerStats, Season, sepTeam)
  
  for(x in 2:length(playerlinks)) {
    temp <- Ref_Player_Scraper(playerlinks[x], ages, playerStats, Season, sepTeam)
    returnTable <- smart_rbind(returnTable, temp)
  }
  
  returnTable
}








Goalie_boolean <- function(link) {
  link %>%
    xml2::read_html() %>%
    rvest::html_nodes('p') %>%
    rvest::html_text() %>%
    stringr::str_split(' ') %>%
    .[grep('Position', .)] %>%
    .[[1]] %>%
    .[2] %>%
    sub("^([[:alpha:]]*).*", "\\1", .) %>%
    grepl('G')
}

NHL_boolean <- function(link) {
  link %>%
    xml2::read_html() %>%
    rvest::html_nodes('h4') %>%
    rvest::html_text() %>%
    grepl('SUMMARY', .) %>%
    any()
}

smart_rbind <- function(table1, table2) {
  if(length(names(table1)) == length(names(table2))) {
    if(all(names(table1) == names(table2))) {
      rbind(table1, table2)
    }
  } else {
    colnames_table1 <- names(table1)
    colnames_table2 <- names(table2)
    not_table1 <- colnames_table2 %>%
      .[!(colnames_table2 %in% colnames_table1)]
    not_table2 <- colnames_table1 %>%
      .[!(colnames_table1 %in% colnames_table2)]
    
    for (x in not_table1) {
      table1 <- wrapr::let(alias = list(rname = x), expr = dplyr::mutate(table1, rname = NA))
    }
    
    for (x in not_table2) {
      table2 <- wrapr::let(alias = list(rname = x), expr = dplyr::mutate(table2, rname = NA))
    }
    
    if(length(colnames_table1) > length(colnames_table2)) {
      table2 <- dplyr::select(table2, colnames_table1)
      rbind(table1, table2)
    } else {
      table1 <- dplyr::select(table1, colnames_table2)
      rbind(table1, table2)
    }
  }
}