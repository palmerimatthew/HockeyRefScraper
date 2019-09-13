#' HockeyRef Player Scraper
#'
#' Returns data from a player given their url.
#' @param website the url of the player's webpage on hockey-reference.com.
#' @param ages 2-length vector for the age ranges desired. first number is lower-bound, second number is upper-bound.
#' @param playerStats vector of the wanted player stats. possible values:
#'     stateSep - seperate goals, assists, and points based on strength (PP, SH, EV)
#'     Cor = Corsi,
#'     Fen = Fenwick,
#'     oiS = On-Ice shooting and save percentage,
#'     PDO = PDO,
#'     IceTime = Ice time,
#'     Awards = Awards with placement,
#'     pmBreak = plus/minus breakdown (GF, GA, etc.),
#'     PS = HockeyRef's point shares metric,
#'     xGF = Expected Goals For percentage
#' @param Season Determines if regular season data, playoff data, or both will be returned for all the players. 'R' for Regular Season, 'P' for Playoffs, 'RP' for both Regular Season and Playoffs
#' @param sepTeam Boolean about whether you want rows for every team a player played on in the NHL in a give year, or if you just want the cumulative production combining the two teams.
#' @return data frame (or list of data frames) with data the desired player.
#' @export

Ref_Player_Scraper <- function(website, ages = c(17,50), Stats = "all", Season = "R", sepTeam = F) {
  print(website)
  tables <- getHockeyRefTables(website)
  if(Stats == "all") {
    Stats <- c("Cor", "Fen", "PDO", "oiS", "IceTime", "Awards", "pmBreak", "PS", "xGF")
  }
  
  #grabbing desired tables
  namesList <- names(tables)
  if(Season == "P" | Season == "RP") {
    playoffTable <- grep("stats_basic(.*?)nhl_po", namesList)
    if (length(playoffTable) != 0) {
      playoffTable <- tables[[playoffTable]]
      
      #Constricting table to age limit
      playoffTable$Age <- as.numeric(levels(playoffTable$Age))[playoffTable$Age]
      playoffTable <- playoffTable[playoffTable$Age >= ages[1],]
      playoffTable <- playoffTable[playoffTable$Age <= ages[2],]
      columns_wanted <- playoffTable %>%
        names() %>%
        .[!grepl('%', .)]
      playoffTable <- dplyr::select(playoffTable, columns_wanted)
    } #if(length(playoffTable) == 0)
  } #if(Season == "P" | Season == "RP")
  if(Season == "R" | Season == "RP") {
    #grabbing wanted stats
    generalStats <- grep("stats_basic(.*?)nhl$", namesList)
    if(length(generalStats) != 0) {
      generalStats <- tables[[generalStats]]
      ev <- grep("EV", colnames(generalStats))
      
      #Based on stateSep inclusion in 'Stats'
      if("stateSep" %in% Stats) {
        colnames(generalStats)[ev[1]:(ev[1] + 3)] <- c("EVG", "PPG", "SHG", "GWG")
        colnames(generalStats)[ev[2]:(ev[2] + 2)] <- c("EVA", "PPA", "SHA")
        generalStats <- generalStats[,-((ev[1] - 5):(ev[1] - 3))]
      } else {
        generalStats <- generalStats[,-(ev[1]:(ev[2] + 2))]
      }
      
      #Based on Awards inclusion in 'Stats'
      if(!("Awards" %in% Stats)) {
        generalStats <- generalStats[,-ncol(generalStats)]
      }
      
      #Based on Ice Time inclusion in 'Stats'
      if(!("IceTime" %in% Stats)) {
        index <- grep("TOI", colnames(generalStats))
        generalStats <- generalStats[,-(index:(index + 1))]
      }
      generalStats <- removeDuplicateYears(generalStats, sepTeam)
      
      #Based on Corsi, Fenwick, or PDO inclusion in 'Stats'
      if("Cor" %in% Stats || "Fen" %in% Stats || "PDO" %in% Stats || "oiS" %in% Stats) {
        possessionTable <- grep("skaters_advanced", namesList)
        if(length(possessionTable) != 0) {
          possessionTable <- tables[[possessionTable]]
          #removing zone starts
          index <- grep("oZS", colnames(possessionTable))
          if (length(index) != 0) {
            possessionTable <- possessionTable[, -(index:(index + 1))]
          }
          #If Corsi is not wanted
          if(!("Cor" %in% Stats)) {
            index <- grep("CF$", colnames(possessionTable))
            possessionTable <- possessionTable[,-(index:(index + 3))]
          }
          #If Fenwick is not wanted
          if(!("Fen" %in% Stats)) {
            index <- grep("FF$", colnames(possessionTable))
            possessionTable <- possessionTable[,-(index:(index + 3))]
          }
          #If PDO is not wanted
          if(!("PDO" %in% Stats)) {
            index <- grep("PDO", colnames(possessionTable))
            possessionTable <- possessionTable[,-index]
          }
          #If On-Ice shooting and save percentage is not wanted
          if(!("oiS" %in% Stats)) {
            index <- grep("oiGF", colnames(possessionTable))
            possessionTable <- possessionTable[,-(index:(index + 3))]
          }
          possessionTable <- removeDuplicateYears(possessionTable, sepTeam)
          possessionTable <- possessionTable[, -c(1,4:6)]
          generalStats <- mergeTableHockeyRef(generalStats, possessionTable, sepTeam)
        }
      }
      
      #Based on breaking up plus/minus, Point-Shares, or expected GF% being in Stats
      if("pmBreak" %in% Stats || "PS" %in% Stats || "xGF" %in% Stats) {
        miscTable <- grep("stats_misc_plus_nhl", namesList)
        miscTable <-tables[[miscTable]]
        index <- grep("Att.", colnames(miscTable))
        if (length(index) != 0) {
          miscTable <- miscTable[,-(index:(index + 3))]
        }
        #Based on inclusion of plus/minus in Stats
        if ("pmBreak" %in% Stats) {
          index <- grep("+\\-", colnames(generalStats))
          generalStats <- generalStats[,-index]
        } else {
          index <- grep("TGF", colnames(miscTable))
          miscTable <- miscTable[,-(index:(index + 4))]
        }
        #If point-shares are not wanted
        if(!("PS" %in% Stats)) {
          index <- grep("OPS", colnames(miscTable))
          miscTable <- miscTable[,-(index:(index+2))]
        }
        #If Expected goals for is not wanted
        if(!("xGF" %in% Stats)) {
          index <- grep("xGF", colnames(miscTable))
          miscTable <- miscTable[-(index:(index + 1))]
        }
        index <- grep("E+", colnames(miscTable))
        if (length(index) != 0) {
          miscTable <- miscTable[,-index]
        }
        miscTable <- removeDuplicateYears(miscTable, sepTeam)
        ####################################
        #want to fix this to be more generic
        ####################################
        miscTable <- miscTable[,-c(1,4:16)]
        generalStats <- mergeTableHockeyRef(generalStats, miscTable, sepTeam)
      }
      #Constricting table to age limit
      generalStats$Age <- as.numeric(generalStats$Age)
      generalStats <- generalStats[,c(3,1,2,4:(ncol(generalStats)))]
      generalStats <- generalStats[generalStats$Age >= ages[1],]
      generalStats <- generalStats[generalStats$Age <= ages[2],]
    }
    
    #getting name of player
    name <- website %>%
      xml2::read_html() %>%
      rvest::html_nodes('h1') %>%
      rvest::html_text()
    generalStats <- cbind(Name = name, generalStats)
    
    columns_wanted <- generalStats %>%
      names() %>%
      .[!grepl('%', .)]
    
    generalStats <- dplyr::select(generalStats, columns_wanted)
  } #if(Season == "R" | Season == "RP")
  
  #Constructing return structures
  if(Season == "RP") {
    list(Regular = generalStats, Playoff = playoffTable)
  } else if(Season == "R") {
    generalStats
  } else if (Season == "P") {
    playoffTable
  }
}





getHockeyRefTables <- function(website) {
  html <- readLines(website)
  #Get lines with the start and ends of tables
  start <- grep("<table", html)
  end <- grep("</table>", html)
  matched <- paste(html[start[1]:end[1]], collapse = "\n")
  val <- 2
  while(val <= length(start)) {
    matched <- append(matched, paste(html[start[val]:end[val]], collapse = "\n"))
    val <- val + 1
  }
  #creating data.frame objects with the html tables
  XML::readHTMLTable(matched)
}

removeDuplicateYears <- function(Table, boolean) {
  duplicate <- duplicated(Table$Age)
  if (boolean) {
    i = 1
    while(i < length(duplicate)) {
      if(!duplicate[i] && duplicate[i+1]) {
        duplicate[i] = T
      } else {
        duplicate[i] = F
      }
      i <- i + 1
    }
    duplicate[i] = F
  }
  Table[!duplicate,]
}

mergeTableHockeyRef <- function(Table1, Table2, boolean) {
  Table1 <- tidyr::unite(Table1, Age, Tm, col = "forSort", sep = "-")
  try(Table2 <- tidyr::unite(Table2, Age, Tm, col = "forSort", sep = "-"), silent = T)
  try(Table2 <- tidyr::unite(Table2, Age, Team, col = "forSort", sep = "-"), silent = T)
  returnTable <- merge(x = Table1, y = Table2, by = "forSort", all.x = T)
  tidyr::separate(returnTable, forSort, into = c("Age", "Tm"), sep = "-")
}