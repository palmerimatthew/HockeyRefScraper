#' HockeyRef Player Scraper
#'
#' Returns data from a player given their url.
#' @param website the url of the goalie's webpage on hockey-reference.com.
#' @param ages 2-length vector for the age ranges desired. first number is lower-bound, second number is upper-bound.
#' @param goalieStats vector of the wanted goalie stats. possible values:
#'     QS = quality starts metric
#'     GSAA = goals saved above average
#'     Scoring = goals/assists/points/etc.
#'     Awards - Awards with placement
#' @param Season Determines if regular season data, playoff data, or both will be returned for all the players. 'R' for Regular Season, 'P' for Playoffs, 'RP' for both Regular Season and Playoffs
#' @param sepTeam Boolean about whether you want rows for every team a goalie played on in the NHL in a give year, or if you just want the cumulative production combining the two teams.
#' @return data frame (or list of data frames) with data the desired goalie.
#' @export

Ref_Goalie_Scraper <- function(website, ages = c(17,50), Stats, Season = "R", sepTeam = F) {
  if (Stats == "all") {
    Stats = c("QS", "GSAA", "Scoring", "Awards")
  }
  tables <- getHockeyRefTables(website)
  namesList <- names(tables)
  if(Season == "P" | Season == "RP") {
    playoffTable <- grep("stats_basic_plus_nhl_po", namesList)
    if(length(playoffTable) != 0) {
      playoffTable <- tables[[playoffTable]]
      playoffTable <- playoffTable[,-c(5,10)]
      index <- grep("GA%", colnames(playoffTable))
      playoffTable <- playoffTable[,-index]
      index <- grep("GAA", colnames(playoffTable))
      if(length(index) == 2) {
        playoffTable <- playoffTable[,-index[2]]
      }
      if (!("QS" %in% Stats)) {
        index <- grep("QS", colnames(playoffTable))
        playoffTable <- playoffTable[,-(index:(index + 2))]
      }
      if (!("GSAA" %in% Stats)) {
        index <- grep("GSAA", colnames(playoffTable))
        playoffTable <- playoffTable[,-index]
      }
      if (!("Scoring" %in% Stats)) {
        index <- grep("G$", colnames(playoffTable))
        playoffTable <- playoffTable[,-(index:(index + 3))]
      }
      if (!("Awards" %in% Stats)) {
        index <- grep("Awards", colnames(playoffTable))
        playoffTable <- playoffTable[,-index]
      }
      playoffTable <- removeDuplicateYears(playoffTable, sepTeam)
    }
  }
  if(Season == "R" | Season == "RP") {
    generalStats <- grep("stats_basic_plus_nhl$", namesList)
    if(length(generalStats) != 0) {
      generalStats <- tables[[generalStats]]
      index <- grep("GAA", colnames(playoffTable))
      if(length(index) == 2) {
        playoffTable <- playoffTable[,-index[2]]
      }
      if (!("QS" %in% Stats)) {
        index <- grep("QS", colnames(playoffTable))
        playoffTable <- playoffTable[,-(index:(index + 2))]
      }
      if (!("GSAA" %in% Stats)) {
        index <- grep("GSAA", colnames(playoffTable))
        playoffTable <- playoffTable[,-((index - 1):index)]
      }
      if (!("Scoring" %in% Stats)) {
        index <- grep("G$", colnames(playoffTable))
        playoffTable <- playoffTable[,-(index:(index + 3))]
      }
      if (!("Awards" %in% Stats)) {
        index <- grep("Awards", colnames(playoffTable))
        playoffTable <- playoffTable[,-index]
      }
      generalStats <- removeDuplicateYears(generalStats, sepTeam)
    }
  }
  
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