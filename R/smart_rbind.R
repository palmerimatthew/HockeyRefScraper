#' Smarter rbind
#'
#' Returns a stacked table that combines two tables. Can combine tables with different number of columns.
#' @param table1 data.frame
#' @param table2 data.frame
#' @return stacked combo of both tables. Is able to deal with tables of different columns
#' @export



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