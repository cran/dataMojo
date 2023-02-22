
library(data.table)
utils::globalVariables(c("."))

#' Split one column to multiple columns based on patterns
#'
#' @param dt input data table
#' @param by_col by this column
#' @param by_pattern split by this patter
#' @param match_to_names created new columns names
#'
#' @return data table with new columns
#' @import data.table
#' @export
#'
#' @examples
#' data("dt_dates")
#' str_split_col(dt_dates,
#'               by_col = "Full_name",
#'               by_pattern = ", ",
#'               match_to_names = c("First Name", "Last Name"))
str_split_col <- function(dt, by_col, by_pattern, match_to_names = NULL){
  dt <- setDT(dt)
  if(inherits(dt[[by_col]], "Date")) stop("
  Hmm... Note that your by_col is a Date foramt, please convert to character and try again!")
  length_items <- length( strsplit(dt[[by_col]], split = by_pattern)[[1]] )
  if(is.null(match_to_names)) message(
    "Please note that you can customize new created column names using match_to_names...
    For example, match_to_names = c('first_col_name', 'second_col_name')"
    )
  if(is.null(match_to_names)){
    dt[, paste0(by_col,"_", 1:length_items) := tstrsplit(dt[[by_col]], split = by_pattern)][]
  }else{
    dt[, paste0(match_to_names) := tstrsplit(dt[[by_col]], split = by_pattern)][]
  }
  return(dt)
}


#' Select columns
#'
#' @param dt input data table
#' @param cols select columns
#'
#' @return data table with selected columns
#' @export
#'
#' @examples
#' data("dt_dates")
#' select_cols(dt_dates, c("Start_Date", "Full_name"))
select_cols <- function(dt, cols){
  dt2 <- dt[, c(cols), with = FALSE]
  return(dt2)
}


#' Fill missing values
#'
#' @param dt input data table
#' @param fill_cols filter by this columns
#' @param fill_value fill NA with this value
#'
#' @return data table which NAs are filled
#' @export
#'
#' @examples
#' data("dt_missing")
#' fill_NA_with(dt_missing, fill_cols = c("Full_name"), fill_value = "pending")
fill_NA_with <- function(dt, fill_cols, fill_value){
  if(fill_cols == "All"){
    for (j in seq_len(ncol(dt)))
      set(dt,which(is.na(dt[[j]])),j,fill_value)
  }else{
    for (j in fill_cols)
      set(dt,which(is.na(dt[[j]])),j,fill_value)
  }
  return(dt)
}



#' Reshape data frame to a longer format
#'
#' @param dt input data
#' @param keep_cols columns to be kept
#' @param label_cols column name that contains the melted columns
#' @param value_cols column name that contains the value of melted columns
#'
#' @return data table in a longer format
#' @import data.table
#' @export
#'
#' @examples
#' data("dt_dates")
#' reshape_longer(dt_dates,
#'                         keep_cols = "Full_name",
#'                         label_cols = c("Date_Type"),
#'                         value_cols = "Exact_date")
#'
reshape_longer <- function(dt, keep_cols, label_cols, value_cols){
  if(anyNA(dt)) message("Hmm... Looks like your data.table contains missing values!")
  dt_result <- melt(dt,
                    id.vars = keep_cols,
                    variable.name = label_cols,
                    value.name = value_cols)
  return(dt_result)
}

#' Reshape data frame to a wider format
#'
#' @param dt input data table
#' @param keep_cols columns to be kept
#' @param col_label columns that each unique values will be reshaped as a column name
#' @param col_value columns that fill the reshaped columns
#' @importFrom stats formula as.formula complete.cases median
#'
#' @return reshaped widen data table
#' @export
#'
#' @examples
#' data("dt_long")
#' dataMojo::reshape_wider(dt_long,
#'   keep_cols = c("Full_name"),
#'   col_label = c("Date_Type"),
#'   col_value = "Exact_date")
#'
reshape_wider <- function(dt, keep_cols, col_label, col_value){

  formu <- as.formula(paste(paste(keep_cols, collapse = " + "), "~", col_label))
  dt2 <- dcast(dt,
               formu,
               value.var = col_value)
  return(dt2)
}

#' Filter all rows that meeting requirements
#'
#' @param dt input data.table
#' @param operator operator should be one of l, g. l means less than, g means greater than.
#' @param cutoff_value threshold value
#'
#' @return filtered data table
#' @import data.table
#' @export
#'
#' @examples
#' data("dt_values")
#' dataMojo::filter_all(dt_values, operator = "l", .2)
filter_all <- function(dt, operator, cutoff_value){
  if(operator == 'l'){
    dt2 <- dt[rowMeans(dt < cutoff_value)==1]
  }else if(operator == 'g'){
    dt2 <- dt[rowMeans(dt > cutoff_value)==1]
  }else{
    stop("operator should be one of l, g.
         l means less than, g means greater than!")
  }
  return(dt2)
}


#' Filter any rows that meeting requirements
#'
#' @param dt input data.table
#' @param operator  operator should be one of l, g. l means less than, g means greater than.
#' @param cutoff_value threshold value
#'
#' @return fitlered data table
#' @import data.table
#' @export
#'
#' @examples
#' data("dt_values")
#' dataMojo::filter_any(dt_values, operator = "l", .1)
filter_any <- function(dt, operator, cutoff_value){
  if(operator == 'l'){
    dt2 <- dt[rowSums(dt < cutoff_value) > 0]
  }else if(operator == 'g'){
    dt2 <- dt[rowSums(dt > cutoff_value) > 0]
  }else{
    stop("operator should be one of l, g.
         l means less than, g means greater than!")
  }
  return(dt2)
}

#' Filter any rows that meet requirements with selected columns
#'
#' @param dt  input data table
#' @param operator operator should be one of l, or g. l means less than, g means greater than
#' @param cutoff_value cutoff value
#' @param selected_cols selected columns from input data table
#'
#' @return filtered data table
#' @import data.table
#' @export
#'
#' @examples
#' data("dt_values")
#' dataMojo::filter_all_at(dt_values, operator = "l", .1, c("A1", "A2"))
filter_any_at <- function(dt, operator, cutoff_value, selected_cols ){
  if(operator == 'l'){
    dt2 <- dt[rowSums(dt[, paste0(selected_cols), with=FALSE ] < cutoff_value) > 0]
  }else if(operator == 'g'){
    dt2 <- dt[rowSums(dt[, paste0(selected_cols) , with=FALSE] > cutoff_value) > 0]
  }else{
    stop("operator should be one of l, or g.
         l means less than, g means greater than!")
  }
  return(dt2)
}


#' Filter all rows that meet requirements with selected columns
#'
#' @param dt input data table
#' @param operator operator should be one of l, or g. l means less than, g means greater than
#' @param cutoff_value cutoff value
#' @param selected_cols selected columns from input data table
#'
#' @return filtered data table
#' @import data.table
#' @export
#'
#' @examples
#' data("dt_values")
#' dataMojo::filter_all_at(dt_values, operator = "l", .1, c("A1", "A2"))
filter_all_at <- function(dt, operator, cutoff_value, selected_cols ){
  if(operator == 'l'){
    dt2 <- dt[rowMeans(dt[, paste0(selected_cols), with=FALSE ] < cutoff_value) ==1 ]
  }else if(operator == 'g'){
    dt2 <- dt[rowMeans(dt[, paste0(selected_cols) , with=FALSE] > cutoff_value) ==1 ]
  }else{
    stop("operator should be one of l, or g.
         l means less than, g means greater than!")
  }
  return(dt2)
}



#' group by columns and return a summarized table
#'
#' @param dt input data.table
#' @param group_by_cols group by columns
#' @param summarize_at column summarize at
#' @param operation calculation operation, value should be one of following: sum, mean, median, max, min
#'
#' @return a summarized table
#' @import data.table
#' @importFrom stats formula
#' @export
#'
#' @examples
#' data("dt_groups")
#' dataMojo::dt_group_by(dt_groups,
#'                       group_by_cols = c("group1", "group2"),
#'                       summarize_at = "A1",
#'                       operation = "mean")
dt_group_by <- function(dt, group_by_cols,  summarize_at, operation){
  if(operation == "sum"){
    dt2 <- dt[, .( summary_col = sum(get(summarize_at ))
    ), by = group_by_cols ]
  }else if(operation == "mean"){
    dt2 <- dt[, .( summary_col = mean(get(summarize_at ))
    ), by = group_by_cols ]
  }else if(operation == "median"){
    dt2 <- dt[, .( summary_col = median(get(summarize_at ))
    ), by = group_by_cols ]
  }else if(operation == "max"){
    dt2 <- dt[, .( summary_col = max(get(summarize_at ))
    ), by = group_by_cols ]
  }else if(operation == "min"){
    dt2 <- dt[, .( summary_col = min(get(summarize_at ))
    ), by = group_by_cols ]
  }else{
    stop("Hmm... operation should be one of sum, mean, median, max and min!")
  }
  return(dt2)
}


#' Fetch one row from each grouped by group
#'
#' @param dt input data table
#' @param group_by_cols group by columns
#' @param fetch_row first means to fetch first row and last means to fetch last row
#'
#' @return grouped by data table
#' @import data.table
#' @export
#'
#' @examples
#' data("dt_groups")
#' dataMojo::get_row_group_by(dt_groups,
#'                            group_by_cols = c("group1", "group2"),
#'                            fetch_row = "first")
get_row_group_by <- function(dt, group_by_cols, fetch_row){
  if(fetch_row == "first"){
    dt2 <- dt[, .SD[1], by = group_by_cols]
  }else if(fetch_row == "last"){
    dt2 <- dt[, .SD[.N], by = group_by_cols]
  }else{
    stop("Hmm... fetch_row should be either first or last!")
  }
  return(dt2)
}



#' Expand row based on pattern
#'
#' @param dt input data table
#' @param col_name column to be expanded
#' @param split_by_pattern split based on pattern
#' @param new_name new generated column name
#'
#' @return expanded data table
#' @import data.table
#' @export
#'
#' @examples
#' data("starwars_simple")
#' row_expand_pattern(starwars_simple, "films", ", ", "film")[]
row_expand_pattern <- function(dt, col_name, split_by_pattern, new_name){
  dt[, nrow:= 1:nrow(dt)]
  dt2 <- dt[rep(seq(1, nrow(dt)), lengths(strsplit(dt[[col_name]], split = split_by_pattern) ) )]

  dt2[, "Splited_new_col" := unlist(strsplit(get(col_name), split = split_by_pattern)[1]), by = nrow ]
  dt3 <- dt2[, c(col_name, "nrow"):=NULL ]
  setnames(dt3, old = "Splited_new_col", new = new_name)
  return(dt3)
}


#' Expand row given start and end dates
#'
#' @param dt input data table
#' @param start_date_col start date column
#' @param end_date_col end date column
#' @param new_name new generated column name
#'
#' @return expanded data table
#' @import data.table
#' @export
#'
#' @examples
#' dt_dates_simple <- data.table::data.table(
#'   Start_Date = as.Date(c("2020-02-03", "2020-03-01") ),
#'   End_Date = as.Date(c("2020-02-05", "2020-03-02") ),
#'   group = c("A", "B")
#' )
#' row_expand_dates(dt_dates_simple, "Start_Date", "End_Date", "Date")[]
row_expand_dates <- function(dt, start_date_col, end_date_col, new_name){
  dt3 <- dt[rep(seq(1, nrow(dt)), as.numeric( difftime(as.Date(get(end_date_col) ),
                                                       as.Date(get(start_date_col)), units ="days") ) +1 )]

  dt3[, "day" := seq.Date(get(start_date_col)[1], get(end_date_col)[1], by = "day"),
      by = .(get(start_date_col), get(end_date_col)) ]
  setnames(dt3, old = "day", new = new_name)
  return(dt3)
}
