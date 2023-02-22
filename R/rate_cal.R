#' Create an aggregated data table with all proportion of one selected column
#'
#' @param dt data table
#' @param question_col column selected as questions
#' @param aggregated_by_cols grouped by columns
#'
#' @return aggregated data table
#' @export
#'
#' @examples
#' test_dt <- data.table::data.table(
#' Question = c(rep("Good", 3), rep("OK", 3), rep("Bad", 3)),
#' Gender = c(rep("F", 4), rep("M", 5))
#' )
#' dataMojo::pivot_percent_at(test_dt,
#'   question_col = "Question", aggregated_by_cols = "Gender")
pivot_percent_at <- function(dt, question_col, aggregated_by_cols){

  dt <- dt[complete.cases(dt[, question_col, with = FALSE])]
  dt_trans <- dt[,as.list(unlist(lapply(.SD, function(x) list(
    "total" = .N,
    "rate" = lapply(unique(dt[[question_col]]), function(x){
      round((length(x[get(question_col) == x])/.N)*100, 2)}
    ),
    "count" = lapply(unique(dt[[question_col]]), function(x){
      length(x[get(question_col) == x])}
    )
  )
  ) ) ), by = aggregated_by_cols, .SDcols=c(question_col)]
  colnames(dt_trans)[grepl( "rate" , names(dt_trans))] <- paste0( colnames(dt_trans)[grepl( "rate" , names( dt_trans))],
                                                                  "value",
                                                                  unique(dt[[question_col]]) )
  colnames(dt_trans)[grepl( "count" , names(dt_trans))] <- paste0( colnames(dt_trans)[grepl( "count" , names( dt_trans))],
                                                                   "value",
                                                                   unique(dt[[question_col]]) )
  return(dt_trans)

}

#' Create an aggragated data table with all proportion of multiple selected column
#'
#' @param dt data table
#' @param question_col columns selected as questions
#' @param aggregated_by_cols grouped by columns
#'
#' @return an aggragated data table
#' @export
#'
#' @examples
#' test_dt <- data.table::data.table(
#'   Question1 = c(rep("Good", 3), rep("OK", 3), rep("Bad", 3)),
#'   Question2 = c(rep("Good", 2), rep("OK", 2), rep("Bad", 5)),
#'   Gender = c(rep("F", 4), rep("M", 5))
#' )
#' dataMojo::pivot_percent_at_multi(test_dt,
#'   question_col = c("Question1","Question2") , aggregated_by_cols = "Gender")
pivot_percent_at_multi <- function(dt, question_col, aggregated_by_cols){

  multi_q_output <- lapply(c(question_col), function(x) pivot_percent_at(dt,
                                                                            x,
                                                                            aggregated_by_cols = aggregated_by_cols) )
  final_output <- Reduce(function(d1, d2) merge(d1, d2, by = aggregated_by_cols, all.x = TRUE, all.y = FALSE),
                         multi_q_output)
  return(final_output)
}




#' Convert count to percentage
#'
#' @param data data frame
#' @param cols_rowsum columns need to be converted to percentage
#'
#' @return data frame with calculated row percentage
#' @export
#'
#' @examples
#' test_df <- data.frame(
#'   Group = c("A", "B", "C"),
#'   Female = c(2,3,5),
#'   Male = c(10,11, 13)
#' )
#' dataMojo::row_percent_convert(test_df, cols_rowsum = c("Female", "Male"))
row_percent_convert <- function(data, cols_rowsum){
  row_sum <- rowSums(data[,cols_rowsum], na.rm = T)
  data[,cols_rowsum]<- data[cols_rowsum] / row_sum
  return(data)
}


#' create a new column which is the percentage of other columns
#'
#' @param df input data frame
#' @param new_col_name new column name
#' @param numerator_cols numerator columns
#' @param denominator_cols denominator columns
#'
#' @return data frame with a new percentage column
#' @export
#'
#' @examples
#' test_df <- data.frame(
#'   hc1 = c(2, 0, 1, 5, 6, 7, 10),
#'   hc2 = c(1, 0, 10, 12, 4, 1, 9 ),
#'   total = c(10, 2, 0, 39, 23, 27, 30)
#' )
#' dataMojo::col_cal_percent(test_df,
#'   new_col_name = "hc_percentage",
#'   numerator_cols = c("hc1", "hc2"),
#'   denominator_cols = "total"
#' )
col_cal_percent <- function(df, new_col_name, numerator_cols, denominator_cols){
  df[[new_col_name]] <- ifelse(
    rowSums(df[, denominator_cols, drop=FALSE], na.rm = T) == 0,
    "N/A",
    paste0(
      round(rowSums(df[,numerator_cols, drop=FALSE], na.rm = T) / rowSums(df[, denominator_cols, drop=FALSE], na.rm = T), 2) * 100,
      "%")
  )
  return(df)
}
