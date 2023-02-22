## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----row----------------------------------------------------------------------
library(dataMojo)
library(data.table)
test_df <- data.frame(
      Group = c("A", "B", "C"),
      Female = c(2,3,5),
      Male = c(10,11, 13)
    )
print(test_df)
dataMojo::row_percent_convert(test_df, cols_rowsum = c("Female", "Male"))

## ----survey1------------------------------------------------------------------
library(dataMojo)
library(data.table)
   test_dt <- data.table::data.table(
      Question = c(rep("Good", 3), rep("OK", 3), rep("Bad", 3)),
      Gender = c(rep("F", 4), rep("M", 5))
    )
   print(test_dt)
   dataMojo::pivot_percent_at(test_dt, 
                                 question_col = "Question", aggregated_by_cols = "Gender")

## ----survey2------------------------------------------------------------------
library(dataMojo)
library(data.table)
test_dt <- data.table(
      Question1 = c(rep("Good", 3), rep("OK", 3), rep("Bad", 3)),
      Question2 = c(rep("Good", 2), rep("OK", 2), rep("Bad", 5)),
      Gender = c(rep("F", 4), rep("M", 5))
    )
print(test_dt)
dataMojo::pivot_percent_at_multi(test_dt, 
                                    question_col = c("Question1","Question2") , aggregated_by_cols = "Gender") 

## ----col_wise_percentage------------------------------------------------------
library(dataMojo)
test_df <- data.frame(
      hc1 = c(2, 0, 1, 5, 6, 7, 10),
      hc2 = c(1, 0, 10, 12, 4, 1, 9 ),
      total = c(10, 2, 0, 39, 23, 27, 30)
    )
print(test_df)
dataMojo::col_cal_percent(test_df, 
                          new_col_name = "hc_percentage", 
                          numerator_cols = c("hc1", "hc2"), 
                          denominator_cols = "total"
                          ) 

## ----ex1----------------------------------------------------------------------
library(dataMojo)
library(data.table)
data("dt_dates")
dt_dates <- setDT(dt_dates)
dataMojo::select_cols(dt_dates, c("Start_Date", "Full_name"))

## ----ex2----------------------------------------------------------------------
data("dt_dates")
library(data.table)
data("dt_dates")
dataMojo::str_split_col(dt_dates,
              by_col = "Full_name",
              by_pattern = ", ",
              match_to_names = c("First Name", "Last Name"))

## ----ex3----------------------------------------------------------------------
data("dt_values")
dataMojo::filter_all(dt_values, operator = "l", .2)

## ----ex4----------------------------------------------------------------------
data("dt_values")
dataMojo::filter_any(dt_values, operator = "l", .1)

## ----ex5----------------------------------------------------------------------
data("dt_values")
dataMojo::filter_all_at(dt_values, operator = "l", .1, c("A1", "A2"))

## ----ex6----------------------------------------------------------------------
data("dt_values")
dataMojo::filter_any_at(dt_values, operator = "l", .1, c("A1", "A2"))

## ----ex7----------------------------------------------------------------------
data("dt_missing")
dataMojo::fill_NA_with(dt_missing, fill_cols = c("Full_name"), fill_value = "pending")

## ----ex8----------------------------------------------------------------------
data("dt_groups")
print(head(dt_groups))

## ----ex9----------------------------------------------------------------------
data("dt_groups")
dataMojo::dt_group_by(dt_groups, 
            group_by_cols = c("group1", "group2"), 
            summarize_at = "A1", 
            operation = "mean")

## ----ex10---------------------------------------------------------------------
data("dt_groups")
dataMojo::get_row_group_by(dt_groups, 
                 group_by_cols = c("group1", "group2"), 
                 fetch_row = "first")

## ----ex11---------------------------------------------------------------------
data("dt_groups")
dataMojo::get_row_group_by(dt_groups, 
                 group_by_cols = c("group1", "group2"), 
                 fetch_row = "last")

## ----ex12---------------------------------------------------------------------
data("dt_dates")
print(head(dt_dates))
dataMojo::reshape_longer(dt_dates, 
               keep_cols = "Full_name", 
               label_cols = c("Date_Type"), 
               value_cols = "Exact_date")

## ----ex13---------------------------------------------------------------------
data("dt_long")
print(head(dt_long))
dataMojo::reshape_wider(dt_long, 
              keep_cols = c("Full_name"), 
              col_label = c("Date_Type"), 
              col_value = "Exact_date")

## ----ex14---------------------------------------------------------------------
data("starwars_simple")
starwars_simple[]
row_expand_pattern(starwars_simple, "films", ", ", "film")[]

## ----ex15---------------------------------------------------------------------
dt_dates_simple <- data.table(
  Start_Date = as.Date(c("2020-02-03", "2020-03-01") ),
  End_Date = as.Date(c("2020-02-05", "2020-03-02") ),
  group = c("A", "B")
)
dt_dates_simple[]
row_expand_dates(dt_dates_simple, "Start_Date", "End_Date", "Date")[]


