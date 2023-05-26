## code to prepare `school_dates` dataset goes here

library(readxl)
library(tidyverse)
library(strayr)
library(janitor)
library(pointblank)
library(stringr)
# remotes::install_github("coolbutuseless/numberwang")
library(numberwang)

xls <- list.files("./data/", full.names = TRUE)
xl <- xls[[21]]


clean_terms <- function(xl) {
	dat_raw <- read_excel(xl, sheet = "STATE_school_holidays") |>
		clean_names() |>
		drop_na()

	# Check that all the row status is Valid Input
	invalid_rows <- dat_raw |>
		filter(term == "Status") |>
		pivot_longer(nsw:act) |>
		filter(value != "Valid Input") |>
		nrow()

	if (invalid_rows > 0) {
		warning("There are ", invalid_rows, " invalid rows in the file ", xl)
	}

	dat_raw |>
		filter(term != "Status") |>
		pivot_longer(nsw:act, names_to = "state", values_to = "date") |>
		mutate(
			date = excel_numeric_to_date(as.numeric(date)),
			state = clean_state(state),
			term = snakecase::to_snake_case(term)
		) |>
		separate(term, into = c("word", "term", "phase")) |>
		select(-word) |>
		pivot_wider(names_from = phase, values_from = date) |>
		mutate(term = numberwang::words_to_num(term), year = year(start)) |>
		relocate(state, year, term, start, end)
}

terms <- purrr::map_dfr(xls, clean_terms)

# 1978-05-06 to 1978-05-21

terms |>
	arrange(state) |>
	group_by(state) |>
	mutate(
		start2 = dplyr::lag(end, order_by = start) + 1,
	end2 = start - 1
    ) |>
    select(state, year, start = start2, end = end2) |>
					ungroup() |>
drop_na()

hols |> bind_rows(
	terms |>
		mutate(type = "term")
) |>
arrange(state, start) |>
	dsp_plot(aes(x = ))

# usethis::use_data(school_dates, overwrite = TRUE)
terms |> write_csv("terms.csv")
