url <- "https://www.australia.gov.au/school-term-dates"
url <- "https://web.archive.org/web/20230407100710/https://www.australia.gov.au/school-term-dates"

states <- rvest::read_html(url) |>
    rvest::html_elements(".title") |>
    rvest::html_text() |>
    stringr::str_squish()


dates_raw <- rvest::read_html(url) |>
	rvest::html_elements(".cmp-text") |>
	rvest::html_text() |>
    # Keep just the ones that start with term
    stringr::str_squish() |>
    purrr::keep(
        stringr::str_detect,
        pattern = "^(Term)(.*)"
    ) |>
 purrr::map(
		stringr::str_split,
		pattern = "Term [:digit:] "
	) |>
		purrr::flatten() |>
	purrr::map(purrr::compact) |>
    rlang::set_names(states)

dates_raw

library(RVerbalExpressions)
rx_find(value = "Term") |>
rx_start_of_line() |>
	rx_anything() |>
	rx_one_or_more() |>

	rx_end_of_line()

terminate <- function(strings) {
    split <- stringr::str_split(
	rx_find("\n") |>     strings,
        "Term [:digit:] [:digit:]*: "
    ) |>
        unlist() |>
        purrr::keep(nzchar)

    ranges <- split |>
        stringr::str_extract_all("[:digit:]{1,2}+ [A-z]+") |>
        purrr::map(magrittr::extract, 1:2) |>
        purrr::map(lubridate::dmy)

    make_term <- function(range) {
        seq(range[[1]], range[[2]], by = 1)
    }
    ranges |>
        purrr::map(make_term) |>
        purrr::reduce(c)
}

school_terms <- dates_raw |>
    purrr::map(terminate)

library(pins)
board <- board_connect()

board |>
    pin_write(
        x = school_terms,
        name = "school_terms",
        description = "Scraped from https://www.australia.gov.au/school-term-dates"
    )
school_terms

pins::pin_read(board, "bas.latcham/school_terms")

url2 <- "https://mumcentral.com.au/australian-school-term-dates-2019-2020/"

read_html(url2)

states <- read_html(url2) |>
    html_elements(".post-content") |>
    html_elements("h3") |>
    html_text() |>
    str_squish() |>
    keep(str_detect, pattern = "[A-z]") |>
    magrittr::extract(2:9)



read_html(url2) |>
    html_elements(".post-content") |>
    # html_elements("h3") |>
    html_elements("ul") |>
    html_text() |>
    tibble(
        state = rep(states, each = 2),
        year = rep(c(2019, 2020), 8),
        str = _
    )
