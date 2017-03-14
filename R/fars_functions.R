# William Green
# Coursera "Building R Packages"
# Week 4 Assigment -- Documenting Functions
# Mar 14, 2017
#'
#'
#' This function reads data in a data file from the US National Highway Traffic
#' Safety Administration's Fatality Analysis Reporting System.
#'
#' @param filename A file name that you want to read; it should be a character.
#'
#' @return This function returns a data frame table that stores data. However,
#' If the file you want to read does not exist, it gives you an error message.
#'
#' @details \code{fars_read(filename)} utilizes \code{\link[readr]{read_csv}}.
#' In the end, this function provides a '\code{\link[dplyr]{tbl_df}}' class to a data frame.
#' See \code{\link[tibble]{tibble-package}} for more details.
#'
#' @note If you need to read data for multiple years at a time, use \code{\link{fars_read_years}}.
#' In addition, you can use \code{\link{make_filename}} to generate a file name
#' more easily.
#'
#' @examples \dontrun{make_filename(2015) %>% fars_read}
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
     if(!file.exists(filename))
          stop("file '", filename, "' does not exist")
     data <- suppressMessages({
          readr::read_csv(filename, progress = FALSE)
     })
     dplyr::tbl_df(data)
}

#' Create a file name
#'
#' This function creates an appropriate file name for a given year.
#'
#' @param year A year used to  make a file name. This should be a numeric.
#'
#' @return This function returns an appropriate file name.
#' This is implemented for \code{\link{fars_read}}. You should put in a numeric;
#' otherwise, it leads to an error.
#'
#' @examples \dontrun{make_filename(2013)} #returns "accident_2013.csv.bz2"
#'
#' @export
make_filename <- function(year) {
     year <- as.integer(year)
     sprintf("accident_%d.csv.bz2", year)
}

#' Read files into a list of data frame tables
#'
#' This function reads data from the US National Highway Traffic
#' Safety Administration's Fatality Analysis Reporting System, and it returns
#' a list containing the data frame tables.
#'
#' @param years A numeric vector of years that you are interested in.
#'
#' @return This function returns a list of data frame tables. If there are any
#' inappropriate inputs, it leads to a warning message and returns \code{NULL}.
#'
#' @examples \dontrun{fars_read_years(c(2013, 2015))}
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @export
fars_read_years <- function(years) {
     lapply(years, function(year) {
          file <- make_filename(year)
          tryCatch({
               dat <- fars_read(file)
               dplyr::mutate_(dat, year = ~ year) %>%
                    dplyr::select_(.dots = c("MONTH", "year"))
          }, error = function(e) {
               warning("invalid year: ", year)
               return(NULL)
          })
     })
}

#' Summarize FARS data
#'
#' This function summarizes a list of data frames. You can check the number of
#' cases per year every month.
#'
#' @inheritParams fars_read_years
#'
#' @return This function returns a data frame table(\code{tbl_df}) summarizing data.
#' One of the column names is "MONTH", and the others are years coerced to characters.
#' If any element of the input vector does not match a file list, it gives you a warning message.
#'
#' @examples \dontrun{fars_summarize_years(2014:2015)}
#'
#' @importFrom dplyr bind_rows group_by summarize n
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
     dat_list <- fars_read_years(years)
     dplyr::bind_rows(dat_list) %>%
          dplyr::group_by_(~ year, ~ MONTH) %>%
          dplyr::summarize_(n = ~ n()) %>%
          tidyr::spread_(key_col = "year", value_col = "n")
}

#' Map the locations of accidents from FARS data
#'
#' This function maps the locations of accidents using latitude and longitude information
#' from the FARS data files.
#'
#' @param state.num A numeric indicating a state number.
#' @param year A year to plot; it should be a numeric.
#'
#' @return This function makes a plot that represents locations of accidents.
#' If there are no matches of \code{state.num} in a data set, it results in an error.
#'
#' @details \code{\link[maps]{map}} is utilized to draw a state map.
#'
#' @examples \dontrun{fars_map_state(1, 2015)}
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
     filename <- make_filename(year)
     data <- fars_read(filename)
     state.num <- as.integer(state.num)

     if(!(state.num %in% unique(data$STATE)))
          stop("invalid STATE number: ", state.num)
     data.sub <- dplyr::filter_(data, ~ STATE == state.num)
     if(nrow(data.sub) == 0L) {
          message("no accidents to plot")
          return(invisible(NULL))
     }
     is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
     is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
     with(data.sub, {
          maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                    xlim = range(LONGITUD, na.rm = TRUE))
          graphics::points(LONGITUD, LATITUDE, pch = 46)
     })
}
