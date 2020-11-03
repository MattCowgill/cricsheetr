#' Read a cricsheet CSV as a tidy tibble
#' @return  a `tbl_df`
#' @importFrom rlang .data
#' @export
#' @details Will not recognise where "penalty runs" have been awarded, unless
#' they are included in extras.
#' @param path Filepath to CSV
#' @importFrom rlang .data

read_cricsheet_csv <- function(path) {
  raw <- data.table::fread(path, skip = 1, fill = TRUE, sep = ",") %>%
    dplyr::as_tibble()

  info <- raw %>%
    dplyr::filter(.data$V1 == "info")

  balls <- raw %>%
    dplyr::filter(.data$V1 == "ball") %>%
    dplyr::select(-.data$V1)

  # stopifnot(nrow(balls) + nrow(info) == nrow(raw))

  colnames(balls) <- c(
    "innings",
    "delivery",
    "team",
    "batsman",
    "non_striker",
    "bowler",
    "runs_batsman",
    "runs_extras",
    "wicket",
    "player_out"
  )

  balls <- balls %>%
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                dplyr::na_if, y = ""))

  info <- info %>%
    dplyr::select("series" = 2, "value" = 3)

  info <- info %>%
    dplyr::arrange(.data$series) %>%
    dplyr::mutate(series = dplyr::case_when(dplyr::row_number() == 1 ~ .data$series,
                                            .data$series == dplyr::lag(.data$series) ~ paste(.data$series, dplyr::row_number(), sep = "_"),
                                            TRUE ~ .data$series),
                  series = dplyr::if_else(.data$series == "team", "team_a", .data$series))

  comb <- info %>%
    tidyr::spread(key = .data$series, value = .data$value) %>%
    dplyr::bind_cols(balls)

  comb
}
