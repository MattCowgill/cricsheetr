#' Read a cricsheet CSV as a tidy tibble
#' @return  a `tbl_df`
#' @importFrom rlang .data
#' @export
#' @details Will not recognise where "penalty runs" have been awarded, unless
#' they are included in extras.
#' @param path Filepath to CSV

read_cricsheet_csv <- function(path) {
  raw <- data.table::fread(path, skip = 1, fill = TRUE, sep = ",") %>%
    dplyr::as_tibble()

  info <- raw %>%
    dplyr::filter(V1 == "info")

  balls <- raw %>%
    dplyr::filter(V1 == "ball") %>%
    dplyr::select(-V1)

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
    dplyr::arrange(series) %>%
    dplyr::mutate(series = dplyr::case_when(dplyr::row_number() == 1 ~ series,
                                            series == dplyr::lag(series) ~ paste(series, dplyr::row_number(), sep = "_"),
                                            TRUE ~ series),
                  series = dplyr::if_else(series == "team", "team_a", series))

  comb <- info %>%
    tidyr::spread(key = series, value = value) %>%
    dplyr::bind_cols(balls)

  comb
}
