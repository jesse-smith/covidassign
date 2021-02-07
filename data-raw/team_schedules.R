team_schedules <- tibble::tribble(
  ~team,         ~schedule,       ~anchor,
  "a",        "weekdays", as.Date(NA),
  "b",        "weekdays", as.Date(NA),
  "c",        "weekdays", as.Date(NA),
  "d",     "5-2-6-2-5-4", as.Date("2021-06-01"),
  "e",     "5-2-6-2-5-4", as.Date("2021-04-06"),
  "f",     "5-2-6-2-5-4", as.Date("2020-02-09"),
  "g", "nights-weekends", as.Date(NA),
  "h", "nights-weekends", as.Date(NA)
)

usethis::use_data(team_schedules, overwrite = TRUE)
