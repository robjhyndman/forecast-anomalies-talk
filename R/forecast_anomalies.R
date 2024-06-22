density_plot <- function(df, fill) {
  miny <- min(df$y)
  maxy <- max(df$y)
  bind_rows(
      tibble(y = miny, fy = 0),
      df,
      tibble(y = maxy, fy = 0),
    ) |>
    ggplot(aes(y, fy)) +
    geom_polygon(fill = fill) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}
savepng <- function(p, file) {
  file <- here::here("figures", file)
  png(file, width = 12, height = 6, units="cm", res=300, type="cairo-png")
  print(p)
  crop::dev.off.crop(file)
}

tscv_plot <- function(.init, .step, h = 1) {
  expand.grid(
    time = seq(26),
    .id = seq(trunc(11 / .step))
  ) |>
    group_by(.id) |>
    mutate(
      observation = case_when(
        time <= ((min(.id) - 1) * .step + .init) ~ "train",
        time %in% c((min(.id) - 1) * .step + .init + h) ~ "test",
        TRUE ~ "unused"
      )
    ) |>
    ungroup() |>
    filter(.id <= 26 - .init) |>
    ggplot(aes(x = time, y = .id)) +
    geom_segment(
      aes(x = 0, xend = 27, y = .id, yend = .id),
      arrow = arrow(length = unit(0.015, "npc")),
      col = "black", linewidth = .25
    ) +
    geom_point(aes(col = observation), size = 2) +
    scale_y_reverse() +
    scale_color_manual(values = c(train = "#0072B2", test = "#D55E00", unused = "gray")) +
    guides(col = "none") +
    labs(x = "time", y = "") +
    theme_void() +
    theme(axis.title.x = element_text(margin = margin(t = 2))) +
    theme(text = ggplot2::element_text(family = 'Fira Sans'))
}

pbs_plot <- function(pbs, anomalies, series) {
  dates <- anomalies |> dplyr::filter(ATC2 == series) |> dplyr::pull(Month) |> as.Date()
  pbs |>
    dplyr::filter(ATC2 == series) |>
    ggplot2::ggplot(ggplot2::aes(x = Month, y = Scripts)) +
    tsibble::scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::geom_vline(xintercept = dates, color = "red", alpha = 0.4) +
    ggplot2::geom_line() +
    ggplot2::labs(title = paste("Scripts for ATC group", series))
}
