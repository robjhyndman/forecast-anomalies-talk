library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("tibble", "tarchetypes", "fpp3", "weird", "distributional")
)

tar_source()

list(
  # Fira Sans font for graphics
  tar_target(
    name = ggfont,
    command = ggplot2::theme(text = ggplot2::element_text(family = "Fira Sans"))
  ),
  # Paradigms
  tar_target(
    name = paradigm1_plot,
    command = pbs |>
      filter(ATC2 == "A12", Month <= yearmonth("2006 Feb")) |>
      autoplot() +
      theme_void()
  ),
  tar_target(
    name = paradigm1,
    command = savepng(paradigm1_plot, "paradigm1.png")
  ),
  tar_target(
    name = paradigm2_plot,
    command = fr_mortality |>
      filter(Age == 25, Sex == "Male") |>
      autoplot(Mortality) +
      theme_void()
  ),
  tar_target(
    name = paradigm2,
    command = savepng(paradigm2_plot, "paradigm2.png")
  ),
  tar_target(
    name = paradigm3_plot,
    command = fr_mortality |>
      filter(Age %in% c(2:6, 60), Sex == "Female") |>
      autoplot(Mortality) +
      guides(col = "none") +
      theme_void() +
      scale_y_log10()
  ),
  tar_target(
    name = paradigm3,
    command = savepng(paradigm3_plot, "paradigm3.png")
  ),
  # Generic graphs explaining the idea
  tar_target( # One-step forecast distribution
    name = p1,
    command = tibble(y = seq(-4, 4, l = 501), fy = dnorm(y)) |>
      density_plot(fill = "#c1653a") +
      labs(
        y = "f(y)",
        title = "One-step forecast density"
      ) +
      ggfont
  ),
  tar_target( # Anomaly score threshold
    name = u,
    command = 0.5 * qchisq(0.90, df = 1) + 0.5 * log(2 * pi)
  ),
  tar_target( # Anomaly score distribution
    name = p2,
    command = tibble(
      y = seq(0, 8, l = 501),
      fy = 0.5 * dchisq(y, df = 1)
    ) |>
      density_plot(fill = "#478cb2") +
      labs(
        x = "s",
        y = "f(s)",
        title = "Anomaly score density"
      ) +
      geom_vline(xintercept = u, linetype = "dashed") +
      annotate("text",
        x = u + 0.1, y = 0.15,
        label = "90% quantile", vjust = 1, hjust = 0
      ) +
      ggfont
  ),
  tar_target( # Exceedance distribution about 95% quantile
    name = p3,
    command = tibble(
      y = c(0, u, seq(u, 8, l = 501)),
      fy = c(0, 0, 0.5 * dchisq(y[-(1:2)], df = 1))
    ) |>
      density_plot(fill = "#478cb2") +
      labs(
        x = "s",
        y = "f(s | s>u)",
        title = "Anomaly score exceedance density"
      ) +
      geom_vline(xintercept = u, linetype = "dashed") +
      scale_x_continuous(breaks = u, labels = "u") +
      theme(axis.text.x = element_text()) +
      ggfont
  ),
  tar_target(
    name = n01,
    command = savepng(p1, "n01.png")
  ),
  tar_target(
    name = as,
    command = savepng(p2, "as.png")
  ),
  tar_target(
    name = ase,
    command = savepng(p3, "ase.png")
  ),
  tar_target(
    name = tscvplot,
    command = tscv_plot(.init = 8, .step = 1, h = 1) +
      annotate("text",
        x = 9, y = 0, label = "h = 1",
        color = "#c14b14", family = "Fira Sans"
      )
  ),
  # PBS example
  tar_target(
    name = pbs,
    command = PBS |>
      group_by(ATC2) |>
      summarise(Scripts = sum(Scripts) / 1e3) |>
      ungroup()
  ),
  tar_target(
    name = a12plot,
    command = pbs |>
      filter(ATC2 == "A12") |>
      autoplot() +
      labs(title = "Scripts for ATC group A12 (Mineral supplements)") +
      ggfont
  ),
  tar_target(
    name = a12,
    command = pbs |> filter(
      ATC2 == "A12",
      Month <= yearmonth("2006 Jan")
    )
  ),
  tar_target(
    name = a12plus,
    command = pbs |> filter(
      ATC2 == "A12",
      Month <= yearmonth("2006 Feb")
    )
  ),
  tar_target(
    name = fc_a12,
    command = a12 |> model(ets = ETS(Scripts)) |> forecast(h = 1)
  ),
  tar_target(
    name = a12plot1,
    command = fc_a12 |>
      autoplot(a12) +
      labs(
        y = "Scripts (thousands)",
        title = "Forecast of A12 scripts: Feb 2006"
      ) +
      theme(legend.position = "none") +
      ylim(35, 145)
  ),
  tar_target(
    name = a12plot2,
    command = fc_a12 |>
      autoplot(a12plus) +
      labs(
        y = "Scripts (thousands)",
        title = "Forecast of A12 scripts: Feb 2006"
      ) +
      theme(legend.position = "none") +
      ylim(35, 145)
  ),
  tar_target(
    name = pbs_stretch,
    command = stretch_tsibble(pbs, .step = 1, .init = 36)
  ),
  tar_target(
    name = pbs_fit,
    command = pbs_stretch |> model(ets = ETS(Scripts))
  ),
  tar_target(
    name = pbs_fc,
    command = forecast(pbs_fit, h = 1) |> select(-.mean, -.model)
  ),
  tar_target(
    name = pbs_scores,
    command = pbs_fc |>
      left_join(pbs |> rename(actual = Scripts), by = c("ATC2", "Month")) |>
      group_by(.id) |>
      mutate(
        s = -log_likelihood(Scripts, actual),
        prob = lookout(density_scores = s, threshold = 0.9)
      ) |>
      ungroup()
  ),
  tar_target(
    name = pbs_anomalies,
    command = pbs_scores |>
      filter(prob < 0.05)
  ),
  tar_target(
    name = l03,
    command = pbs_plot(pbs, pbs_anomalies, "L03")
  ),
  tar_target(
    name = n07,
    command = pbs_plot(pbs, pbs_anomalies, "N07")
  ),
  tar_target(
    name = r06,
    command = pbs_plot(pbs, pbs_anomalies, "R06")
  ),
  # French mortality example
  tar_target(
    name = fr_mortality,
    command = vital::read_hmd_files("Mx_1x1.txt") |>
      filter(Sex != "Total", Year < 2000) |>
      vital::collapse_ages(max_age = 85) |>
      as_tsibble() |>
      select(Year, Age, Sex, Mortality)
  ),
  tar_target(
    name = fr_mortality_time_plots,
    command = fr_mortality |>
      ggplot(aes(x = Year, y = Mortality,
                 color = as.factor(Age), group = Age)) +
      geom_line() +
      facet_grid(. ~ Sex) +
      scale_y_log10() +
      theme(legend.position = "none")
  ),
  tar_target(
    name = fr_fit,
    command = fr_mortality |>
      model(stl = STL(log(Mortality)))
  ),
  tar_target(
    name = fr_sigma,
    command = augment(fr_fit) |>
      as_tibble() |>
      group_by(Age, Sex) |>
      summarise(sigma = IQR(.innov) / 1.349, .groups = "drop")
  ),
  tar_target(
    name = fr_scores,
    command = augment(fr_fit) |>
      as_tibble() |>
      left_join(fr_sigma) |>
      mutate(
        s = -log(dnorm(.innov / sigma)),
        prob = lookout(density_scores = s, threshold_probability = 0.9)
      ) |>
      select(-.model, -.resid, -.fitted, -sigma)
  ),
  tar_target(
    name = fr_anomalies,
    command = fr_scores |>
      filter(prob < 0.05) |>
      as_tibble() |>
      select(Year, Sex, Age) |>
      distinct() |>
      left_join(fr_mortality)
  ),
  tar_target(
    name = yrs,
    command = fr_anomalies |>
      select(Year, Sex) |>
      distinct()
  ),
  tar_target(
    name = fr_anomalies_plot_male,
    command = fr_anomalies |>
      filter(Sex == "Male") |>
      ggplot(aes(x = Year, y = Age)) +
      facet_grid(. ~ Sex) +
      scale_x_continuous(
        breaks = seq(1820, 2000, by = 20),
        limits = range(yrs$Year)
      ) +
      geom_point(col = "#478cb2") +
      ylim(4, 85)
  ),
  tar_target(
    name = fr_anomalies_plot_female,
    command = fr_anomalies |>
      filter(Sex == "Female") |>
      ggplot(aes(x = Year, y = Age)) +
      facet_grid(. ~ Sex) +
      scale_x_continuous(
        breaks = seq(1820, 2000, by = 20),
        limits = range(yrs$Year)
      ) +
      labs(title = "French mortality anomalies") +
      geom_point(col = "#c1653a") +
      ylim(4, 85)
  ),
  tar_target(
    name = fr_anomalies_plot,
    command = patchwork::wrap_plots(
      fr_anomalies_plot_female,
      fr_anomalies_plot_male,
      nrow = 1
    )
  ),
  tar_target(
    name = fr_anomalies_plot_male2,
    command = fr_anomalies |>
      filter(Sex == "Male") |>
      ggplot(aes(x = Year, y = Age)) +
      facet_grid(. ~ Sex) +
      scale_x_continuous(
        breaks = seq(1820, 2000, by = 20),
        limits = range(yrs$Year)
      ) +
      geom_vline(
        xintercept = unique(yrs$Year[yrs$Sex == "Male"]),
        alpha = 0.5, color = "grey"
      ) +
      geom_point(col = "#478cb2") +
      ggrepel::geom_text_repel(
        data = yrs[yrs$Sex == "Male", ],
        aes(y = 75, label = Year), col = "#478cb2", size = 3, seed = 1967
      ) +
      ylim(4, 85)
  ),
  tar_target(
    name = fr_anomalies_plot_female2,
    command = fr_anomalies |>
      filter(Sex == "Female") |>
      ggplot(aes(x = Year, y = Age)) +
      facet_grid(. ~ Sex) +
      scale_x_continuous(
        breaks = seq(1820, 2000, by = 20),
        limits = range(yrs$Year)
      ) +
      geom_vline(
        xintercept = unique(yrs$Year[yrs$Sex == "Female"]),
        alpha = 0.5, color = "grey"
      ) +
      labs(title = "French mortality anomalies") +
      geom_point(col = "#c1653a") +
      ggrepel::geom_text_repel(
        data = yrs[yrs$Sex == "Female", ],
        aes(y = 75, label = Year), col = "#c1653a", size = 3, seed = 1967
      ) +
      ylim(4, 85)
  ),
  tar_target(
    name = fr_anomalies_plot2,
    command = patchwork::wrap_plots(
      fr_anomalies_plot_female2,
      fr_anomalies_plot_male2,
      nrow = 1
    )
  ),
  tar_target(
    name = fr_anomalies_plot3,
    command = patchwork::wrap_plots(
      fr_anomalies_plot_female2 +
        geom_hline(yintercept = 25, color = "gray"),
      fr_anomalies_plot_male2 +
        geom_hline(yintercept = 25, color = "gray") +
        annotate("text", x = 1890, y = 27, label = "Age 25", family = "Fira Sans"),
      nrow = 1
    )
  ),
  tar_target(
    name = fr_25,
    command =
      fr_mortality |>
        filter(Age == 25) |>
        ggplot(aes(x = Year, y = Mortality, color = Sex)) +
        geom_line() +
        facet_grid(Sex ~ ., scales = "free_y") +
        geom_point(data = fr_anomalies |> filter(Age == 25)) +
        labs(title = "French mortality: Age 25")
  ),
  # Slides
  tar_quarto(
    name = slides,
    path = "forecast_anomalies.qmd",
    extra_files = c("before-title.tex", fs::dir_ls("figures"))
  )
)
