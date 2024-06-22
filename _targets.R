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
  # Generic graphs explaining the idea
  tar_target(# One-step forecast distribution
    name = p1,
    command = tibble(y = seq(-4,4,l=501), fy = dnorm(y)) |>
      density_plot(fill = "#e67540") +
      labs(
        y = "f(y)",
        title = "One-step forecast density"
      ) +
      ggfont
  ),
  tar_target(# Anomaly score threshold
    name = u,
    command = 0.5*qchisq(0.95, df = 1) + 0.5*log(2*pi)
  ),
  tar_target(# Anomaly score distribution
    name = p2,
    command = tibble(
      y = seq(0, 8, l=501),
      fy = 0.5*dchisq(y, df = 1)
    ) |>
      density_plot(fill = "#94a8df") +
      labs(
        x = "s",
        y = "f(s)",
        title = "Anomaly score density"
      ) +
      geom_vline(xintercept = u, linetype = "dashed") +
      annotate("text", x = u+0.1, y = 0.15, label = "95% quantile", vjust = 1, hjust = 0) +
      ggfont
  ),
  tar_target(# Exceedance distribution about 95% quantile
    name = p3,
    command = tibble(
      y = c(0,u,seq(u, 8, l=501)),
      fy = c(0,0, 0.5*dchisq(y[-(1:2)], df = 1))
    ) |>
      density_plot(fill = "#94a8df") +
      labs(
        x = "s",
        y = "f(s | s>u)",
        title = "Anomaly score exceedance density"
      ) +
      geom_vline(xintercept = u, linetype = "dashed") +
      scale_x_continuous(breaks = u, labels = "u") + theme(axis.text.x = element_text()) +
      ggfont
  ),
  tar_target(name = n01, command = savepng(p1, "n01.png")),
  tar_target(name = as, command =  savepng(p2, "as.png")),
  tar_target(name = ase, command = savepng(p3, "ase.png")),
  tar_target(
    name = tscvplot,
    command = tscv_plot(.init = 8, .step = 1, h = 1) +
      annotate("text", x = 9, y = 0, label = "h = 1",
               color = "#D55E00", family = 'Fira Sans')
  ),
  # PBS example
  tar_target(
    name = pbs,
    command = PBS |>
      group_by(ATC2) |>
      summarise(Scripts = sum(Scripts)/1e3) |>
      ungroup()
  ),
  tar_target(
    name = a12plot,
    command = pbs  |>
      filter(ATC2 == "A12") |>
      autoplot() +
      labs(title = "Scripts for ATC group A12") +
      ggfont
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
    command = forecast(pbs_fit, h=1) |> select(-.mean, -.model)
  ),
  tar_target(
    name = pbs_scores,
    command = pbs_fc |>
        left_join(pbs |> rename(actual = Scripts), by = c("ATC2", "Month")) |>
        mutate(
          s = -log_likelihood(Scripts, actual), # Density scores
          prob = lookout(density_scores = s)    # Probability not an anomaly
        )
  ),
  tar_target(
    name = pbs_anomalies,
    command = pbs_scores |>
      filter(prob < 0.01)
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
      vital::collapse_ages(max_age = 100) |>
      filter(Sex != "Total") |>
      as_tsibble() |>
      select(Year, Age, Sex, Mortality)
  ),
  tar_target(
    name = fr_stretch,
    command = fr_mortality |>
      stretch_tsibble(.init = 30, .step=1) |>
      select(.id, everything())
  ),
  tar_target(
    name = fr_fit,
    command = fr_stretch |> model(arima = ARIMA(Mortality))
  ),
  tar_target(
    name = fr_fc,
    command = forecast(fr_fit, h = 1)
  ),
  tar_target(
    name = fr_scores,
    command = fr_fc |>
      select(Year, Age, Sex, Mortality) |>
      left_join(fr_mortality |> rename(actual = Mortality)) |>
      mutate(
        s = -log_likelihood(Mortality, actual), # Density scores
        prob = lookout(density_scores = s)    # Probability not an anomaly
      )
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
      distinct() |>
      mutate(Age = 10)
  ),
  tar_target(
    name = fr_anomalies_plot,
    command = fr_anomalies |>
      ggplot(aes(x = Year, y = Age, color = Sex)) +
      facet_grid(. ~ Sex) +
      scale_x_continuous(breaks = seq(1870, 1945, by=10)) +
      labs(title = "French mortality anomalies") +
      geom_point() +
      ylim(4,100) +
      theme(legend.position = "none")
  ),
  tar_target(
    name = fr_anomalies_plot2,
    command = fr_anomalies |>
      ggplot(aes(x = Year, y = Age, color = Sex)) +
      facet_grid(. ~ Sex) +
      scale_x_continuous(breaks = seq(1870, 1945, by=10)) +
      labs(title = "French mortality anomalies") +
      geom_vline(xintercept = yrs$Year, color = "grey") +
      geom_point() +
      ylim(4,100) +
      theme(legend.position = "none") +
      ggrepel::geom_text_repel(data = yrs, aes(label = Year), size=3)
  ),
  # Slides
  tar_quarto(
    name = slides,
    path = "forecast_anomalies.qmd",
    extra_files = "before-title.tex"
  )
)
