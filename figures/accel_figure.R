library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(patchwork)

gt3x_file_raw = here::here("data", "accel_data_raw", "GT3X+ (01 day).gt3x")
gt3x_file = here::here("data", "accel_data_raw", "sample_data.gt3x")

if(!file.exists(gt3x_file)){
  url = "http://dl.theactigraph.com/demo/files/GT3XPlus-RawData(1-20).zip"

  out_file = here::here("data", "accel_data_raw.zip")
  if(!file.exists(out_file)){
    curl::curl_download(url, out_file, mode = "wb")
  }

  zip_file = here::here("data", "accel_data_raw.zip")
  out_dir  = here::here("data", "accel_data_raw")

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  zip_list = unzip(zip_file, list = TRUE)

  unzip(zip_file, files = zip_list[1]$Name, exdir = out_dir)
  file.rename(gt3x_file_raw, gt3x_file)
}

df = read.gt3x::read.gt3x(path = gt3x_file,
                          asDataFrame = TRUE,
                          imputeZeroes = TRUE)

out = here::here("tutorials", "data", "rf_out.rds")

if(!file.exists(out)){
  envname = "stepcount"
  reticulate::conda_create(envname = envname, packages = c("python=3.9", "openjdk", "pip"))
  Sys.unsetenv("RETICULATE_PYTHON")
  reticulate::use_condaenv(envname)
  reticulate::py_install("stepcount", envname = envname, method = "conda", pip = TRUE)
  stepcount::unset_reticulate_python()
  stepcount::use_stepcount_condaenv()
  library(reticulate)
  stepcount::stepcount_check()
  rf = stepcount::stepcount(df, sample_rate = sample_rate, model_type = "rf")
  readr::write_rds(rf, out)
}

rf = readr::read_rds(out)


rf_segments =
  rf$steps %>%
  mutate(is_walking = steps > 0,
         segment = cumsum(!is_walking & lag(is_walking, default = FALSE))) %>%
  filter(is_walking) %>%
  group_by(segment) %>%
  summarize(start = min(time),
            end = max(time)) %>%
  ungroup() %>%
  select(start, end)


long %>%
  ggplot(aes(x = time, y = accel, color = axis)) +
  geom_line(linewidth = .05) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H") +
  theme(panel.grid = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.6, 0.1)) +
  labs(x = "Time of day (hour)", y = "Acceleration (g)") +
  paletteer::scale_color_paletteer_d("ggthemr::flat", name = "Axis") +
  guides(color = guide_legend(
    nrow = 1,
    override.aes = list(alpha = 1, linewidth = 1)
  ))

df %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2)) %>%
  ggplot(aes(x = time, y = vm)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(panel.grid = element_blank()) +
  labs(x = "Time of day", y = "Acceleration (g)") +
  scale_y_continuous(limits = c(0, 7)) +
  geom_rect(
    data = rf_segments,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "#FFAD72FF",
    alpha = .9,
    inherit.aes = FALSE
  ) +
  geom_line(linewidth = .05)

p1 =
  df %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2)) %>%
  filter(time >= as.POSIXct("2012-06-27 17:01:20", tz = "GMT") &
           time <= as.POSIXct("2012-06-27 17:02:40", tz = "GMT")) %>%
  ggplot(aes(x = time, y = vm)) +
  annotate(geom = "rect",
           xmin =  as.POSIXct("2012-06-27 17:01:21", tz = "GMT"),
           xmax =  as.POSIXct("2012-06-27 17:02:39", tz = "GMT"),
           ymin = -Inf,
           ymax = Inf,
           fill = "#FFAD72FF",
           alpha = 0.5
  ) +
  geom_line(linewidth = .2) +
  scale_x_datetime(date_breaks = "10 secs", date_labels = "%H:%M:%S") +
  theme(panel.grid = element_blank()) +
  labs(x = "Time of day", y = "Acceleration (g)")

p2 = df %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2)) %>%
  filter(time >= as.POSIXct("2012-06-27 17:01:25", tz = "GMT") &
           time <= as.POSIXct("2012-06-27 17:01:35", tz = "GMT")) %>%
  ggplot(aes(x = time, y = vm)) +
  annotate(geom = "rect",
           xmin = as.POSIXct("2012-06-27 17:01:25", tz = "GMT"),
           xmax = as.POSIXct("2012-06-27 17:01:35", tz = "GMT"),
           ymin = -Inf,
           ymax = Inf,
           fill = "#FFAD72FF",
           alpha = 0.5
  ) +
  geom_line(linewidth = .5) +
  scale_x_datetime(date_breaks = "5 secs", date_labels = "%H:%M:%S") +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "", y = "")
p1 + inset_element(p2, left = 0.5, bottom = 0.5, right = 1, top = 1)


