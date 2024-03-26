library(tidyverse)

## Make a dunning-kruger graph and animation

dk <- data.frame(id = c("Unsure", "Unsure", "Interest", "Ignorance", "Despair", "Enlightenment", "Expertise", "Expertise"),
                 confidence = c(0, 0, 10, 90, 10, 90, 90, 90),
                 year = c(0, 10, 40, 80, 320, 850, 990, 1000)
)

# Create a full sequence for x axis
full_range <- data.frame(year = seq(0, 1000, by = 1))

# Merge the full range of years with existing data using a left join
complete_dk <- merge(full_range, dk, by = "year", all.x = TRUE)

complete_dk$confidence <- approx(
  x = dk$year,
  y = dk$confidence,
  xout = complete_dk$year,
  method = "linear",
  rule = 2
)$y #$y specifically extracts the interpolated y-values from the result

# Define the window size for the moving average
window_size <- 30  # Adjust this value as needed

# Calculate the moving average of confidence using rollapply from zoo package
#smoothed_values <- zoo::rollapply(complete_dk$confidence, width = window_size, FUN = mean, align = "center", fill = NA)

# Create a new dataframe with the smoothed values
smoothed_dk <- data.frame(year = complete_dk$year, confidence_smoothed = zoo::rollapply(complete_dk$confidence, width = window_size, FUN = mean, align = "center", fill = NA))

# Merge the smoothed data with the original data
complete_dk <- inner_join(complete_dk, smoothed_dk) %>%
  mutate(confidence_smoothed = ifelse(is.na(confidence_smoothed), confidence, confidence_smoothed))

complete_dk$id[complete_dk$year >= 1 & complete_dk$year <= 9] <- "Unsure"
complete_dk$id[complete_dk$year >= 11 & complete_dk$year <= 39] <- "Unsure"
complete_dk$id[complete_dk$year >= 41 & complete_dk$year <= 79] <- "Interest"
complete_dk$id[complete_dk$year >= 81 & complete_dk$year <= 319] <- "Abandonment"
complete_dk$id[complete_dk$year >= 321 & complete_dk$year <= 849] <- "Enlightenment"
complete_dk$id[complete_dk$year >= 851 & complete_dk$year <= 989] <- "Expertise"
complete_dk$id[complete_dk$year >= 991 & complete_dk$year <= 999] <- "Expertise"

dk_pause <- complete_dk %>%
  mutate(show_time = case_when(year %in% c(0, 10, 40, 80, 320, 850, 990, 1000) ~ 100,
                               TRUE           ~ 1)) %>%
  # uncount is a tidyr function which copies each line 'n' times
  uncount(show_time) %>%
  mutate(reveal_time = row_number())

dk_filter <- complete_dk %>% filter(year %in% c( "0", "10", "60", "80", "320", "700", "1000"))

d <- ggplot(dk_pause, aes(year, confidence_smoothed)) +
  geom_line(size = 1.5, color = "blue", lineend = "round") +
  coord_cartesian(clip = 'off') + 
  labs(title = 'Transitioning to knowledge work', y = 'Confidence', x = 'Compentence (Self-assessed)') + 
  theme_minimal() + 
  theme(plot.background = element_rect(fill = "white"),
        plot.margin = margin(5.5, 70, 5.5, 5.5),
        axis.ticks = element_blank(),
        axis.text = element_blank()) 

dk_plot <- d +
  geom_text(data = dk_filter,
            aes(x = 1000.9, label = id), hjust = 0) +
  geom_point(data = dk_filter, 
             size = 2) +
  geom_segment(data = dk_filter, 
               aes(xend = 1000, yend = confidence_smoothed), linetype = 2, colour = 'grey') 

dk_plot_imp <- dk_plot + 
  geom_vline(xintercept = 320, linetype = 2, colour = 'red') +
  geom_label(aes(x = 450, y = 65, label = 'Peak\nimposter\nsyndrom'), colour = 'red')

ggplot2::ggsave(filename = "dunning-kruger.svg", plot = dk_plot, path = "~/BrownLabNotebook/literature-workshop/images/", dpi = 600, width = 130, height = 70, units = "mm")

ggplot2::ggsave(filename = "dunning-kruger-imposter.svg", plot = dk_plot_imp, path = "~/BrownLabNotebook/literature-workshop/images/", dpi = 600, width = 130, height = 70, units = "mm")

dk_anim <- d + 
  geom_text(aes(x = 1000.9, label = id), hjust = 0) + 
  geom_point(size = 2) +
  geom_segment(aes(xend = 1000, yend = confidence_smoothed), linetype = 2, colour = 'grey') +
  gganimate::transition_reveal(reveal_time) +
  gganimate::ease_aes('linear')

gganimate::animate(dk_anim, fps = 5, nframes = 100, res = 300, width = 130, height = 70, units = "mm")

gganimate::anim_save("dunning-kruger.gif", animation = dk_anim, path = "~/BrownLabNotebook/literature-workshop/images", fps = 5, nframes = 100, res = 300, width = 130, height = 70, units = "mm")

## Make a Gartner Hype Cycle plot and animation

gh <- data.frame(id = c("Trigger", "Overestimate", "Disillusionment", "Productivity"),
                 visibility = c(0, 90, 20, 50),
                 time = c(0, 300, 500, 700)
)

# Create a sequence of years from 0 to 30
full_range <- data.frame(time = seq(0, 1000, by = 1))

# Merge the full range of years with existing data using a left join
complete_gh <- merge(full_range, gh, by = "time", all.x = TRUE)

complete_gh$visibility <- approx(
  x = gh$time,
  y = gh$visibility,
  xout = complete_gh$time,
  method = "linear",
  rule = 2
)$y #$y specifically extracts the interpolated y-values from the result

# Define the window size for the moving average
window_size <- 30  # Adjust this value as needed

# Calculate the moving average of confidence using rollapply from zoo package
#smoothed_values <- zoo::rollapply(complete_gh$visibility, width = window_size, FUN = mean, align = "center", fill = NA)

# Create a new dataframe with the smoothed values
smoothed_gh <- data.frame(time = complete_gh$time, visibility_smoothed = zoo::rollapply(complete_gh$visibility, width = window_size, FUN = mean, align = "center", fill = NA))

# Merge the smoothed data with the original data
complete_gh <- inner_join(complete_gh, smoothed_gh) %>%
  mutate(visibility_smoothed = ifelse(is.na(visibility_smoothed), visibility, visibility_smoothed))

complete_gh$id[complete_gh$time >= 1 & complete_gh$time <= 299] <- "Excitement"
complete_gh$id[complete_gh$time >= 301 & complete_gh$time <= 499] <- "Abandonment"
complete_gh$id[complete_gh$time >= 501 & complete_gh$time <= 699] <- "Enlightenment"
complete_gh$id[complete_gh$time >= 701 & complete_gh$time <= 1000] <- "Productivity"

gh_pause <- complete_gh %>%
  mutate(show_time = case_when(time %in% c(0, 300, 500, 700, 1000) ~ 100,
                               TRUE           ~ 1)) %>%
  # uncount is a tidyr function which copies each line 'n' times
  uncount(show_time) %>%
  mutate(reveal_time = row_number())

gh_filter <- complete_gh %>%
  filter(time %in% c(0, 120, 300, 350, 500, 1000))

g <- ggplot(gh_pause, aes(time, visibility_smoothed)) +
  geom_line(size = 1.5, color = "blue", lineend = "round") +

  coord_cartesian(clip = 'off') + 
  labs(title = 'Adopting new projects or techniques', y = 'Priority', x = 'Time') + 
  theme_minimal() + 
  theme(plot.background = element_rect(fill = "white"),
        plot.margin = margin(5.5, 70, 5.5, 5.5), 
        axis.ticks = element_blank(),
        axis.text = element_blank()) 

g
gh_plot <- g + 
  geom_text(data = gh_filter,
            aes(x = 1000.9, label = id), hjust = 0) +
  geom_point(data = gh_filter, 
             size = 2) +
  geom_segment(data = gh_filter,
               aes(xend = 1000, yend = visibility_smoothed), linetype = 2, colour = 'grey') 
gh_plot

gh_plot_imp <- gh_plot + 
  geom_vline(xintercept = 500, linetype = 2, colour = 'red') +
  geom_label(aes(x = 650, y = 60, label = 'Peak\nimposter\nsyndrom'), colour = 'red')
gh_plot_imp

ggplot2::ggsave("gartner-hype.svg", plot = gh_plot, path = "~/BrownLabNotebook/literature-workshop/images/", dpi = 600, width = 130, height = 70, units = "mm")

ggplot2::ggsave("gartner-hype-imposter.svg", plot = gh_plot_imp, path = "~/BrownLabNotebook/literature-workshop/images/", dpi = 600, width = 130, height = 70, units = "mm")

g_anim <- g + geom_text(aes(x = 1000.9, label = id), hjust = 0) +
  geom_segment(aes(xend = 1000, yend = visibility_smoothed), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  gganimate::transition_reveal(reveal_time) +
  gganimate::ease_aes('linear')
gganimate::animate(g_anim, fps = 5, nframes = 100, res = 300, width = 130, height = 70, units = "mm")

gganimate::anim_save("gartner-hype.gif", animation = g_anim, path = "~/BrownLabNotebook/literature-workshop/images/", fps = 5, nframes = 100, res = 300, width = 130, height = 70, units = "mm")
