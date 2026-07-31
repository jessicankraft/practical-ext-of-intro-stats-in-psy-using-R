# this is for figure generation only. 
# this is not proper statistics by any means.

# pkgs
pkgs <- c('ggplot2', 'ggimage', 'dplyr')
xfun::pkg_attach2(pkgs, message = FALSE)

in_logo_psi <- "docs/figs/psi.svg"
in_logo_r <- "docs/figs/r.svg"
out_path <- "docs/figs/logo.svg"

df <- carData::Salaries |>
  mutate(logo = case_when(
    discipline == 'A' ~ in_logo_psi,
    discipline == 'B' ~ in_logo_r
  ))


model_fit <- lm(salary ~ yrs.since.phd, data = df)
e <- residuals(model_fit)
z_thr <- 2.5
i <- which(scale(e^2) > z_thr)
c <- 1
while(length(i) > 0) {
  cat(paste0('[', c, ']'))
  df <- df[-i, ]
  model_fit <- lm(salary ~ yrs.since.phd, data = df)
  e <- residuals(model_fit)
  i <- which(scale(e^2) > z_thr)
  c <- c + 1
}

df <- df |>
  filter(yrs.since.phd < 30)
i_a <- sample(which(df$discipline == 'A'), size = 15)
i_b <- sample(which(df$discipline == 'B'), size = 15)
i_c <- c(i_a, i_b)

ggplot(df[i_c, ], aes(yrs.since.phd, salary)) +
  geom_smooth(method = 'lm', color = 'black') +
  geom_image(aes(image = logo)) +
  theme_minimal() +
  labs(x = NULL,
       y = NULL) +
  theme(axis.text = element_blank())

ggsave(out_path, width = 5, height = 5)


