## ---- warning = FALSE, message = FALSE, echo = FALSE---------------------
library(highcharter)
options(highcharter.theme = hc_theme_smpl(), highcharter.debug = TRUE)

## ------------------------------------------------------------------------
data("mpg", package = "ggplot2")
head(mpg)

## ------------------------------------------------------------------------
hchart(mpg, "point", hcaes(x = displ, y = cty))

## ------------------------------------------------------------------------
highchart() %>% 
  hc_add_series(mpg, "point", hcaes(x = displ, y = cty))

## ---- message=FALSE------------------------------------------------------
library(dplyr)
library(broom)


## ------------------------------------------------------------------------
data(diamonds, package = "ggplot2")

set.seed(123)
data <- sample_n(diamonds, 300)

modlss <- loess(price ~ carat, data = data)
fit <- arrange(augment(modlss), carat)

head(fit)

highchart() %>% 
  hc_add_series(data, type = "scatter",
                hcaes(x = carat, y = price, size = depth, group = cut)) %>%
  hc_add_series(fit, type = "spline", hcaes(x = carat, y = .fitted),
                name = "Fit", id = "fit") %>% 
  hc_add_series(fit, type = "arearange",
                hcaes(x = carat, low = .fitted - 2*.se.fit,
                      high = .fitted + 2*.se.fit),
                linkedTo = "fit")


## ------------------------------------------------------------------------
dfdiam <- diamonds %>% 
  group_by(cut, clarity) %>%
  summarize(price = median(price))

hchart(dfdiam, "heatmap", hcaes(x = cut, y = clarity, value = price)) 

## ------------------------------------------------------------------------
data(economics_long, package = "ggplot2")

economics_long2 <- filter(economics_long,
                          variable %in% c("pop", "uempmed", "unemploy"))

hchart(economics_long2, "line", hcaes(x = date, y = value01, group = variable))

## ------------------------------------------------------------------------
data(mpg, package = "ggplot2")
mpgman <- mpg %>% 
  group_by(manufacturer) %>% 
  summarise(n = n(),
            unique = length(unique(model))) %>% 
  arrange(-n, -unique)

head(mpgman)

hchart(mpgman, "treemap", hcaes(x = manufacturer, value = n, color = unique))

## ------------------------------------------------------------------------
mpgman2 <- count(mpg, manufacturer, year)

hchart(mpgman2, "bar", hcaes(x = manufacturer, y = n, group = year),
       color = c("#FCA50A", "#FCFFA4"),
       name = c("year 1999", "Year 2008"))

