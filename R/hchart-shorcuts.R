#' Shortcut to make spkarlines
#' @param x A numeric vector.
#' @param type Type sparkline: line, bar, etc.
#' @param ... Additional arguments for the data series \url{https://api.highcharts.com/highcharts/series}.
#'
#' @examples
#'
#' set.seed(123)
#' x <- cumsum(rnorm(10))
#'
#' hcspark(x)
#' hcspark(x, "columnn")
#' hcspark(c(1, 4, 5), "pie")
#' hcspark(x, type = "area")
#' @export
hcspark <- function(x = NULL, type = NULL, ...) {
  .Deprecated(
    msg = "Use type 'hc_theme_sparkline' or hc_theme_sparkline_bv theme instead."
  )

  stopifnot(is.numeric(x))

  highchart() %>%
    hc_plotOptions(
      series = list(showInLegend = FALSE, dataLabels = list(enabled = FALSE)),
      line = list(marker = list(enabled = FALSE))
    ) %>%
    hc_add_series(data = x, type = type, ...) %>%
    hc_add_theme(hc_theme_sparkline())
}

#' Shortcut to make a boxplot
#' @param x A numeric vector.
#' @param var A string vector same length of x.
#' @param var2 A string vector same length of x.
#' @param outliers A boolean value to show or not the outliers.
#' @param ... Additional arguments for the data series \url{https://api.highcharts.com/highcharts/series}.
#' @examples
#' \dontrun{
#' hcboxplot(x = iris$Sepal.Length, var = iris$Species, color = "red")
#' }
#' @importFrom dplyr rename
#' @importFrom tidyr unnest
#' @importFrom grDevices boxplot.stats
#' @importFrom rlang .data
#' @export
hcboxplot <- function(x = NULL, var = NULL, var2 = NULL, outliers = TRUE, ...) {
  .Deprecated("data_to_boxplot")

  stopifnot(is.numeric(x))

  if (is.null(var)) {
    var <- NA
  }
  if (is.null(var2)) {
    var2 <- NA
  }

  df <- tibble(x, g1 = var, g2 = var2)

  get_box_values <- function(x) {
    boxplot.stats(x)$stats %>%
      t() %>%
      as.data.frame() %>%
      setNames(c("low", "q1", "median", "q3", "high"))
  }

  get_outliers_values <- function(x) {
    boxplot.stats(x)$out
  }

  series_box <- df %>%
    group_by(.data$g1, .data$g2) %>%
    do(data = get_box_values(.$x)) %>%
    ungroup() %>%
    unnest(cols = c(data)) %>%
    group_by(.data$g2) %>%
    do(data = list_parse(rename(select(., -.data$g2), name = .data$g1))) %>%
    mutate(type = "boxplot") %>%
    mutate(id = as.character(.data$g2))

  if (length(list(...)) > 0) {
    series_box <- add_arg_to_df(series_box, ...)
  }

  series_out <- df %>%
    group_by(.data$g1, .data$g2) %>%
    do(data = get_outliers_values(.$x)) %>%
    ungroup() %>%
    filter(map_lgl(data, ~ length(.x) != 0)) %>%
    group_by(.data$g2) %>%
    do(data = list_parse(select(., name = .data$g1, y = .data$data))) %>%
    mutate(type = "scatter") %>%
    mutate(linkedTo = as.character(.data$g2))

  if (length(list(...)) > 0) {
    series_out <- add_arg_to_df(series_out, ...)
  }

  if (!has_name(list(...), "color")) {
    colors <- colorize(seq(1, nrow(series_box)))
    colors <- hex_to_rgba(colors, alpha = 0.75)
  }

  if (!has_name(list(...), "name")) {
    series_box <- rename(series_box, name = .data$g2)
    series_out <- rename(series_out, name = .data$g2)
  }


  hc <- highchart() %>%
    hc_chart(type = "bar") %>%
    hc_xAxis(type = "category") %>%
    hc_plotOptions(series = list(
      marker = list(
        symbol = "circle"
      )
    ))

  hc <- hc_add_series_list(hc, list_parse(series_box))

  if (is.na(var2) || is.na(var)) {
    hc <- hc %>%
      hc_xAxis(categories = "") %>%
      hc_plotOptions(series = list(showInLegend = FALSE))
  }

  if (outliers) {
    hc <- hc_add_series_list(hc, list_parse(series_out))
  }

  hc
}

#' Shortcut to make icon arrays charts
#' @param labels A character vector
#' @param counts A integer vector
#' @param rows A integer to set
#' @param icons A character vector same length (o length 1) as labels
#' @param size Font size
#' @param ... Additional arguments for the data series \url{https://api.highcharts.com/highcharts/series}.
#' @importFrom dplyr ungroup group_by
#' @importFrom rlang .data
#' @export
hciconarray <- function(labels, counts, rows = NULL, icons = NULL, size = 4,
                        ...) {
  .Deprecated(
    msg = "Use type 'item' instead (`hchart(df, \"item\", hcaes(name = labels, y = counts))`).
Item chart provides better behaviour beside is a specific type of chart of HighchartsJS."
  )

  assertthat::assert_that(length(counts) == length(labels))

  if (is.null(rows)) {
    sizegrid <- n2mfrow(sum(counts))
    w <- sizegrid[1]
    h <- sizegrid[2]
  } else {
    h <- rows
    w <- ceiling(sum(counts) / rows)
  }

  ds <- tibble(x = rep(1:w, h), y = rep(1:h, each = w)) %>%
    head(sum(counts)) %>%
    mutate(y = -.data$y) %>%
    mutate(gr = rep(seq_along(labels), times = counts)) %>%
    left_join(tibble(gr = seq_along(labels), name = as.character(labels)),
      by = "gr"
    ) %>%
    group_by(.data$name) %>%
    do(data = list_parse2(tibble(.$x, .$y))) %>%
    ungroup() %>%
    left_join(tibble(labels = as.character(labels), counts),
      by = c("name" = "labels")
    ) %>%
    arrange_("-counts") %>%
    mutate(percent = .data$counts / sum(.data$counts) * 100)

  if (!is.null(icons)) {
    assertthat::assert_that(length(icons) %in% c(1, length(labels)))

    dsmrk <- ds %>%
      mutate(iconm = icons) %>%
      group_by(.data$name) %>%
      do(marker = list(symbol = fa_icon_mark(.$iconm)))

    ds <- ds %>%
      left_join(dsmrk, by = "name") %>%
      mutate(icon = fa_icon(icons))
  }

  ds <- mutate(ds, ...)

  hc <- highchart() %>%
    hc_chart(type = "scatter") %>%
    hc_add_series_list(ds) %>%
    hc_plotOptions(
      series =
        list(
          cursor = "default",
          marker = list(radius = size),
          states = list(hover = list(enabled = FALSE)),
          events = list(
            legendItemClick = JS("function () { return false; }")
          )
        )
    ) %>%
    hc_tooltip(pointFormat = "{point.series.options.counts} ({point.series.options.percent:.2f}%)") %>%
    hc_add_theme(
      hc_theme_merge(
        getOption("highcharter.theme"),
        hc_theme_null()
      )
    )

  if (!is.null(icons)) {
    hc <- hc %>% hc_add_dependency_fa()
  }

  hc
}

#' Shortcut for create treemaps
#'
#' This function helps to create highcharts treemaps from \code{treemap} objects
#' from the package \code{treemap}. NOTE: This function is deprecated. Please use \code{hctreemap2} instead.
#'
#' @param tm A \code{treemap} object from the treemap package.
#' @param ... Additional shared arguments for the data series
#'   (\url{https://api.highcharts.com/highcharts/series}).
#'
#' @examples
#' \dontrun{
#'
#' library("treemap")
#' library("viridis")
#'
#' data(GNI2014)
#' head(GNI2014)
#'
#' tm <- treemap(GNI2014,
#'   index = c("continent", "iso3"),
#'   vSize = "population", vColor = "GNI",
#'   type = "comp", palette = rev(viridis(6)),
#'   draw = FALSE
#' )
#'
#' hctreemap(tm, allowDrillToNode = TRUE, layoutAlgorithm = "squarified") %>%
#'   hc_title(text = "Gross National Income World Data") %>%
#'   hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
#'                              Pop: {point.value:,.0f}<br>
#'                              GNI: {point.valuecolor:,.0f}")
#' }
#'
#' @importFrom dplyr filter_ mutate_ rename select_ tbl_df
#' @importFrom purrr map map_df map_if
#' @importFrom rlang .data
#' @export
hctreemap <- function(tm, ...) {
  .Deprecated("data_to_hierarchical")

  assertthat::assert_that(is.list(tm))

  df <- tm$tm %>%
    tibble::as_tibble() %>%
    select(-.data$x0, -.data$y0, -.data$w, -.data$h, -.data$stdErr, -.data$vColorValue) %>%
    rename(value = .data$vSize, valuecolor = .data$vColor) %>%
    mutate_if(is.factor, as.character)

  ndepth <- which(names(df) == "value") - 1

  ds <- map_df(seq(ndepth), function(lvl) {
    df2 <- df %>%
      filter_(sprintf("level == %s", lvl)) %>%
      rename(name = names(df)[lvl]) %>%
      mutate(id = highcharter::str_to_id(.data$name))

    if (lvl > 1) {
      df2 <- df2 %>%
        mutate(
          parent = names(df)[lvl - 1],
          parent = highcharter::str_to_id(.data$parent)
        )
    } else {
      df2 <- df2 %>%
        mutate(parent = NA)
    }

    df2
  })

  ds <- list_parse(ds)

  ds <- map(ds, function(x) {
    if (is.na(x$parent)) {
      x$parent <- NULL
    }
    x
  })

  hc_add_series(highchart(), data = ds, type = "treemap", ...)
}

#' Shortcut to create treemaps.
#'
#' This function helps create highcharts treemaps from data frames.
#'
#' @param data data frame containing variables to organize each level of the treemap on
#' @param group_vars vector of strings containing column names of variables to generate treemap levels from. the first listed column will specify the top level of the treemap. the unique values in each of these columns must have no intersection (including NAs).
#' @param size_var string name of column containing numeric data to aggregate by
#' @param color_var string name of column containing numeric data to color by. defaults to same column as \code{size_var}
#' @param ... additional shared arguments for the data series
#'   (\url{https://api.highcharts.com/highcharts/series}).
#'
#' @return highchart plot object
#' @examples
#' \dontrun{
#'
#' library(tidyverse)
#' library(highcharter)
#' library(RColorBrewer)
#'
#' tibble(
#'   index1 = sample(LETTERS[1:5], 500, replace = T),
#'   index2 = sample(LETTERS[6:10], 500, replace = T),
#'   index3 = sample(LETTERS[11:15], 500, replace = T),
#'   value = rpois(500, 5),
#'   color_value = rpois(500, 5)
#' ) %>%
#'   hctreemap2(
#'     group_vars = c("index1", "index2", "index3"),
#'     size_var = "value",
#'     color_var = "color_value",
#'     layoutAlgorithm = "squarified",
#'     levelIsConstant = FALSE,
#'     levels = list(
#'       list(level = 1, dataLabels = list(enabled = TRUE)),
#'       list(level = 2, dataLabels = list(enabled = FALSE)),
#'       list(level = 3, dataLabels = list(enabled = FALSE))
#'     )
#'   ) %>%
#'   hc_colorAxis(
#'     minColor = brewer.pal(7, "Greens")[1],
#'     maxColor = brewer.pal(7, "Greens")[7]
#'   ) %>%
#'   hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
#'              Value: {point.value:,.0f}<br>
#'              Color Value: {point.colorValue:,.0f}")
#' }
#'
#' @importFrom dplyr bind_rows filter group_by mutate_at pull select summarise
#' @importFrom purrr map pmap_chr
#'
#' @export
hctreemap2 <- function(data, group_vars, size_var, color_var = NULL, ...) {
  .Deprecated("data_to_hierarchical")

  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(is.character(group_vars))
  assertthat::assert_that(is.character(size_var))
  if (!is.null(color_var)) assertthat::assert_that(is.character(color_var))

  group_syms <- rlang::syms(group_vars)
  size_sym <- rlang::sym(size_var)
  color_sym <- rlang::sym(ifelse(is.null(color_var), size_var, color_var))

  if (data %>%
    select(!!!group_syms) %>%
    map(unique) %>%
    unlist() %>%
    anyDuplicated()) {
    stop("Treemap data uses same label at multiple levels.")
  }

  data <- data %>% mutate_at(group_vars, as.character)

  name_cell <- function(..., depth) paste0(list(...), seq_len(depth), collapse = "")

  data_at_depth <- function(depth) {
    data %>%
      group_by(!!!group_syms) %>%
      summarise(
        value = sum(!!size_sym),
        colorValue = sum(!!color_sym)
      ) %>%
      ungroup() %>%
      mutate(
        name = !!group_syms[[depth]],
        level = depth
      ) %>%
      mutate_at(group_vars, as.character()) %>%
      {
        if (depth == 1) {
          mutate(., id = paste0(name, 1))
        } else {
          mutate(
            .,
            parent = pmap_chr(
              list(!!!group_syms[seq_len(depth) - 1]),
              name_cell,
              depth = depth - 1
            ),
            id = paste0(parent, name, depth)
          )
        }
      }
  }

  treemap_df <- seq_along(group_vars) %>%
    map(data_at_depth) %>%
    bind_rows()

  data_list <- treemap_df %>%
    highcharter::list_parse() %>%
    purrr::map(~ .[!is.na(.)])

  colorVals <- treemap_df %>%
    filter(level == length(group_vars)) %>%
    pull(colorValue)

  highchart() %>%
    hc_add_series(
      data = data_list,
      type = "treemap",
      allowDrillToNode = TRUE, ...
    ) %>%
    hc_colorAxis(
      min = min(colorVals),
      max = max(colorVals),
      enabled = TRUE
    )
}

#' Shortcut to create parallel coordinates
#' @param df A data frame object.
#' @param ... Additional shared arguments for the data series
#'   (\url{https://api.highcharts.com/highcharts/series}) for the
#'   \code{hchar.data.frame} function.
#' @examples
#' require(viridisLite)
#'
#' n <- 15
#'
#' hcparcords(head(mtcars, n), color = hex_to_rgba(magma(n), 0.5))
#'
#' require(dplyr)
#' data(iris)
#' set.seed(123)
#'
#' iris <- sample_n(iris, 60)
#'
#' hcparcords(iris, color = colorize(iris$Species))
#' @importFrom dplyr mutate_if
#' @export
hcparcords <- function(df, ...) {
  stopifnot(is.data.frame(df))

  rescale01 <- function(x) {
    rng <- range(x, na.rm = TRUE)
    (x - rng[1]) / (rng[2] - rng[1])
  }

  df <- df[map_lgl(df, is.numeric)]

  # Add row identifier
  df <- rownames_to_column(df, ".row")

  df <- dplyr::mutate_if(df, is.numeric, rescale01)

  df <- tidyr::gather(df, "var", "val", setdiff(names(df), ".row"))

  hchart(df, "line", hcaes_(x = "var", y = "val", group = ".row")) %>%
    hc_plotOptions(series = list(showInLegend = FALSE)) %>%
    hc_yAxis(min = 0, max = 1) %>%
    hc_tooltip(sort = TRUE, table = TRUE, valueDecimals = 2)
}
