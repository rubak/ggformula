context("layer factory")

mtcars2 <- df_stats( wt ~ cyl, data = mtcars, median_wt = median)

test_that(
  "gf_abline()",
  {
    vdiffr::expect_doppelganger(
      "gf_abline1",
      gf_point(Sepal.Length ~ Sepal.Width, data = iris) %>%
        gf_abline(intercept = 3, slope = 1, color = "red")
    )
    vdiffr::expect_doppelganger(
      "gf_abline2",
      gf_point(Sepal.Length ~ Sepal.Width, data = iris) %>%
        gf_abline(intercept = 1:3, slope = 1, color = c("red", "green", "blue"))
    )
    vdiffr::expect_doppelganger(
      "gf_abline3",
      gf_point(wt ~ hp, size = ~ wt, color = ~ cyl, data = mtcars) %>%
        gf_abline(slope = ~ 0, intercept = ~ median_wt, color = ~ cyl, data = mtcars2)
    )

    vdiffr::expect_doppelganger(
      "gf_abline4",
      gf_point(wt ~ hp, size = ~ wt, color = ~ cyl, data = mtcars) %>%
        gf_abline(slope = 0, intercept = 3, color = "green", data = NA)
    )

    vdiffr::expect_doppelganger(
      "gf_abline5",
      gf_point(wt ~ hp, size = ~ wt, color = ~ cyl, data = mtcars) %>%
        gf_hline(yintercept = ~ median_wt, color = ~ cyl, data = mtcars2)
    )

    vdiffr::expect_doppelganger(
      "gf_abline6",
      gf_point(mpg ~ hp, color = ~ cyl, size = ~ wt, data = mtcars) %>%
        gf_abline(color="red", slope = -0.10, intercept = 35)
    )

    vdiffr::expect_doppelganger(
      "gf_abline7",
      gf_point(mpg ~ hp, color = ~ cyl, size = ~ wt, data = mtcars) %>%
        gf_abline(color = "red", slope = ~ slope, intercept = ~ intercept,
                  data = data.frame(slope = -0.10, intercept = 33:35))
    )
  }
)

test_that(
  "gf_area() & gf_ribbon()",
  {
    Temps <- mosaicData::Weather %>%
      dplyr::filter(city == "Chicago", year == 2016, month <= 4)
    vdiffr::expect_doppelganger(
      "gf_ribbon1",
      gf_ribbon(low_temp + high_temp ~ date, data = Temps, color = "navy", alpha = 0.3)
    )
    vdiffr::expect_doppelganger(
      "gf_area1",
      gf_area(high_temp ~ date, data = Temps, color = "navy", alpha = 0.3)
    )

    vdiffr::expect_doppelganger(
      "gf_ribbon2",
      gf_ribbon(low_temp + high_temp ~ date, data = mosaicData::Weather, alpha = 0.3) %>%
        gf_facet_grid(city ~ .)
    )
  }
)


test_that(
  "gf_ash()",
  {
    vdiffr::expect_doppelganger(
      "gf_ash1",
      gf_ash(~Sepal.Length, color = ~ Species, data = iris)
    )
    vdiffr::expect_doppelganger(
      "gf_ash2",
      gf_ash(~Sepal.Length, color = ~ Species, data = iris, binwidth = 0.3)
    )
    vdiffr::expect_doppelganger(
      "gf_ash3",
      gf_ash(~Sepal.Length, color = ~ Species, data = iris, adjust = 2)
    )
  }
)


test_that(
  "gf_bar() and gf_col()",
  {
    vdiffr::expect_doppelganger(
      "gf_bar1",
      gf_bar(~Species, data = iris)
    )
    D <- data.frame(
      group = LETTERS[1:3],
      count = c(20, 25, 18)
    )
    vdiffr::expect_doppelganger(
      "gf_col1",
      gf_col(count ~ group, data = D)
    )
  }
)


test_that(
  "gf_point()",
  {
    vdiffr::expect_doppelganger(
      "gf_point1",
      gf_point(Sepal.Length ~ Sepal.Width, data = iris)
    )
    vdiffr::expect_doppelganger(
      "gf_point2",
      gf_point(Sepal.Length ~ Sepal.Width | Species, color = ~ Species, data = iris)
    )
  }
)

test_that(
  "gf_line()",
  {
    vdiffr::expect_doppelganger(
      "gf_line1",
      gf_line(births ~ date, data = mosaicData::Births78)
    )
    vdiffr::expect_doppelganger(
      "gf_line2",
      gf_line(births ~ date, color = ~ wday, data = mosaicData::Births78)
    )
  }
)

