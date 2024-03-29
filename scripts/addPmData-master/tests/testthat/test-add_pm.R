input_data <- function() {
  tibble::tribble(
    ~id, ~lat, ~lon, ~start_date, ~end_date,
    "55000100280", 39.2, -84.6, "2008-09-09", "2008-09-11",
    "55000100281", 39.2, -84.6, "2007-08-05", "2007-08-08",
    "55000100282", 39.2, -84.6, "2015-08-31", "2015-09-02"
  )
}

input_data_h3 <- function() {
  tibble::tribble(
    ~id, ~h3, ~start_date, ~end_date,
    "55000100280", "882a930a23fffff", "2008-09-09", "2008-09-11",
    "55000100281", "882a930a23fffff", "2007-08-05", "2007-08-08",
    "55000100282", "882a930a23fffff", "2015-08-31", "2015-09-02"
  )
}

test_that("add_pm works", {
  expect_equal(
    add_pm(input_data()),
    tibble::tibble(
      id = c(
        "55000100280", "55000100280", "55000100280",
        "55000100281", "55000100281", "55000100281", "55000100281",
        "55000100282", "55000100282", "55000100282"
      ),
      lat = c(39.2, 39.2, 39.2, 39.2, 39.2, 39.2, 39.2, 39.2, 39.2, 39.2),
      lon = c(-84.6, -84.6, -84.6, -84.6, -84.6, -84.6, -84.6, -84.6, -84.6, -84.6),
      start_date = as.Date(c(
        "2008-09-09", "2008-09-09", "2008-09-09",
        "2007-08-05", "2007-08-05", "2007-08-05", "2007-08-05",
        "2015-08-31", "2015-08-31", "2015-08-31"
      )),
      end_date = as.Date(c(
        "2008-09-11", "2008-09-11", "2008-09-11",
        "2007-08-08", "2007-08-08", "2007-08-08", "2007-08-08",
        "2015-09-02", "2015-09-02", "2015-09-02"
      )),
      date = as.Date(c(
        "2008-09-09", "2008-09-10", "2008-09-11",
        "2007-08-05", "2007-08-06", "2007-08-07", "2007-08-08",
        "2015-08-31", "2015-09-01", "2015-09-02"
      )),
      year = c(2008, 2008, 2008, 2007, 2007, 2007, 2007, 2015, 2015, 2015),
      h3 = rep("882a930a23fffff", 10),
      h3_3 = rep("832a93fffffffff", 10),
      pm_pred = c(
        8.227, 9.576, 13.05,
        29.28, 28.53, 22.3, 20.74,
        12.68, 17.21, 19.45
      ),
      pm_se = c(
        1.133, 2.074, 2.9,
        4.138, 5.034, 2.562, 3.277,
        0.9904, 2.237, 1.45
      )
    )
  )
})

test_that("add_pm returns NA for dates outside of range (with warning)", {
  expect_equal(
    tibble::tribble(
      ~id, ~lat, ~lon, ~start_date, ~end_date,
      "55000100280", 39.2, -84.6, "2021-09-09", "2021-09-11",
      "55000100281", 39.2, -84.6, "2007-08-05", "2007-08-08",
      "55000100282", 39.2, -84.6, "2015-08-31", "2015-09-02"
    ) %>%
      add_pm(),
    tibble::tibble(
      id = c(
        "55000100280", "55000100280", "55000100280",
        "55000100281", "55000100281", "55000100281", "55000100281",
        "55000100282", "55000100282", "55000100282"
      ),
      lat = c(39.2, 39.2, 39.2, 39.2, 39.2, 39.2, 39.2, 39.2, 39.2, 39.2),
      lon = c(-84.6, -84.6, -84.6, -84.6, -84.6, -84.6, -84.6, -84.6, -84.6, -84.6),
      start_date = as.Date(c(
        "2021-09-09", "2021-09-09", "2021-09-09",
        "2007-08-05", "2007-08-05", "2007-08-05", "2007-08-05",
        "2015-08-31", "2015-08-31", "2015-08-31"
      )),
      end_date = as.Date(c(
        "2021-09-11", "2021-09-11", "2021-09-11",
        "2007-08-08", "2007-08-08", "2007-08-08", "2007-08-08",
        "2015-09-02", "2015-09-02", "2015-09-02"
      )),
      date = as.Date(c(
        "2021-09-09", "2021-09-10", "2021-09-11",
        "2007-08-05", "2007-08-06", "2007-08-07", "2007-08-08",
        "2015-08-31", "2015-09-01", "2015-09-02"
      )),
      year = c(2021, 2021, 2021, 2007, 2007, 2007, 2007, 2015, 2015, 2015),
      h3 = c(NA, NA, NA, rep("882a930a23fffff", 7)),
      h3_3 = c(NA, NA, NA, rep("832a93fffffffff", 7)),
      pm_pred = c(NA, NA, NA, 29.28, 28.53, 22.3, 20.74, 12.68, 17.21, 19.45),
      pm_se = c(NA, NA, NA, 4.138, 5.034, 2.562, 3.277, 0.9904, 2.237, 1.45)
    )
  )
})

test_that("add_pm type = h3 works", {
  expect_equal(
    add_pm(input_data_h3(), type = "h3"),
    tibble::tibble(
      id = c(
        "55000100280", "55000100280", "55000100280",
        "55000100281", "55000100281", "55000100281", "55000100281",
        "55000100282", "55000100282", "55000100282"
      ),
      h3 = rep("882a930a23fffff", 10),
      start_date = as.Date(c(
        "2008-09-09", "2008-09-09", "2008-09-09",
        "2007-08-05", "2007-08-05", "2007-08-05", "2007-08-05",
        "2015-08-31", "2015-08-31", "2015-08-31"
      )),
      end_date = as.Date(c(
        "2008-09-11", "2008-09-11", "2008-09-11",
        "2007-08-08", "2007-08-08", "2007-08-08", "2007-08-08",
        "2015-09-02", "2015-09-02", "2015-09-02"
      )),
      date = as.Date(c(
        "2008-09-09", "2008-09-10", "2008-09-11",
        "2007-08-05", "2007-08-06", "2007-08-07", "2007-08-08",
        "2015-08-31", "2015-09-01", "2015-09-02"
      )),
      year = c(2008, 2008, 2008, 2007, 2007, 2007, 2007, 2015, 2015, 2015),
      h3_3 = rep("832a93fffffffff", 10),
      pm_pred = c(
        8.227, 9.576, 13.05,
        29.28, 28.53, 22.3, 20.74,
        12.68, 17.21, 19.45
      ),
      pm_se = c(
        1.133, 2.074, 2.9,
        4.138, 5.034, 2.562, 3.277,
        0.9904, 2.237, 1.45
      )
    )
  )
})
