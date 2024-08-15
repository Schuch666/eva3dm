test_that("q2rh / rh2q are ok!", {

  times <- seq(as.POSIXct('2024-01-01',tz = 'UTC'),
               as.POSIXct('2024-01-02',tz = 'UTC'),
               by = 'hour')[1:5]
  q2   <- data.frame(time = times, a = rep(0.0002038,5))
  temp <- data.frame(time = times, a = rep(   302.45,5))
  pres <- data.frame(time = times, a = rep(   100800,5))

  q2rh(q = 0.0002038, t = 302.45, p = 100800)
  q2rh(q = q2, t = temp, p = pres)
  q2rh(q = q2, t = temp, p = 100000)
  q2rh(q = q2, t = 300, p = pres)
  q2rh(q = q2, t = 300, p = 100000)

  times <- seq(as.POSIXct('2024-01-01',tz = 'UTC'),
               as.POSIXct('2024-01-02',tz = 'UTC'),
               by = 'hour')

  rh2q(rh = 99, temp = 25)
  rh2q(rh = c(0,seq(1,100, by = 4)), temp = 25)
  rh2q(rh = c(0,seq(1,100, by = 4)), temp = 10:35)
  rh2q(rh   = data.frame(time = times, a = seq(1,100, by = 4)),temp = 25)
  rh2q(rh   = data.frame(time = times, a = seq(1,100, by = 4)),
       temp = data.frame(time = times, a = 11:35))

  expect_equal(2 * 2, 4)
})
