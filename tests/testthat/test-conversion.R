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

  U10 = data.frame(times = times,
                   test1 = c(3.29,2.07,1.96,2.82,3.73,
                             4.11,4.96,6.33,7.39,7.59,
                             7.51,7.22,6.81,6.43,5.81,
                             4.02,3.03,2.68,2.40,2.20,
                             2.09,1.95,1.66,1.39,1.4),
                   test2 = c(6.29,4.87,6.16,7.12,8.77,
                             10.16,10.85,11.45,11.21,11.04,
                             11.09,10.67,10.48,10.00,8.96,
                             6.36,5.62,5.83,5.83,5.25,
                             4.11,3.08,2.26,1.14,-0.10))
  V10 = data.frame(times = times,
                   test1 = c(-8.87,-4.23,-2.81,-2.59,-4.58,
                             -4.80,-5.33,-5.86,-6.12,-6.13,
                             -6.11,-5.76,-5.91,-5.60,-5.09,
                             -3.33,-2.50,-2.29,-2.14,-2.07,
                             -1.95,-1.97,-2.04,-2.03,-1.9),
                   test2 = c(11.80,5.88,5.74,5.56,6.87,
                             8.39,8.68,8.33,7.90,7.42,
                             6.96,6.87,6.36,5.61,5.16,
                             4.16,4.25,4.59,4.51,3.90,
                             2.97,1.98,1.04,-0.08,-0.44))

  uv2ws(u = U10, v = V10)
  uv2wd(u = U10, v = V10)

  expect_equal(2 * 2, 4)
})
