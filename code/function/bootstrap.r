meanci_boot <- function(x, boot = 100, alpha = 0.05, bias.correct = FALSE) {
  # observation
  n <- length(x)
  # observed mean
  mu <- mean(x)
  # bootstrap mean
  bmu <- numeric(boot)
  set.seed(120511)
  for (i in seq_len(boot)) {
    bx <- sample(x, size = n, replace = TRUE)
    bmu[i] <- mean(bx, na.rm = TRUE)
  }
  # output
  out <- list(mean = mu, bootdist = bmu)
  # bias corrected?
  if (!bias.correct) {
    # quantile bootstrap
    out$ci <- quantile(bmu, probs = c(alpha / 2, 1 - alpha / 2))
    out
  } else {
    # Bias corrected and accelerated (BCa) bootstrap
    # reference: https://www.erikdrysdale.com/bca_python/
    # bias-correction factor
    z0 <- qnorm(mean(bmu < mu), lower.tail = TRUE)
    # acceleration factor by jackknife
    jack <- numeric(n)
    for (i in seq_len(n)) {
      jack[i] <- mean(x[-i])
    }
    jack_mean <- mean(jack)
    a <- sum((jack_mean - jack)^3) / (6 * sum((jack_mean - jack)^2)^(3 / 2))
    # quantile point of normal distribution
    za1 <- qnorm(alpha, lower.tail = TRUE)
    za2 <- qnorm(1 - alpha, lower.tail = TRUE)
    # bias corrected quantile point
    a1 <- pnorm(
      z0 + (z0 + za1) / (1 - a * (z0 + za1))
    )
    a2 <- pnorm(
      z0 + (z0 + za2) / (1 - a * (z0 + za2))
    )
    out$ci <- quantile(bmu, probs = c(a1, a2))
    out$correction.factor <- z0
    out$acceleration.factor <- a
    out
  }
}

cal_testcondnega_selection1 <- function(data, boot = 1000) {
  obs_negacondtest <- with(subset(data, aw1_test == 1), mean(aw1_testnega))
  obs_test <- with(data, mean(aw1_test))

  observe <- list(
    negacondtest = obs_negacondtest,
    test = obs_test,
    testcondnega = obs_negacondtest * obs_test / 0.2
  )

  negacondtest <- numeric(boot)
  testcondnega <- numeric(boot)
  test <- numeric(boot)
  diff <- numeric(boot)
  n <- nrow(data)
  set.seed(120511)
  for (i in seq_len(boot)) {
    select <- sample(1:n, size = n, replace = TRUE)
    usedt <- data[select, ]
    negacondtest[i] <- with(subset(usedt, aw1_test == 1), mean(aw1_testnega))
    test[i] <- with(usedt, mean(aw1_test))
    testcondnega[i] <- negacondtest[i] * test[i] / 0.2
    diff[i] <- testcondnega[i] - test[i]
  }

  boot <- list(
    negacondtest = negacondtest,
    test = test,
    testcondnega = testcondnega,
    diff = diff
  )

  return(list(
    obs = observe,
    boot = boot
  ))
}

cal_testcondnega_selection2 <- function(data, boot = 1000) {
  obs_negacondtest <- with(subset(data, abw1_test == 1), mean(abw1_testnega))
  obs_test <- with(data, mean(abw1_test))

  observe <- list(
    negacondtest = obs_negacondtest,
    test = obs_test,
    testcondnega = obs_negacondtest * obs_test / 0.2
  )

  negacondtest <- numeric(boot)
  testcondnega <- numeric(boot)
  test <- numeric(boot)
  diff <- numeric(boot)
  n <- nrow(data)
  set.seed(120511)
  for (i in seq_len(boot)) {
    select <- sample(1:n, size = n, replace = TRUE)
    usedt <- data[select, ]
    negacondtest[i] <- with(subset(usedt, abw1_test == 1), mean(abw1_testnega))
    test[i] <- with(usedt, mean(abw1_test))
    testcondnega[i] <- negacondtest[i] * test[i] / 0.2
    diff[i] <- testcondnega[i] - test[i]
  }

  boot <- list(
    negacondtest = negacondtest,
    test = test,
    testcondnega = testcondnega,
    diff = diff
  )

  return(list(
    obs = observe,
    boot = boot
  ))
}