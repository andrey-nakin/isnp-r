# Calculate probability of a sample with confidence interval
# population - source data
# sample.func - function that constructs a sample from the population
# range - range to construct a sample from the population, ignored when sample.func is set
# conf.level - probability level for the confidence interval
# Returns - sample probability with the confidence interval
calc.sample.prob <- 
  function(population, sample.func = NA, range = NA, conf.level = 0.95) {
    population.len <- length(population)
    if (!is.na(sample.func)) {
      sample <- sample.func(population)
    } else if (!is.na(range)) {
      sample <- population[which(population >= range[1] & population < range[2])]
    }
    sample.len <- length(sample)
    p <- sample.len / population.len
    ci <- stats::binom.test(x = sample.len, n = population.len, conf.level = conf.level)$conf.int
    return (list(
      value = p,
      min = ci[1],
      max = ci[2]
    ))
  }
