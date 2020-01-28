
### To this end we simulate a dataset (m.data) of length 400 with multiple changepoints at 100, 200, 300. 
### The sequence has four segments and the means for each segment are 0, 1, 0, 0.2.
```{r}
library(changepoint)
set.seed(10)
m.data <- c(rnorm(100, 0, 1), rnorm(100, 1, 1), rnorm(100, 0, 1), rnorm(100, 0.2, 1))
ts.plot(m.data, xlab = "Index")
```
```{r}
m.pelt <- cpt.mean(m.data, method = "PELT", penalty = "SIC")
plot(m.pelt, type = "l", cpt.col = "blue", xlab = "Index", cpt.width = 4)
cpts(m.pelt)

```

```{r}
m.binseg <- cpt.mean(m.data, method = "BinSeg", penalty = "SIC")
plot(m.binseg, type = "l", xlab = "Index", cpt.width = 4)
cpts(m.binseg)

```

```{r}
m.pm <- cpt.mean(m.data, penalty = "Manual", pen.value = "1.5 * log(n)", method = "PELT")
plot(m.pm, type = "l", cpt.col = "blue", xlab = "Index", cpt.width = 4)
cpts(m.pm)
```
```{r}
m.bsm <- cpt.mean(m.data, "Manual", pen.value = "1.5 * log(n)", method = "BinSeg")
plot(m.bsm, type = "l", xlab = "Index", cpt.width = 4)

cpts(m.bsm)
```

