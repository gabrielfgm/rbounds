# Econometric Models for Partial Identification

This document describes the mathematical foundations for the bounds estimation implemented in the `rbounds` package, drawing primarily on the work of Charles Manski.

## 1. Introduction to Partial Identification

Traditional econometric analysis seeks point identification of parameters. However, many economically interesting questions involve parameters that cannot be point-identified from available data. Following Manski (2003, 2007), partial identification acknowledges that the best we can do is bound the parameter of interest within an **identified set**.

The key insight is that even when data cannot uniquely determine a parameter, it often can restrict the parameter to lie within a bounded interval. These bounds are **sharp** when they are the tightest possible given the available data and maintained assumptions.

## 2. Bounds on Conditional Means with Missing Outcomes

### 2.1 The Problem

Consider estimation of the conditional expectation $E[Y|X]$ where:
- $Y \in \{0, 1\}$ is a binary outcome
- $X$ is a vector of covariates observed for all units
- $Z \in \{0, 1\}$ is an indicator where $Z=1$ means $Y$ is observed

When $Z=0$, the outcome $Y$ is missing. The missingness may be non-random (selection on unobservables).

### 2.2 Derivation of Bounds

By the Law of Total Probability:

$$E[Y|X] = E[Y|X, Z=1] \cdot P(Z=1|X) + E[Y|X, Z=0] \cdot P(Z=0|X)$$

From the observed data we can identify:
- $E[Y|X, Z=1]$: conditional mean among observed cases
- $P(Z=1|X)$: conditional probability of observation

We **cannot** identify $E[Y|X, Z=0]$ - the conditional mean for missing cases.

### 2.3 Worst-Case (Manski) Bounds

Since $Y \in \{0, 1\}$, we know that $0 \leq E[Y|X, Z=0] \leq 1$.

**Lower Bound:** Set $E[Y|X, Z=0] = 0$ (worst case for lower bound):
$$E^L[Y|X] = E[Y|X, Z=1] \cdot P(Z=1|X)$$

**Upper Bound:** Set $E[Y|X, Z=0] = 1$ (worst case for upper bound):
$$E^U[Y|X] = E[Y|X, Z=1] \cdot P(Z=1|X) + P(Z=0|X)$$

The identified set is:
$$E[Y|X] \in [E^L[Y|X], E^U[Y|X]]$$

### 2.4 General Bounded Outcomes

For outcomes bounded by $Y \in [\gamma_0, \gamma_1]$:

$$E^L[Y|X] = E[Y|X, Z=1] \cdot P(Z=1|X) + \gamma_0 \cdot P(Z=0|X)$$

$$E^U[Y|X] = E[Y|X, Z=1] \cdot P(Z=1|X) + \gamma_1 \cdot P(Z=0|X)$$

## 3. Bounds on Average Treatment Effects (ATE)

### 3.1 The Selection Problem

Consider estimation of the Average Treatment Effect:
$$\tau = E[Y(1) - Y(0)] = E[Y(1)] - E[Y(0)]$$

where:
- $Y(1)$ is the potential outcome under treatment
- $Y(0)$ is the potential outcome under control
- $D \in \{0, 1\}$ is treatment assignment

The fundamental problem of causal inference is that we only observe:
$$Y = D \cdot Y(1) + (1-D) \cdot Y(0)$$

We observe $Y(1)$ only when $D=1$ and $Y(0)$ only when $D=0$.

### 3.2 Derivation of ATE Bounds

For each potential outcome, apply the law of iterated expectations:

$$E[Y(1)] = E[Y(1)|D=1] \cdot P(D=1) + E[Y(1)|D=0] \cdot P(D=0)$$

$$E[Y(0)] = E[Y(0)|D=1] \cdot P(D=1) + E[Y(0)|D=0] \cdot P(D=0)$$

**Identified quantities:**
- $E[Y(1)|D=1] = E[Y|D=1]$ (observable)
- $E[Y(0)|D=0] = E[Y|D=0]$ (observable)
- $P(D=1)$, $P(D=0)$ (observable)

**Unidentified quantities:**
- $E[Y(1)|D=0]$: treatment effect for untreated units
- $E[Y(0)|D=1]$: control outcome for treated units

### 3.3 Worst-Case ATE Bounds (No Assumptions)

For $Y \in [y_{\min}, y_{\max}]$ (typically $[0,1]$ for binary outcomes):

**Lower bound on $E[Y(1)]$:**
$$E^L[Y(1)] = E[Y|D=1] \cdot P(D=1) + y_{\min} \cdot P(D=0)$$

**Upper bound on $E[Y(1)]$:**
$$E^U[Y(1)] = E[Y|D=1] \cdot P(D=1) + y_{\max} \cdot P(D=0)$$

**Lower bound on $E[Y(0)]$:**
$$E^L[Y(0)] = y_{\min} \cdot P(D=1) + E[Y|D=0] \cdot P(D=0)$$

**Upper bound on $E[Y(0)]$:**
$$E^U[Y(0)] = y_{\max} \cdot P(D=1) + E[Y|D=0] \cdot P(D=0)$$

**Bounds on ATE:**
$$\tau^L = E^L[Y(1)] - E^U[Y(0)]$$
$$\tau^U = E^U[Y(1)] - E^L[Y(0)]$$

Expanding:
$$\tau^L = E[Y|D=1] \cdot P(D=1) + y_{\min} \cdot P(D=0) - y_{\max} \cdot P(D=1) - E[Y|D=0] \cdot P(D=0)$$

$$\tau^U = E[Y|D=1] \cdot P(D=1) + y_{\max} \cdot P(D=0) - y_{\min} \cdot P(D=1) - E[Y|D=0] \cdot P(D=0)$$

For binary outcomes ($y_{\min}=0$, $y_{\max}=1$):
$$\tau^L = E[Y|D=1] \cdot P(D=1) - P(D=1) - E[Y|D=0] \cdot P(D=0)$$
$$\tau^U = E[Y|D=1] \cdot P(D=1) + P(D=0) - E[Y|D=0] \cdot P(D=0)$$

Or more simply:
$$\tau^L = P(D=1)(E[Y|D=1] - 1) - P(D=0) \cdot E[Y|D=0]$$
$$\tau^U = P(D=1) \cdot E[Y|D=1] + P(D=0)(1 - E[Y|D=0])$$

### 3.4 Conditional ATE Bounds (CATE)

Conditioning on covariates $X$, the bounds become:

$$\tau^L(x) = E^L[Y(1)|X=x] - E^U[Y(0)|X=x]$$
$$\tau^U(x) = E^U[Y(1)|X=x] - E^L[Y(0)|X=x]$$

Where the component bounds are:
$$E^L[Y(1)|X] = E[Y|D=1, X] \cdot P(D=1|X) + y_{\min} \cdot P(D=0|X)$$
$$E^U[Y(1)|X] = E[Y|D=1, X] \cdot P(D=1|X) + y_{\max} \cdot P(D=0|X)$$
$$E^L[Y(0)|X] = y_{\min} \cdot P(D=1|X) + E[Y|D=0, X] \cdot P(D=0|X)$$
$$E^U[Y(0)|X] = y_{\max} \cdot P(D=1|X) + E[Y|D=0, X] \cdot P(D=0|X)$$

## 4. Confidence Intervals for Bounds

Following Imbens and Manski (2004), we construct confidence intervals that cover the **parameter** (not just the bounds) with specified probability.

### 4.1 The Problem

Let $\theta$ be the true parameter with identified set $[\theta_L, \theta_U]$. We estimate the bounds obtaining $[\hat{\theta}_L, \hat{\theta}_U]$. A confidence interval $[C_L, C_U]$ should satisfy:
$$P(\theta \in [C_L, C_U]) \geq 1 - \alpha$$

### 4.2 Imbens-Manski Confidence Intervals

For estimated bounds $\hat{\theta}_L$ and $\hat{\theta}_U$ with standard errors $\hat{\sigma}_L$ and $\hat{\sigma}_U$:

$$C_L = \hat{\theta}_L - c_n \frac{\hat{\sigma}_L}{\sqrt{n}}$$
$$C_U = \hat{\theta}_U + c_n \frac{\hat{\sigma}_U}{\sqrt{n}}$$

The critical value $c_n$ solves:
$$\Phi\left(c_n + \frac{\sqrt{n}(\hat{\theta}_U - \hat{\theta}_L)}{\max(\hat{\sigma}_L, \hat{\sigma}_U)}\right) - \Phi(-c_n) = 1 - \alpha$$

where $\Phi(\cdot)$ is the standard normal CDF.

### 4.3 Variance Estimation

For the missing outcomes bounds, using the delta method:

$$\text{Var}(\hat{E}^L) = \text{Var}(\hat{\mu} \cdot \hat{p}) \approx \sigma_\mu^2 p^2 + \mu^2 \sigma_p^2$$

$$\text{Var}(\hat{E}^U) = \text{Var}(\hat{\mu} \cdot \hat{p} + (1-\hat{p})) \approx \sigma_\mu^2 p^2 + (\mu - 1)^2 \sigma_p^2$$

where:
- $\mu = E[Y|X, Z=1]$
- $p = P(Z=1|X)$
- $\sigma_\mu^2$ = variance of the outcome regression estimator
- $\sigma_p^2$ = variance of the missingness probability estimator

## 5. References

- Manski, C.F. (2003). *Partial Identification of Probability Distributions*. Springer.
- Manski, C.F. (2007). *Identification for Prediction and Decision*. Harvard University Press.
- Imbens, G.W. and Manski, C.F. (2004). "Confidence Intervals for Partially Identified Parameters." *Econometrica*, 72(6), 1845-1857.
- Horowitz, J.L. and Manski, C.F. (2000). "Nonparametric Analysis of Randomized Experiments with Missing Covariate and Outcome Data." *Journal of the American Statistical Association*, 95(449), 77-84.
