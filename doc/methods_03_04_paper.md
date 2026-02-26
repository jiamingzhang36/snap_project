# Methods: Income (Unemployment–SNAP) and EA End Analysis

*Short text for the paper: Sections 3 (distributed lag) and 4 (EA end event study). Equations are in LaTeX; table/figure notes can be pasted into captions.*

---

## 3. Unemployment and SNAP Participation (Distributed Lag)

We estimate the dynamic response of SNAP participation to changes in local unemployment using a distributed-lag (DL) specification in county–month panel data. To interpret coefficients as the effect of an unemployment *shock* rather than level correlations, we use the 12‑month change in the unemployment rate,
\(\Delta u_{ct} = u_{ct} - u_{c,t-12}\),
as the key regressor.

**Specification.** For county \(c\) and month \(t\), we estimate
\[
Y_{ct} = \alpha_c + \gamma_t + \sum_{\ell=0}^{L} \beta_\ell \, \Delta u_{c,t-\ell} + \varepsilon_{ct},
\]
where \(Y_{ct}\) is \(\ln(1 + \text{recipients}_{ct})\); \(\alpha_c\) and \(\gamma_t\) are county and month fixed effects; \(\Delta u_{c,t-\ell}\) is the 12‑month change in the unemployment rate lagged by \(\ell\) months; and \(L = 6\). Standard errors are clustered at the county level. The coefficients \(\beta_\ell\) trace the impulse response of (log) participation to a one‑percentage‑point increase in the unemployment rate; \(\sum_{\ell=0}^{L} \beta_\ell\) is the cumulative effect over the horizon.

**Heterogeneity.** We split counties by *pre-determined* exposure to unemployment, defined as each county’s average unemployment rate over 2014–2016 (i.e., before the main ABAWD rollout). Counties above the median of this baseline average are “high exposure”; the rest are “low exposure.” We estimate the same DL model separately in the two subsamples and report the sum of the lag coefficients (and its standard error) in each group.

**Table/Figure notes (suggested).**

- **Table X (Income DL, main).** *Distributed-lag estimates: SNAP participation (log 1 + recipients) on 12‑month change in county unemployment rate.* County and month fixed effects; lags 0–6; standard errors clustered by county. Sample: Michigan counties, 2014–2022 (or as in data). \(\sum \beta\) is the cumulative effect of a 1 pp increase in the unemployment rate.
- **Figure X (Income DL).** *Dynamic response of SNAP participation to unemployment shock.* Point estimates and 95% confidence intervals for \(\beta_\ell\), \(\ell = 0,\ldots,6\). Regressor: 12‑month change in unemployment rate; county and month FE.
- **Table X (Income heterogeneity).** *Heterogeneity by pre-determined unemployment exposure.* Baseline exposure: county average unemployment rate, 2014–2016; median split. “Sum DL effect” is \(\sum_{\ell=0}^{6} \beta_\ell\) in each group; standard errors from the estimated coefficient covariance matrix.

---

## 4. End of Emergency Allotments (EA) Event Study

We study the end of SNAP Emergency Allotments (EA) using a national event date (March 2023). Event time is defined in *integer months* relative to the EA end:
\[
\text{event time}_t = (12 \times \text{year}_t + \text{month}_t) - (12 \times \text{year}_{\text{EA}} + \text{month}_{\text{EA}}).
\]

### 4.1 Average benefit per person (descriptive)

We estimate a standard event-study regression for the average benefit per person (or per case, depending on data):
\[
Y_{ct} = \alpha_c + \sum_{k \neq -1} \delta_k \,\mathbf{1}(\text{event time}_t = k) + \varepsilon_{ct},
\]
with month \(k = -1\) as the reference. County fixed effects \(\alpha_c\) absorb time-invariant county differences. The coefficients \(\delta_k\) describe the path of average benefits around the EA end; this serves as a first-stage style check that benefits drop at the event.

**Figure note.** *Event-study: Average benefit per person around EA end.* Coefficients \(\delta_k\) relative to month \(-1\); county fixed effects; 95% CI; standard errors clustered by county.

### 4.2 Participation: intensity differential (main identification)

For participation (e.g., \(\ln(1+\text{recipients})\)), we use a *differential* event-study that controls for common time shocks and identifies the effect through variation in pre-EA benefit intensity. We define “intensity” as county \(c\)’s average benefit per person over event months \(-12\) to \(-1\). Counties above the median of this measure are “high intensity”; the rest are “low intensity.”

We estimate
\[
Y_{ct} = \alpha_c + \gamma_t + \sum_{k \neq -1} \theta_k \,\big(\mathbf{1}(\text{event time}_t = k) \times \text{High intensity}_c\big) + \varepsilon_{ct},
\]
where \(\gamma_t\) are month (date) fixed effects. Only the interaction terms are included; the main effect of event time is absorbed by \(\gamma_t\), and the main effect of intensity is absorbed by \(\alpha_c\). The coefficients \(\theta_k\) are the *difference* in outcome between high- and low-intensity counties at each event month \(k\), relative to month \(-1\). Under the assumption that, conditional on county and common time effects, the only differential driver at the EA end is exposure to the benefit cut, \(\theta_k\) traces the causal differential response of participation by intensity.

**Figure note.** *EA end: Participation (log recipients)—high vs low pre-EA benefit intensity.* Differential event-study (high minus low) by event month; county and month fixed effects; reference month \(-1\); 95% CI; standard errors clustered by county.

### 4.3 Appendix: Pre/post by intensity

As a descriptive complement, we report mean outcomes in the pre window (event months \(-6\) to \(-1\)) and post window (event months \(+1\) to \(+6\)) by intensity group (high/low, median split on pre-EA average benefit). The change (post minus pre) in each group is reported in the appendix table; the main text relies on the intensity DID specification above.

**Table note (appendix).** *Pre/post means by pre-EA benefit intensity.* High (low) intensity: counties above (below) median of average benefit per person in event months \(-12\) to \(-1\). Pre: months \(-6\) to \(-1\); post: months \(+1\) to \(+6\). “Change” = post mean − pre mean.

---

## File and output reference

| Section   | Output (tables/figures) |
|----------|--------------------------|
| 3 DL     | `outputs/tables/income_dl_main.csv`; `outputs/figures/income_dl_recipients.png` |
| 3 Het    | `outputs/tables/income_heterogeneity_unemp.csv` |
| 4.1 Avg  | `outputs/figures/ea_es_avgpp.png`; `outputs/tables/ea_es_avgpp.csv` |
| 4.2 Part | `outputs/figures/ea_es_participation.png`; `outputs/tables/ea_es_participation.csv` |
| 4.3 App  | `outputs/tables/ea_heterogeneity_intensity.csv` |
