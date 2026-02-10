# 05_forecast_2026 — 2026 OBBA 政策情景影响（非简单预测）

## 定位

本模块评估 **2026 年起生效的联邦 OBBA 政策** 对 Michigan SNAP（尤其是 ABAWD）参与和退出的影响，而不是对历史序列做简单外推。

## 2026 OBBA 政策要点

- **生效日期**：2026-01-01（见 `config/globals.R` 中 `OBBA_EFFECTIVE_DATE`）。
- **年龄扩展**：ABAWD 工作规定从 **18–54 岁** 扩展到 **18–64 岁**，即 **55–64 岁** 人群新纳入工作要求（`OBBA_AGE_EXPANSION = c(55, 64)`）。
- **豁免收紧**：州一级的裁量豁免名额减少（Fiscal Responsibility Act 等），可用于情景假设或从 `data/raw/abawd_exemptions_fy26.csv` 读入（若存在）。

## 流水线逻辑

1. **01_baseline_model.R**  
   在“当前规则”（18–54 岁 ABAWD）下建立基线模型（如用 step1 DID、step2 DML 的效应 + 03_income 的失业弹性），并做 backtest。

2. **02_scenarios.R**  
   在 **OBBA 生效** 假设下构造情景：  
   - 政策情景：55–64 岁新纳入、豁免收紧（可结合县/州豁免数据）；  
   - 经济情景：失业率/劳动力需求高/低等。  
   输出各县（或州）在 2026 及之后的参与/退出预测（水平或相对基线的变化）。

3. **03_outputs_maps.R**  
   根据情景结果出图、出表、出地图（例如各县 2026 预测参与率变化、新受影响人口等）。

## 数据与配置

- 基线效应来自：`data/derived/panel_analysis.rds`、`outputs/step1_did/`、可选 `outputs/step2_dml/`。
- 政策参数：`config/globals.R`（`OBBA_EFFECTIVE_DATE`, `OBBA_AGE_EXPANSION`, `PATH_OBBA_EXEMPTIONS`）。
- 可选：`data/raw/abawd_exemptions_fy26.csv` 提供 FY2026 各县/州豁免或覆盖信息，供情景使用。

## 实现状态

当前 01/02/03 仍为 stub，需按上述逻辑实现：基线模型 → OBBA 情景 → 图表与地图。
