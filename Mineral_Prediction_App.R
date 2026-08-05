# ===========================================================================
# Mineral_Prediction_App.R
# 矿物共生关系智能推荐系统 — 完整交互式 Shiny 应用
# ===========================================================================
# 功能说明：
#   1. 用户选择目标国家，系统自动判断本地是否已缓存该国的矿物数据。
#      若已缓存则直接加载；若不存在则调用 Mindat API 获取数据、
#      构建事务数据库并缓存至本地。
#   2. 用户通过模糊搜索输入已知矿物（一种或多种），可选设置预测目标。
#   3. 系统验证输入矿物的有效性，在后台运行定向 Apriori 关联规则挖掘。
#   4. 每条候选规则的 LHS 与用户输入计算匹配度：
#      匹配度 = |LHS 交 输入矿物| / |LHS|
#   5. 综合得分 = 匹配度 x 提升度，按综合得分降序取 Top N。
#   6. 结果展示：(1)推荐矿物列表 (2)展开查看关联规则 (3)共生网络图。
#   7. 支持 CSV 导出。
#
# 运行方式（R/RStudio）：
#   setwd("D:/Desktop/新建文件夹/Mineral-Association-Analysis/self-codes")
#   source("Mineral_Prediction_App.R")
# ===========================================================================

# ===========================================================================
# 0. 加载全部依赖包
# ===========================================================================
library(shiny)
library(shinythemes)
library(arules)
library(DT)
library(dplyr)
library(stringr)
library(igraph)
library(OpenMindat)
library(httr)
library(jsonlite)

# ===========================================================================
# 1. 全局变量与辅助函数
# ===========================================================================
`%+%` <- function(x, y) paste0(x, y)

CACHE_DIR <- "data_cache"
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

# ===========================================================================
# 2. 国家列表
# ===========================================================================
AVAILABLE_COUNTRIES <- sort(c(
  "Afghanistan", "Albania", "Algeria", "Andorra", "Angola",
  "Antarctica", "Argentina", "Armenia", "Australia", "Austria",
  "Azerbaijan", "Bahamas", "Bangladesh", "Belarus", "Belgium",
  "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina",
  "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina Faso",
  "Burundi", "Cambodia", "Cameroon", "Canada", "Cape Verde",
  "Central African Republic", "Chad", "Chile", "China", "Colombia",
  "Comoros", "Congo", "Costa Rica", "Croatia", "Cuba",
  "Cyprus", "Czech Republic", "Denmark", "Djibouti",
  "Dominican Republic", "DR Congo", "Ecuador", "Egypt",
  "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia",
  "Eswatini", "Ethiopia", "Fiji", "Finland", "France",
  "Gabon", "Gambia", "Georgia", "Germany", "Ghana",
  "Greece", "Greenland", "Guatemala", "Guinea", "Guinea-Bissau",
  "Guyana", "Haiti", "Honduras", "Hungary", "Iceland",
  "India", "Indonesia", "Iran", "Iraq", "Ireland",
  "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan",
  "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kosovo",
  "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon",
  "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania",
  "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives",
  "Mali", "Malta", "Mauritania", "Mauritius", "Mexico",
  "Moldova", "Monaco", "Mongolia", "Montenegro", "Morocco",
  "Mozambique", "Myanmar", "Namibia", "Nepal", "Netherlands",
  "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Korea",
  "North Macedonia", "Norway", "Oman", "Pakistan", "Palau",
  "Palestine", "Panama", "Papua New Guinea", "Paraguay", "Peru",
  "Philippines", "Poland", "Portugal", "Qatar", "Romania",
  "Russia", "Rwanda", "Saudi Arabia", "Senegal", "Serbia",
  "Sierra Leone", "Singapore", "Slovakia", "Slovenia",
  "Solomon Islands", "Somalia", "South Africa", "South Korea",
  "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname",
  "Sweden", "Switzerland", "Syria", "Taiwan", "Tajikistan",
  "Tanzania", "Thailand", "Timor-Leste", "Togo", "Tonga",
  "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan",
  "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom",
  "Uruguay", "USA", "Uzbekistan", "Vanuatu", "Vatican City",
  "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe"
))

# ===========================================================================
# 3. 数据获取引擎
# ===========================================================================

load_country_data <- function(country_name, api_key) {
  cache_file <- file.path(CACHE_DIR,
    paste0(gsub("[^A-Za-z0-9_]", "_", country_name), ".rds"))

  if (file.exists(cache_file)) {
    cat(sprintf("  [Cache] Loading %s from local...\n", country_name))
    result <- readRDS(cache_file)
    cat(sprintf("  [Cache] Done: %s localities, %s minerals\n",
                result$n_localities, result$n_minerals))
    return(result)
  }

  cat(sprintf("  [API] Fetching %s data from Mindat...\n", country_name))
  mindat_connection(api_key)

  cat("  [API] Downloading localities...\n")
  locs <- localities_list_country(country_name, expand = "~all")
  cat(sprintf("  [API] Got %d localities\n", length(locs$id)))

  cat("  [API] Downloading IMA mineral dictionary...\n")
  mineral_dict <- minerals_ima_list()
  cat(sprintf("  [API] Dictionary: %d x %d\n", nrow(mineral_dict), ncol(mineral_dict)))
  mineral_map <- setNames(mineral_dict$name, as.character(mineral_dict$id))

  cat("  [API] Building Locality-Mineral table...\n")
  locality_minerals <- data.frame(
    locality_id = locs$id, locality_name = locs$txt,
    mineral_list = character(nrow(locs)), stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(locs))) {
    if (i %% 5000 == 0) cat(sprintf("  [API] %d / %d\n", i, nrow(locs)))
    ids <- locs$geomaterials[[i]]
    if (length(ids) > 0) {
      mins <- mineral_map[as.character(ids)]
      mins <- unique(na.omit(mins))
      locality_minerals$mineral_list[i] <- paste(mins, collapse = ";")
    }
  }
  empty_n <- sum(locality_minerals$mineral_list == "")
  cat(sprintf("  [API] Empty records removed: %d\n", empty_n))
  locality_minerals <- locality_minerals[locality_minerals$mineral_list != "", ]

  cat("  [API] Converting to transactions...\n")
  mineral_split <- lapply(locality_minerals$mineral_list, function(x) {
    mins <- strsplit(x, ";")[[1]]; mins <- trimws(mins); mins[mins != ""]
  })
  names(mineral_split) <- locality_minerals$locality_id
  lm_df <- data.frame(
    locality_id = rep(locality_minerals$locality_id, sapply(mineral_split, length)),
    mineral     = unlist(mineral_split), stringsAsFactors = FALSE
  )
  trans <- as(split(lm_df$mineral, lm_df$locality_id), "transactions")

  result <- list(trans = trans, n_localities = length(trans),
                 n_minerals = ncol(trans), all_minerals = sort(colnames(trans)))
  saveRDS(result, cache_file)
  cat(sprintf("  [Cache] Saved to %s\n", cache_file))
  return(result)
}

# ===========================================================================
# 4. 核心预测引擎（一次挖掘 + 粗筛 + 精细匹配）
# ===========================================================================
# 策略：
#   Apriori: 1次全量挖掘 → is.redundant(仅1次) → 粗筛(RHS+Lift) →
#            strsplit(仅对粗筛后子集) → 精细评分 → Top N
#   FP-Growth/Eclat: 1次全量项集 → ruleInduction → is.redundant(仅1次) →
#                    粗筛(RHS+Lift) → strsplit(仅对粗筛后子集) → 精细评分
# 关键：is.redundant 只执行1次，strsplit 只对粗筛后 10-50 万条规则执行

predict_minerals <- function(known_minerals, target_minerals, trans_data,
                              min_support, min_confidence, min_lift, top_n,
                              algorithm = "Apriori") {

  all_mins <- colnames(trans_data)
  n_known  <- length(known_minerals)
  rule_maxlen <- max(n_known + 1L, 4L)
  minlen_val  <- max(n_known + 1L, 2L)

  # ---- 诊断 ----
  cat(sprintf("  [诊断] 事务数据：%d 产地 x %d 矿物\n",
              length(trans_data), ncol(trans_data)))
  for (km in known_minerals) {
    if (km %in% all_mins) {
      freq_km <- itemFrequency(trans_data, type = "absolute")[km]
      cat(sprintf("  [诊断] '%s' 出现 %d 次 (相对频率 %.4f)\n",
                  km, as.integer(freq_km), round(freq_km / length(trans_data), 4)))
    }
  }
  cat(sprintf("  [诊断] rule_maxlen=%d minlen=%d | supp=%.5f conf=%.2f\n",
              rule_maxlen, minlen_val, min_support, min_confidence))

  # ---- 第 1 步：构建候选 RHS 集 ----
  use_targets <- !is.null(target_minerals) && length(target_minerals) > 0
  if (use_targets) {
    valid_tg <- target_minerals[target_minerals %in% all_mins]
    if (length(valid_tg) == 0) return(data.frame())
    candidate_rhs <- valid_tg
  } else {
    n_trans <- length(trans_data)
    min_abs_count <- max(1L, as.integer(ceiling(min_support * n_trans)))
    freq_abs <- sort(itemFrequency(trans_data, type = "absolute"), decreasing = TRUE)
    freq_abs <- freq_abs[freq_abs >= min_abs_count]
    candidate_rhs <- setdiff(names(freq_abs), known_minerals)
    candidate_rhs <- head(candidate_rhs, 200)
  }
  if (length(candidate_rhs) == 0) return(data.frame())
  cat(sprintf("  [诊断] 候选 RHS: %d 种矿物\n", length(candidate_rhs)))

  # ==================================================================
  # 第 2 步：一次性挖掘全部规则（各算法统一：1 次全量 + 1 次去冗余）
  # ==================================================================
  cat(sprintf("  [挖掘] 一次性挖掘 (%s)...\n", algorithm))

  rules_all <- tryCatch({
    if (algorithm == "Apriori") {
      apriori(trans_data,
        parameter = list(supp = min_support, conf = min_confidence,
                         target = "rules",
                         minlen = minlen_val, maxlen = rule_maxlen),
        control = list(verbose = FALSE))
    } else if (algorithm == "FP-Growth") {
      itemsets <- fpgrowth(trans_data,
        parameter = list(supp = min_support, minlen = minlen_val,
                         maxlen = rule_maxlen),
        control = list(verbose = FALSE))
      ruleInduction(itemsets, trans_data, confidence = min_confidence)
    } else {
      itemsets <- eclat(trans_data,
        parameter = list(supp = min_support, minlen = minlen_val,
                         maxlen = rule_maxlen),
        control = list(verbose = FALSE))
      ruleInduction(itemsets, trans_data, confidence = min_confidence)
    }
  }, error = function(e) NULL)

  if (is.null(rules_all) || length(rules_all) == 0) return(data.frame())
  cat(sprintf("  [挖掘] 原始规则: %s 条\n", format(length(rules_all), big.mark = ",")))

  # ---- 去冗余（仅1次） ----
  rules_all <- rules_all[!is.redundant(rules_all)]
  cat(sprintf("  [去重] 去冗余后: %s 条\n", format(length(rules_all), big.mark = ",")))

  # ==================================================================
  # 第 3 步：粗筛 —— 纯向量操作，秒级完成
  # ==================================================================
  # 提取 RHS 文本和 Lift，做向量化筛选
  rhs_raw <- as.character(labels(rhs(rules_all)))
  rhs_clean <- gsub("[{}]", "", rhs_raw)
  q_all    <- quality(rules_all)

  coarse_keep <- which(rhs_clean %in% candidate_rhs &
                       q_all$lift >= min_lift)
  cat(sprintf("  [粗筛] RHS+Lift 筛选后: %s 条\n",
              format(length(coarse_keep), big.mark = ",")))

  if (length(coarse_keep) == 0) { rm(rules_all); return(data.frame()) }

  # ---- 仅对粗筛子集做文本处理 ----
  rules_sub   <- rules_all[coarse_keep]
  q_sub       <- q_all[coarse_keep, ]
  rhs_sub     <- rhs_clean[coarse_keep]
  lhs_raw_sub <- as.character(labels(lhs(rules_sub)))
  lhs_clean_sub <- gsub("[{}]", "", lhs_raw_sub)

  # 释放大对象
  rm(rules_all, rhs_raw, rhs_clean, q_all, coarse_keep, lhs_raw_sub)

  # ==================================================================
  # 第 4 步：精细匹配 —— LHS 拆分 + 匹配度 + 综合评分
  # ==================================================================
  lhs_split <- strsplit(lhs_clean_sub, ",\\s*")
  n_sub     <- nrow(q_sub)

  results_list <- vector("list", length(candidate_rhs))
  idx <- 0

  for (j in seq_len(n_sub)) {
    lhs_i <- lhs_split[[j]]
    matched <- intersect(lhs_i, known_minerals)
    if (length(matched) < n_known) next

    mr    <- length(matched) / length(lhs_i)
    score <- mr * q_sub$lift[j]

    idx <- idx + 1
    results_list[[idx]] <- data.frame(
      Predicted      = rhs_sub[j],
      Rule_LHS       = paste(lhs_i, collapse = ", "),
      Rule_RHS       = rhs_sub[j],
      Full_Rule      = paste0("{", paste(lhs_i, collapse = ", "),
                              "} => {", rhs_sub[j], "}"),
      Matched_Input  = paste(matched, collapse = ", "),
      Match_Ratio    = round(mr, 2),
      Support        = round(q_sub$support[j], 4),
      Confidence     = round(q_sub$confidence[j], 4),
      Lift           = round(q_sub$lift[j], 2),
      Combined_Score = round(score, 2),
      stringsAsFactors = FALSE
    )
  }

  cat(sprintf("  [精细] 有效结果: %d 条 (LHS包含全部已知)\n", idx))

  rm(lhs_split, q_sub, rules_sub)

  if (idx == 0) return(data.frame())

  results <- do.call(rbind, results_list[1:idx])

  # 每目标矿物仅保留综合得分最高的一条
  results %>%
    group_by(Predicted) %>%
    slice_max(order_by = Combined_Score, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(desc(Combined_Score)) %>%
    head(top_n) %>%
    mutate(Rank = row_number()) %>%
    select(Rank, everything())
}

# ===========================================================================
# 5. 共生网络图构建
# ===========================================================================

build_network <- function(pred_df, known_mins) {
  if (nrow(pred_df) == 0) return(NULL)
  edges <- data.frame(from = character(), to = character(),
                      lift = numeric(), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(pred_df))) {
    lhs_items <- trimws(strsplit(pred_df$Rule_LHS[i], ",")[[1]])
    for (lhs_min in lhs_items) {
      edges <- rbind(edges, data.frame(
        from = lhs_min, to = pred_df$Predicted[i],
        lift = pred_df$Lift[i], stringsAsFactors = FALSE
      ))
    }
  }
  g <- graph_from_data_frame(edges, directed = TRUE)
  node_type <- ifelse(V(g)$name %in% known_mins, "Known", "Predicted")
  V(g)$type  <- node_type
  V(g)$color <- ifelse(node_type == "Known", "#3498db", "#e74c3c")
  list(graph = g, edges = edges)
}

# ===========================================================================
# 6. Shiny UI
# ===========================================================================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$style(HTML("
    .title-banner {
      background: linear-gradient(135deg, #1a5276, #2e86c1);
      color: white; padding: 22px 28px; border-radius: 8px;
      margin-bottom: 18px; text-align: center;
    }
    .section-box {
      background: #f8f9fa; border: 1px solid #dee2e6;
      border-radius: 6px; padding: 14px; margin-bottom: 14px;
    }
    .rule-detail { font-family: monospace; font-size: 12px;
                   background: #f0f0f0; padding: 6px 10px;
                   border-radius: 4px; margin: 3px 0; }
  "))),

  div(class = "title-banner",
    h2("Mineral Association Recommendation System", style = "margin: 0; font-weight: 400;"),
    p("Data-driven mineral co-occurrence prediction powered by Mindat open database",
      style = "margin: 6px 0 0 0; opacity: 0.8; font-size: 13px;")
  ),

  sidebarLayout(
    sidebarPanel(width = 4,
      div(class = "section-box",
        h4("Step 1 - Data Preparation", style = "margin-top: 0;"),
        selectInput("country", "Select country:",
          choices = AVAILABLE_COUNTRIES, selected = "USA"),
        passwordInput("api_key", "Mindat API Key:",
          value = "321e35e57066ccb129a9d9af7ffe1958"),
        actionButton("load_data", "Load Data",
          icon = icon("cloud-download-alt"),
          class = "btn-primary", style = "width: 100%;"),
        br(), br(), uiOutput("data_status")
      ),
      # ---- 矿物属性筛选模块（新增）----
      div(class = "section-box",
        h4("Step 2 - Mineral Attribute Filter (optional)", style = "margin-top: 0;"),
        p("Narrow down minerals by their physical/chemical properties. "
          %+% "Results are intersected with the loaded country data.",
          style = "font-size: 11px; color: #888;"),

        # --- 晶系 ---
        selectInput("crystal_sys", "Crystal System:",
          choices = c("(any)" = "", "Triclinic", "Monoclinic", "Orthorhombic",
                      "Tetragonal", "Trigonal", "Hexagonal", "Cubic",
                      "Amorphous"),
          selected = ""),

        # --- 莫氏硬度范围 ---
        sliderInput("hardness_range", "Mohs Hardness Range:",
          min = 0, max = 10, value = c(0, 10), step = 0.5),

        # --- 元素组成（逗号分隔的元素符号） ---
        textInput("elem_input", "Chemical Elements (comma separated symbols):",
          value = "", placeholder = "e.g.: Cu, Fe, S"),

        # --- 光泽 ---
        selectInput("lustre_type", "Lustre:",
          choices = c("(any)" = "", "Metallic", "Sub-Metallic", "Vitreous",
                      "Adamantine", "Resinous", "Greasy", "Pearly",
                      "Silky", "Dull", "Earthy", "Waxy"),
          selected = ""),

        fluidRow(
          column(6,
            actionButton("apply_filter", "Apply Filter",
              icon = icon("filter"), class = "btn-info",
              style = "width: 100%;")
          ),
          column(6,
            actionButton("reset_filter", "Reset Filter",
              icon = icon("undo"), class = "btn-default",
              style = "width: 100%;")
          )
        ),
        br(),
        uiOutput("filter_status")
      ),

      div(class = "section-box",
        h4("Step 3 - Mineral Query", style = "margin-top: 0;"),
        p("Select from candidate minerals (filtered by Step 2, or all if no filter applied).",
          style = "font-size: 11px; color: #888;"),
        selectizeInput("known_minerals",
          label = "Known minerals (required):",
          choices = NULL, multiple = TRUE,
          options = list(placeholder = "Type mineral name to search...",
                         maxItems = 20, plugins = list("remove_button"))),
        selectizeInput("target_mineral",
          label = "Prediction target (optional):",
          choices = NULL, multiple = TRUE,
          options = list(placeholder = "Leave blank to search all candidates",
                         maxItems = 10, plugins = list("remove_button")))
      ),
      div(class = "section-box",
        h4("Step 4 - Parameters", style = "margin-top: 0;"),

        # --- 算法选择 ---
        selectInput("algo", "Mining Algorithm:",
          choices = c("Apriori", "FP-Growth", "Eclat"),
          selected = "Apriori"),

        numericInput("min_supp", "Minimum Support:",
          value = 0.0001, min = 0.00001, max = 0.1, step = 0.0001),
        numericInput("min_conf", "Minimum Confidence:",
          value = 0.3, min = 0.1, max = 1.0, step = 0.05),
        numericInput("min_lift", "Minimum Lift:",
          value = 1.0, min = 0.5, max = 50, step = 0.5),
        sliderInput("top_n", "Top N results:",
          min = 5, max = 100, value = 20, step = 5),
        p("Combined Score = Match Ratio x Lift",
          style = "font-size: 11px; color: #999; margin-top: 4px;")
      ),
      hr(),
      fluidRow(
        column(8, actionButton("predict_btn", "Run Prediction",
          icon = icon("search"), class = "btn-success btn-lg",
          style = "width: 100%;")),
        column(4, actionButton("clear_btn", "Clear",
          icon = icon("trash"), class = "btn-default", style = "width: 100%;"))
      ),
      br(),
      downloadButton("download_csv", "Export CSV",
        class = "btn-info", style = "width: 100%;")
    ),

    mainPanel(width = 8,
      tabsetPanel(id = "main_tabs",
        tabPanel("Recommendations", icon = icon("list-alt"),
          br(), uiOutput("warning_box"),
          h4("Recommended Minerals (Ranked by Combined Score)"),
          DT::dataTableOutput("results_table"),
          br(), p(textOutput("query_info"),
            style = "color: #888; font-size: 12px;")),
        tabPanel("Rule Details", icon = icon("code"),
          br(), p("Each row shows the association rule behind the recommendation.",
            style = "color: #888;"),
          DT::dataTableOutput("rule_details_table")),
        tabPanel("Co-occurrence Network", icon = icon("project-diagram"),
          br(), plotOutput("network_plot", height = "700px"),
          br(), p("Blue = input minerals. Red = predicted. Edge width proportional to Lift.",
            style = "color: #888; font-size: 12px;")),
        tabPanel("Database", icon = icon("database"),
          br(), uiOutput("db_info"),
          h4("Top 20 Most Frequent Minerals"),
          DT::dataTableOutput("freq_table"))
      )
    )
  )
)

# ===========================================================================
# 7. Shiny Server
# ===========================================================================
server <- function(input, output, session) {
  trans_data    <- reactiveVal(NULL)
  data_ready    <- reactiveVal(FALSE)
  data_info     <- reactiveVal(list(country = "", n_loc = 0, n_min = 0))
  all_mins      <- reactiveVal(NULL)
  filtered_mins <- reactiveVal(NULL)   # 属性筛选后的候选矿物集合
  pred_result   <- reactiveVal(NULL)
  query_str     <- reactiveVal("")
  elapsed_t     <- reactiveVal(0)

  # ---- 7.1 数据加载 ----
  observeEvent(input$load_data, {
    country <- input$country; api_key <- input$api_key
    if (is.null(api_key) || trimws(api_key) == "") {
      showNotification("Please enter a valid Mindat API Key.", type = "error", duration = 5)
      return()
    }
    withProgress(message = paste("Processing", country, "..."), value = 0, {
      incProgress(0.1, detail = "Connecting...")
      result <- tryCatch({ load_country_data(country, api_key) },
        error = function(e) { showNotification(paste("Error:", e$message), type = "error"); NULL })
      if (is.null(result)) return()
      incProgress(0.9, detail = "Finalizing...")
      trans_data(result$trans)
      data_ready(TRUE)
      all_mins(result$all_minerals)
      filtered_mins(result$all_minerals)     # 初始时全部矿物均为候选
      data_info(list(country = country, n_loc = result$n_localities, n_min = result$n_minerals))
      updateSelectizeInput(session, "known_minerals",
        choices = filtered_mins(), server = TRUE)
      updateSelectizeInput(session, "target_mineral",
        choices = filtered_mins(), server = TRUE)
      showNotification(
        sprintf("%s: %s localities, %s minerals.",
          country, format(result$n_localities, big.mark = ","),
          format(result$n_minerals, big.mark = ",")),
        type = "message", duration = 5)
    })
  })

  # ---- 7.2 状态 ----
  output$data_status <- renderUI({
    if (!data_ready())
      return(div(style = "padding:8px; background:#fdf2e9; border-radius:4px;",
        icon("warning"), " No data loaded."))
    info <- data_info()
    div(style = "padding:8px; background:#d5f5e3; border-radius:4px;",
      icon("check-circle"), " ", info$country, ": ",
      format(info$n_loc, big.mark = ","), " localities, ",
      format(info$n_min, big.mark = ","), " minerals")
  })

  # ---- 7.3 清除 ----
  observeEvent(input$clear_btn, {
    updateSelectizeInput(session, "known_minerals", selected = character(0))
    updateSelectizeInput(session, "target_mineral", selected = character(0))
    pred_result(NULL); query_str(""); elapsed_t(0)
    # 重置属性筛选控件
    updateSelectInput(session, "crystal_sys", selected = "")
    updateSliderInput(session, "hardness_range", value = c(0, 10))
    updateTextInput(session, "elem_input", value = "")
    updateSelectInput(session, "lustre_type", selected = "")
    if (data_ready()) {
      filtered_mins(all_mins())
      updateSelectizeInput(session, "known_minerals",
        choices = all_mins(), server = TRUE)
      updateSelectizeInput(session, "target_mineral",
        choices = all_mins(), server = TRUE)
    }
  })

  # ---- 7.3b 属性筛选：Apply Filter ----
  observeEvent(input$apply_filter, {
    if (!data_ready()) {
      showNotification("Please load data first.", type = "warning", duration = 3)
      return()
    }
    # 检查用户是否至少设置了一个筛选条件
    has_crystal <- !is.null(input$crystal_sys) && input$crystal_sys != ""
    has_lustre  <- !is.null(input$lustre_type) && input$lustre_type != ""
    has_elem    <- !is.null(input$elem_input) && trimws(input$elem_input) != ""
    is_hardness <- !is.null(input$hardness_range) &&
                   (input$hardness_range[1] > 0 || input$hardness_range[2] < 10)

    # 用户可选择零个、一个或多个筛选条件；未填写项自动忽略
    if (!has_crystal && !has_lustre && !has_elem && !is_hardness) {
      # 全空 ≈ 不做筛选，直接恢复全量候选
      filtered_mins(all_mins())
      updateSelectizeInput(session, "known_minerals",
        choices = all_mins(), server = TRUE)
      updateSelectizeInput(session, "target_mineral",
        choices = all_mins(), server = TRUE)
      showNotification("No filter applied. All minerals available.",
                       type = "message", duration = 3)
      return()
    }

    country_mins <- all_mins()
    api_key <- isolate(input$api_key)

    withProgress(message = "Querying Mindat...", value = 0, {
      result_sets <- list()

      # ---- 晶系筛选 ----
      if (has_crystal) {
        incProgress(0.1, detail = paste("Crystal system:", input$crystal_sys))
        crystal_mins <- tryCatch({
          mindat_connection(api_key)
          res <- geomaterials_crystal_system(input$crystal_sys)
          if (!is.null(res) && "name" %in% names(res)) unique(res$name)
          else character(0)
        }, error = function(e) { cat("Crystal filter error:", e$message, "\n"); character(0) })
        result_sets[["crystal"]] <- crystal_mins
        cat(sprintf("  [Filter] Crystal System '%s': %d results\n",
                    input$crystal_sys, length(crystal_mins)))
      }

      # ---- 硬度范围筛选 ----
      if (is_hardness) {
        incProgress(0.15, detail = sprintf("Hardness: %.1f-%.1f",
                 input$hardness_range[1], input$hardness_range[2]))
        hard_mins <- tryCatch({
          mindat_connection(api_key)
          res <- geomaterials_hardness_range(
            input$hardness_range[1], input$hardness_range[2])
          if (!is.null(res) && "name" %in% names(res)) unique(res$name)
          else character(0)
        }, error = function(e) { cat("Hardness filter error:", e$message, "\n"); character(0) })
        result_sets[["hardness"]] <- hard_mins
        cat(sprintf("  [Filter] Hardness [%.1f, %.1f]: %d results\n",
                    input$hardness_range[1], input$hardness_range[2], length(hard_mins)))
      }

      # ---- 元素组成筛选 ----
      if (has_elem) {
        incProgress(0.2, detail = paste("Elements:", input$elem_input))
        elem_vec <- trimws(strsplit(input$elem_input, "[,;]+")[[1]])
        elem_vec <- elem_vec[elem_vec != ""]
        elem_mins <- tryCatch({
          mindat_connection(api_key)
          res <- geomaterials_contain_any_elems(elem_vec)
          if (!is.null(res) && "name" %in% names(res)) unique(res$name)
          else character(0)
        }, error = function(e) { cat("Element filter error:", e$message, "\n"); character(0) })
        result_sets[["elements"]] <- elem_mins
        cat(sprintf("  [Filter] Elements [%s]: %d results\n",
                    paste(elem_vec, collapse=","), length(elem_mins)))
      }

      # ---- 光泽筛选 ----
      if (has_lustre) {
        incProgress(0.25, detail = paste("Lustre:", input$lustre_type))
        lustre_mins <- tryCatch({
          mindat_connection(api_key)
          res <- geomaterials_lustretype(input$lustre_type)
          if (!is.null(res) && "name" %in% names(res)) unique(res$name)
          else character(0)
        }, error = function(e) { cat("Lustre filter error:", e$message, "\n"); character(0) })
        result_sets[["lustre"]] <- lustre_mins
        cat(sprintf("  [Filter] Lustre '%s': %d results\n",
                    input$lustre_type, length(lustre_mins)))
      }

      incProgress(0.3, detail = "Intersecting with country minerals...")

      # ---- 多条件交集 + 与国家矿物求交 ----
      if (length(result_sets) == 1) {
        candidates <- intersect(result_sets[[1]], country_mins)
      } else {
        candidates <- Reduce(intersect, result_sets)
        candidates <- intersect(candidates, country_mins)
      }

      incProgress(0.35, detail = "Done")
      filtered_mins(sort(candidates))
    })

    n_cand <- length(filtered_mins())
    cat(sprintf("  [Filter] Final candidates: %d (intersection with %d country minerals)\n",
                n_cand, length(country_mins)))

    updateSelectizeInput(session, "known_minerals",
      choices = filtered_mins(), server = TRUE)
    updateSelectizeInput(session, "target_mineral",
      choices = filtered_mins(), server = TRUE)

    if (n_cand == 0) {
      showNotification("No minerals match all filter conditions in this country.",
                       type = "warning", duration = 5)
    } else {
      showNotification(sprintf("Filter applied: %d candidate minerals.", n_cand),
                       type = "message", duration = 4)
    }
  })

  # ---- 7.3c 属性筛选：Reset Filter ----
  observeEvent(input$reset_filter, {
    updateSelectInput(session, "crystal_sys", selected = "")
    updateSliderInput(session, "hardness_range", value = c(0, 10))
    updateTextInput(session, "elem_input", value = "")
    updateSelectInput(session, "lustre_type", selected = "")
    if (data_ready()) {
      filtered_mins(all_mins())
      updateSelectizeInput(session, "known_minerals",
        choices = all_mins(), server = TRUE)
      updateSelectizeInput(session, "target_mineral",
        choices = all_mins(), server = TRUE)
    }
  })

  # ---- 7.3d 筛选状态显示 ----
  output$filter_status <- renderUI({
    if (!data_ready()) return(NULL)
    n_total  <- length(all_mins())
    n_filter <- length(filtered_mins())
    if (n_filter == n_total) {
      return(div(style = "padding:6px; background:#eaf2f8; border-radius:4px; font-size:12px;",
        "Candidates: ", tags$b(format(n_total, big.mark=",")), " minerals (all)"))
    }
    div(style = "padding:6px; background:#d5f5e3; border-radius:4px; font-size:12px;",
      "Candidates: ", tags$b(format(n_filter, big.mark=",")), " minerals",
      " (filtered from ", format(n_total, big.mark=","), ")")
  })

  # ---- 7.4 预测 ----
  observeEvent(input$predict_btn, {
    if (!data_ready()) {
      showNotification("Please load data first.", type = "error", duration = 4); return()
    }
    known_mins <- input$known_minerals
    if (is.null(known_mins) || length(known_mins) == 0) {
      showNotification("Please select at least one known mineral.", type = "warning", duration = 4); return()
    }
    target_mins <- input$target_mineral
    if (length(target_mins) == 0) target_mins <- NULL

    am <- all_mins()
    valid_k <- known_mins[known_mins %in% am]
    invalid_k <- setdiff(known_mins, am)
    query_str(paste(valid_k, collapse = ", "))

    invalid_t <- NULL
    if (!is.null(target_mins)) invalid_t <- target_mins[!(target_mins %in% am)]

    if (length(valid_k) == 0) {
      pred_result(list(results = data.frame(),
        warnings = "None of the entered minerals found in database.")); return()
    }

    t_start <- Sys.time()
    withProgress(message = "Mining rules...", value = 0, {
      incProgress(0.2, detail = paste("Running", input$algo, "..."))
      results <- predict_minerals(
        known_minerals  = valid_k, target_minerals = target_mins,
        trans_data      = trans_data(),
        min_support     = input$min_supp, min_confidence = input$min_conf,
        min_lift        = input$min_lift, top_n = input$top_n,
        algorithm       = input$algo
      )
      incProgress(0.9, detail = "Done")
    })
    t_end <- Sys.time()
    elapsed_t(round(as.numeric(difftime(t_end, t_start, units = "secs")), 1))

    warns <- NULL
    if (length(invalid_k) > 0)
      warns <- c(warns, paste0("Not in database: ", paste(invalid_k, collapse = ", ")))
    if (!is.null(invalid_t) && length(invalid_t) > 0)
      warns <- c(warns, paste0("Target not found: ", paste(invalid_t, collapse = ", ")))
    if (nrow(results) == 0)
      warns <- c(warns, "No rules found. Lower thresholds or broaden search.")
    if (nrow(results) > 0 && nrow(results) <= 10 && length(valid_k) == 1 &&
        (is.null(target_mins) || length(target_mins) == 0))
      warns <- c(warns, paste0(
        "Diagnostic note: only ", nrow(results),
        " recommendations found. With a single known mineral ",
        "and min_conf=", input$min_conf, ", the confidence of each rule ",
        "{known} => {target} is P(target|known) = ",
        "support(known+target) / support(known). ",
        "For a common mineral like Quartz (support ~42%), ",
        "a target needs >= ", round(input$min_conf * 100), "% of Quartz localities ",
        "to co-occur, which mathematically limits results. ",
        "This is a parameter constraint, not a data error. ",
        "Try: (a) lowering min_conf (e.g. 0.05), ",
        "(b) adding a second known mineral, or ",
        "(c) using Target mineral mode to focus the search."))

    pred_result(list(results = results, warnings = warns))
  })

  # ---- 7.5 警告 ----
  output$warning_box <- renderUI({
    res <- pred_result()
    if (is.null(res) || is.null(res$warnings)) return(NULL)
    lapply(res$warnings, function(w) {
      div(class = "alert alert-warning", style = "padding:8px 12px; margin-bottom:6px;",
        icon("exclamation-triangle"), " ", w)
    })
  })

  # ---- 7.6 查询信息 ----
  output$query_info <- renderText({
    res <- pred_result(); q <- query_str()
    if (is.null(res) || q == "") return("")
    paste0("Query: ", q, "  |  Results: ", nrow(res$results), "  |  Time: ", elapsed_t(), "s")
  })

  # ---- 7.7 推荐结果表（纯数值列名，DT JS 渲染稳定） ----
  output$results_table <- DT::renderDataTable({
    res <- pred_result()
    if (is.null(res) || nrow(res$results) == 0)
      return(datatable(data.frame(Message = "Select minerals and click Run Prediction."),
        options = list(dom = "t"), rownames = FALSE))

    df <- data.frame(
      Rank                = res$results$Rank,
      `Recommended Mineral` = res$results$Predicted,
      Score               = res$results$Combined_Score,
      Lift                = res$results$Lift,
      Confidence          = res$results$Confidence,
      Support             = res$results$Support,
      check.names = FALSE, stringsAsFactors = FALSE
    )

    datatable(df, rownames = FALSE, selection = "single",
      options = list(pageLength = 15, lengthMenu = c(10, 15, 25, 50, 100),
        order = list(list(0, "asc")),
        language = list(search = "Filter:",
          info = "Showing _START_ to _END_ of _TOTAL_ recommendations"))
    ) %>%
      formatRound(columns = 3:6, digits = c(2, 2, 4, 4)) %>%
      formatStyle(columns = 3,
        background = styleColorBar(df$Score, "#aed6f1"),
        backgroundSize = "85% 55%", backgroundRepeat = "no-repeat",
        backgroundPosition = "left center")
  })

  # ---- 7.8 规则详情表 ----
  output$rule_details_table <- DT::renderDataTable({
    res <- pred_result()
    if (is.null(res) || nrow(res$results) == 0)
      return(datatable(data.frame(Message = "No results. Run prediction first."),
        options = list(dom = "t"), rownames = FALSE))
    df <- res$results[, c("Rank", "Predicted", "Full_Rule",
      "Match_Ratio", "Support", "Confidence", "Lift", "Combined_Score")]
    colnames(df) <- c("Rank", "Recommended", "Association Rule",
      "Match", "Support", "Confidence", "Lift", "Score")
    datatable(df, rownames = FALSE,
      options = list(pageLength = 15, lengthMenu = c(10, 15, 25, 50, 100),
        order = list(list(0, "asc")),
        language = list(search = "Filter:",
          info = "Showing _START_ to _END_ of _TOTAL_ rules"),
        columnDefs = list(list(targets = 2, render = JS(
          "function(data) { return '<span class=\"rule-detail\">' + data + '</span>'; }")))),
      escape = FALSE
    ) %>% formatRound(columns = c(4, 5, 6, 7, 8), digits = c(2, 4, 4, 2, 2))
  })

  # ---- 7.9 共生网络图 ----
  output$network_plot <- renderPlot({
    res <- pred_result()
    if (is.null(res) || nrow(res$results) == 0) {
      plot.new(); text(0.5, 0.5, "No results.", cex = 1.2, col = "gray"); return()
    }
    net <- build_network(res$results, input$known_minerals)
    if (is.null(net)) { plot.new(); return() }
    g <- net$graph
    vsize <- 8 + log(degree(g, mode = "all") + 1) * 4
    set.seed(42); lo <- layout_with_fr(g)
    par(mar = c(1, 1, 3, 1))
    plot(g, layout = lo, vertex.size = vsize,
      vertex.label = V(g)$name, vertex.label.cex = 0.6,
      vertex.label.color = "black", vertex.color = V(g)$color,
      vertex.frame.color = "gray40", edge.arrow.size = 0.35,
      edge.width = 0.5 + E(g)$lift / max(E(g)$lift) * 3,
      edge.color = adjustcolor("gray50", 0.5), edge.curved = 0.2,
      main = paste0("Mineral Co-occurrence Network\n(Input: ", query_str(), ")"))
    legend("bottomright", legend = c("Known (input)", "Predicted"),
      col = c("#3498db", "#e74c3c"), pch = 19, pt.cex = 1.2, bty = "n", cex = 0.8)
  })

  # ---- 7.10 数据库 ----
  output$db_info <- renderUI({
    if (!data_ready()) return(p("No data loaded."))
    info <- data_info()
    tags$ul(
      tags$li(tags$b("Country: ") %+% info$country),
      tags$li(tags$b("Localities: ") %+% format(info$n_loc, big.mark = ",")),
      tags$li(tags$b("Minerals: ") %+% format(info$n_min, big.mark = ","))
    )
  })

  output$freq_table <- DT::renderDataTable({
    if (!data_ready())
      return(datatable(data.frame(Info = "No data"), options = list(dom = "t"), rownames = FALSE))
    tdata <- trans_data()
    fa <- sort(itemFrequency(tdata, type = "absolute"), decreasing = TRUE)
    n <- min(20, length(fa))
    df <- data.frame(Rank = 1:n, Mineral = names(fa)[1:n],
      Count = as.integer(fa[1:n]),
      Frequency = round(as.numeric(fa[1:n]) / length(tdata), 4), stringsAsFactors = FALSE)
    datatable(df, rownames = FALSE,
      options = list(pageLength = 20, dom = "t", ordering = FALSE)) %>%
      formatRound("Frequency", 4)
  })

  # ---- 7.11 CSV 导出 ----
  output$download_csv <- downloadHandler(
    filename = function() paste0("mineral_prediction_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      res <- pred_result()
      if (is.null(res) || nrow(res$results) == 0) {
        write.csv(data.frame(Message = "No results"), file, row.names = FALSE); return()
      }
      export <- res$results[, c("Predicted", "Combined_Score", "Match_Ratio",
        "Support", "Confidence", "Lift", "Full_Rule")]
      colnames(export) <- c("Recommended_Mineral", "Combined_Score", "Match_Ratio",
        "Support", "Confidence", "Lift", "Association_Rule")
      write.csv(export, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

# ===========================================================================
# 8. 启动
# ===========================================================================
cat("\n================================================\n")
cat("  Mineral Association Recommendation System     \n")
cat("================================================\n\n")
shinyApp(ui = ui, server = server, options = list(
  host = "127.0.0.1", port = 3838, launch.browser = TRUE
))
