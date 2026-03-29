#!/usr/bin/env Rscript
# Honeymoon itinerary solver
#
# Constraint: visit exactly 3 distinct islands (excl PPT/RFP), staying 4 nights
# on each. Transiting through an island on a connection does NOT count as visiting.
#
#   2-Sep  : fly PPT → Island A   (stay 2–5 Sep)
#   6-Sep  : fly → Island B       (stay 6–9 Sep)  B ≠ A
#  10-Sep  : fly → Island C       (stay 10–13 Sep) C ≠ A,B
#  14-Sep  : fly → RFP
#
# Same-day multi-leg connections allowed. Minimises total flight segments.

library(tidyverse)

# ── Flight data (loaded from CSV) ─────────────────────────────────────────────
flights <- read_csv("flights.csv", show_col_types = FALSE)

hubs          <- c("PPT", "RFP")

# Derive dates (sorted chronologically) and islands from the data
flights <- flights %>%
  mutate(date_parsed = dmy(paste0(date, "-2025")))  # year is arbitrary, for sorting
dates_ordered <- flights %>%
  distinct(date, date_parsed) %>%
  arrange(date_parsed) %>%
  pull(date)
flights <- flights %>% select(-date_parsed)

islands_all <- sort(unique(c(flights$from, flights$to))) %>% setdiff(hubs)

# Build stay labels from consecutive dates
date_nums <- dmy(paste0(dates_ordered, "-2025"))
stay_labels <- map_chr(seq_along(dates_ordered), function(i) {
  d <- date_nums[i]
  if (i < length(dates_ordered)) {
    d_next <- date_nums[i + 1] - days(1)
    sprintf("%d–%d %s", day(d), day(d_next), month(d, label = TRUE))
  } else {
    sprintf("%d %s →", day(d), month(d, label = TRUE))
  }
})

# ── Same-day reachability: all (destination, n_flights, leg_list) from start ──
# DFS with no location revisits within a day.
day_reach <- function(date, start) {
  edges <- flights %>% filter(date == !!date)
  adj   <- split(edges$to, edges$from)

  results <- list()

  dfs <- function(loc, nf, legs, visited) {
    # Record current location as reachable
    results[[length(results) + 1]] <<- list(dest = loc, nf = nf, legs = legs)
    dests <- adj[[loc]]
    if (is.null(dests)) return()
    for (d in dests) {
      if (d %in% visited) next
      dfs(d, nf + 1L, c(legs, paste0(loc, "-", d)), c(visited, d))
    }
  }

  dfs(start, 0L, character(0), start)
  results
}

# ── Enumerate all valid itineraries ──────────────────────────────────────────
# For each (A, B, C) triple of distinct islands:
#   1. Can we reach A from PPT on 2-Sep?
#   2. Can we reach B from A on 6-Sep? (B ≠ A)
#   3. Can we reach C from B on 10-Sep? (C ≠ A, B)
#   4. Can we reach RFP from C on 14-Sep?
# For each feasible triple, record all route combos and their total flights.

itineraries <- list()
itin_id <- 0L

for (A in islands_all) {
  reach_A <- day_reach("2-Sep", "PPT")
  paths_A <- keep(reach_A, ~ .x$dest == A & .x$nf > 0)  # must move
  if (length(paths_A) == 0) next

  for (B in setdiff(islands_all, A)) {
    reach_B <- day_reach("6-Sep", A)
    paths_B <- keep(reach_B, ~ .x$dest == B & .x$nf > 0)
    if (length(paths_B) == 0) next

    for (C in setdiff(islands_all, c(A, B))) {
      reach_C <- day_reach("10-Sep", B)
      paths_C <- keep(reach_C, ~ .x$dest == C & .x$nf > 0)
      if (length(paths_C) == 0) next

      reach_R <- day_reach("14-Sep", C)
      paths_R <- keep(reach_R, ~ .x$dest == "RFP" & .x$nf > 0)
      if (length(paths_R) == 0) next

      # Enumerate all combinations of routes for this (A, B, C) triple
      for (pa in paths_A) {
        for (pb in paths_B) {
          for (pc in paths_C) {
            for (pr in paths_R) {
              itin_id <- itin_id + 1L
              total_nf <- pa$nf + pb$nf + pc$nf + pr$nf
              itineraries[[itin_id]] <- list(
                id       = itin_id,
                nf       = total_nf,
                A = A, B = B, C = C,
                islands  = paste(sort(c(A, B, C)), collapse = ", "),
                legs_A   = pa$legs,
                legs_B   = pb$legs,
                legs_C   = pc$legs,
                legs_R   = pr$legs
              )
            }
          }
        }
      }
    }
  }
}

if (length(itineraries) == 0) {
  cat("No valid itineraries found.\n")
  quit(status = 0)
}

# ── Build results data frames ────────────────────────────────────────────────
summary_df <- map_dfr(itineraries, function(it) {
  tibble(
    itin_id   = it$id,
    n_flights = it$nf,
    island_A  = it$A,
    island_B  = it$B,
    island_C  = it$C,
    islands   = it$islands,
    route_A   = paste(it$legs_A, collapse = " → "),
    route_B   = paste(it$legs_B, collapse = " → "),
    route_C   = paste(it$legs_C, collapse = " → "),
    route_R   = paste(it$legs_R, collapse = " → ")
  )
})

# Legs data frame for plots
legs_df <- map_dfr(itineraries, function(it) {
  bind_rows(
    tibble(itin_id = it$id, n_flights = it$nf, islands = it$islands,
           date = "2-Sep",  from = str_extract(it$legs_A, "^[^-]+"),
           to = str_extract(it$legs_A, "[^-]+$")),
    tibble(itin_id = it$id, n_flights = it$nf, islands = it$islands,
           date = "6-Sep",  from = str_extract(it$legs_B, "^[^-]+"),
           to = str_extract(it$legs_B, "[^-]+$")),
    tibble(itin_id = it$id, n_flights = it$nf, islands = it$islands,
           date = "10-Sep", from = str_extract(it$legs_C, "^[^-]+"),
           to = str_extract(it$legs_C, "[^-]+$")),
    tibble(itin_id = it$id, n_flights = it$nf, islands = it$islands,
           date = "14-Sep", from = str_extract(it$legs_R, "^[^-]+"),
           to = str_extract(it$legs_R, "[^-]+$"))
  )
})

# Stay summary
stay_df <- map_dfr(itineraries, function(it) {
  tibble(
    itin_id   = it$id,
    n_flights = it$nf,
    islands   = it$islands,
    date      = dates_ordered,
    stay      = stay_labels,
    flights   = c(
      paste(it$legs_A, collapse = " → "),
      paste(it$legs_B, collapse = " → "),
      paste(it$legs_C, collapse = " → "),
      paste(it$legs_R, collapse = " → ")
    ),
    location  = c(it$A, it$B, it$C, "RFP")
  )
})

# ── Pricing (loaded from CSV) ─────────────────────────────────────────────────
# prices.csv has origin→destination fares (connections bundled into one price).
prices <- read_csv("prices.csv", show_col_types = FALSE)

# For each optimal itinerary, the 4 legs are priced as:
#   date 1: PPT → A,  date 2: A → B,  date 3: B → C,  date 4: C → RFP
price_lookup <- function(date, from, to) {
  p <- prices %>% filter(date == !!date, from == !!from, to == !!to)
  if (nrow(p) == 1) p$price_aud else NA_real_
}

summary_df <- summary_df %>%
  rowwise() %>%
  mutate(
    price_1 = price_lookup(dates_ordered[1], "PPT",     island_A),
    price_2 = price_lookup(dates_ordered[2], island_A,  island_B),
    price_3 = price_lookup(dates_ordered[3], island_B,  island_C),
    price_4 = price_lookup(dates_ordered[4], island_C,  "RFP"),
    total_aud = price_1 + price_2 + price_3 + price_4
  ) %>%
  ungroup()

# ── Console output ───────────────────────────────────────────────────────────
has_prices <- !all(is.na(summary_df$total_aud))
n_total    <- nrow(summary_df)

cat(sprintf("Found %d valid itinerary(s) across all island combos.\n\n", n_total))

# For each unique island combo, find the cheapest itinerary
fmt_price <- function(p) if (!is.na(p)) sprintf("$%d", p) else "?"

best_per_combo <- summary_df %>%
  group_by(islands) %>%
  slice_min(total_aud, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(total_aud)

cat(strrep("=", 75), "\n")
cat(" CHEAPEST ITINERARY PER ISLAND COMBO (ranked by price)\n")
cat(strrep("=", 75), "\n\n")

for (i in seq_len(nrow(best_per_combo))) {
  r <- best_per_combo[i, ]
  price_tag <- if (!is.na(r$total_aud)) sprintf("$%s AUD", format(r$total_aud, big.mark = ",")) else "price TBD"
  cat(sprintf("  %d. [%s]  %s → %s → %s   %s  (%d flights)\n",
              i, r$islands, r$island_A, r$island_B, r$island_C, price_tag, r$n_flights))
  cat(sprintf("     %8s : %-28s %6s → stay %s\n", dates_ordered[1], r$route_A, fmt_price(r$price_1), r$island_A))
  cat(sprintf("     %8s : %-28s %6s → stay %s\n", dates_ordered[2], r$route_B, fmt_price(r$price_2), r$island_B))
  cat(sprintf("     %8s : %-28s %6s → stay %s\n", dates_ordered[3], r$route_C, fmt_price(r$price_3), r$island_C))
  cat(sprintf("     %8s : %-28s %6s → RFP\n",     dates_ordered[4], r$route_R, fmt_price(r$price_4)))
  cat("\n")
}

# ── Visualisations ───────────────────────────────────────────────────────────
coords <- tribble(
  ~loc,  ~lon,     ~lat,
  "PPT", -149.57, -17.53,
  "BOB", -151.75, -16.50,
  "HUH", -151.02, -16.69,
  "RGI", -147.70, -14.97,
  "TIH", -148.23, -15.12,
  "FAV", -145.71, -16.05,
  "RFP", -151.34, -16.72
)

# 1. Bar chart: itineraries by flight count
p1 <- summary_df %>%
  count(n_flights) %>%
  ggplot(aes(x = factor(n_flights), y = n, fill = factor(n_flights))) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.3, size = 4) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Valid itineraries by number of flights",
       x = "Total flights", y = "Count") +
  theme_minimal(base_size = 14)

ggsave("plot_flight_counts.png", p1, width = 7, height = 5, dpi = 150)
cat("Saved: plot_flight_counts.png\n")

# 2. Tile heatmap: cheapest itinerary per combo — overnight location per period
best_ids <- best_per_combo$itin_id
opt_stays <- stay_df %>%
  filter(itin_id %in% best_ids) %>%
  left_join(best_per_combo %>% select(itin_id, total_aud), by = "itin_id") %>%
  mutate(
    stay  = factor(stay, levels = stay_labels),
    label = sprintf("%s → %s → %s  ($%s)",
                    location[date == dates_ordered[1]],
                    location[date == dates_ordered[2]],
                    location[date == dates_ordered[3]],
                    if (!is.na(total_aud[1])) format(total_aud[1], big.mark = ",") else "TBD"),
    .by = itin_id
  )

p2 <- opt_stays %>%
  ggplot(aes(x = stay, y = reorder(label, -total_aud), fill = location)) +
  geom_tile(colour = "white", linewidth = 1.2) +
  geom_text(aes(label = location), size = 3.5, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Cheapest itinerary per island combo — overnight location by period",
    x = NULL, y = NULL, fill = "Location"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank(), axis.text.y = element_text(size = 9))

n_opt <- n_distinct(opt_stays$itin_id)
ggsave("plot_optimal_stays.png", p2, width = 10,
       height = max(4, 0.5 * n_opt + 2), dpi = 150)
cat("Saved: plot_optimal_stays.png\n")

# 3. Route maps: cheapest per combo, legs coloured by date
opt_legs <- legs_df %>%
  filter(itin_id %in% best_ids) %>%
  left_join(best_per_combo %>% select(itin_id, total_aud), by = "itin_id") %>%
  left_join(coords, by = c("from" = "loc")) %>%
  rename(lon_from = lon, lat_from = lat) %>%
  left_join(coords, by = c("to" = "loc")) %>%
  rename(lon_to = lon, lat_to = lat) %>%
  mutate(
    date  = factor(date, levels = dates_ordered),
    label = sprintf("%s  ($%s)",
                    islands,
                    if_else(!is.na(total_aud), format(total_aud, big.mark = ","), "TBD"))
  )

n_facets <- n_distinct(opt_legs$itin_id)

p3 <- ggplot() +
  geom_segment(
    data = opt_legs,
    aes(x = lon_from, y = lat_from, xend = lon_to, yend = lat_to, colour = date),
    arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
    linewidth = 0.9, alpha = 0.8
  ) +
  geom_point(data = coords, aes(x = lon, y = lat), size = 4, colour = "grey30") +
  geom_label(data = coords, aes(x = lon, y = lat, label = loc),
             size = 3, nudge_y = 0.18, linewidth = 0, fill = "white", alpha = 0.8) +
  facet_wrap(~ label, ncol = 3) +
  scale_colour_brewer(palette = "Dark2", name = "Flight date") +
  labs(title = "Cheapest route per island combo",
       x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("plot_routes.png", p3, width = 14,
       height = max(5, 4 * ceiling(n_facets / 3)), dpi = 150)
cat("Saved: plot_routes.png\n")

# 4. All itineraries heatmap (if manageable)
if (n_total <= 80) {
  all_stays <- stay_df %>%
    mutate(
      stay  = factor(stay, levels = stay_labels),
      label = sprintf("#%d (%df): %s → %s → %s", itin_id, n_flights,
                      location[date == "2-Sep"], location[date == "6-Sep"],
                      location[date == "10-Sep"]),
      .by = itin_id
    )

  p4 <- all_stays %>%
    ggplot(aes(x = stay, y = reorder(label, -itin_id), fill = location)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(aes(label = location), size = 2.5, fontface = "bold") +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "All valid itineraries — overnight location by period",
      x = NULL, y = NULL, fill = "Location"
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(), axis.text.y = element_text(size = 6))

  ggsave("plot_all_stays.png", p4, width = 10,
         height = max(5, 0.35 * n_total + 2), dpi = 150)
  cat("Saved: plot_all_stays.png\n")
}

# 5. Price comparison bar chart — cheapest per island combo
if (has_prices) {
  price_data <- best_per_combo %>%
    filter(!is.na(total_aud)) %>%
    select(itin_id, islands, island_A, island_B, island_C, n_flights,
           price_1, price_2, price_3, price_4, total_aud) %>%
    mutate(label = sprintf("%s → %s → %s\n[%s] %df", island_A, island_B, island_C, islands, n_flights)) %>%
    pivot_longer(cols = starts_with("price_"),
                 names_to = "leg", values_to = "price") %>%
    mutate(leg = recode(leg,
      price_1 = dates_ordered[1],
      price_2 = dates_ordered[2],
      price_3 = dates_ordered[3],
      price_4 = dates_ordered[4]
    )) %>%
    mutate(leg = factor(leg, levels = dates_ordered))

  totals <- best_per_combo %>%
    filter(!is.na(total_aud)) %>%
    mutate(label = sprintf("%s → %s → %s\n[%s] %df", island_A, island_B, island_C, islands, n_flights))

  p5 <- price_data %>%
    ggplot(aes(x = reorder(label, total_aud), y = price, fill = leg)) +
    geom_col() +
    geom_text(
      data = totals,
      aes(x = reorder(label, total_aud), y = total_aud,
          label = sprintf("$%s", format(total_aud, big.mark = ","))),
      inherit.aes = FALSE, hjust = -0.1, size = 3.5, fontface = "bold"
    ) +
    coord_flip() +
    scale_fill_brewer(palette = "Dark2", name = "Flight date") +
    scale_y_continuous(labels = scales::dollar_format(prefix = "$"),
                       expand = expansion(mult = c(0, 0.2))) +
    labs(
      title = "Cheapest itinerary per island combo (AUD)",
      subtitle = sprintf("Prices from %s",
                         format(max(prices$snapshot_date, na.rm = TRUE))),
      x = NULL, y = "Total flight cost (AUD)"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")

  n_combos <- nrow(totals)
  ggsave("plot_prices.png", p5, width = 10, height = max(4, 0.6 * n_combos + 1.5), dpi = 150)
  cat("Saved: plot_prices.png\n")
}

# ── Missing prices report ────────────────────────────────────────────────────
# For every valid itinerary, check which OD fares are missing from prices.csv
needed_od <- summary_df %>%
  rowwise() %>%
  reframe(
    date = dates_ordered,
    from = c("PPT",    island_A, island_B, island_C),
    to   = c(island_A, island_B, island_C, "RFP")
  ) %>%
  distinct()

missing_od <- needed_od %>%
  anti_join(prices, by = c("date", "from", "to"))

if (nrow(missing_od) > 0) {
  missing_od <- missing_od %>% arrange(date, from, to)
  cat(sprintf("\n%s\n", strrep("=", 75)))
  cat(sprintf(" %d MISSING PRICES in prices.csv\n", nrow(missing_od)))
  cat(sprintf("%s\n\n", strrep("=", 75)))
  cat("  date,from,to,price_aud,snapshot_date\n")
  for (i in seq_len(nrow(missing_od))) {
    cat(sprintf("  %s,%s,%s,,\n", missing_od$date[i], missing_od$from[i], missing_od$to[i]))
  }
  cat("\n")
} else {
  cat("\nAll OD pairs are priced.\n")
}

cat("\nDone.\n")
