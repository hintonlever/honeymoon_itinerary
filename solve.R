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

# ── Console output ───────────────────────────────────────────────────────────
min_flights <- min(summary_df$n_flights)
n_total     <- nrow(summary_df)
optimal     <- summary_df %>% filter(n_flights == min_flights)

cat(sprintf("Found %d valid itinerary(s) visiting exactly 3 islands.\n", n_total))
cat(sprintf("Minimum flights: %d\n\n", min_flights))

cat(strrep("=", 65), "\n")
cat(sprintf(" %d OPTIMAL ITINERARY(S) — %d flights\n", nrow(optimal), min_flights))
cat(strrep("=", 65), "\n\n")

for (i in seq_len(nrow(optimal))) {
  r <- optimal[i, ]
  cat(sprintf("  #%-3d  Stay: %s → %s → %s   [%s]\n",
              r$itin_id, r$island_A, r$island_B, r$island_C, r$islands))
  cat(sprintf("         2-Sep : %-35s → stay %s\n", r$route_A, r$island_A))
  cat(sprintf("         6-Sep : %-35s → stay %s\n", r$route_B, r$island_B))
  cat(sprintf("        10-Sep : %-35s → stay %s\n", r$route_C, r$island_C))
  cat(sprintf("        14-Sep : %-35s → RFP\n", r$route_R))
  cat("\n")
}

# Summary by flight count
counts <- summary_df %>% count(n_flights, name = "n_itins")
if (nrow(counts) > 1) {
  cat(strrep("=", 65), "\n")
  cat(" ALL VALID — by flight count\n")
  cat(strrep("=", 65), "\n")
  for (i in seq_len(nrow(counts))) {
    tag <- if (counts$n_flights[i] == min_flights) "  ← min" else ""
    cat(sprintf("  %d flights : %d itinerary(s)%s\n", counts$n_flights[i], counts$n_itins[i], tag))
  }
  cat("\n")
}

# Non-optimal
non_opt <- summary_df %>% filter(n_flights > min_flights) %>% arrange(n_flights)
if (nrow(non_opt) > 0) {
  cat(strrep("=", 65), "\n")
  cat(" OTHER VALID ITINERARIES\n")
  cat(strrep("=", 65), "\n\n")
  for (i in seq_len(nrow(non_opt))) {
    r <- non_opt[i, ]
    cat(sprintf("  #%-3d  (%d flights)  Stay: %s → %s → %s\n",
                r$itin_id, r$n_flights, r$island_A, r$island_B, r$island_C))
    cat(sprintf("         2-Sep : %-35s → stay %s\n", r$route_A, r$island_A))
    cat(sprintf("         6-Sep : %-35s → stay %s\n", r$route_B, r$island_B))
    cat(sprintf("        10-Sep : %-35s → stay %s\n", r$route_C, r$island_C))
    cat(sprintf("        14-Sep : %-35s → RFP\n", r$route_R))
    cat("\n")
  }
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

# 2. Tile heatmap: overnight location per period (optimal only)
opt_stays <- stay_df %>%
  filter(n_flights == min_flights) %>%
  mutate(
    stay  = factor(stay, levels = stay_labels),
    label = sprintf("#%d: %s → %s → %s", itin_id,
                    location[date == "2-Sep"], location[date == "6-Sep"],
                    location[date == "10-Sep"]),
    .by = itin_id
  )

p2 <- opt_stays %>%
  ggplot(aes(x = stay, y = reorder(label, -itin_id), fill = location)) +
  geom_tile(colour = "white", linewidth = 1.2) +
  geom_text(aes(label = location), size = 3.5, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = sprintf("Optimal itineraries (%d flights) — overnight location by period", min_flights),
    x = NULL, y = NULL, fill = "Location"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank(), axis.text.y = element_text(size = 9))

n_opt <- n_distinct(opt_stays$itin_id)
ggsave("plot_optimal_stays.png", p2, width = 10,
       height = max(4, 0.5 * n_opt + 2), dpi = 150)
cat("Saved: plot_optimal_stays.png\n")

# 3. Route maps: one facet per optimal itinerary, legs coloured by date
opt_legs <- legs_df %>%
  filter(n_flights == min_flights) %>%
  left_join(coords, by = c("from" = "loc")) %>%
  rename(lon_from = lon, lat_from = lat) %>%
  left_join(coords, by = c("to" = "loc")) %>%
  rename(lon_to = lon, lat_to = lat)

# Deduplicate visually identical routes (same sequence of legs)
opt_legs <- opt_legs %>%
  group_by(itin_id) %>%
  mutate(route_key = paste(from, to, sep = "-", collapse = "|")) %>%
  ungroup()

unique_routes <- opt_legs %>%
  distinct(route_key, .keep_all = FALSE) %>%
  pull(route_key)

# Keep first itin_id per unique route
rep_ids <- opt_legs %>%
  distinct(itin_id, route_key) %>%
  group_by(route_key) %>%
  slice_min(itin_id, n = 1) %>%
  pull(itin_id)

route_legs <- opt_legs %>%
  filter(itin_id %in% rep_ids) %>%
  mutate(
    date  = factor(date, levels = dates_ordered),
    label = paste0("#", itin_id, ": ", islands)
  )

n_facets <- n_distinct(route_legs$itin_id)

p3 <- ggplot() +
  geom_segment(
    data = route_legs,
    aes(x = lon_from, y = lat_from, xend = lon_to, yend = lat_to, colour = date),
    arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
    linewidth = 0.9, alpha = 0.8
  ) +
  geom_point(data = coords, aes(x = lon, y = lat), size = 4, colour = "grey30") +
  geom_label(data = coords, aes(x = lon, y = lat, label = loc),
             size = 3, nudge_y = 0.18, linewidth = 0, fill = "white", alpha = 0.8) +
  facet_wrap(~ label, ncol = 3) +
  scale_colour_brewer(palette = "Dark2", name = "Flight date") +
  labs(title = sprintf("Optimal routes (%d flights) — unique flight sequences", min_flights),
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

cat("\nDone.\n")
