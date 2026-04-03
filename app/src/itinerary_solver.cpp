#include "itinerary_solver.h"
#include <algorithm>

static FlightData* g_data;
static const Constraints* g_cons;
static std::vector<Itinerary>* g_results;

static int get_cheapest(const std::string& from, const std::string& to, int date_idx) {
    std::string key = from + ":" + to + ":" + std::to_string(date_idx);
    auto it = g_data->cheapest.find(key);
    return (it != g_data->cheapest.end()) ? it->second : -1;
}

static void dfs(const std::string& loc, int arrive_idx, uint32_t visited_mask,
                std::vector<Leg>& legs) {
    int isl_idx = island_index(loc);

    // Minimum nights at current island (at least 1 always)
    int min_nights = 1;
    if (isl_idx >= 0 && g_cons->min_nights[isl_idx] > 1)
        min_nights = g_cons->min_nights[isl_idx];

    // Try completing: fly to RFP on Sep 14 (date_index 12)
    int nights_here = 12 - arrive_idx;
    if (nights_here >= min_nights) {
        int fi = get_cheapest(loc, "RFP", 12);
        if (fi >= 0) {
            // Check all required islands are visited
            bool all_required = true;
            for (int i = 0; i < ISL_COUNT; i++) {
                if (g_cons->required_islands[i] && !(visited_mask & (1u << i)))
                    all_required = false;
            }
            if (all_required) {
                auto& f = g_data->flights[fi];
                Itinerary it;
                it.legs = legs;
                it.legs.push_back({fi, 12, loc, "RFP", f.pp_aud, f.total_aud});
                it.n_flights = (int)it.legs.size();
                it.total_pp = 0;
                it.total_both = 0;
                for (auto& l : it.legs) {
                    it.total_pp += l.pp_aud;
                    it.total_both += l.total_aud;
                    if (l.to != "RFP") it.island_sequence.push_back(l.to);
                }
                g_results->push_back(std::move(it));
            }
        }
    }

    // Try extending: fly to another island
    for (int di = arrive_idx + 1; di <= 11; di++) {
        int nh = di - arrive_idx;
        if (nh < min_nights) continue;

        for (int isl = 0; isl < ISL_COUNT; isl++) {
            if (visited_mask & (1u << isl)) continue;
            int fi = get_cheapest(loc, ISLANDS[isl].code, di);
            if (fi < 0) continue;
            auto& f = g_data->flights[fi];
            legs.push_back({fi, di, loc, ISLANDS[isl].code, f.pp_aud, f.total_aud});
            dfs(ISLANDS[isl].code, di, visited_mask | (1u << isl), legs);
            legs.pop_back();
        }
    }
}

std::vector<Itinerary> solve(FlightData& data, const Constraints& c) {
    g_data = &data;
    g_cons = &c;
    std::vector<Itinerary> results;
    g_results = &results;

    rebuild_cheapest(data, c);

    std::vector<Leg> legs;
    // Start: fly from PPT on Sep 2 (date_index 0)
    for (int isl = 0; isl < ISL_COUNT; isl++) {
        int fi = get_cheapest("PPT", ISLANDS[isl].code, 0);
        if (fi < 0) continue;
        auto& f = data.flights[fi];
        legs.push_back({fi, 0, "PPT", ISLANDS[isl].code, f.pp_aud, f.total_aud});
        dfs(ISLANDS[isl].code, 0, 1u << isl, legs);
        legs.pop_back();
    }

    std::sort(results.begin(), results.end(),
              [](const Itinerary& a, const Itinerary& b) {
                  return a.total_pp < b.total_pp;
              });

    return results;
}
