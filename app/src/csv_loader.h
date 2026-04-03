#pragma once
#include "types.h"
#include <vector>
#include <string>
#include <unordered_map>

struct FlightData {
    std::vector<Flight> flights;
    // Lookup: cheapest flight index for (from, to, date_index), or -1
    // Key: "FROM:TO:DATE_IDX"
    std::unordered_map<std::string, int> cheapest;

    // All flights from a given location on a given date
    // Key: "FROM:DATE_IDX"
    std::unordered_map<std::string, std::vector<int>> from_on_date;

    std::set<std::string> all_carriers;
};

FlightData load_flights(const std::string& csv_path);

// Build cheapest lookup applying filters
void rebuild_cheapest(FlightData& data, const Constraints& c);
