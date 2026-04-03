#include "csv_loader.h"
#include <fstream>
#include <sstream>
#include <algorithm>
#include <cstdio>

// Parse a single CSV line respecting quoted fields
static std::vector<std::string> parse_csv_line(const std::string& line) {
    std::vector<std::string> fields;
    std::string field;
    bool in_quotes = false;
    for (size_t i = 0; i < line.size(); i++) {
        char c = line[i];
        if (c == '"') {
            in_quotes = !in_quotes;
        } else if (c == ',' && !in_quotes) {
            fields.push_back(field);
            field.clear();
        } else if (c == '\r') {
            // skip
        } else {
            field += c;
        }
    }
    fields.push_back(field);
    return fields;
}

// "2026-09-02" -> date_index 0, "2026-09-14" -> 12
static int parse_date_index(const std::string& date_str) {
    int y, m, d;
    if (sscanf(date_str.c_str(), "%d-%d-%d", &y, &m, &d) == 3) {
        if (m == 9 && d >= 2 && d <= 14) return d - 2;
    }
    return -1;
}

static bool carrier_allowed(const std::string& carrier, const Constraints& c) {
    // Carrier can be "Air Moana, Air Tahiti" — check each sub-carrier
    auto contains = [&](const char* name) {
        return carrier.find(name) != std::string::npos;
    };
    bool has_tahiti = contains("Air Tahiti");
    bool has_moana = contains("Air Moana");
    bool has_hahn = contains("Hahn Air");

    // Allow if ALL carriers in the string are allowed
    if (has_tahiti && !c.carrier_air_tahiti) return false;
    if (has_moana && !c.carrier_air_moana) return false;
    if (has_hahn && !c.carrier_hahn_air) return false;
    return true;
}

FlightData load_flights(const std::string& csv_path) {
    FlightData data;
    std::ifstream file(csv_path);
    if (!file.is_open()) return data;

    std::string line;
    std::getline(file, line); // skip header

    while (std::getline(file, line)) {
        auto fields = parse_csv_line(line);
        if (fields.size() < 12) continue;

        Flight f;
        f.from = fields[0];
        f.to = fields[1];
        f.date_index = parse_date_index(fields[2]);
        if (f.date_index < 0) continue;
        f.dep = fields[3];
        f.arr = fields[4];
        f.carrier = fields[5];
        f.duration_mins = std::atoi(fields[6].c_str());
        f.stops = std::atoi(fields[7].c_str());
        f.stop_codes = fields[8];
        f.layover = fields[9];
        f.pp_aud = std::strtof(fields[10].c_str(), nullptr);
        f.total_aud = std::strtof(fields[11].c_str(), nullptr);

        // Track unique carriers (split compound carriers)
        for (auto& part : {"Air Tahiti", "Air Moana", "Hahn Air Technologies"}) {
            if (f.carrier.find(part) != std::string::npos)
                data.all_carriers.insert(part);
        }

        data.flights.push_back(f);
    }

    // Build from_on_date index
    for (int i = 0; i < (int)data.flights.size(); i++) {
        auto& f = data.flights[i];
        std::string key = f.from + ":" + std::to_string(f.date_index);
        data.from_on_date[key].push_back(i);
    }

    printf("Loaded %zu flights from %s\n", data.flights.size(), csv_path.c_str());
    fflush(stdout);
    return data;
}

void rebuild_cheapest(FlightData& data, const Constraints& c) {
    data.cheapest.clear();
    for (int i = 0; i < (int)data.flights.size(); i++) {
        auto& f = data.flights[i];

        // Apply filters
        if (c.max_stops >= 0 && f.stops > c.max_stops) continue;
        if (!carrier_allowed(f.carrier, c)) continue;

        std::string key = f.from + ":" + f.to + ":" + std::to_string(f.date_index);
        auto it = data.cheapest.find(key);
        if (it == data.cheapest.end() || f.pp_aud < data.flights[it->second].pp_aud) {
            data.cheapest[key] = i;
        }
    }
}
