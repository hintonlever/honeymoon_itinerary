#pragma once
#include <string>
#include <vector>
#include <set>
#include <map>
#include <cmath>

// Date index: 0 = Sep 2, 1 = Sep 3, ..., 12 = Sep 14
constexpr int NUM_DAYS = 13;
inline const char* DATE_LABELS[] = {
    "Sep 2", "Sep 3", "Sep 4", "Sep 5", "Sep 6", "Sep 7", "Sep 8",
    "Sep 9", "Sep 10", "Sep 11", "Sep 12", "Sep 13", "Sep 14"
};

struct Flight {
    std::string from, to;
    int date_index;       // 0..12
    std::string dep, arr;
    std::string carrier;
    int duration_mins;
    int stops;
    std::string stop_codes;
    std::string layover;
    float pp_aud;
    float total_aud;
};

struct Leg {
    int flight_index;
    int date_index;
    std::string from, to;
    float pp_aud;
    float total_aud;
};

struct Itinerary {
    std::vector<Leg> legs;
    float total_pp = 0;
    float total_both = 0;
    std::vector<std::string> island_sequence; // intermediate islands (excl PPT/RFP)
    int n_flights = 0;
};

struct Constraints {
    bool required_islands[6] = {};   // indexed by island enum
    int  min_nights[6] = {};         // per island, 0 = no minimum (default 1 enforced)
    int  max_stops = -1;             // -1 = any, 0 = direct, 1, 2, ...
    bool carrier_air_tahiti = true;
    bool carrier_air_moana = true;
    bool carrier_hahn_air = true;
};

struct Island {
    std::string code;
    float lon, lat;
};

// Island indices (for the 6 visitiable islands)
enum IslandIdx { ISL_BOB=0, ISL_FAV, ISL_HUH, ISL_MAU, ISL_RGI, ISL_TIH, ISL_COUNT };

inline const Island ISLANDS[] = {
    {"BOB", -151.75f, -16.50f},
    {"FAV", -145.71f, -16.05f},
    {"HUH", -151.02f, -16.69f},
    {"MAU", -152.27f, -16.43f},
    {"RGI", -147.70f, -14.97f},
    {"TIH", -148.23f, -15.12f},
};

inline const Island HUB_PPT = {"PPT", -149.57f, -17.53f};
inline const Island HUB_RFP = {"RFP", -151.34f, -16.72f};

inline int island_index(const std::string& code) {
    for (int i = 0; i < ISL_COUNT; i++)
        if (ISLANDS[i].code == code) return i;
    return -1;
}

// All locations for the map
inline const Island ALL_LOCATIONS[] = {
    {"PPT", -149.57f, -17.53f},
    {"BOB", -151.75f, -16.50f},
    {"FAV", -145.71f, -16.05f},
    {"HUH", -151.02f, -16.69f},
    {"MAU", -152.27f, -16.43f},
    {"RGI", -147.70f, -14.97f},
    {"TIH", -148.23f, -15.12f},
    {"RFP", -151.34f, -16.72f},
};
constexpr int NUM_LOCATIONS = 8;
