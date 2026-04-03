#pragma once
#include "types.h"
#include "csv_loader.h"
#include <vector>

std::vector<Itinerary> solve(FlightData& data, const Constraints& c);
