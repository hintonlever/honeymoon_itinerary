#include "map_panel.h"
#include "imgui.h"
#include <cmath>
#include <cstdio>

// Map bounds (lon/lat of all islands with padding)
static constexpr float LON_MIN = -152.8f, LON_MAX = -145.0f;
static constexpr float LAT_MIN = -17.8f,  LAT_MAX = -14.5f;

// Leg colors
static ImU32 leg_colors[] = {
    IM_COL32(46, 204, 113, 255),   // green
    IM_COL32(52, 152, 219, 255),   // blue
    IM_COL32(231, 76, 60, 255),    // red
    IM_COL32(241, 196, 15, 255),   // yellow
    IM_COL32(155, 89, 182, 255),   // purple
    IM_COL32(230, 126, 34, 255),   // orange
    IM_COL32(26, 188, 156, 255),   // teal
};

static ImVec2 geo_to_screen(float lon, float lat, ImVec2 origin, ImVec2 size) {
    float nx = (lon - LON_MIN) / (LON_MAX - LON_MIN);
    float ny = (lat - LAT_MAX) / (LAT_MIN - LAT_MAX); // flip Y: north up
    float pad = 40.0f;
    return ImVec2(origin.x + pad + nx * (size.x - 2*pad),
                  origin.y + pad + ny * (size.y - 2*pad));
}

static void draw_arrow(ImDrawList* dl, ImVec2 from, ImVec2 to, ImU32 col, float thickness) {
    // Shorten slightly so arrow doesn't overlap island dot
    float dx = to.x - from.x, dy = to.y - from.y;
    float len = sqrtf(dx*dx + dy*dy);
    if (len < 1.0f) return;
    float ux = dx/len, uy = dy/len;
    ImVec2 p1 = {from.x + ux*12, from.y + uy*12};
    ImVec2 p2 = {to.x - ux*12, to.y - uy*12};

    dl->AddLine(p1, p2, col, thickness);

    // Arrowhead
    float ax = 10.0f;
    ImVec2 tip = p2;
    ImVec2 left = {tip.x - ux*ax - uy*ax*0.4f, tip.y - uy*ax + ux*ax*0.4f};
    ImVec2 right = {tip.x - ux*ax + uy*ax*0.4f, tip.y - uy*ax - ux*ax*0.4f};
    dl->AddTriangleFilled(tip, left, right, col);
}

void draw_map_panel(const std::vector<Itinerary>& itineraries, int selected_idx) {
    ImGui::Begin("Map");

    ImVec2 canvas_pos = ImGui::GetCursorScreenPos();
    ImVec2 canvas_size = ImGui::GetContentRegionAvail();
    if (canvas_size.x < 50 || canvas_size.y < 50) { ImGui::End(); return; }

    ImDrawList* dl = ImGui::GetWindowDrawList();

    // Background
    dl->AddRectFilled(canvas_pos,
        ImVec2(canvas_pos.x + canvas_size.x, canvas_pos.y + canvas_size.y),
        IM_COL32(20, 30, 48, 255));

    // Ocean grid lines (subtle)
    for (float lon = -152; lon <= -145; lon += 1.0f) {
        ImVec2 top = geo_to_screen(lon, LAT_MAX, canvas_pos, canvas_size);
        ImVec2 bot = geo_to_screen(lon, LAT_MIN, canvas_pos, canvas_size);
        dl->AddLine(top, bot, IM_COL32(40, 55, 75, 255), 1.0f);
    }
    for (float lat = -18; lat <= -14; lat += 0.5f) {
        ImVec2 left = geo_to_screen(LON_MIN, lat, canvas_pos, canvas_size);
        ImVec2 right = geo_to_screen(LON_MAX, lat, canvas_pos, canvas_size);
        dl->AddLine(left, right, IM_COL32(40, 55, 75, 255), 1.0f);
    }

    // Draw selected route first (so it's behind island dots)
    if (selected_idx >= 0 && selected_idx < (int)itineraries.size()) {
        auto& it = itineraries[selected_idx];
        for (size_t i = 0; i < it.legs.size(); i++) {
            auto& leg = it.legs[i];
            // Find coordinates
            float lon1=0, lat1=0, lon2=0, lat2=0;
            for (int j = 0; j < NUM_LOCATIONS; j++) {
                if (ALL_LOCATIONS[j].code == leg.from) { lon1 = ALL_LOCATIONS[j].lon; lat1 = ALL_LOCATIONS[j].lat; }
                if (ALL_LOCATIONS[j].code == leg.to)   { lon2 = ALL_LOCATIONS[j].lon; lat2 = ALL_LOCATIONS[j].lat; }
            }
            ImVec2 p1 = geo_to_screen(lon1, lat1, canvas_pos, canvas_size);
            ImVec2 p2 = geo_to_screen(lon2, lat2, canvas_pos, canvas_size);

            ImU32 col = leg_colors[i % 7];
            draw_arrow(dl, p1, p2, col, 3.0f);

            // Leg label at midpoint
            ImVec2 mid = {(p1.x+p2.x)*0.5f, (p1.y+p2.y)*0.5f - 12};
            char buf[32];
            snprintf(buf, sizeof(buf), "%s $%.0f", DATE_LABELS[leg.date_index], leg.pp_aud);
            dl->AddText(mid, col, buf);
        }
    }

    // Draw islands
    for (int i = 0; i < NUM_LOCATIONS; i++) {
        auto& loc = ALL_LOCATIONS[i];
        ImVec2 pos = geo_to_screen(loc.lon, loc.lat, canvas_pos, canvas_size);

        bool is_hub = (loc.code == "PPT" || loc.code == "RFP");
        float radius = is_hub ? 8.0f : 10.0f;
        ImU32 fill = is_hub ? IM_COL32(180, 180, 180, 255) : IM_COL32(255, 255, 255, 255);
        ImU32 outline = IM_COL32(60, 60, 60, 255);

        dl->AddCircleFilled(pos, radius, fill);
        dl->AddCircle(pos, radius, outline, 0, 2.0f);

        // Label
        ImVec2 text_pos = {pos.x - 10, pos.y - radius - 16};
        dl->AddText(text_pos, IM_COL32(220, 220, 220, 255), loc.code.c_str());

        // Tooltip on hover
        ImVec2 mouse = ImGui::GetMousePos();
        float dx = mouse.x - pos.x, dy = mouse.y - pos.y;
        if (dx*dx + dy*dy < radius*radius*4 && selected_idx >= 0 &&
            selected_idx < (int)itineraries.size()) {
            auto& it = itineraries[selected_idx];
            // Find stay info
            for (auto& leg : it.legs) {
                if (leg.to == loc.code) {
                    ImGui::BeginTooltip();
                    ImGui::Text("%s", loc.code.c_str());
                    ImGui::Text("Arrive: %s ($%.0f pp)", DATE_LABELS[leg.date_index], leg.pp_aud);
                    ImGui::EndTooltip();
                }
            }
        }
    }

    // Legend
    if (selected_idx >= 0 && selected_idx < (int)itineraries.size()) {
        auto& it = itineraries[selected_idx];
        ImVec2 legend_pos = {canvas_pos.x + 10, canvas_pos.y + canvas_size.y - 20};
        char buf[128];
        snprintf(buf, sizeof(buf), "Selected: #%d  $%.0f pp  $%.0f total",
                 selected_idx + 1, it.total_pp, it.total_both);
        dl->AddText(legend_pos, IM_COL32(200, 200, 200, 255), buf);
    }

    // Consume the space
    ImGui::Dummy(canvas_size);
    ImGui::End();
}
