#include "gantt_panel.h"
#include "imgui.h"
#include <cstdio>
#include <cmath>
#include <algorithm>

static ImU32 island_colors[] = {
    IM_COL32(46, 204, 113, 200),   // BOB - green
    IM_COL32(52, 152, 219, 200),   // FAV - blue
    IM_COL32(231, 76, 60, 200),    // HUH - red
    IM_COL32(241, 196, 15, 200),   // MAU - yellow
    IM_COL32(155, 89, 182, 200),   // RGI - purple
    IM_COL32(230, 126, 34, 200),   // TIH - orange
};

static ImU32 color_for_island(const std::string& code) {
    int idx = island_index(code);
    if (idx >= 0 && idx < ISL_COUNT) return island_colors[idx];
    if (code == "PPT") return IM_COL32(120, 120, 120, 200);
    if (code == "RFP") return IM_COL32(180, 180, 180, 200);
    return IM_COL32(100, 100, 100, 200);
}

void draw_gantt_panel(const std::vector<Itinerary>& itineraries, int selected_idx) {
    ImGui::Begin("Timeline");

    if (itineraries.empty()) {
        ImGui::Text("No itineraries to display.");
        ImGui::End();
        return;
    }

    ImVec2 avail = ImGui::GetContentRegionAvail();
    if (avail.x < 100 || avail.y < 50) { ImGui::End(); return; }

    // Layout: NUM_DAYS columns for labels + 1 extra slot for the final block edge
    float left_margin = std::min(160.0f, avail.x * 0.15f);
    float right_pad = 5.0f;
    float chart_width = avail.x - left_margin - right_pad;
    float day_width = chart_width / (NUM_DAYS + 1); // +1 so blocks ending at day 13 still fit

    // Row sizing: fit as many as we can, up to 25
    float top_margin = 22.0f;
    float legend_height = 24.0f;
    float row_height = 26.0f;
    int max_rows = std::max(1, (int)((avail.y - top_margin - legend_height) / row_height));
    int show_count = std::min((int)itineraries.size(), std::min(max_rows, 25));

    ImVec2 origin = ImGui::GetCursorScreenPos();
    ImDrawList* dl = ImGui::GetWindowDrawList();

    // Date header
    for (int d = 0; d < NUM_DAYS; d++) {
        float x = origin.x + left_margin + d * day_width;
        char buf[8];
        snprintf(buf, sizeof(buf), "%d", d + 2);
        dl->AddText(ImVec2(x + day_width * 0.3f, origin.y), IM_COL32(180,180,180,255), buf);
    }

    // Rows
    for (int r = 0; r < show_count; r++) {
        auto& it = itineraries[r];
        float row_y = origin.y + top_margin + r * row_height;

        // Highlight selected
        if (r == selected_idx) {
            dl->AddRectFilled(
                ImVec2(origin.x, row_y),
                ImVec2(origin.x + avail.x, row_y + row_height),
                IM_COL32(255, 255, 255, 40));
        }

        // Vertical grid lines
        for (int d = 0; d <= NUM_DAYS; d++) {
            float x = origin.x + left_margin + d * day_width;
            dl->AddLine(ImVec2(x, row_y), ImVec2(x, row_y + row_height),
                        IM_COL32(50, 50, 60, 255), 1.0f);
        }

        // Row label: compact
        char label[64];
        snprintf(label, sizeof(label), "#%d $%.0f ", r+1, it.total_pp);
        std::string lbl = label;
        for (size_t i = 0; i < it.island_sequence.size(); i++) {
            if (i > 0) lbl += ">";
            lbl += it.island_sequence[i];
        }
        // Clip label text to left_margin
        dl->PushClipRect(ImVec2(origin.x, row_y),
                         ImVec2(origin.x + left_margin - 4, row_y + row_height), true);
        dl->AddText(ImVec2(origin.x + 4, row_y + 5),
                    IM_COL32(200, 200, 200, 255), lbl.c_str());
        dl->PopClipRect();

        // Stay blocks
        for (size_t i = 0; i < it.legs.size(); i++) {
            auto& leg = it.legs[i];
            int start_day = leg.date_index;
            int end_day;
            if (i + 1 < it.legs.size()) {
                end_day = it.legs[i+1].date_index;
            } else {
                end_day = NUM_DAYS - 1;
            }
            if (leg.to == "RFP" && i == it.legs.size() - 1) {
                end_day = std::min(start_day + 1, (int)NUM_DAYS);
            }

            float x1 = origin.x + left_margin + start_day * day_width + 1;
            float x2 = origin.x + left_margin + end_day * day_width - 1;
            float y1 = row_y + 3;
            float y2 = row_y + row_height - 3;

            ImU32 col = color_for_island(leg.to);
            dl->AddRectFilled(ImVec2(x1, y1), ImVec2(x2, y2), col, 4.0f);

            float block_width = x2 - x1;
            if (block_width > 25) {
                ImVec2 text_size = ImGui::CalcTextSize(leg.to.c_str());
                float tx = x1 + (block_width - text_size.x) * 0.5f;
                float ty = y1 + (row_height - 6 - text_size.y) * 0.5f;
                dl->AddText(ImVec2(tx, ty), IM_COL32(0,0,0,220), leg.to.c_str());
            }

            // Flight marker
            dl->AddTriangleFilled(
                ImVec2(x1, y1), ImVec2(x1+5, y1+3.5f), ImVec2(x1, y1+7),
                IM_COL32(255,255,255,180));
        }
    }

    // Legend at bottom
    float legend_y = origin.y + top_margin + show_count * row_height + 6;
    float lx = origin.x + left_margin;
    for (int i = 0; i < ISL_COUNT; i++) {
        dl->AddRectFilled(ImVec2(lx, legend_y), ImVec2(lx+12, legend_y+12),
                          island_colors[i], 3.0f);
        dl->AddText(ImVec2(lx+16, legend_y - 1), IM_COL32(200,200,200,255),
                    ISLANDS[i].code.c_str());
        lx += 60;
    }

    ImGui::Dummy(avail);
    ImGui::End();
}
