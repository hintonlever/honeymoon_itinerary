#include "table_panel.h"
#include "imgui.h"
#include <cstdio>
#include <algorithm>

void draw_table_panel(const std::vector<Itinerary>& itineraries, int& selected_idx) {
    ImGui::Begin("Itineraries");

    ImGui::Text("%d itineraries found", (int)itineraries.size());
    ImGui::Separator();

    if (itineraries.empty()) {
        ImGui::TextColored(ImVec4(1,0.4f,0.4f,1), "No itineraries match your filters.");
        ImGui::End();
        return;
    }

    ImGuiTableFlags flags = ImGuiTableFlags_ScrollY | ImGuiTableFlags_RowBg |
                            ImGuiTableFlags_BordersOuter | ImGuiTableFlags_BordersV |
                            ImGuiTableFlags_Resizable | ImGuiTableFlags_Reorderable |
                            ImGuiTableFlags_Hideable;

    if (ImGui::BeginTable("itin_table", 5, flags, ImVec2(0, 0))) {
        ImGui::TableSetupScrollFreeze(0, 1);
        ImGui::TableSetupColumn("#", ImGuiTableColumnFlags_WidthFixed, 40);
        ImGui::TableSetupColumn("Islands", ImGuiTableColumnFlags_WidthStretch);
        ImGui::TableSetupColumn("Route", ImGuiTableColumnFlags_WidthStretch);
        ImGui::TableSetupColumn("pp AUD", ImGuiTableColumnFlags_WidthFixed, 80);
        ImGui::TableSetupColumn("Total (2)", ImGuiTableColumnFlags_WidthFixed, 80);
        ImGui::TableHeadersRow();

        ImGuiListClipper clipper;
        clipper.Begin((int)itineraries.size());
        while (clipper.Step()) {
            for (int row = clipper.DisplayStart; row < clipper.DisplayEnd; row++) {
                auto& it = itineraries[row];
                ImGui::TableNextRow();

                bool is_selected = (row == selected_idx);
                ImGui::TableSetColumnIndex(0);
                char label[32];
                snprintf(label, sizeof(label), "%d", row + 1);
                if (ImGui::Selectable(label, is_selected,
                        ImGuiSelectableFlags_SpanAllColumns |
                        ImGuiSelectableFlags_AllowOverlap)) {
                    selected_idx = row;
                }

                // Islands
                ImGui::TableSetColumnIndex(1);
                {
                    std::string isl_str;
                    for (size_t i = 0; i < it.island_sequence.size(); i++) {
                        if (i > 0) isl_str += " > ";
                        isl_str += it.island_sequence[i];
                    }
                    ImGui::TextUnformatted(isl_str.c_str());
                }

                // Route detail
                ImGui::TableSetColumnIndex(2);
                {
                    std::string route;
                    for (size_t i = 0; i < it.legs.size(); i++) {
                        if (i > 0) route += " | ";
                        auto& l = it.legs[i];
                        char buf[64];
                        snprintf(buf, sizeof(buf), "%s>%s %s",
                                 l.from.c_str(), l.to.c_str(),
                                 DATE_LABELS[l.date_index]);
                        route += buf;
                    }
                    ImGui::TextUnformatted(route.c_str());
                }

                // Price pp
                ImGui::TableSetColumnIndex(3);
                ImGui::Text("$%.0f", it.total_pp);

                // Price total
                ImGui::TableSetColumnIndex(4);
                ImGui::Text("$%.0f", it.total_both);
            }
        }
        ImGui::EndTable();
    }
    ImGui::End();
}
