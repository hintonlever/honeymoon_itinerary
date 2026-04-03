#include "imgui.h"
#include "imgui_impl_glfw.h"
#include "imgui_impl_opengl3.h"
#include <GLFW/glfw3.h>
#include <cstdio>
#include <string>
#include <vector>

#include "types.h"
#include "csv_loader.h"
#include "itinerary_solver.h"
#include "panels/table_panel.h"
#include "panels/map_panel.h"
#include "panels/gantt_panel.h"

static void glfw_error_callback(int error, const char* desc) {
    fprintf(stderr, "GLFW Error %d: %s\n", error, desc);
}

int main(int, char**) {
    glfwSetErrorCallback(glfw_error_callback);
    if (!glfwInit()) return 1;

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GLFW_TRUE);
    glfwWindowHint(GLFW_COCOA_RETINA_FRAMEBUFFER, GLFW_TRUE);

    GLFWwindow* window = glfwCreateWindow(1600, 1000,
        "Honeymoon Itinerary Planner", nullptr, nullptr);
    if (!window) { glfwTerminate(); return 1; }
    glfwMakeContextCurrent(window);
    glfwSwapInterval(1);

    IMGUI_CHECKVERSION();
    ImGui::CreateContext();
    ImGuiIO& io = ImGui::GetIO();
    io.FontGlobalScale = 1.0f;

    ImGui::StyleColorsDark();
    ImGuiStyle& style = ImGui::GetStyle();
    style.WindowRounding = 6.0f;
    style.FrameRounding = 4.0f;
    style.GrabRounding = 3.0f;
    style.Colors[ImGuiCol_WindowBg] = ImVec4(0.08f, 0.08f, 0.12f, 1.0f);

    ImGui_ImplGlfw_InitForOpenGL(window, true);
    ImGui_ImplOpenGL3_Init("#version 150");

    // Load data — try several paths relative to likely working dirs
    FlightData flight_data;
    for (auto& path : {"../full_prices_cleaned.csv",
                        "full_prices_cleaned.csv",
                        "../../full_prices_cleaned.csv"}) {
        flight_data = load_flights(path);
        if (!flight_data.flights.empty()) break;
    }

    Constraints constraints = {};
    // Default: all carriers on, no required islands, no min nights
    constraints.carrier_air_tahiti = true;
    constraints.carrier_air_moana = true;
    constraints.carrier_hahn_air = true;
    constraints.max_stops = -1;

    std::vector<Itinerary> itineraries = solve(flight_data, constraints);
    int selected_idx = 0;
    bool needs_resolve = false;
    bool first_frame = true;

    while (!glfwWindowShouldClose(window)) {
        glfwPollEvents();

        ImGui_ImplOpenGL3_NewFrame();
        ImGui_ImplGlfw_NewFrame();
        ImGui::NewFrame();

        // Set initial window layout on first frame
        if (first_frame) {
            ImGui::SetNextWindowPos(ImVec2(0, 0));
            ImGui::SetNextWindowSize(ImVec2(250, 1000));
        }
        // ── Sidebar: Constraints ──
        ImGui::Begin("Filters");

        ImGui::SeparatorText("Required Islands");
        for (int i = 0; i < ISL_COUNT; i++) {
            if (ImGui::Checkbox(ISLANDS[i].code.c_str(), &constraints.required_islands[i]))
                needs_resolve = true;
        }

        ImGui::SeparatorText("Minimum Nights");
        ImGui::TextDisabled("(0 = default 1 night)");
        for (int i = 0; i < ISL_COUNT; i++) {
            char label[32];
            snprintf(label, sizeof(label), "##min_%s", ISLANDS[i].code.c_str());
            ImGui::SetNextItemWidth(80);
            if (ImGui::InputInt(label, &constraints.min_nights[i])) {
                if (constraints.min_nights[i] < 0) constraints.min_nights[i] = 0;
                if (constraints.min_nights[i] > 12) constraints.min_nights[i] = 12;
                needs_resolve = true;
            }
            ImGui::SameLine();
            ImGui::Text("%s", ISLANDS[i].code.c_str());
        }

        ImGui::SeparatorText("Flight Filters");
        const char* stop_items[] = {"Any stops", "Direct only", "Max 1 stop", "Max 2 stops"};
        int stop_sel = (constraints.max_stops < 0) ? 0 : constraints.max_stops + 1;
        if (ImGui::Combo("Stops", &stop_sel, stop_items, 4)) {
            constraints.max_stops = (stop_sel == 0) ? -1 : stop_sel - 1;
            needs_resolve = true;
        }

        ImGui::SeparatorText("Carriers");
        if (ImGui::Checkbox("Air Tahiti", &constraints.carrier_air_tahiti)) needs_resolve = true;
        if (ImGui::Checkbox("Air Moana", &constraints.carrier_air_moana)) needs_resolve = true;
        if (ImGui::Checkbox("Hahn Air", &constraints.carrier_hahn_air)) needs_resolve = true;

        ImGui::Separator();
        ImGui::Text("Results: %d", (int)itineraries.size());

        if (needs_resolve) {
            itineraries = solve(flight_data, constraints);
            if (selected_idx >= (int)itineraries.size())
                selected_idx = itineraries.empty() ? -1 : 0;
            needs_resolve = false;
        }

        ImGui::End();

        // ── Panels ──
        if (first_frame) {
            ImGui::SetNextWindowPos(ImVec2(250, 0));
            ImGui::SetNextWindowSize(ImVec2(750, 500));
        }
        draw_table_panel(itineraries, selected_idx);
        if (first_frame) {
            ImGui::SetNextWindowPos(ImVec2(1000, 0));
            ImGui::SetNextWindowSize(ImVec2(600, 500));
        }
        draw_map_panel(itineraries, selected_idx);
        if (first_frame) {
            ImGui::SetNextWindowPos(ImVec2(250, 500));
            ImGui::SetNextWindowSize(ImVec2(1350, 500));
        }
        draw_gantt_panel(itineraries, selected_idx);
        first_frame = false;

        // Render
        ImGui::Render();
        int display_w, display_h;
        glfwGetFramebufferSize(window, &display_w, &display_h);
        glViewport(0, 0, display_w, display_h);
        glClearColor(0.06f, 0.06f, 0.09f, 1.0f);
        glClear(GL_COLOR_BUFFER_BIT);
        ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());

        glfwSwapBuffers(window);
    }

    ImGui_ImplOpenGL3_Shutdown();
    ImGui_ImplGlfw_Shutdown();
    ImGui::DestroyContext();
    glfwDestroyWindow(window);
    glfwTerminate();
    return 0;
}
