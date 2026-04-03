cmake_minimum_required(VERSION 3.20)
project(honeymoon_planner LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 17)

# --- GLFW (system, via brew) ---
find_package(glfw3 3.3 REQUIRED)

# --- Dear ImGui (fetched at configure time) ---
include(FetchContent)
FetchContent_Declare(
  imgui
  GIT_REPOSITORY https://github.com/ocornut/imgui.git
  GIT_TAG        v1.91.8
  SOURCE_DIR     ${CMAKE_SOURCE_DIR}/external/imgui
)
FetchContent_MakeAvailable(imgui)

add_library(imgui_lib STATIC
  ${imgui_SOURCE_DIR}/imgui.cpp
  ${imgui_SOURCE_DIR}/imgui_demo.cpp
  ${imgui_SOURCE_DIR}/imgui_draw.cpp
  ${imgui_SOURCE_DIR}/imgui_tables.cpp
  ${imgui_SOURCE_DIR}/imgui_widgets.cpp
  ${imgui_SOURCE_DIR}/backends/imgui_impl_glfw.cpp
  ${imgui_SOURCE_DIR}/backends/imgui_impl_opengl3.cpp
)
target_include_directories(imgui_lib PUBLIC
  ${imgui_SOURCE_DIR}
  ${imgui_SOURCE_DIR}/backends
)
target_link_libraries(imgui_lib PUBLIC glfw)

# --- macOS OpenGL framework ---
find_package(OpenGL REQUIRED)

# --- Application ---
add_executable(honeymoon_planner
  src/main.cpp
  src/csv_loader.cpp
  src/itinerary_solver.cpp
  src/panels/table_panel.cpp
  src/panels/map_panel.cpp
  src/panels/gantt_panel.cpp
)
target_include_directories(honeymoon_planner PRIVATE src)
target_link_libraries(honeymoon_planner PRIVATE imgui_lib OpenGL::GL)
target_compile_definitions(honeymoon_planner PRIVATE GL_SILENCE_DEPRECATION)
