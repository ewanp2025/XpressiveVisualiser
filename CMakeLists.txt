cmake_minimum_required(VERSION 3.16)

project(LmmsVisualizer VERSION 0.1 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Qt6 REQUIRED COMPONENTS Core Gui Widgets Xml)

qt_standard_project_setup()

add_executable(LmmsVisualizer
    main.cpp
)

target_link_libraries(LmmsVisualizer PRIVATE
    Qt6::Core
    Qt6::Gui
    Qt6::Widgets
    Qt6::Xml
)

if(MINGW)
    # Tells the assembler to support >65k sections (Big Object format)
    target_compile_options(LmmsVisualizer PRIVATE "-Wa,-mbig-obj")
elseif(MSVC)
    target_compile_options(LmmsVisualizer PRIVATE "/bigobj")
endif()

if(WIN32)
    set_property(TARGET LmmsVisualizer PROPERTY WIN32_EXECUTABLE ON)
endif()
