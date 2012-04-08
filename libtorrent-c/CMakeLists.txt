project(libtorrent-c)
cmake_minimum_required(VERSION 2.8)

set(CMAKE_CXX_FLAGS "-std=c++0x")

# External dependencies
find_package(Boost)
include(cmake/FindLibTorrent.cmake)

add_library(libtorrent-c SHARED
                libtorrent-c.cpp)
target_link_libraries(libtorrent-c ${LIBTORRENT_LIBRARY})

install(TARGETS libtorrent-c
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)
