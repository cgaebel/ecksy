# Find libtorrent-rasterbar
find_path(LIBTORRENT_INCLUDE_DIR libtorrent /usr/include /usr/local/include)
find_library(LIBTORRENT_LIBRARY torrent-rasterbar /usr/lib /usr/local/lib)

if(LIBTORRENT_INCLUDE_DIR AND LIBTORRENT_LIBRARY)
  set(LIBTORRENT_FOUND TRUE)
endif(LIBTORRENT_INCLUDE_DIR AND LIBTORRENT_LIBRARY)

if(LIBTORRENT_FOUND)
  message(STATUS "Found libtorrent: ${LIBTORRENT_LIBRARY}")
else(LIBTORRENT_FOUND)
  message(FATAL_ERROR "libtorrent not found!")
endif(LIBTORRENT_FOUND)
