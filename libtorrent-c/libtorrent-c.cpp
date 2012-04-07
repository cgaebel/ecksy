#include "libtorrent-c.h"

#define BOOST_ASIO_SEPARATE_COMPILATION

#include <libtorrent/ip_filter.hpp>
#include <libtorrent/peer_id.hpp>
#include <libtorrent/torrent.hpp>
#include <libtorrent/torrent_handle.hpp>

#include <string>
#include <vector>

#include <assert.h>
#include <string.h>

// BEGIN sha1_hash //

struct sha1_hash
{
    libtorrent::sha1_hash h;
};

void free_sha1_hash(struct sha1_hash* h) { delete h; }

// END sha1_hash //

// BEGIN ip_filter //

struct ip_filter
{
    libtorrent::ip_filter f;
};

struct ip_filter* make_ip_filter() { return new ip_filter; }
void free_ip_filter(struct ip_filter* f) { delete f; }

void add_filtered_range(struct ip_filter* f, const char* start, const char* end)
{
    try {
        f->f.add_rule(libtorrent::address::from_string(start),
                      libtorrent::address::from_string(end),
                      libtorrent::ip_filter::blocked);
    } catch(...) {
        assert(0);
    }
}

// END ip_filter //

// BEGIN torrent_list //

struct torrent_list
{
    std::vector<struct torrent_handle*> v;
};

int tlist_elems(const struct torrent_list* tl)
{
    return tl->v.size();
}

void tlist_dump(const struct torrent_list* tl, struct torrent_handle* dst[])
{
    memcpy(dst, tl->v.data(), tl->v.size()*sizeof(struct torrent_handle*));
}

void free_torrent_list(struct torrent_list* tl) { delete tl; }

// END torrent_list //

// BEGIN torrent_handle //

struct torrent_handle
{
    libtorrent::torrent_handle h;
};

static char* str2str(const std::string& s)
{
    size_t len = s.length();
    char* r = (char*)malloc(len);
    memcpy(r, s.c_str(), len);
    return r;
}

char* torrent_save_path(const struct torrent_handle* h)
{
    try {
        return str2str(h->h.save_path());
    } catch(...) {
        assert(0);
    }
}

char* torrent_name(const struct torrent_handle* h)
{
    try {
        return str2str(h->h.name());
    } catch(...) {
        assert(0);
    }
}

// END torrent_handle //
