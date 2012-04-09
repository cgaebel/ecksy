#include "libtorrent-c.h"

#define BOOST_ASIO_SEPARATE_COMPILATION

#include <libtorrent/extensions/metadata_transfer.hpp>
#include <libtorrent/extensions/ut_metadata.hpp>
#include <libtorrent/extensions/ut_pex.hpp>
#include <libtorrent/extensions/smart_ban.hpp>

#include <libtorrent/ip_filter.hpp>
#include <libtorrent/magnet_uri.hpp>
#include <libtorrent/peer_id.hpp>
#include <libtorrent/session.hpp>
#include <libtorrent/torrent.hpp>
#include <libtorrent/torrent_handle.hpp>

#include <algorithm>
#include <memory>
#include <string>
#include <vector>

#include <assert.h>
#include <stdio.h>
#include <string.h>

extern "C" {

#define WRAP(stuff) try { stuff; } catch(const libtorrent::libtorrent_exception& e) { printf("!!! Libtorrent error :%i: %s\n", __LINE__, e.what()); } \
                                   catch(...) { assert(0); }

// BEGIN ip_filter //

struct ip_filter
{
    libtorrent::ip_filter f;
};

struct ip_filter* make_ip_filter() { return new ip_filter; }

void free_ip_filter(struct ip_filter* f) { delete f; }

void add_filtered_range(struct ip_filter* f, const char* start, const char* end)
{
    WRAP(
        f->f.add_rule(libtorrent::address::from_string(start),
                      libtorrent::address::from_string(end),
                      libtorrent::ip_filter::blocked);
    )
}

// END ip_filter //

// BEGIN torrent_list //

struct torrent_list
{
    std::vector<struct torrent_handle*> v;

    torrent_list(const std::vector<struct torrent_handle*>& _v)
        : v(_v) {}
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

    torrent_handle(const libtorrent::torrent_handle& h_)
        : h(h_) {}
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
    assert(h);

    WRAP(return str2str(h->h.save_path()));

    return NULL;
}

char* torrent_name(const struct torrent_handle* h)
{
    assert(h);

    WRAP(return str2str(h->h.name()));

    return NULL;
}

void set_ratio(struct torrent_handle* h, float ratio)
{
    assert(h);

    WRAP(h->h.set_upload_limit(h->h.download_limit()*ratio));
}

void set_torrent_upload_limit(struct torrent_handle* h, int limit)
{
    assert(h);

    WRAP(h->h.set_upload_limit(limit));
}

int get_torrent_upload_limit(const struct torrent_handle* h)
{
    assert(h);

    WRAP(return h->h.upload_limit());
    return 0;
}

void set_torrent_download_limit(struct torrent_handle* h, int limit)
{
    assert(h);

    WRAP(h->h.set_download_limit(limit));
}

int get_torrent_download_limit(const struct torrent_handle* h)
{
    assert(h);

    WRAP(return h->h.download_limit());
    return 0;
}

void pause_torrent(struct torrent_handle* h)
{
    assert(h);

    WRAP(h->h.pause());
}

void resume_torrent(struct torrent_handle* h)
{
    assert(h);

    WRAP(h->h.resume());
}

bool is_paused(const struct torrent_handle* h)
{
    assert(h);

    WRAP(return h->h.status().paused);
    return false;
}

bool is_seed(const struct torrent_handle* h)
{
    assert(h);

    WRAP(return h->h.status().seed_mode);
    return false;
}

char* info_hash(const struct torrent_handle* h)
{
    assert(h);

    WRAP(
        auto s = h->h.info_hash().to_string();
        size_t n = s.length() + 1;
        char* p = (char*)malloc(n);
        memcpy((char*)p, s.c_str(), n);
        return p;
    )

    return NULL;
}

float torrent_progress(const struct torrent_handle* h)
{
    assert(h);

    WRAP(return h->h.status().progress);
    return 0.0f;
}

int torrent_download_rate(const struct torrent_handle* h)
{
    assert(h);

    WRAP(return h->h.status().download_rate);
    return 0;
}

int torrent_upload_rate(const struct torrent_handle* h)
{
    assert(h);

    WRAP(return h->h.status().upload_rate);
    return 0;
}

size_t total_torrent_size(const struct torrent_handle* h)
{
    assert(h);

    WRAP(return h->h.status().total_wanted);
    return 1; // stops a divide by 0 on error.
}

size_t total_downloaded(const struct torrent_handle* h)
{
    assert(h);

    WRAP(return h->h.status().total_wanted_done);
    return 0;
}

void move_storage (struct torrent_handle* h, const char* new_path)
{
    assert(h);
    assert(new_path);

    WRAP(
        if(!h->h.get_storage_impl()->move_storage(new_path))
            printf("Error moving storage for %s to %s.", h->h.name().c_str(), new_path);
    )
}

// 0 = queued for checking
// 1 = checking files
// 2 = downloading metadata
// 3 = downloading
// 4 = finished
// 5 = seeding
// 6 = allocating
// 7 = checking resume data
int torrent_state(const struct torrent_handle* h)
{
    assert(h);

    WRAP(h->h.status().state);
    return -1;
}

void free_torrent_handle(struct torrent_handle* h)
{
    delete h;
}

// END torrent_handle //

struct session
{
    libtorrent::session s;

    session(const char* fprint)
        : s(libtorrent::fingerprint(fprint, 1, 0, 0, 0), 0)
    {
    }
};

struct session* make_session()
{
    WRAP(

    // EY is for EcksY
    std::auto_ptr<struct session> s(new session("EY"));

    s->s.add_extension(&libtorrent::create_metadata_plugin);
    s->s.add_extension(&libtorrent::create_ut_metadata_plugin);
    s->s.add_extension(&libtorrent::create_ut_pex_plugin);
    s->s.add_extension(&libtorrent::create_smart_ban_plugin);

    libtorrent::pe_settings p_settings;

    // TODO: Should we force encryption? Test speeds with and without.
    p_settings.out_enc_policy    = libtorrent::pe_settings::enabled;
    p_settings.in_enc_policy     = libtorrent::pe_settings::enabled;
    p_settings.allowed_enc_level = libtorrent::pe_settings::rc4;
    p_settings.prefer_rc4        = true;

    s->s.set_pe_settings(p_settings);

    // since we're going to be all embedded and shit...
    libtorrent::session_settings s_settings = libtorrent::min_memory_usage();

    s_settings.user_agent = "Ecksy/1.0 libtorrent/" LIBTORRENT_VERSION;
    s_settings.file_pool_size = 512; // limit on linux is 1024, but we need some for the web server.
    s_settings.ignore_limits_on_local_network = true;
    s_settings.lazy_bitfields = true;
    s_settings.use_parole_mode = true;
    s_settings.active_downloads = -1;
    s_settings.active_seeds = 4;
    s_settings.dont_count_slow_torrents = true;
    s_settings.share_ratio_limit = 2;
    s_settings.peer_turnover_interval = 60;
    s_settings.peer_turnover = 0.008;
    s_settings.rate_limit_ip_overhead = true;
    s_settings.announce_to_all_trackers = true;
    s_settings.disable_hash_checks = false;

    s->s.set_settings(s_settings);

    // http://stackoverflow.com/questions/1181301/how-does-a-dht-in-a-bittorent-client-get-bootstrapped
    s->s.add_dht_router(std::make_pair("router.bittorrent.com", 6881));

    s->s.start_lsd();
    s->s.start_upnp();

    return s.release();

    )

    return NULL;
}

struct torrent_handle* add_magnet_uri(struct session* s, const char* uri, const char* targetPath)
{
    assert(s);
    assert(uri);
    assert(targetPath);

    WRAP(

    libtorrent::add_torrent_params params;

    params.storage_mode = libtorrent::storage_mode_allocate; // used for move robust resumption
    params.save_path = targetPath;
    params.auto_managed = false; // is this a bad idea?
    params.paused = false;

    return new torrent_handle(libtorrent::add_magnet_uri(s->s, uri, params));

    )

    return NULL;
}

void pause_session(struct session* s)
{
    assert(s);

    WRAP(s->s.pause());
}

void resume_session(struct session* s)
{
    assert(s);

    WRAP(s->s.resume());
}

bool is_session_paused(struct session* s)
{
    assert(s);

    WRAP(return s->s.is_paused());
    return false;
}

void remove_torrent(struct session* s, struct torrent_handle* h, bool delete_files)
{
    assert(s);
    assert(h);

    WRAP(s->s.remove_torrent(h->h, delete_files ? libtorrent::session::delete_files : 0));
}

struct torrent_handle* find_torrent(struct session* s, const char* h)
{
    assert(s);
    assert(h);

    WRAP(return new torrent_handle(s->s.find_torrent(libtorrent::sha1_hash(h))));
    return NULL;
}

static inline std::vector<struct torrent_handle*> heapify(const std::vector<libtorrent::torrent_handle>& v)
{
    std::vector<struct torrent_handle*> result;
    result.resize(v.size());

    std::transform(v.begin(), v.end(), result.begin(),
        [](const libtorrent::torrent_handle& h) { return new torrent_handle(h); });

    return result;
}

struct torrent_list* get_torrents(const struct session* s)
{
    assert(s);

    WRAP(return new torrent_list(heapify(s->s.get_torrents())));
    return NULL;
}

// rate limiting
void set_session_upload_rate_limit(struct session* s, int bytes_per_second)
{
    assert(s);

    WRAP(s->s.set_upload_rate_limit(bytes_per_second));
}

int session_upload_rate_limit(const struct session* s)
{
    assert(s);

    WRAP(return s->s.upload_rate_limit());
    return 0;
}

void set_session_download_rate_limit(struct session* s, int bytes_per_second)
{
    assert(s);

    WRAP(s->s.set_download_rate_limit(bytes_per_second));
}

int session_download_rate_limit(const struct session* s)
{
    assert(s);

    WRAP(return s->s.download_rate_limit());
    return 0;
}

// ip filtering
void set_ip_filter(struct session* s, struct ip_filter* f)
{
    assert(s);
    assert(f);

    WRAP(s->s.set_ip_filter(f->f));
}

void free_session(struct session* s)
{
    delete s;
}

}
