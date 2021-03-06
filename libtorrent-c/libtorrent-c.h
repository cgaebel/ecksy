#pragma once

#include <string.h>

#ifdef __cplusplus
extern "C" {
#else
#include <stdbool.h>
#endif

// BEGIN ip_filter //
struct ip_filter;

struct ip_filter* make_ip_filter();

void add_filtered_range(struct ip_filter*, const char* start, const char* end);

void free_ip_filter(struct ip_filter*);
// END ip_filter //

// BEGIN torrent_list //
struct torrent_list;
struct torrent_handle;

int tlist_elems(const struct torrent_list*);
// dumps the torrents into an array, whose size must be at least the amount
// returned by tlist_elems.
void tlist_dump(const struct torrent_list*, struct torrent_handle* tgt[]);
void free_torrent_list(struct torrent_list*);
// END torrent_list //

// BEGIN torrent_handle //
struct torrent_handle;

char* torrent_save_path(const struct torrent_handle*); // free() the return value.
char* torrent_name(const struct torrent_handle*); // free() the return value.
void set_ratio(struct torrent_handle*, float ratio); // 2.0 for upload 2x what was downloaded.
void set_torrent_upload_limit(struct torrent_handle*, int);
int get_torrent_upload_limit(const struct torrent_handle*);
void set_torrent_download_limit(struct torrent_handle*, int);
int get_torrent_download_limit(const struct torrent_handle*);
void pause_torrent(struct torrent_handle*);
void resume_torrent(struct torrent_handle*);
bool is_paused(const struct torrent_handle*);
bool is_seed(const struct torrent_handle*); // are we seeding?
char* info_hash(const struct torrent_handle*);
float torrent_progress(const struct torrent_handle*); // [0.0, 1.0]
int torrent_download_rate(const struct torrent_handle*);
int torrent_upload_rate(const struct torrent_handle*);
size_t total_torrent_size(const struct torrent_handle*);
size_t total_downloaded(const struct torrent_handle*);
void move_storage(struct torrent_handle*, const char* new_path);
char* torrent_magnet_uri(const struct torrent_handle*);

// 0 = queued for checking
// 1 = checking files
// 2 = downloading metadata
// 3 = downloading
// 4 = finished
// 5 = seeding
// 6 = allocating
// 7 = checking resume data
int torrent_state(const struct torrent_handle*);

void free_torrent_handle(struct torrent_handle*);
// END torrent_handle //

// BEGIN session //
struct session;

struct session* make_session();

struct torrent_handle* add_magnet_uri(struct session*, const char* uri, const char* targetPath);

void pause_session(struct session*);
void resume_session(struct session*);
bool is_session_paused(struct session*);

void remove_torrent(struct session*, struct torrent_handle*, bool delete_files);
struct torrent_handle* find_torrent(struct session*, const char* info_hash);
struct torrent_list* get_torrents(const struct session*);

// rate limiting
void set_session_upload_rate_limit(struct session*, int bytes_per_second);
int session_upload_rate_limit(const struct session*);
void set_session_download_rate_limit(struct session*, int bytes_per_second);
int session_download_rate_limit(const struct session*);

// ip filtering
void set_ip_filter(struct session*, struct ip_filter*);
struct ip_filter* get_ip_filter(const struct session*);

void free_session(struct session*);
// END session //

#ifdef __cplusplus
}
#endif
