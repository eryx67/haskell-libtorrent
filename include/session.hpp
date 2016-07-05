#ifndef __SESSION_HPP__
#define __SESSION_HPP__

#include "rss.hpp"
#include "extensions.hpp"

typedef std::deque<libtorrent::alert*> DequeAlertPtr;

typedef std::vector<char> VectorChar;

typedef void AlertDispatchCallback (libtorrent::alert*);

typedef bool TorrentStatusFilter (libtorrent::torrent_status*);

#endif /* __SESSION_HPP__ */
