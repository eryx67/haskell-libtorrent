#ifndef __EXTENSIONS_HPP__
#define __EXTENSIONS_HPP__

typedef boost::shared_ptr<libtorrent::torrent_plugin> TorrentPlugin (libtorrent::torrent*, void*);

typedef boost::shared_ptr<libtorrent::plugin> Plugin;

#endif /* __EXTENSIONS_HPP__ */
