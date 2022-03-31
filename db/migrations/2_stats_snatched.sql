ALTER TABLE user_torrent_stats
      ADD COLUMN snatched boolean DEFAULT false NOT NULL;
