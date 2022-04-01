CREATE TABLE peer_data (
    torrent_id integer NOT NULL,
    data json
);

ALTER TABLE ONLY peer_data
      ADD CONSTRAINT peer_data_pkey PRIMARY KEY (torrent_id);

CREATE INDEX peer_data_torrent_id_idx ON peer_data USING btree (torrent_id);

ALTER TABLE ONLY peer_data
     ADD CONSTRAINT peer_data_torrent_id_fkey FOREIGN KEY (torrent_id) REFERENCES torrents(id);
