ALTER TABLE torrents
      ADD COLUMN category integer DEFAULT 0 NOT NULL;

CREATE INDEX torrents_category_idx ON torrents USING btree (category);
