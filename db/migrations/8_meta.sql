ALTER TABLE torrents
      ADD COLUMN meta json DEFAULT '{}'::json NOT NULL;

ALTER TABLE posts
      ADD COLUMN meta json DEFAULT '{}'::json NOT NULL;
