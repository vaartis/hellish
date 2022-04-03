ALTER TABLE users
      ADD COLUMN profile json DEFAULT '{}'::json NOT NULL;
