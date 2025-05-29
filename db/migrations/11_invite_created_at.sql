ALTER TABLE invites
      ADD COLUMN created_at timestamp DEFAULT now() NOT NULL;
