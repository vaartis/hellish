CREATE TABLE torrent_groups (
    id integer NOT NULL,
    name text NOT NULL,
    description text NOT NULL,
    creator integer,
    data json DEFAULT '{}'::json NOT NULL
);
CREATE SEQUENCE torrent_groups_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE torrent_groups_id_seq OWNED BY torrent_groups.id;

ALTER TABLE ONLY torrent_groups ALTER COLUMN id SET DEFAULT nextval('torrent_groups_id_seq'::regclass);

SELECT pg_catalog.setval('torrent_groups_id_seq', 1, false);

ALTER TABLE ONLY torrent_groups
    ADD CONSTRAINT torrent_groups_name_key UNIQUE (name);

ALTER TABLE ONLY torrent_groups
    ADD CONSTRAINT torrent_groups_pkey PRIMARY KEY (id);

CREATE INDEX torrent_groups_creator_idx ON torrent_groups USING btree (creator);

CREATE INDEX torrent_groups_name_idx ON torrent_groups USING btree (name);

ALTER TABLE torrents
      ADD COLUMN "group" integer;

CREATE INDEX torrents_group_idx ON torrents USING btree ("group");

ALTER TABLE ONLY torrents
    ADD CONSTRAINT group_fk FOREIGN KEY ("group") REFERENCES torrent_groups(id);

ALTER TABLE ONLY torrent_groups
    ADD CONSTRAINT torrent_groups_creator_fkey FOREIGN KEY (creator) REFERENCES users(id);
