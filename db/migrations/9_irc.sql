CREATE TABLE irc_channels (
    id integer NOT NULL,
    name text NOT NULL,
    data json DEFAULT '{}'::json NOT NULL
);

CREATE SEQUENCE irc_channels_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE irc_channels_id_seq OWNED BY irc_channels.id;

ALTER TABLE ONLY irc_channels ALTER COLUMN id SET DEFAULT nextval('irc_channels_id_seq'::regclass);
SELECT pg_catalog.setval('irc_channels_id_seq', 1, false);

ALTER TABLE ONLY irc_channels
    ADD CONSTRAINT irc_channels_name_key UNIQUE (name);
ALTER TABLE ONLY irc_channels
    ADD CONSTRAINT irc_channels_pkey PRIMARY KEY (id);

CREATE INDEX irc_channels_name_idx ON irc_channels USING btree (name);
