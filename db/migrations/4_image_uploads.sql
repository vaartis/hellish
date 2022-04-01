 CREATE TABLE image_uploads (
     id integer NOT NULL,
     by_user integer NOT NULL,
     filename text NOT NULL
 );

ALTER TABLE image_uploads OWNER TO postgres;

CREATE SEQUENCE image_uploads_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE image_uploads_id_seq OWNER TO postgres;

ALTER SEQUENCE image_uploads_id_seq OWNED BY image_uploads.id;

ALTER TABLE ONLY image_uploads ALTER COLUMN id SET DEFAULT nextval('image_uploads_id_seq'::regclass);

SELECT pg_catalog.setval('image_uploads_id_seq', 1, false);

ALTER TABLE ONLY image_uploads
    ADD CONSTRAINT image_uploads_filename_key UNIQUE (filename);

ALTER TABLE ONLY image_uploads
    ADD CONSTRAINT image_uploads_pkey PRIMARY KEY (id);

CREATE INDEX image_uploads_by_user_idx ON image_uploads USING btree (by_user);

CREATE INDEX image_uploads_filename_idx ON image_uploads USING btree (filename);

ALTER TABLE ONLY image_uploads
    ADD CONSTRAINT image_uploads_by_user_fkey FOREIGN KEY (by_user) REFERENCES users(id);
