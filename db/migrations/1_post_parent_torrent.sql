ALTER TABLE posts
      ADD COLUMN parent_torrent INTEGER;

CREATE INDEX posts_parent_torrent_idx ON public.posts USING btree (parent_torrent);

ALTER TABLE ONLY public.posts
      ADD CONSTRAINT posts_parent_torrent_fkey FOREIGN KEY (parent_torrent) REFERENCES public.torrents(id);
