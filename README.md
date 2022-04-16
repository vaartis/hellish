# Hellish

A torrent tracker.

## Command line options

- `--invite-not-required` makes the registration process work without invites,
  you can create the first account like this and then invite other people with it
- `--https` makes the website show the tracker url as https instead of http,
  if you're running behind a reverse-proxy with https, you should set this
- `--server-host` sets the address which the website shows as the tracker address,
  the full tracker url is `(http|https)://(ip+port|your server host)/(user passkey)/announce`.
  Once again, this is useful when you're running behind a reverse-proxy, so the actual program
  listens on localhost but people need to connect to the proxy.
- `--ssl-cert` and `--ssl-privkey` set the SSL certificate and private key for the IRC server,
  if you set these then the IRC will also run with TLS on a separate port.

You will need [libsodium](https://doc.libsodium.org/) installed on your system for the program to run.
The database used is PostgreSQL, it tries to connect to the database `hellish` as user `postgres`.

Build with [alire](https://github.com/alire-project/alire):

```sh
# optimized build
alr build
# or like this to get debug symbols
HELLISH_BUILD_MODE=debug alr build

./bin/hellish
```

The build is entirely static except the libsodium and postgresql libraries, but be sure to copy
the assets to the server too. You can run `deploy.sh` which will put everything you need except `aws.ini`
into `_deploy`.
