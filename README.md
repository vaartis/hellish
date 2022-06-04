# Hellish

A torrent tracker.

## Command line options

- `--invite-not-required` makes the registration process work without invites,
  you can create the first account like this and then invite other people with it
- `--config` sets the config file, which by default is `hellish_config.ini`, the options are described in the file itself

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
