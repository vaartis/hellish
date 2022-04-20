#/usr/env bash

BASE=$(pwd)

if [[ ! -d obj/md4c/ ]]; then
    mkdir -p ${BASE}/obj/md4c
    cmake -B obj/md4c/ deps/md4c -DCMAKE_BUILD_TYPE=Debug -DBUILD_SHARED_LIBS=OFF
    make -Cobj/md4c/ md4c md4c-html
fi

if [[ ! -d "${BASE}/obj/aws" ]]; then

    pushd deps/aws/
    make setup build install ZLIB=false DEMOS=false LAL=false XMLADA=false prefix=${BASE}/obj/aws/\
         SOCKET="openssl" PRJ_TARGET="UNIX" TARGET=$(gcc -dumpmachine)
    popd
fi
