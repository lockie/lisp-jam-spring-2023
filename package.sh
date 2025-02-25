#!/usr/bin/env bash

set -euo pipefail

if [ $# -ne 1 ]; then
    echo "USAGE: $0 <package flavor>"
    exit 1
fi

export VERSION=${GITHUB_REF_NAME:-$(git describe --always --tags --dirty=+ --abbrev=6)}

function do_build () {
    sbcl --dynamic-space-size 2048 --disable-debugger --quit --load package/build.lisp
}

case $1 in
    linux)
        do_build
        linuxdeploy --appimage-extract-and-run --executable=bin/thoughtbound \
                    --custom-apprun=package/AppRun \
                    --icon-file=package/icon.png \
                    --desktop-file=package/thoughtbound.desktop \
                    --appdir=appimage $(find bin -name "lib*" -printf "-l%p ")
        cp bin/thoughtbound appimage/usr/bin
        cp -R Resources appimage/usr
        appimagetool --appimage-extract-and-run --comp xz -g appimage "thoughtbound-${VERSION}.AppImage"
        ;;

    windows)
        if ! command -v mingw-ldd > /dev/null 2>&1
        then
            echo "Missing mingw-ldd helper binary"
            exit 1
        fi
        do_build
        ntldd -R bin/* | grep ucrt64 | awk -F '=> ' '{ print $2 }' | awk '{ print $1 }' | sed 's/\\/\\\\/g' | xargs -I deps cp deps bin
        magick package/icon.png -define icon:auto-resize=16,32,48,64,256 ICO:- > "$TEMP/icon.ico"
        magick package/icon.png -resize 150x57 -extent 150x57 -gravity center -background white -alpha remove -alpha off BMP2:- > "$TEMP/icon.bmp"
        makensis package/installer.nsi
        ;;

    *)
        echo "Uknown package flavor: $1"
        exit 1
        ;;
esac
