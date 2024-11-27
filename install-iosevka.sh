#!/bin/sh

mkdir /tmp/iosevka-font
cd /tmp/iosevka-font
curl -s 'https://api.github.com/repos/be5invis/Iosevka/releases/latest' | jq -r ".assets[] | .browser_download_url" | grep PkgTTC-Iosevka | xargs -n 1 curl -L -O --fail --silent --show-error
unzip \*.zip
sudo mkdir /usr/local/share/fonts/iosevka-font
sudo mv *.ttc /usr/local/share/fonts/iosevka-font/
fc-cache
