#!/bin/sh

set -eu

# Remove old documentation files
rm -rf api/* ppx/*

# Create working directory
mkdir tmp
cd tmp

# Obtain source files
wget -O slap.tar.gz https://github.com/akabe/slap/tarball/master
tar zxvf slap.tar.gz --strip-components 1

# Generate documentation files
./configure --enable-ppx
make
make doc

mv slap.docdir/* ../api/
mv slap_ppx.docdir/* ../ppx/

# Remove working directory
cd ..
rm -rf tmp

rm -f api/html.stamp ppx/html.stamp
find api ppx -name '*.html' | \
    xargs sed -i 's#</body>#<script type="text/javascript" src="../google-analytics.js"></script></body>#g'
