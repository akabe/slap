export PREFIX="./usr"
export BINDIR="$PREFIX/bin"
export LIBDIR="$PREFIX/lib"
export PATH="$BINDIR:$PATH"
export OPAMYES=1
export OPAMVERBOSE=1

mkdir -p $PREFIX

# Download and install OPAM and OCaml
wget -q -O opam_installer.sh "https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh"
if [ -n "${OPAM_VERSION:-}" ]; then
    sed -i "s/^VERSION=.*$/VERSION='$OPAM_VERSION'/" opam_installer.sh
fi
echo y | sh opam_installer.sh $BINDIR $OCAML_VERSION
eval `opam config env`

# Install OPAM packages
opam install cppo lacaml>=8.0.0 ounit

# Test
./configure $CONFIG --enable-tests --enable-examples
make
make test

# Execute examples
for file in *.native; do
    if [ "$file" != "ppx_slap.native" ] && [ "$file" != "test.native" ] && [ "$file" != "visualization.native" ]; then
        echo "Execute ./$file";
        ./$file
    fi
done
