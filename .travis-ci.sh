# Dependencies
OPAM_DEPS="ocamlbuild ocamlfind cppo lacaml>=8.0.0 ounit"

export PREFIX="./usr"
export BINDIR="$PREFIX/bin"
export LIBDIR="$PREFIX/lib"
export PATH="$BINDIR:$PATH"

mkdir -p $PREFIX

# Download and install OPAM and OCaml
wget -q -O opam_installer.sh "https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh"
if [ -n "${OPAM_VERSION:-}" ]; then
    sed -i "s/^VERSION=.*$/VERSION='$OPAM_VERSION'/" opam_installer.sh
fi
echo y | sh opam_installer.sh $BINDIR

# Install OCaml
export OPAMYES=1
export OPAMVERBOSE=1
opam init
opam switch $OCAML_VERSION
eval `opam config env`

# Install OPAM packages
if [ -n "${OPAM_DEPS:-}" ]; then
    opam install $OPAM_DEPS
fi

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
