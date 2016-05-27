OPAM_PKGS="cppo lacaml>=8.0.0 ounit"

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
set +e # Disable '-e' option for branching
if [ "$OCAML_VERSION" == "4.03.0" ]; then
    set -e  # Enable '-e'
    # OCaml 4.03.0 possibly requires reinstallation of ocamlbuild. (bug?)
    # [STDERR] Warning: Won't be able to compile a native plugin
    #          Failure: Cannot find "ocamlbuild.cmo" in ocamlbuild -where directory.
    if ! opam install $OPAM_PKGS; then
        echo "\033[31m[WARN] Retry installation of OPAM packages after rebuilding OCamlbuild.\033[0m"
        opam reinstall ocamlbuild
        opam install $OPAM_PKGS
    fi
else
    set -e # Enable '-e'
    opam install $OPAM_PKGS
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
