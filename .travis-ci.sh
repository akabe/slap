# Dependencies
# (Lacaml 7.2.5 or below cannot pass tests because they have a bug in the complex version of Vec.ssqr_diff)
APT_DEPENDS="opam liblapack-dev"
# OPAM_DEPENDS="ocamlfind cppo lacaml>=7.2.6 ounit"
OPAM_DEPENDS="ocamlfind cppo ounit"

# Select PPA
case "$OCAML_VERSION,$OPAM_VERSION" in
  3.12.1,1.2.0) ppa=avsm/ocaml312+opam12 ;;
  4.00.1,1.2.0) ppa=avsm/ocaml40+opam12 ;;
  4.01.0,1.2.0) ppa=avsm/ocaml41+opam12 ;;
  4.02.0,1.2.0) ppa=avsm/ocaml42+opam12 ;;
  4.02.1,1.2.0) ppa=avsm/ocaml42+opam12 ;;
  4.02.2,1.2.0) ppa=avsm/ocaml42+opam12 ;;
  *) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

# Install OPAM and $APT_DEPENDS
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ${APT_DEPENDS}

# Install OCaml
export OPAMYES=1
export OPAMVERBOSE=1
opam init
opam switch $OCAML_VERSION
eval `opam config env`

# Show OCaml and OPAM versions
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

# Install $OPAM_DEPENDS
opam install ${OPAM_DEPENDS}

# Install Lacaml
wget https://github.com/mmottl/lacaml/releases/download/v7.2.6/lacaml-7.2.6.tar.gz
tar zxvf lacaml-7.2.6.tar.gz
cd lacaml-7.2.6
./configure
make
make install
cd ..

# Test
./configure $CONFIG --enable-tests --enable-examples
make
make test

# Execute examples
for file in *.native; do
    if [ "$file" != "ppx_slap.native" ] && [ "$file" != "test.native" ]; then
        echo "Execute ./$file";
        ./$file
    fi
done
