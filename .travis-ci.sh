# Dependencies
APT_DEPENDS="ocaml ocaml-native-compilers camlp4-extra ocaml-compiler-libs opam liblapack-dev"
OPAM_DEPENDS="ocamlfind cppo lacaml ounit"

# Select PPA
case "$OCAML_VERSION,$OPAM_VERSION" in
  3.12.1,1.0.0) ppa=avsm/ocaml312+opam10 ;;
  3.12.1,1.1.0) ppa=avsm/ocaml312+opam11 ;;
  3.12.1,1.2.0) ppa=avsm/ocaml312+opam12 ;;
  4.00.1,1.0.0) ppa=avsm/ocaml40+opam10 ;;
  4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
  4.00.1,1.2.0) ppa=avsm/ocaml40+opam12 ;;
  4.01.0,1.0.0) ppa=avsm/ocaml41+opam10 ;;
  4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
  4.01.0,1.2.0) ppa=avsm/ocaml41+opam12 ;;
  4.02.0,1.1.0) ppa=avsm/ocaml42+opam11 ;;
  4.02.0,1.2.0) ppa=avsm/ocaml42+opam12 ;;
  *) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

# Install OCaml, OPAM and $APT_DEPENDS
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ${APT_DEPENDS}
export OPAMYES=1
export OPAMVERBOSE=1
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

# Install $OPAM_DEPENDS
opam init
opam install ${OPAM_DEPENDS}
eval `opam config env`

# Test
./configure --enable-tests --enable-examples
make
make test