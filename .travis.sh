echo "Working on branch" $TRAVIS_BRANCH

OPAM_DEPENDS="ocamlfind camlzip extlib camomile"

opam switch $OCAML_VERSION
eval `opam config env`

if [[ "$TRAVIS_OS_NAME" == linux ]]
then
    opam install -y camlp4
fi

opam install -y ${OPAM_DEPENDS}

./configure.sh
make

cd tests
make
make clean

cd ..
make clean
