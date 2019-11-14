echo "OS: " $TRAVIS_OS_NAME

if [[ "$TRAVIS_OS_NAME" == "linux" ]]
then
    sudo add-apt-repository -y ppa:avsm/ppa
    sudo apt-get update
    sudo apt-get install opam
else
    brew install opam ocaml
    export OCAML_VERSION="system"
fi

opam init -a
