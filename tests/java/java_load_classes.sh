classes=`find ./javatests/out -maxdepth 1 -type f -name '*.class' -printf '%f\n'`

for fullname in $classes; do
    rootname=${fullname%.class}
    pathname=${rootname//\./\/}
    dir=$(dirname $pathname)
    mkdir -p ./javatests/out/$dir
    src=./javatests/out/$fullname
    dst=./javatests/out/$dir/$(basename $pathname).class
    if [ $(realpath $src) != $(realpath $dst) ]; then
       cp $src $dst
    fi
done

for name in $classes; do
    java -classpath :./java/javac:./javatests/out Loader ${name%.class}
done
