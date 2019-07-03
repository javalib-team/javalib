path=`cat java/javac/out/JmodPath`

if [ -n "$path" ]; then
    cp $path/*.jmod ./javatests/jmods
    for file in `ls ./javatests/jmods/*.jmod`; do
	jmod extract $file --dir ./javatests/jmods
    done
fi
