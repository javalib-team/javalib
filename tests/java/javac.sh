sources=`cat java/sources.txt`

if [ -n "$sources" ]; then
    javac -g -parameters -sourcepath java/src -d java/javac @java/sources.txt
fi
