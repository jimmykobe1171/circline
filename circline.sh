# Check whether the file "utils.bc" exist
file="utils.bc"
if [ ! -e "$file" ]
then
	clang -emit-llvm -o utils.bc -c ./compiler/lib/utils.c -Wno-varargs
fi

if [ $# -eq 1 ]
then
    ./compiler/circline.native <$1 >a.ll
else
    ./compiler/circline.native $1 <$2 >a.ll
fi
clang -Wno-override-module utils.bc a.ll -o $1.exe
./$1.exe
rm a.ll
rm ./$1.exe

# /usr/local/opt/llvm38/bin/clang-3.8
