clang -emit-llvm -o utils.bc -c lib/utils.c
if [ $# -eq 1 ]
then
    ./circline.native <$1 >a.ll
else
    ./circline.native $1 <$2 >a.ll
fi
clang utils.bc a.ll -o run
./run
rm a.ll
rm run

# /usr/local/opt/llvm38/bin/clang-3.8
