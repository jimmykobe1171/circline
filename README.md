# Circline
Manager: Jia Zhang

System Architecture: Haikuo Liu

Language Guru: Zehao Song

Tester: Qing Lan

## Install LLVM

### Create soft link

```
sudo ln -s /usr/local/opt/llvm38/bin/lli-3.8 /usr/local/bin/lli
sudo ln -s /usr/local/opt/llvm38/bin/llvm-link-3.8 /usr/local/bin/llvm-link
```
### How to run

```
make all
sh compiler/circline.sh program.file
./complier/test.in.exe
```

## Current Target
- Build the Travis CI (done in 24-Oct)
- Build the Ocaml Compiler
- BUild the make file and test the program

## Recent Update 2016-10-22
Figure out the major obstacle on the Scanner Part. User can run the test module by doing the follows:
```
sh test_scanner.sh
```
This work is the same as to do:
```
scanner/tokenize < $input_file | $output_file -
```
After Comparing the difference between the standard output file, we will see the difference if the scanner parsing procedure are not doing well
