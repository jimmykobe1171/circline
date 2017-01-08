# Circline
Manager: Jia Zhang

System Architecture: Haikuo Liu

Language Guru: Zehao Song

Tester: Qing Lan


## Prerequisite

LLVM & Clang

Create a soft-link to lli

```
sudo ln -s /usr/local/opt/llvm38/bin/lli-3.8 /usr/local/bin/lli
```

## Compile & Test

```
make all
make test
```

### Hello World

Create the following `test.in` file
```
print("Hello World!");
```

Run `sh circline.sh test.in` in terminal

Output

```
Hello World!
```

## Examples:

![ex1](/examples_pictures/example1.png)

![ex1](/examples_pictures/example2.png)

### for more examples and details of circline, please refer to the docs in Report file
