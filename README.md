# Circline
Manager: Jia Zhang

System Architecture: Haikuo Liu

Language Guru: Zehao Song

Tester: Qing Lan

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