Bit_Sets
========

This is the library source code

Building
--------

This project was build with alire and depends on it to build.

If you have alire installed build it with:

    alr build

If you don't you should really give it a try but you can still build without it.
You should be able to copy the the file
[share/bit_sets/bit_sets_config.gpr](./share/bit_sets/bit_sets_config.gpr)
to `config/bit_sets_config.gpr` and you should then be able to build with:

   gprbuild bit_sets_tests.gpr
