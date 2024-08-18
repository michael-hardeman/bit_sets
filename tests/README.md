Bit_Sets Tests
==============

These are unit tests for the Bit_Sets library.

Running
-------

This project was build with alire and depends on it to run.

If you have alire installed run it with:

    alr run

If you don't you should really give it a try but you can still run without it.
You should be able to copy the config project file from
[share/bit_sets_tests/bit_sets_test_config.gpr](./share/bit_sets_tests/bit_sets_test_config.gpr)
to `config/bit_sets_test_config.gpr` and modify the `with "aunit.gpr"` to point to a valid AUnit
project and it will run with:

   gprbuild bit_sets_tests.gpr
