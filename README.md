# CoMP Pipeline code


## Run unit tests

To run the unit tests:

    make unit

This requires an [mgunit] installation. The Makefile will look for mgunit in your `software` directory, but any location can be specified:

    make unit MGUNIT_DIR=$HOME/projects/mgunit/src


## Generate API documentation

To generate the API documentation:

    make doc

This requires an [IDLdoc] and [mglib] installation. The Makefile will look for IDLdoc in your `projects` directory and mglib in your `software` directory, but any location can be specified:

    make doc IDLDOC_DIR=$HOME/software/idldoc/src MGUNIT_DIR=$HOME/projects/mglib/src



[mglib]: https://github.com/mgalloy/mglib "mgalloy/mglib"
[IDLdoc]: https://github.com/mgalloy/idldoc "mgalloy/idldoc"
[mgunit]: https://github.com/mgalloy/mgunit "mgalloy/mgunit"
