# CoMP Pipeline code


## Installation

To build the CoMP pipeline code, your system must have IDL, the MySQL client development package, and CMake 3.1.3 or later. Make sure these are installed on your system before continuing.


### Configuring your system

To configure the CoMP pipeline for your system, do the following from the top-level of the pipeline source code (change the location of your IDL installation and the location where you want the pipeline to your needs):

    mkdir build
    cmake \
      -DCMAKE_INSTALL_PREFIX:PATH=~/software/comp-pipeline \
      -DIDL_ROOT_DIR:PATH=/opt/share/idl8.5/idl85
    ..

There are example configuration scripts, `linux_configure.sh` and `mac_configure.sh`, in the pipeline source code.


### Build and install

Next, run:

    cd build
    make install


## Running the pipeline

A configuration file is needed to run the pipeline. See the `config` directory of the installation for an example file with explanations for each option. Copy this configuration file to:

    comp.{username}.{machine_name}.cfg

and edit it for where your data is and what options you want to run with.

To run the pipeline, from the installation root directory, do:

    make pipe


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
