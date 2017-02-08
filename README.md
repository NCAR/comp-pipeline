# CoMP Pipeline code


## Installation

To build the CoMP pipeline code, your system must have IDL, the MySQL client development package, and CMake 3.1.3 or later. Make sure these are installed on your system before continuing.


### Configuring your system

To configure the CoMP pipeline for your system, do the following from the top-level of the pipeline source code (change the location of your IDL installation and the location where you want the pipeline to your needs):

    mkdir build
    cmake \
      -DCMAKE_INSTALL_PREFIX:PATH=~/software/comp-pipeline \
      -DIDL_ROOT_DIR:PATH=/opt/share/idl8.5/idl85 \
    ..

There are example configuration scripts, `linux_configure.sh` and `mac_configure.sh`, in the pipeline source code.


### Build and install

Next, run:

    cd build
    make install


## Running the pipeline

A configuration file is needed to run the pipeline. See the `config` directory of the installation for an example file with explanations for each option. Copy this configuration file to:

    comp.{username}.{machine_name}.cfg

with your username and machine name filled in. Edit this file, indicating where your data is and what options you want to run with.

To run the pipeline, from the installation root directory, do:

    make pipe


## Run unit tests

If you specified a valid [mgunit] location in your installation configuration, you can run the unit tests. To run the unit tests:

    make unit


## Generate API documentation

If you specified a valid [IDLdoc] location in your installation configuration, you can generate the API documentation. To generate the API documentation from the build directory, simply do:

    make doc



[mglib]: https://github.com/mgalloy/mglib "mgalloy/mglib"
[IDLdoc]: https://github.com/mgalloy/idldoc "mgalloy/idldoc"
[mgunit]: https://github.com/mgalloy/mgunit "mgalloy/mgunit"
