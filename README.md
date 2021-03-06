- [Program 'myemacs'](#orgdcdfab5)
  - [What is 'myemacs'](#orgdcad717)
  - [Motivation](#org8c2b0a5)
  - [Installation/Run the program](#orgbdcd702)
    - [REPL](#orgb21c9e3)
    - [STANDALONE PROGRAM](#orgeaf3462)
    - [SCRIPT](#orgc2256d3)
  - [Usage](#org5d1d2ff)
  - [Inner workings](#org9b6999c)
  - [Author](#org028fcae)
    - [Disclaimer](#org310f24b)
  - [Copyright](#orgef3dcd4)
  - [License](#orgd6d2522)
    - [The 3-Clause BSD License](#orgbab1910)


<a id="orgdcdfab5"></a>

# Program 'myemacs'


<a id="orgdcad717"></a>

## What is 'myemacs'

This program makes it possible to have multiple emacs configurations, and switch between them at will.


<a id="org8c2b0a5"></a>

## Motivation

No special order

-   I am interested in learning common lisp. Hence there is no other way but to program in common lisp. and learn from errors.
-   I find this program useful because I am always testing different emacs configurations in order to find the ones that suit me best, and doing it by making, moving and renaming directories, creating and deleting symlinks, &#x2026; is a very error prone task.
-   I find stimulating and enjoy programming in lisp more than in other languages. That said, I don't refuse to use other languages, in fact, learning common lisp might help me to get the most out of other programming languages, as well.


<a id="orgbdcd702"></a>

## Installation/Run the program

It is important to have a working 'quicklisp' installed. You can get the source code from 'github' (it is important to place it in the '~/quicklisp/local-projects' directory:

`$ cd ~/quicklisp/local-projects/`

`$ git clone https://github.com/janr57/myemacs`

`$ cd myemacs`

There are three different forms of using this program:

-   Using the REPL.
-   Using the standalone program `myemacs`
-   Using the script `myemacs_script`


<a id="orgb21c9e3"></a>

### REPL

This is the 'lispy way'. At the moment there are only two tested common lisps environments that work with 'myemacs':

-   SBCL.
-   Clozure Common Lisp, CCL.

Recommended use of these Lisps:

-   Using SLIME in 'emacs': `M-x slime`
-   At the command line: `$ rlwrap sbcl`
-   At the command line: `$ rlwrap ccl`

Once in the REPL we must load the package 'myemacs' first:

`(ql:quickload :myemacs)`

`(in-package :myemacs)`

To test if it's working try: `(myemacs)`

When you are done with the program but want to stay in the REPL, type: `(in-package :cl-user)`


<a id="orgeaf3462"></a>

### STANDALONE PROGRAM

To get en executable file you must enter the 'myemacs' directory: `$ cd ~/quicklisp/local-projects/myemacs`

and type: `$ make myemacs_sbcl` or `$ make myemacs_ccl`

depending on what supported common lisp environment you want to use. At the end you will see a 'myemacs<sub>sbcl</sub>' or 'myemacs<sub>ccl</sub>' executable file. Rename it as 'myemacs' and place it in any directory in the PATH.

To test if this is working, try: `$ myemacs` if the file is in the path, or `$ ./myemacs` it it's not.


<a id="orgc2256d3"></a>

### SCRIPT

At the moment, this form of running the program is not documented yet. But it will be soon enough.


<a id="org5d1d2ff"></a>

## Usage

**\*** Quick instructions `myemacs` or `myemacs :help` to get the commands that you can use.

Before using 'myemacs' it is important to have a working NATIVE 'emacs' configuration. You can test if 'myemacs' detects it by typying: `$ myemacs :show`

Once you have a native configuration that you want to manage with 'myemacs' then you cave to SAVE it. This is done by:

`$ myemacs :save-native-as <cfg>`

where <cfg> is a lower case string identifier for the saved configuration.

Type again the ':show' command. Then you will see the NATIVE configuration and a saved 'cfg' one. In order to use the saved 'cfg' one you must delete the native configuration:

`$ myemacs :del-native`

and activate the saved 'cfg':

`$ myemacs :use <cfg>`

Now, typying 'emacs' you will use the saved configuration.

There are more things you can do, like copying a saved configuration to any other name, restoring the native configuration from one of the saved ones&#x2026;

One last thing: The command

`$ myemacs :show`

will give you hints of what to do with the found configuration.


<a id="org9b6999c"></a>

## Inner workings

The configurations are directories which contain an init file. These configuration directories must be of the form 'emacs.d-<cfg>', where <cfg> is the lowercase name of the configuration, which identifies the configuration. All these directories are place in a central directory '.myemacs.d'. Note that the configuration directories do not start with a dot. Examples: 'emacs.d-doom', 'emacs.d-personal', 'emacs.d-testing', etc.

When a configuration is made 'active' the program creates a soft link from './emacs.d' to one of the available configuration directories. The user is responsible for the proper working of the configuration in emacs.


<a id="org028fcae"></a>

## Author

-   José A. Navarro (janr-devel@gmail.com)


<a id="org310f24b"></a>

### Disclaimer

English is not my native language, so expect to bump into wrong expressions more often than not :)


<a id="orgef3dcd4"></a>

## Copyright

Copyright (c) 2020 José A. Navarro (janr-devel@gmail.com)


<a id="orgd6d2522"></a>

## License

Licensed under the BSD Clause-3 License.


<a id="orgbab1910"></a>

### The 3-Clause BSD License

See <https://opensource.org/licenses/BSD-3-Clause>

Copyright 2020 José A. Navarro Ramón <janr-devel@gmail.com>

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1.  Redistribution of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2.  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.  Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.