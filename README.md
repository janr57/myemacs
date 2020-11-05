- [Program 'myemacs'](#org59fe9b2)
  - [What is 'myemacs'](#orgf093c24)
  - [Installation/Run the program](#org7972aaa)
    - [REPL](#orgdfd97ee)
    - [STANDALONE PROGRAM](#orgec0da7e)
    - [SCRIPT](#orgd0db17e)
  - [Usage](#org5f02c8c)
  - [Inner workings](#orga03bc4d)
  - [Author](#orgabf5f93)
  - [Copyright](#orgedf3c7e)
  - [License](#org56a0ac6)
    - [The 3-Clause BSD License](#org58364a9)


<a id="org59fe9b2"></a>

# Program 'myemacs'


<a id="orgf093c24"></a>

## What is 'myemacs'

This program makes it possible to have multiple emacs configurations, and switch between them at will.


<a id="org7972aaa"></a>

## Installation/Run the program

It is important to have a working 'quicklisp' installed. You can get the source code from 'github' (it is important to place it in the '~/quicklisp/local-projects' directory: `$ cd ~/quicklisp/local-projects/` `$ git clone https://github.com/janr57/myemacs` `$ cd myemacs` There are three different forms of using this program:

-   Using the REPL.
-   Using the standalone program `myemacs`
-   Using the script `myemacs_script`


<a id="orgdfd97ee"></a>

### REPL

This is the 'lispy way'. At the moment there are only two tested common lisps environments that work with 'myemacs':

-   SBCL.
-   Clozure Common Lisp, CCL.

Recommended use of these Lisps:

-   Using SLIME in 'emacs': `ALT-x slime`
-   At the command line: `$ rlwrap sbcl`
-   At the command line: `$ rlwrap ccl`

Once in the REPL we must load the package 'myemacs' first: `(ql:quickload :myemacs)` `(in-package :myemacs)` To test if it's working try: `(myemacs)` When you are done with the program but want to stay in the REPL, type: `(in-package :cl-user)`


<a id="orgec0da7e"></a>

### STANDALONE PROGRAM

To get en executable file you must enter the 'myemacs' directory: `$ cd ~/quicklisp/local-projects/myemacs` and type: `$ make myemacs_sbcl` or `$ make myemacs_ccl` depending on what supported common lisp environment you want to use. At the end you will see a 'myemacs<sub>sbcl</sub>' or 'myemacs<sub>ccl</sub>' executable file. Rename it as 'myemacs' and place it in any directory in the PATH. To test if this is working, try: `$ myemacs` if the file is in the path, or `$ ./myemacs` it it's not.


<a id="orgd0db17e"></a>

### SCRIPT

At the moment, this form of running the program is not documented yet. But it will be soon enough.


<a id="org5f02c8c"></a>

## Usage

**\*** Quick instructions `myemacs` or `myemacs :help` to get the commands that you can use. Before using 'myemacs' it is important to have a working NATIVE 'emacs' configuration. You can test if 'myemacs' detects it by typying: `$ myemacs :show` Once you have one that you want to manage with 'myemacs' then you cave to SAVE it. This is done by: `$ myemacs :save-native-as <cfg>` where <cfg> is a lower case string identifier for the saved configuration. Type again the ':show' command. Then you will se the NATIVE configuration and a saved 'cfg' one. In order to use 'cfg' you must delete the native configuration: `$ myemacs :del-native` and activate the saved 'cfg': `$ myemacs :use <cfg>` Now, typying 'emacs' you will use the saved configuration.


<a id="orga03bc4d"></a>

## Inner workings

The configurations are directories which contain an init file. These configuration directories must be of the form 'emacs.d-<cfg>', where <cfg> is the lowercase name of the configuration, which identifies the configuration. All these directories are place in a central directory '.myemacs.d'. Note that the configuration directories do not start with a dot. Examples: 'emacs.d-doom', 'emacs.d-personal', 'emacs.d-testing', etc.

When a configuration is made 'active' the program creates a soft link from './emacs.d' to one of the available configuration directories. The user is responsible for the


<a id="orgabf5f93"></a>

## Author

-   José A. Navarro (josea.navarro1@gmail.com)


<a id="orgedf3c7e"></a>

## Copyright

Copyright (c) 2020 José A. Navarro (josea.navarro1@gmail.com)


<a id="org56a0ac6"></a>

## License

Licensed under the BSD Clause-3 License.


<a id="org58364a9"></a>

### The 3-Clause BSD License

See <https://opensource.org/licenses/BSD-3-Clause>

Copyright 2020 José A. Navarro Ramón <josea.navarro1@gmail.com>

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1.  Redistribution of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2.  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.  Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.