# LispWorks Project Generator
### Version 1.0.1 (June 17th, 2013)

## Overview

LW-PROJECT-GENERATOR is a program that helps in the creation of cross-platform LispWorks projects.

## Features

Currently it provides two templates:

 -  `capi-application`

    For creating a CAPI based application with split lisp image and optional codesigning on Mac OS X.

 -  `tool`

    For creating a command-line based application.

Both project templates also have:

 -  An optional Git repository initialization with default `.gitignore` file,
 -  A `defsystem` definition,
 -  A `defpackage` definition,
 -  A load script,
 -  A build / deliver shell script that puts built product in a directory that depends on the built architecture,
 -  A basic top-level debugger hook,

Finally:

 -  The project generator will never overwrite existing files.
 -  It is easy to create new project templates.


## Examples

This creates a CAPI application project called `my-app` in the current working directory whose system is named `COM.WILDORA.MY-APP`.  A Git repository will be initialized and the produced application will be named "My App".

    CL-USER 1 > (project-generator:run "COM.WILDORA.MY-APP" "My App")
    ; Initialized empty Git repository in /Users/camille/lisp/my-app/.git/
    #P"/Users/camille/lisp/my-app/"

The new project is ready to build:

    tortilla:my-app$ cd my-app/
    tortilla:my-app$ sh build.sh

    [... lots of output ...]
    ; Delivery successful - /Users/camille/lisp/my-app/delivery/../build/Darwin-x64/My App.app/Contents/MacOS/My App
    ; *** Codesigning
    ; /usr/bin/codesign -s Developer ID Application /Users/camille/lisp/my-app/delivery/../build/Darwin-x64/My App.app 
    
    tortilla:my-app$


We can also create a command-line application using the `tool` project template.

    CL-USER 2 > (project-generator:run "COM.WILDORA.FROBULATOR" "frobulator"
                                       :project-template-name "tool"
                                       :destination "/net/projects/"
                                       :git-init-p nil)
    #P"/net/projects/frobulator/"


## Caveats

This is early release software, it has only been tested on Mac OS X.
Please apologize if it does not work on your operating system yet.

The lisp template generator does not output comments yet.


## Feedback and suggestions

I would love to receive feedback on how to improve this program.
I also need some testing on other platforms than Mac OS X.

Please contact: Camille Troillard <camille@osculator.net>
