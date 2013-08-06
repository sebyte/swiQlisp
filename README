# swiQlisp — site-wide Quicklisp

Tools for installing Quicklisp site-wide and then managing that installation
(using a lisp implementation of your choice).

Works by creating an unprivileged system user (default swiqlisp) whose home
directory is in a site-wide location (default /usr/share/common-lisp/swiqlisp)
and then running lisp as that system user.  Symbolic links pointing to system
definition files are gathered together under a single directory;
~swiqlisp/installed-systems/.

Unprivileged users can then use ASDF:LOAD-SYSTEM to load these site-wide
installed Quicklisp libraries by simply adding ~swiqlisp/installed-systems/ to
their ASDF:*CENTRAL-REGISTRY*.  To assist with this last step, the system
administrator is asked whether or not she would like to add a stanza to the
site-wide lisp initialisation file to this effect when installing swiqlisp.


## Installation

```shell
sudo ./swiqlisp-install
```

## Usage

```shell
[sudo] swiqlisp <query-or-action> [<system>] [<lisp>]
```
Queries don't require root privileges.  Actions do.

Queries implemented so far:

 releases
 systems
 release-systems
 system-apropos
 depends
 versions

Actions implemented so far:

 install
 install-no-compile
 uninstall
 update-systems
 downdate-systems
 self-update
 refresh-system-links

Default lisp: sbcl

## swiQlisp SLIME usage

'swank' is the name of the Common Lisp component of SLIME (the Superior Lisp
Interaction Mode for Emacs) the rest of which is written in Emacs Lisp.  For
this reason, SLIME is called 'swank' in Quicklisp (since Quicklisp is a Common
Lisp library manager, not an Emacs Lisp library manager) so first of all you
must install the Quicklisp package 'swank':

```shell
sudo swiqlisp install swank
```
Do not install the package 'quicklisp-slime-helper'.  It assumes Quicklisp is
installed under your home directory and will not work as expected.

Then add the following line:

```elisp
 (load (expand-file-name "~swiqlisp/swiqlisp-slime-helper.el"))
```
to your ~/.emacs init file (and remove any existing Quicklisp SLIME
configuration).

Finally, restart Emacs and you should be good to go.

## Uninstallation

Installation includes an uninstallation script (/usr/bin/swiqlisp-uninstall) so
to uninstall swiQlisp, simply run the script:

```shell
swiqlisp-uninstall
```
If you confirm your decision, swiQlisp and all installed systems will be
removed and the unprivileged system user (swiqlisp) will be deleted.

## TODO

 • Improve install script so that it leaves an existing Quicklisp untouched but
   updates all swiQlisp files.

 • Fix non-privileged queries.

 • Work out precisely why, with a fresh swiqlisp, 'install' hunchentoot says
   that "30 newly installed systems are now availiable" but if you then run
   'systems' you'll see that 35 systems are in fact installed.

 • Invocations for lisps other than SBCL.

 • Package collections.


Local Variables:
mode:markdown
End:
