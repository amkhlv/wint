# wint

Semi-automatic window tiler. It uses the configuration files of [wmjump](https://github.com/amkhlv/wmjump) for highlighting.

# Installation

This program requires `wmctrl`:

    aptitude install wmctrl

Also, to build this program, need to install some dev libraries:

    aptitude install libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev

# Sample config file

See `wint.xml`. It should be copied into `~/.wmjump/`

# Use

The `wint` window contains:

1. A char-hinted list of windows on the current desktop

2. A command line at the bottom

In the command line, type
the description of the desired layout, for example:

    a,tex c,pdf

and press `Enter`.

The window charhinted `a` will be laid out
according to the `tex` model, and the window charhinted `c` according to the `pdf` model. The models are defined
in the `wint.xml` config file.
