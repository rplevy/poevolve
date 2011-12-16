POEVOLVE
========

A naive model of human creativity / dadaistic song-writing code.

Thesis
------

Code that I wrote as my college thesis project at the [State University of New York at Oswego](http://www.oswego.edu), circa 1999-2000.  Implementing a naive demonstration of potential paradigm of human creativity: blind variation and selective retention (evolutionary computation), with a trained neural network standing in for human subjective judgement (fitness evaluator).

I have left this code untouched for posterity's sake.  It is a code time capsule.

A full account of the theory, model, documentation of code, and results of computational experiments can be found in thesis/doc/levythesis2000.pdf

Song Writer
-----------

A stripped down POEVOLVE that I pulled out a few years later is a useful piece of code that I have never bothered to improve but have used many times.  It generates random lyrics based on syllabic stress templates.

To use, create a template (see the included example templates *.sng).

```
sudo apt-get install clisp
chmod +x ./song-writer.l
./song-writer.l sonnet.sng
```

Copyright (C) 1999-2000 Robert P. Levy
------------------------------------------

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.