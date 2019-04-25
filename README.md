# heightmap-gen

A simple Clojure library for doing basic heightmap generation for games or 
other graphical programs.

Heightmaps are generated as a grayscale image of NxN pixels where N = 2^x + 1 
for some x. Can be run directly to generate PNG files, or can be called 
as a library to generate the image as a 1D vector of floating-point numbers.

Functionality included at time of writing:
- Diamond-square algorithm
- White noise function
- Various basic filters

![Example heightmap](https://raw.githubusercontent.com/brundonsmith/heightmap-gen/master/test.png)

## Usage

TODO

## License

Copyright © 2019 Brandon Smith

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
