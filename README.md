# puzzle-generator

## Description

This is a small command line program to generate jigsaw puzzles from images. All pieces are blank,
there is no color. Instead the image is created by varying the curvature of the puzzle pieces.
In the Data folder there is a source image and the puzzle generated from it.


## Usage

The program is based on the [Diagrams](http://projects.haskell.org/diagrams/) package.
To compile, just use `ghc Main.hs`. Then you can use all the options available in the
Diagrams package. See the website for details. The last argument should be the name of the
input image. So, for example, `./Main -w 1000 -o test.svg test.jpg` generates an svg file
named test whose width is 1000 pixels from an image named test.jpg which is in the same
folder with Main.

