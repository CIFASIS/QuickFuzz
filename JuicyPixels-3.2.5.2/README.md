![Juicy logo](https://raw.github.com/Twinside/Juicy.Pixels/master/docimages/juicy.png)

Juicy.Pixels
============

This library provides saving & loading of different picture formats for the
Haskell language. The aim of the library is to be as lightweight as possible,
you ask it to load an image, and it'll dump you a big Vector full of juicy
pixels. Or squared pixels, or whatever, as long as they're unboxed.

This version of Juicy.Pixels was modified to expose its internal types, allowing QuickFuzz to generate structuraly correct, but
usually malformed images.
