# QuickFuzz

QuickFuzz, a tool written in Haskell designed for testing un-
expected inputs of common file formats on third-party software,
taking advantage of off-the-shelf, well known fuzzers.
Unlike other generational fuzzers, QuickFuzz does not require
to write specifications for the file formats in question since it relies
on existing file-format-handling libraries available on the Haskell
code repository. There is more information in its [website](http://quickfuzz.cifasis-conicet.gov.ar/).

[![CircleCI](https://circleci.com/gh/CIFASIS/QuickFuzz.svg?style=svg)](https://circleci.com/gh/CIFASIS/QuickFuzz)


## Example

In this example, we uncover a null pointer dereference in gif2webp from [libwebp 0.5](https://github.com/webmproject/libwebp/releases/tag/v0.5.0):

```
$ QuickFuzz gentest gif "./gif2webp @@ -o /dev/null" -l 1 -u 10 -f radamsa
...
Test case number 4481 has failed. 
Moving to outdir/QuickFuzz.68419739009.4481.3692945303624111961.1.gif
...
```

We found a crash. We can inspect it manually to verify it is a null pointer issue:

```
$ ./gif2webp outdir/QuickFuzz.68419739009.4481.3692945303624111961.1.gif
==10953== ERROR: AddressSanitizer: SEGV on unknown address 0x000000000000 
(pc 0x000000403ff9 sp 0x7fffffffd6e0 bp 0x7fffffffded0 T0)
AddressSanitizer can not provide additional info.
#0 0x403ff8 (examples/gif2webp+0x403ff8)
#1 0x7ffff437af44 (/lib/x86_64-linux-gnu/libc-2.19.so+0x21f44)
#2 0x401b18 (examples/gif2webp+0x401b18)
==10953== ABORTING
```

Finally, we can shrink the crashing input to obtain a smaller file:

```
$ QuickFuzz gentest gif "./gif2webp @@ -o /dev/null" -l 1 -s 3692945303624111961 -f radamsa -r
Test case number 1 has failed. 
Moving to outdir/QuickFuzz.68997856397.1.3692945303624111961.1.gif
Shrinking over bytes has begun...
Testing shrink of size 48
Testing shrink of size 47
...
Testing shrink of size 15
Shrinking finished
Reduced from 48 bytes to 16 bytes
After executing 554 shrinks with 33 failing shrinks. 
Saving to outdir/QuickFuzz.68997856397.1.3692945303624111961.1.gif.reduced
Finished!
```

## Installation

We **support** [Stack](www.haskellstack.org) to compile and install QuickFuzz. Before starting with it, make sure you have libgmp-dev installed otherwise ghc will fail to compile. Also, zlib.h is required to compile QuickFuzz (some packages require it). For instance, in Ubuntu/Debian:

    # apt-get install zlib1g-dev libgmp-dev libtinfo-dev

After [installing stack](http://docs.haskellstack.org/en/stable/README/#how-to-install), you should:

    $ git clone https://github.com/CIFASIS/QuickFuzz --depth 1
    $ cd QuickFuzz
    $ install_fuzzers.sh
    $ stack setup

Because *QuickFuzz* generates a lot of dependencies that may not be necessary to test an specific category of files, we modularized the project with different activation flags. Currently we have 7 flags:

|  Flag  |  Supported formats           |
|-------:|:-----------------------------|
|  image | svg, png, gif, tiff, jpeg    |
|   arch | tar, zip                     |
|    doc | html, css, pdf, ps, eps, xml |
|   code | c, js, py, go, lua, evm      |
|  media | wav                          |
|    net | http                         |
|    pki | asn1, crl, x509              |
|   bnfc | cf, grammer format           |

For instance, to compile only with image generation (Bmp, Gif, Png, Ico, ..):

    $ stack install --flag QuickFuzz:image

Because of a Stack issue, you must install `alex` and `happy` manually before enabling the `code` flag:

    $ stack install alex happy

### Cabal Installation

Direct cabal installation is **not** recommended nor supported.

## Authors

* Pablo **Buiras** ([Chalmers University of Technology](http://www.chalmers.se/en/Pages/default.aspx))
* Martín **Ceresa** ([CIFASIS-Conicet](http://cifasis-conicet.gov.ar/))
* Gustavo **Grieco** ([CIFASIS-Conicet](http://cifasis-conicet.gov.ar/) and [VERIMAG](http://www-verimag.imag.fr/?lang=en))
* Agustín **Mista** ([Universidad Nacional de Rosario](http://www.unr.edu.ar/))

### Students

* Franco **Costantini**
* Lucas **Salvatore**

### Former Members

* Martín **Escarrá** ([Universidad Nacional de Rosario](http://www.unr.edu.ar/))

## Mailing list

You can join the [QuickFuzz mailing group](https://groups.google.com/forum/#!forum/QuickFuzz-users) to get notifications of new features and releases. To join, you can send an empty email to QuickFuzz-users+subscribe@googlegroups.com.
