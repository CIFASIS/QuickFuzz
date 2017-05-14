# QuickFuzz

QuickFuzz, a tool written in Haskell designed for testing unexpected inputs of common file formats on third-party software,
taking advantage of off-the-shelf, well known fuzzers.
Unlike other generational fuzzers, QuickFuzz does not require
to write specifications for the file formats in question since it relies
on existing file-format-handling libraries available on the Haskell
code repository. QuickFuzz is open-source (GPL3) and it can use other bug detection tools like [zzuf](http://caca.zoy.org/wiki/zzuf), [radamsa](https://github.com/aoh/radamsa), [honggfuzz](http://google.github.io/honggfuzz/) and [valgrind](http://valgrind.org).

## News

* We will be presenting QuickFuzz in [C◦mp◦se 2017](http://www.composeconference.org/)!
* Our intern, Franco Costantini improved a lot the generation of random source code [enforcing variable coherence](https://github.com/CIFASIS/QuickFuzz/blob/gh-pages/variable-fix.md). This feature is enabled in Javascript, Lua, Python and Bash, but it can easily extended for other languages.
* QuickFuzz is now included in [Gentoo](https://packages.gentoo.org/packages/app-forensics/quickfuzz)!
* An academic article on QuickFuzz will be presented at the Haskell Symposium 2016 ([preprint](https://github.com/CIFASIS/QuickFuzz/releases/download/haskell16-draft/draft-haskell16.pdf))!

## **Bugs ~~lost and~~ found**

* Firefox ([failed assert in gif loader](https://bugzilla.mozilla.org/show_bug.cgi?id=1210745), [CVE-2016-1933](https://www.mozilla.org/en-US/security/advisories/mfsa2016-02/), [CVE-2015-7194](https://www.mozilla.org/en-US/security/advisories/mfsa2015-128/), [CVE-2015-7216, CVE-2015-7217](https://www.mozilla.org/en-US/security/advisories/mfsa2015-143/))
* VLC ([CVE-2016-3941](https://bugs.launchpad.net/ubuntu/+source/vlc/+bug/1533633))
* Libxml2 ([CVE-2016-3627](http://seclists.org/oss-sec/2016/q1/682), [CVE-2016-4483](http://seclists.org/oss-sec/2016/q2/214))
* Mxml ([CVE-2016-4570, CVE-2016-4571](http://www.openwall.com/lists/oss-security/2016/05/09/16))
* Cairo ([CVE-2016-3190](http://seclists.org/oss-sec/2016/q1/676))
* GraphicsMagick ( [CVE-2015-8808](http://seclists.org/oss-sec/2016/q1/288), [CVE-2016-2317, CVE-2016-2318](http://seclists.org/oss-sec/2016/q1/297) )
* LibGD ([CVE-2016-6132](http://seclists.org/oss-sec/2016/q2/636))
* Librsvg ([CVE-2015-7557, CVE-2015-7558](http://www.openwall.com/lists/oss-security/2015/12/21/5), [CVE-2016-4348](http://www.openwall.com/lists/oss-security/2016/04/28/7))
* Gdk-Pixbuf ([CVE-2015-7552](https://bugzilla.suse.com/show_bug.cgi?id=958963),  [CVE-2015-4491](https://www.mozilla.org/en-US/security/advisories/mfsa2015-88/), [CVE-2015-7674](http://www.openwall.com/lists/oss-security/2015/10/02/10), [CVE-2015-7673](http://www.openwall.com/lists/oss-security/2015/10/02/9), [CVE-2015-8875](http://seclists.org/oss-sec/2016/q2/355), undisclosed)
* Mplayer ([CVE-2016-4352](http://www.openwall.com/lists/oss-security/2016/04/29/7), [lots of crashes](https://lists.mplayerhq.hu/pipermail/mplayer-dev-eng/2015-December/073241.html) [and more](http://www.openwall.com/lists/oss-security/2015/11/10/8))
* Jasper ([CVE-2015-5203](https://bugzilla.redhat.com/show_bug.cgi?id=1254242))
* Jq ([CVE-2016-4074](http://www.openwall.com/lists/oss-security/2016/04/24/4))
* Jansson ([CVE-2016-4425](http://www.openwall.com/lists/oss-security/2016/05/02/1))
* Unzip ([CVE-2015-7696, CVE-2015-7697](http://www.openwall.com/lists/oss-security/2015/10/11/5))
* CPIO ([reads out-of-bound](http://seclists.org/oss-sec/2016/q1/440), [CVE-2016-2037](http://seclists.org/oss-sec/2016/q1/136))
* GNU Tar ([out-of-bound read](http://www.openwall.com/lists/oss-security/2015/08/31/1))
* Optipng ([CVE-2015-7802](http://www.openwall.com/lists/oss-security/2015/09/23/4), [CVE-2015-7801](https://bugzilla.redhat.com/show_bug.cgi?id=1264015))
* Libtiff ([CVE-2015-7313](http://www.openwall.com/lists/oss-security/2015/09/21/7))
* Busybox ([pointer misuse](http://www.openwall.com/lists/oss-security/2015/10/25/3))
* Libarchive ([big allocation in tar handling](https://bugs.launchpad.net/ubuntu/+source/libarchive/+bug/1487020))

## Quick introduction to QuickFuzz

In this example, we uncover a null pointer dereference in gif2webp from [libwebp 0.5](https://github.com/webmproject/libwebp/releases/tag/v0.5.0):

```
$ QuickFuzz test gif "./gif2webp @@ -o /dev/null" -l 1 -u 10 -f radamsa
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
$ QuickFuzz test gif "./gif2webp @@ -o /dev/null" -l 1 -s 3692945303624111961 -f radamsa -r
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

## List of file types to generate

<img src="https://rawgit.com/CIFASIS/QuickFuzz/gh-pages/images/file-formats.svg" width="400">

## Downloads

Pre-compiled and compressed (bzexe) binaries supporting all the file formats are available here:

* [Linux x86_64](https://circleci.com/api/v1/project/CIFASIS/QuickFuzz/latest/artifacts/0/$CIRCLE_ARTIFACTS/build/QuickFuzz.bzexe?filter=successful&branch=master)

Otherwise QuickFuzz can be [easily compiled](https://github.com/CIFASIS/QuickFuzz#instalation) using [stack](http://docs.haskellstack.org/en/stable/README/#how-to-install).

[![CircleCI](https://circleci.com/gh/CIFASIS/QuickFuzz.svg?style=svg)](https://circleci.com/gh/CIFASIS/QuickFuzz)

## Mailing list

You can join the [QuickFuzz mailing group](https://groups.google.com/forum/#!forum/QuickFuzz-users) to get notifications of new features and releases. To join, you can send an empty email to QuickFuzz-users+subscribe@googlegroups.com.

## Authors
### The QuickFuzz team

* Pablo **Buiras** ([Harvard University](http://people.seas.harvard.edu/~pbuiras/))
* Martín **Ceresa** ([CIFASIS-Conicet](http://cifasis-conicet.gov.ar/))
* Gustavo **Grieco** ([CIFASIS-Conicet](http://cifasis-conicet.gov.ar/) and [VERIMAG](http://www-verimag.imag.fr/?lang=en))
* Agustín **Mista** ([Universidad Nacional de Rosario](http://www.unr.edu.ar/))

### Students

* Franco **Costantini**
* Lucas **Salvatore**

### Former Members

* Martín **Escarrá** ([Universidad Nacional de Rosario](http://www.unr.edu.ar/))

### **Acknowledgements**

* [ayberkt](https://github.com/ayberkt) and [NineFx](http://ninefx.com/) for the bug reports and pull requests.
* [Sergei Trofimovich](https://github.com/trofi) for adding QuickFuzz to the official Gentoo repository and porting it to GHC8!
* Special thanks go to all the developers of the Hackage packages that make it possible for QuickFuzz to generate several complex file-formats.
