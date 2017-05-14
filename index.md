QuickFuzz is a grammar fuzzer powered by [QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/), [Template Haskell](https://wiki.haskell.org/Template_Haskell) and specific libraries from Hackage to generate many complex file-formats like Jpeg, Png, Svg, Xml, Zip, Tar and more!. QuickFuzz is open-source (GPL3) and it can use other bug detection tools like [zzuf](http://caca.zoy.org/wiki/zzuf), [radamsa](https://github.com/aoh/radamsa), [honggfuzz](http://google.github.io/honggfuzz/) and [valgrind](http://valgrind.org).

## News

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

To generate corrupted gifs to test giffix using QuickFuzz and zzuf:

    $ QuickFuzz Gif "/usr/bin/giffix @@" -a zzuf -t 25 -s 10
    *** Error in `/usr/bin/giffix': double free or corruption (out): 0x0000000000b44f80 ***
    zzuf[s=-1193471787,r=0.004:1e-06]: signal 6 (SIGABRT)
    *** Error in `/usr/bin/giffix': free(): invalid pointer: 0x0000000002565f80 ***
    zzuf[s=1436598283,r=0.004:1e-06]: signal 6 (SIGABRT)
    zzuf[s=88548751,r=0.004:1e-06]: signal 11 (SIGSEGV)
    +++ OK, passed 25 tests.

It looks like we re-discovered several files to trigger [CVE-2015-7555](https://bugzilla.redhat.com/show_bug.cgi?id=1290785) in a few seconds! QuickFuzz can also print the structure of the generated file that triggered a crash in Haskell syntax. For instance:

    GifFile {
             gifHeader = GifHeader {gifVersion = GIF87a, gifScreenDescriptor = LogicalScreenDescriptor {screenWidth = 1, screenHeight = 0, backgroundIndex = 1, hasGlobalMap = True, colorResolution = 0, isColorTableSorted = True, colorTableSize = 1}, gifGlobalMap = }, 
             gifImages = [(Just GraphicControlExtension {gceDisposalMethod = DisposalRestorePrevious, gceUserInputFlag = True, gceTransparentFlag = True, gceDelay = 1, gceTransparentColorIndex = 0},GifImage {imgDescriptor = ImageDescriptor {gDescPixelsFromLeft = 1, gDescPixelsFromTop = 1, gDescImageWidth = 0, gDescImageHeight = 1, gDescHasLocalMap = False, gDescIsInterlaced = False, gDescIsImgDescriptorSorted = False, gDescLocalColorTableSize = 0}, imgLocalPalette = Just , imgLzwRootSize = 0, imgData = ""})], 
             gifLoopingBehaviour = LoopingForever
            }

## List of file types to generate

<img src="https://rawgit.com/CIFASIS/QuickFuzz/gh-pages/images/file-formats.svg" width="400">

## Downloads

Pre-compiled and compressed (bzexe) binaries are available here:

* [Linux x86 (Outdated)](https://github.com/CIFASIS/QuickFuzz/releases/download/v0.1/QuickFuzz.x86)
* [Linux x86_64](https://circleci.com/api/v1/project/CIFASIS/QuickFuzz/latest/artifacts/0/$CIRCLE_ARTIFACTS/build/QuickFuzz.bzexe?filter=successful&branch=master)

Otherwise QuickFuzz can be [easily compiled](https://github.com/CIFASIS/QuickFuzz#instalation) using [stack](http://docs.haskellstack.org/en/stable/README/#how-to-install).

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