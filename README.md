# *Sequencer* – purely algorithmic number sequence identification

Sequencer identifies number sequences. That is, given a list of numbers like

```
(a(n)) = 1, 2, 4, 8, 16, 32, ...
```

it finds a formula that generates them, in this case

```
a(n) = 2^(n-1)   for n >= 1
```

Sequencer employs neither a library of sequences nor a limited set of algorithms to find a closed form. Instead, it generates **all** formulas up to a certain size and then checks them against the provided numbers.

For verification, the system uses a hybrid approach of a fast numerical checker followed by a symbolic verifier powered by the [Symja](https://bitbucket.org/axelclk/symja_android_library/wiki/Home) computer algebra system. Coupled with some tricks and heuristics designed to quickly generate potentially interesting formulas, Sequencer can identify sequences with very complex closed forms in a matter of seconds when run on commodity hardware.

Sequencer is capable of finding closed forms that are beyond any existing system like [OEIS](http://oeis.org/), [Superseeker](http://oeis.org/ol.html) and [Wolfram Alpha](http://www.wolframalpha.com/). It is particularly strong where recurrence relations or unusual combinations of functions are involved. For example, none of the services mentioned above can currently make sense of the sequence

```
(a(n)) = 1, 1, 1, 3, 5, 15, 43, 273, ...
```

while Sequencer reveals that it satisfies the recurrence relation

```
a(1) = 1
a(2) = 1
a(3) = 1
a(n) = a(n-2)^2+a(n-1)+a(n-3)   for n >= 4
```

and provides the continuation

```
2137, 76709, 4643751, 5888916569, 21570312343279, ...
```

## Installation and usage

Sequencer requires [Java](https://www.java.com) to run. Download the latest Sequencer JAR from the [releases page](https://github.com/p-e-w/sequencer/releases) and execute it from a terminal with the numbers to be matched as arguments, i.e.

```
java -jar sequencer.jar 1 2 3 4 5
```

Running the program without arguments displays a help text explaining the various command line parameters that can be used to fine-tune how searches are performed.

## Development

Sequencer is written in Scala. To compile Sequencer from source, you need [Git](http://www.git-scm.com/), a [JDK](http://www.oracle.com/technetwork/java/index.html), the [Scala compiler](http://www.scala-lang.org/), and [sbt](http://www.scala-sbt.org/). Once all of these are installed and on your `PATH`, you are ready to build and run Sequencer:

```
git clone https://github.com/p-e-w/sequencer.git
cd sequencer
sbt run
```

The release JAR can then be created using

```
sbt assembly
```

and will be located at `target/scala-X.XX/sequencer.jar`.

To develop Sequencer using a Scala IDE, have sbt generate project files with a plugin like [sbteclipse](https://github.com/typesafehub/sbteclipse) or [sbt-idea](https://github.com/mpeltonen/sbt-idea).

## Credits

Besides its runtime and compilation environment (Java, Scala and sbt), Sequencer depends on the [Symja](https://bitbucket.org/axelclk/symja_android_library/wiki/Home) computer algebra system and the [scopt](https://github.com/scopt/scopt) command line parser. The standalone release JARs are built using the excellent [sbt-assembly](https://github.com/sbt/sbt-assembly) plugin.

## License

Copyright © 2015 Philipp Emanuel Weidmann (<pew@worldwidemann.com>)

Released under the terms of the [GNU General Public License, Version 3](https://gnu.org/licenses/gpl.html)
