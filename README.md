vmm
===

A matrix library suited for graph algorithms.

Installation with Cabal
-----------------------

The simplest way is using Cabal which is part of the
[Haskell Platform](http://www.haskell.org/platform/).

If you are using git, then simply clone the project into a folder of your choice.
Otherwise download the zipped version and unpack it into a folder of your choice.

Assuming, all project files are located in `~/vmm` you can proceed as follows.

~~~{.sh}
bash> cd ~/vmm
bash> cabal update
bash> cabal install
~~~

The project is now installed and can be either used directly, e.g.:

~~~{.sh}
bash> ghci
ghci> :m VMM
VMM>
~~~

or imported into an own Haskell module via `import VMM`{.haskell}.

Documentation
-------------

You can browse the documentation online:

* [VMM](./src/VMM.html)
  is the main file. It contains the algorithms and examples.
* [RandomMatrix](./src/RandomMatrix.html)
  is an auxiliary file that takes care of the creation of random matrices.
* [Semiring](./src/Semiring.html)
  is another auxiliary file that contains the semiring type class and some common instances.

Alternatively, these files are all located in `src/` and can be viewed locally.