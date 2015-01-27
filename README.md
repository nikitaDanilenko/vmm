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

or imported into an own Haskell module via `import VMM`.

Documentation
-------------

You can browse the documentation online:

### Markdown version

* [VMM](./src/VMM.md) 
  is the main file. It contains the algorithms and examples.
* [RandomMatrix](./src/RandomMatrix.md)
  is an auxiliary file that takes care of the creation of random matrices.
* [Semiring](./src/Semiring.md)
  is another auxiliary file that contains the semiring type class and some common instances.

### Literate Haskell version

* [VMM (*.lhs version)](./src/VMM.lhs) 
  is the main file. It contains the algorithms and examples.
* [RandomMatrix](./src/RandomMatrix.lhs)
  is an auxiliary file that takes care of the creation of random matrices.
* [Semiring](./src/Semiring.lhs)
  is another auxiliary file that contains the semiring type class and some common instances.

Alternatively, you can view these files locally as `.lhs` files directly or as precompiled
`.html` files. All files are located in the `/src` folder.
