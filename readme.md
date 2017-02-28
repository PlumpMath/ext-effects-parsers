Monadic Parsers Combinatos library based on Extensible Effects

**Disclaimer:** This is not a production-ready code in any sense. Use it at you own risk (probably better don't)

This project uses extensible effects [implementation](https://gitlab.com/queertypes/freer) based on [freer monads](http://okmij.org/ftp/Computation/free-monad.html).

## Development

One source [dependency](https://gitlab.com/queertypes/freer) is required. Please, clone the repo and put it in directory named `freers` one level up, as required in `stack.yaml`, you can tweak this though.

To build this project you need to have haskell [stack](http://docs.haskellstack.org/en/stable/README.html) installed.

To build just run:

```
stack build
```

To test:

```
stack test
```
