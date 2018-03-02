levy
=========

### Configure:

```sh
nix-shell --command "cabal configure --enable-tests --enable-coverage"
```

### Build:

```sh
cabal build
```

### Test:

```sh
cabal test
```

### Regenerate Nix file:

After changing `levy.cabal`, you should run:

```sh
nix-env -iA nixpkgs.haskellPackages.cabal2nix && \
cabal2nix . >levy.nix && \
nix-shell --command "cabal configure --enable-tests --enable-coverage"
```

and commit the change to the updated `levy.nix` file.
