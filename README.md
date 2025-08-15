[![Haskell CI](https://github.com/fpringle/effectful-postgresql/actions/workflows/haskell.yml/badge.svg)](https://github.com/fpringle/effectful-postgresql/actions/workflows/haskell.yml)

# effectful-postgresql

These packages lift PostgreSQL functionality in Haskell from `IO` to `Eff`.

See the package READMEs for more detail:

- [effectful-postgresql](./effectful-postgresql#readme)
- [effectful-opaleye](./effectful-opaleye#readme)

## Note to self

The documentation uploaded to Hackage should be built using `effectful-th >= 1.0.0.3`. This will take the haddocks of the `Opaleye` constructors and use them as the haddocks of the corresponding TH-generated functions.
