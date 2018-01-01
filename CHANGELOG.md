https://github.com/Lysxia/generic-random/blob/master/changelog.md

# 1.1.0.1

- Fix build for GHC<8

# 1.1.0.0

- Add option to specify custom generators for certain fields,
  overriding Arbitrary instances
  + Add `genericArbitraryG`, `genericArbitraryUG`, `genericArbitrarySingleG`,
    `genericArbitraryRecG`
- Add `GArbitrary` and `GUniformWeight` synonyms
- Deprecate `Generic.Random.Generic`
- Remove `weights` from the external API

# 1.0.0.0

- Make the main module `Generic.Random`
- Rework generic base case generation
  + You can explicitly provide a trivial generator (e.g., returning a
    nullary constructor) using `withBaseCase`
  + Generically derive `BaseCaseSearch` and let `BaseCase` find small
    values, no depth parameter must be specified anymore
- Add `genericArbitrarySingle`, `genericArbitraryRec`, `genericArbitraryU'`
- Deprecate `weights`
- Fixed bug with `genericArbitrary'` not dividing the size parameter

# 0.5.0.0

- Turn off dependency on boltzmann-samplers by default
- Add `genericArbitraryU`, `genericArbitraryU0` and `genericArbitraryU1`
- Compatible with GHC 7.8.4 and GHC 7.10.3

# 0.4.1.0

- Move Boltzmann sampler modules to another package: boltzmann-samplers

# 0.4.0.0

- Check well-formedness of constructor distributions at compile time.
- No longer support GHC 7.10.3 (the above feature relies on Generic
  information which does not exist before GHC 8)

# 0.3.0.0

- Support GHC 7.10.3
- Replace `TypeApplications` with ad-hoc data types in
  `genericArbitraryFrequency'`/`genericArbitrary'`
