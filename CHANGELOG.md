https://github.com/Lysxia/generic-random/blob/master/changelog.md

# 1.3.0.0

- Add `ConstrGen` (custom generators for fields specified by constructor name
  and index).
- Stop requiring custom generators lists to be terminated by `:+ ()`, or to be
  lists at all.
- Breaking minor change: when a record field has a different type than
  a `FieldGen` custom generator for the same field name, this is now a
  compilation error. This was simply ignored before.
- Miscellaneous documentation improvements in `Generic.Random` module.

# 1.2.0.0

- Fix a bug where generators did not decrease the size parameter with
  single-field constructors

- The sized generators now use a custom generator for lists.
  Use `genericArbitraryRecG ()` to disable that.
  See tutorial for more information.

- Lists of custom generators are now constructed using `(:+)` instead of
  `GenList`
- Rename `Field` to `FieldGen`
- Add `Gen1`, `Gen1_` (custom generators for unary type constructors)
- Add `listOf'`, `listOf1'`, `vectorOf'`
- Remove deprecated module `Generic.Random.Generic`

# 1.1.0.2

- Improved performance

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
