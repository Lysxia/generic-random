# 0.4.1.0

- Move Boltzmann sampler modules to another package: boltzmann-samplers

# 0.4.0.0

- Check well-formedness of constructor distributions at compile time.
- No longer support GHC 7.10.3 (the above feature relies on Generic
  information which does not exist before GHC 8)

# 0.3.0.0

- Support GHC 7.10.3
- Replace TypeApplications with ad-hoc data types in
  genericArbitraryFrequency'/genericArbitrary'
