# memorable-password-generator

This is a command-line tool to generate random passphrases using [EFF's New Wordlists for Random Passphrases](https://www.eff.org/deeplinks/2016/07/new-wordlists-random-passphrases) with a handful of random symbols and numbers. It uses [Megaparsec](https://hackage.haskell.org/package/megaparsec) parser combinators to parse the wordlist. All of the randomization is abstracted as a [Polysemy](https://hackage.haskell.org/package/polysemy) `State` effect to carry forward mutations in the internal state of the standard pseudo-random number generator.

Wordlist license: [Creative Commons Attribution License](https://creativecommons.org/licenses/by/3.0/us/legalcode)
