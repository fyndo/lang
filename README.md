# lang

A Common Lisp toolkit for generating fictional languages — phonology, sound
change, morphology, syntax, and lexicon — and translating English semantic
structures into them.

The pipeline, roughly:

1. **Phonology.** Build a consonant inventory from a typological frequency
   table (`freq.lisp`) over the IPA data in `consonants.csv` / `vowels.csv`,
   grown outward with `symmetrize` so the result looks like a real inventory
   rather than a random set. Syllables are assembled onset–nucleus–coda under
   a sonority hierarchy.
2. **Grammar.** `grammar.lisp` picks a typology (constituent order, case vs.
   agreement, adposition side, relative-clause strategy, …) and mints
   morphemes for the features that typology needs.
3. **Lexicon.** `vocabulary.lisp` supplies English glosses with semantic tags;
   `define-word` coins forms for them, then derivation, compounding, and
   paradigm generation fill the rest out.
4. **Diachrony.** `derive-language` applies ordered sound-change rules to a
   parent language, producing daughters with consistent cognates.
   `loanwords.lisp` handles borrowing between contemporaries, and
   `english.lisp` runs the trick in reverse — annealing proto-forms whose
   descendants land on target English pronunciations.
5. **Rendering.** `semantics.lisp` defines language-neutral meaning structures
   (nouns, clauses, possessives, relative clauses, conditionals, …), which
   `render` linearizes and inflects per language.

## Requirements

SBCL (the demos use `sb-ext:seed-random-state`), ASDF, and these libraries:
`cl-ppcre`, `iterate`, `parse-number`, `fare-csv`, `closer-mop`.

## Running

```lisp
(asdf:load-system :lang)
(in-package :lang)

(load "demo.lisp")            ; world-building demo
(run-demo)                    ; six proto-languages + variants, demo phrases
(run-sample)                  ; narrative excerpts from sample.txt
(run-stress)                  ; ~55 sentences exercising every construct

(load "demo-english.lisp")    ; English back-formation demo
(run-english)                 ; halfling+orc -> creole -> imperial trade tongue
```

Every entry point takes `:seed` and reseeds the RNG, so runs are reproducible.
`demo-output.txt` and `english-demo-output.txt` are captured runs.

## The phone inventory

`consonants.csv` and `vowels.csv` are what the generator actually loads: 59
consonants and 27 vowels, each with a romanization the full IPA chart does not
carry. They were cut from `ipa-data.csv` — every row matches it by IPA number,
modulo `dental;alveolar` collapsed to `alveolar` — and the 61 rows left behind
are withdrawn, superseded, or non-IPA symbols plus the clicks, implosives, and
releases that `remove-weird-consonants` strips at load time anyway. The unit
affricate ligatures (ʦ ʣ ʧ ʤ ʨ ʥ) are among the superseded ones; modern IPA
writes those as stop+fricative sequences, which is how `english.lisp` treats
them. Keep `ipa-data.csv` for provenance; regenerate the inventory from it if
you ever want a phone back, and re-add the romanization column by hand.

## Tests

```lisp
(asdf:test-system :lang)      ; borrowing and marker-disambiguation checks
```

They assert rather than report, so a failure signals. `test-deep-chain.lisp` is
a manual driver with no assertions — load it and call `run-deep-chain`.

## Layout

| File | Contents |
| --- | --- |
| `lang.lisp` | Phones, inventories, syllables, sound change, language objects, serialization |
| `freq.lisp` | Cross-linguistic phoneme frequency tables |
| `loanwords.lisp` | Borrowing and phonological adaptation between languages |
| `semantics.lisp` | Language-neutral meaning structures |
| `grammar.lisp` | Typology selection, morphology, and rendering |
| `vocabulary.lisp` | Glosses, semantic tags, derivation and compound specs |
| `english.lisp` | English-target back-formation by simulated annealing |
| `demo.lisp`, `demo-english.lisp` | Demo drivers (loaded manually, not part of the system) |
| `consonants.csv`, `vowels.csv` | The phone inventory the generator draws from |
| `ipa-data.csv` | Full IPA chart the two inventory files were cut from; reference only, nothing loads it |

## License

GPL-3.0-or-later. See `LICENSE`.
