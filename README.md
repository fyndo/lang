# lang

A toolkit for generating fictional languages (conlangs): phoneme inventories,
lexicons, grammars, sound-change–derived variants, and contact languages
(pidgins/creoles).

## Backward evolution — reconstructing an ancestor from a target

`derive-language` walks a proto-language *forward* into a descendant by applying
an ordered chain of sound-change clauses. The backward-evolution engine runs
that machinery in reverse: given a modern **target** language and the changes
believed to have produced it, it inverts each clause, reverses their order, and
applies them to the target to reconstruct a plausible **ancestor**.

Sound change is many-to-one (mergers, neutralizations), so an inverse is a
*hypothesis*, not a unique pre-image — the situation a historical linguist
faces. A clause describing an irreversible merger (its right-hand side sets a
feature whose original value the left-hand side never pinned down) is reported
and skipped rather than guessed.

### API

- `(invert-spec-clause clause)` — invert one sound-change clause, e.g.
  `(C :voicing voiced :manner plosive -> :manner fricative)` becomes
  `(C :voicing voiced :manner fricative -> :manner plosive)`. Returns
  `(values nil reason)` for an irreversible merger.
- `(invert-transformer-spec specs)` — invert a whole chain: reverses the order
  and inverts each clause. Returns `(values inverted dropped)`, where `dropped`
  pairs each un-invertible clause with the reason it was skipped.
- `(back-derive-language target forward-specs &key name)` — reconstruct an
  ancestor of `target` by applying the inverted `forward-specs` to its lexicon
  and grammar. Returns a `reconstructed-language` whose `source` is the target,
  `forward-specs` are the original changes, and `dropped-changes` records the
  mergers that could not be undone.

### Judge a reconstruction by evolving it forward

An inverse is only as good as the forward derivation it predicts, so don't
trust the inverse — check it. A reconstruction is *forward-consistent* for a
word when re-evolving it forward through the original changes lands back on the
target: it is then a valid pre-image, even if it isn't the unique original
(sound change is many-to-one, so several ancestors can be forward-consistent).

- `(forward-consistent-p reconstruction target-form forward-specs)` — does this
  reconstructed form re-evolve forward to the target?
- `(reconstruction-consistency recon)` — the share of a reconstruction's lexicon
  that is forward-consistent.

This is the same principle the loanword adapter (`find-loanword`) runs on:
because loanword adaptation has no clean invertible rule-chain, it never
inverts — it samples candidate forms, evolves them **forward**, and uses
Metropolis/`anneal` to accept-reject on distance to the target. Analytic
inversion is the fast path for feature-rule chains (and is forward-consistent
for most of a lexicon); a forward + Metropolis search is the right tool for the
non-invertible tail (insertions/deletions, coalescence) and when you want to
*sample* the distribution of plausible ancestors rather than pick one.

### Demo: "the Common Speech descends from a halfling × orc creole"

`(run-back-evolution)` tells the whole story in both directions: it builds a
halfling × orc trade creole (the true ancestor), evolves it *forward* through a
chain of sound changes into the modern "Common Speech", then throws the ancestor
away and *reconstructs* it from the Common Speech alone — reporting how faithfully
the round trip recovered the original and which distinctions the history's
mergers left unrecoverable.

## Running

The system loads with ASDF and depends on `cl-ppcre`, `iterate`, `parse-number`,
`fare-csv`, and `closer-mop`:

```lisp
(asdf:load-system :lang)
(in-package :lang)
(initialize)
(load "demo.lisp")
(run-back-evolution)          ; the ancestor-reconstruction demo

(load "test-back-derive.lisp") ; unit + round-trip tests for backward evolution
```
