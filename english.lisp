(in-package :lang)

;;; English-target back-formation.
;;;
;;; The goal: make a language deep in a derivation chain (e.g. an imperial
;;; trade language descended from a halfling/orc creole) come out close
;;; enough to English that in-world text needs no translation.  Rather than
;;; inverting the sound changes analytically, we reuse the forward-chaining
;;; loanword machinery: BACK-FORM-WORD anneals a root-language form whose
;;; fitness is measured on the surface form after evolving through the whole
;;; chain.  RETROFIT-ENGLISH-LEXICON does that for a set of glosses and
;;; installs the winners in the chain root's lexicon; REFRESH-DERIVATION-CHAIN
;;; then re-derives every language between root and target so all siblings
;;; inherit consistent cognates.

;;; General American English targets, broad transcription, restricted to the
;;; phone inventory in consonants.csv/vowels.csv: diphthongs are vowel
;;; sequences, affricates are stop+fricative sequences, /ɚ/ is ("ə" "ɹ"),
;;; and length is ignored.  Keys match the gloss names in *vocabulary* /
;;; *demo-words*, plus the generated pronoun/modal/wh glosses.

(defparameter *english-ipa*
  '(;; content vocabulary
    ("alone"    "ə" "l" "o" "ʊ" "n")
    ("always"   "ɔ" "l" "w" "e" "ɪ" "z")
    ("ask"      "æ" "s" "k")
    ("bad"      "b" "æ" "d")
    ("believe"  "b" "ɪ" "l" "i" "v")
    ("bird"     "b" "ə" "ɹ" "d")
    ("bitter"   "b" "ɪ" "t" "ə" "ɹ")
    ("blade"    "b" "l" "e" "ɪ" "d")
    ("blind"    "b" "l" "a" "ɪ" "n" "d")
    ("blood"    "b" "l" "ʌ" "d")
    ("brave"    "b" "ɹ" "e" "ɪ" "v")
    ("break"    "b" "ɹ" "e" "ɪ" "k")
    ("bright"   "b" "ɹ" "a" "ɪ" "t")
    ("bring"    "b" "ɹ" "ɪ" "ŋ")
    ("brother"  "b" "ɹ" "ʌ" "ð" "ə" "ɹ")
    ("burn"     "b" "ə" "ɹ" "n")
    ("call"     "k" "ɔ" "l")
    ("chain"    "t" "ʃ" "e" "ɪ" "n")
    ("charge"   "t" "ʃ" "ɑ" "ɹ" "d" "ʒ")
    ("child"    "t" "ʃ" "a" "ɪ" "l" "d")
    ("city"     "s" "ɪ" "t" "i")
    ("clerk"    "k" "l" "ə" "ɹ" "k")
    ("cloth"    "k" "l" "ɔ" "θ")
    ("cold"     "k" "o" "ʊ" "l" "d")
    ("come"     "k" "ʌ" "m")
    ("cook"     "k" "ʊ" "k")
    ("council"  "k" "a" "ʊ" "n" "s" "ə" "l")
    ("dark"     "d" "ɑ" "ɹ" "k")
    ("daughter" "d" "ɔ" "t" "ə" "ɹ")
    ("dawn"     "d" "ɔ" "n")
    ("day"      "d" "e" "ɪ")
    ("dead"     "d" "ɛ" "d")
    ("death"    "d" "ɛ" "θ")
    ("deep"     "d" "i" "p")
    ("die"      "d" "a" "ɪ")
    ("dock"     "d" "ɑ" "k")
    ("dog"      "d" "ɔ" "ɡ")
    ("door"     "d" "ɔ" "ɹ")
    ("dress"    "d" "ɹ" "ɛ" "s")
    ("earth"    "ə" "ɹ" "θ")
    ("eat"      "i" "t")
    ("empire"   "ɛ" "m" "p" "a" "ɪ" "ə" "ɹ")
    ("empty"    "ɛ" "m" "p" "t" "i")
    ("end"      "ɛ" "n" "d")
    ("enemy"    "ɛ" "n" "ə" "m" "i")
    ("fall"     "f" "ɔ" "l")
    ("fast"     "f" "æ" "s" "t")
    ("father"   "f" "ɑ" "ð" "ə" "ɹ")
    ("fear"     "f" "ɪ" "ɹ")
    ("fight"    "f" "a" "ɪ" "t")
    ("find"     "f" "a" "ɪ" "n" "d")
    ("fire"     "f" "a" "ɪ" "ə" "ɹ")
    ("fleet"    "f" "l" "i" "t")
    ("flower"   "f" "l" "a" "ʊ" "ə" "ɹ")
    ("follow"   "f" "ɑ" "l" "o" "ʊ")
    ("food"     "f" "u" "d")
    ("forest"   "f" "ɔ" "ɹ" "ə" "s" "t")
    ("free"     "f" "ɹ" "i")
    ("friend"   "f" "ɹ" "ɛ" "n" "d")
    ("full"     "f" "ʊ" "l")
    ("give"     "ɡ" "ɪ" "v")
    ("go"       "ɡ" "o" "ʊ")
    ("god"      "ɡ" "ɑ" "d")
    ("gold"     "ɡ" "o" "ʊ" "l" "d")
    ("good"     "ɡ" "ʊ" "d")
    ("goods"    "ɡ" "ʊ" "d" "z")
    ("grow"     "ɡ" "ɹ" "o" "ʊ")
    ("guard"    "ɡ" "ɑ" "ɹ" "d")
    ("hand"     "h" "æ" "n" "d")
    ("happy"    "h" "æ" "p" "i")
    ("harbor"   "h" "ɑ" "ɹ" "b" "ə" "ɹ")
    ("hear"     "h" "ɪ" "ɹ")
    ("hide"     "h" "a" "ɪ" "d")
    ("hold"     "h" "o" "ʊ" "l" "d")
    ("home"     "h" "o" "ʊ" "m")
    ("hope"     "h" "o" "ʊ" "p")
    ("horse"    "h" "ɔ" "ɹ" "s")
    ("hot"      "h" "ɑ" "t")
    ("house"    "h" "a" "ʊ" "s")
    ("hunt"     "h" "ʌ" "n" "t")
    ("iron"     "a" "ɪ" "ə" "ɹ" "n")
    ("judge"    "d" "ʒ" "ʌ" "d" "ʒ")
    ("kill"     "k" "ɪ" "l")
    ("king"     "k" "ɪ" "ŋ")
    ("knife"    "n" "a" "ɪ" "f")
    ("know"     "n" "o" "ʊ")
    ("laugh"    "l" "æ" "f")
    ("law"      "l" "ɔ")
    ("lead"     "l" "i" "d")
    ("leaf"     "l" "i" "f")
    ("letter"   "l" "ɛ" "t" "ə" "ɹ")
    ("live"     "l" "ɪ" "v")
    ("long"     "l" "ɔ" "ŋ")
    ("lord"     "l" "ɔ" "ɹ" "d")
    ("lose"     "l" "u" "z")
    ("love"     "l" "ʌ" "v")
    ("make"     "m" "e" "ɪ" "k")
    ("man"      "m" "æ" "n")
    ("merchant" "m" "ə" "ɹ" "t" "ʃ" "ə" "n" "t")
    ("mercy"    "m" "ə" "ɹ" "s" "i")
    ("moon"     "m" "u" "n")
    ("mother"   "m" "ʌ" "ð" "ə" "ɹ")
    ("mountain" "m" "a" "ʊ" "n" "t" "ə" "n")
    ("name"     "n" "e" "ɪ" "m")
    ("never"    "n" "ɛ" "v" "ə" "ɹ")
    ("new"      "n" "u")
    ("not"      "n" "ɑ" "t")
    ("obey"     "o" "ʊ" "b" "e" "ɪ")
    ("old"      "o" "ʊ" "l" "d")
    ("open"     "o" "ʊ" "p" "ə" "n")
    ("own"      "o" "ʊ" "n")
    ("palace"   "p" "æ" "l" "ə" "s")
    ("path"     "p" "æ" "θ")
    ("people"   "p" "i" "p" "ə" "l")
    ("play"     "p" "l" "e" "ɪ")
    ("pride"    "p" "ɹ" "a" "ɪ" "d")
    ("promise"  "p" "ɹ" "ɑ" "m" "ə" "s")
    ("protect"  "p" "ɹ" "ə" "t" "ɛ" "k" "t")
    ("proud"    "p" "ɹ" "a" "ʊ" "d")
    ("rain"     "ɹ" "e" "ɪ" "n")
    ("refuse"   "ɹ" "ə" "f" "j" "u" "z")
    ("return"   "ɹ" "ɪ" "t" "ə" "ɹ" "n")
    ("rich"     "ɹ" "ɪ" "t" "ʃ")
    ("rise"     "ɹ" "a" "ɪ" "z")
    ("river"    "ɹ" "ɪ" "v" "ə" "ɹ")
    ("road"     "ɹ" "o" "ʊ" "d")
    ("root"     "ɹ" "u" "t")
    ("rule"     "ɹ" "u" "l")
    ("run"      "ɹ" "ʌ" "n")
    ("sail"     "s" "e" "ɪ" "l")
    ("see"      "s" "i")
    ("seed"     "s" "i" "d")
    ("sell"     "s" "ɛ" "l")
    ("send"     "s" "ɛ" "n" "d")
    ("serve"    "s" "ə" "ɹ" "v")
    ("sharp"    "ʃ" "ɑ" "ɹ" "p")
    ("shield"   "ʃ" "i" "l" "d")
    ("shine"    "ʃ" "a" "ɪ" "n")
    ("ship"     "ʃ" "ɪ" "p")
    ("short"    "ʃ" "ɔ" "ɹ" "t")
    ("shout"    "ʃ" "a" "ʊ" "t")
    ("silence"  "s" "a" "ɪ" "l" "ə" "n" "s")
    ("silent"   "s" "a" "ɪ" "l" "ə" "n" "t")
    ("sing"     "s" "ɪ" "ŋ")
    ("sit"      "s" "ɪ" "t")
    ("sky"      "s" "k" "a" "ɪ")
    ("slave"    "s" "l" "e" "ɪ" "v")
    ("sleep"    "s" "l" "i" "p")
    ("small"    "s" "m" "ɔ" "l")
    ("snow"     "s" "n" "o" "ʊ")
    ("song"     "s" "ɔ" "ŋ")
    ("speak"    "s" "p" "i" "k")
    ("spirit"   "s" "p" "ɪ" "ɹ" "ə" "t")
    ("square"   "s" "k" "w" "ɛ" "ɹ")
    ("stand"    "s" "t" "æ" "n" "d")
    ("star"     "s" "t" "ɑ" "ɹ")
    ("stone"    "s" "t" "o" "ʊ" "n")
    ("stop"     "s" "t" "ɑ" "p")
    ("story"    "s" "t" "ɔ" "ɹ" "i")
    ("strange"  "s" "t" "ɹ" "e" "ɪ" "n" "d" "ʒ")
    ("strong"   "s" "t" "ɹ" "ɔ" "ŋ")
    ("sun"      "s" "ʌ" "n")
    ("sweet"    "s" "w" "i" "t")
    ("sword"    "s" "ɔ" "ɹ" "d")
    ("take"     "t" "e" "ɪ" "k")
    ("think"    "θ" "ɪ" "ŋ" "k")
    ("tongue"   "t" "ʌ" "ŋ")
    ("trade"    "t" "ɹ" "e" "ɪ" "d")
    ("traitor"  "t" "ɹ" "e" "ɪ" "t" "ə" "ɹ")
    ("tree"     "t" "ɹ" "i")
    ("truth"    "t" "ɹ" "u" "θ")
    ("wait"     "w" "e" "ɪ" "t")
    ("walk"     "w" "ɔ" "k")
    ("wall"     "w" "ɔ" "l")
    ("want"     "w" "ɑ" "n" "t")
    ("war"      "w" "ɔ" "ɹ")
    ("warm"     "w" "ɔ" "ɹ" "m")
    ("water"    "w" "ɔ" "t" "ə" "ɹ")
    ("weep"     "w" "i" "p")
    ("wife"     "w" "a" "ɪ" "f")
    ("wind"     "w" "ɪ" "n" "d")
    ("wise"     "w" "a" "ɪ" "z")
    ("wolf"     "w" "ʊ" "l" "f")
    ("woman"    "w" "ʊ" "m" "ə" "n")
    ("word"     "w" "ə" "ɹ" "d")
    ("write"    "ɹ" "a" "ɪ" "t")
    ("young"    "j" "ʌ" "ŋ")
    ;; pronouns (glosses from generate-pronoun-lexicon)
    ("1stsingular" "a" "ɪ")
    ("2ndsingular" "j" "u")
    ("3rdsingular" "h" "i")
    ("1stplural"   "w" "i")
    ("2ndplural"   "j" "u")
    ("3rdplural"   "ð" "e" "ɪ")
    ;; modals (glosses from generate-modal-lexicon)
    ("can"  "k" "æ" "n")
    ("must" "m" "ʌ" "s" "t")
    ;; wh-words (glosses from generate-wh-lexicon)
    ("wh-what"  "w" "ʌ" "t")
    ("wh-where" "w" "ɛ" "ɹ")
    ("wh-when"  "w" "ɛ" "n")
    ("wh-why"   "w" "a" "ɪ")
    ("wh-how"   "h" "a" "ʊ"))
  "English (General American) IPA targets keyed by gloss.")

(defun english-form (gloss)
  "Return the English target for GLOSS as a reanalyzed word, or NIL."
  (let ((entry (assoc gloss *english-ipa* :test #'string=)))
    (when entry
      (deserialize-form (rest entry)))))

(defun english-glosses ()
  (mapcar #'first *english-ipa*))

;;; Installing back-formed words in the chain root

(defun install-root-word (root gloss form &key (category 'noun) origin)
  "Insert FORM into ROOT's lexicon under GLOSS, replacing the form of an
   existing entry with that gloss if there is one (its category, domain, and
   noun class are kept; stale inflected forms are cleared — re-run
   GENERATE-PARADIGMS afterwards)."
  (let ((existing (find gloss (lexicon root) :key #'gloss :test #'equal)))
    (if existing
        (progn
          (setf (form existing) form
                (origin existing) (or origin (origin existing))
                (inflected-forms existing) nil)
          existing)
        (let ((entry (make-instance 'lexical-entry
                                    :gloss gloss
                                    :form form
                                    :category category
                                    :origin (or origin :retrofit))))
          (push entry (lexicon root))
          entry))))

(defun retrofit-english-lexicon (lang glosses &key (schedule (default-anneal-schedule))
                                                debug (verbose t))
  "Back-form an English-like word for each of GLOSSES through LANG's
   derivation chain and install the results in the chain root's lexicon,
   so that the root form legitimately evolves into an English-like surface
   form in LANG.  Glosses without an entry in *english-ipa* are skipped with
   a warning.  Rebuilds the root's paradigms afterwards.  Returns the root.
   NOTE: derived languages between root and LANG hold snapshots — call
   REFRESH-DERIVATION-CHAIN on each descendant afterwards."
  (multiple-value-bind (root chain) (collect-derivation-chain lang)
    (declare (ignore chain))
    (dolist (gloss glosses)
      (let ((target (english-form gloss)))
        (if (null target)
            (warn "No English IPA target for ~s; skipping." gloss)
            (multiple-value-bind (root-word evolved distance)
                (back-form-word target lang :schedule schedule :debug debug)
              (install-root-word root gloss root-word
                                 :category (let ((old (lookup-word root gloss)))
                                             (if old (category old) 'noun))
                                 :origin '(:retrofit . :english))
              (when verbose
                (format t "~12a  en /~a/  root ~a  surface ~a (~,2f) ~s~%"
                        gloss
                        (alt-print-word target)
                        (alt-print-word root-word)
                        (alt-print-word evolved)
                        distance
                        (alt-print-word evolved nil #'anglicize)))))))
    (generate-paradigms root)
    root))

;;; Re-deriving after the root lexicon changes

(defun refresh-derived-lexicon (derived)
  "Recompute DERIVED's lexicon from its source using its stored transformers,
   leaving its grammar untouched.  Words DERIVED acquired on its own (e.g.
   post-split loans) are dropped — re-borrow them afterwards if needed."
  (setf (lexicon derived)
        (iter (for entry in (lexicon (source derived)))
          (collect (make-instance 'lexical-entry
                                  :gloss (gloss entry)
                                  :form (evolve (transformers derived) (form entry))
                                  :category (category entry)
                                  :origin (cons :inherited (lang-name (source derived)))
                                  :domain (domain entry)
                                  :noun-class (noun-class entry)
                                  :inflected-forms
                                  (mapcar (lambda (pair)
                                            (cons (car pair)
                                                  (evolve (transformers derived)
                                                          (cdr pair))))
                                          (inflected-forms entry))))))
  derived)

(defun refresh-derivation-chain (lang)
  "Refresh the lexicon of every derived language from LANG's chain root down
   to LANG itself, in root-to-leaf order.  Call after editing the root
   lexicon (e.g. via RETROFIT-ENGLISH-LEXICON).  Returns LANG."
  (labels ((path (l)
             (if (typep l 'derived-language)
                 (append (path (source l)) (list l))
                 nil)))
    (mapc #'refresh-derived-lexicon (path lang)))
  lang)
