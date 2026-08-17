(in-package #:lang)

;;; *CONSONANT-FREQUENCIES* and *VOWEL-FREQUENCIES* are declared in lang.lisp,
;;; which uses them and loads first.

(defun collect-frequencies (entries)
  "Turn raw (PHONE-OR-LIST . WEIGHT) pairs into the flat (PHONE . WEIGHT)
   alist FREQ-LOOKUP expects.  A car that is a list — as returned by
   LOOKUP-CONSONANT — is reduced to its first match; entries that matched no
   phone in the loaded inventory are dropped.  Several source rows can land on
   the same phone once distinctions the inventory does not draw are collapsed
   (e.g. two symbols that both read as a voiceless dental fricative here);
   their weights are summed rather than letting the first row win."
  (iter (for (phone-or-list . weight) in entries)
    (for phone = (if (listp phone-or-list) (car phone-or-list) phone-or-list))
    (with result = nil)
    (when phone
      (let ((seen (assoc phone result)))
        (if seen
            (incf (cdr seen) weight)
            (push (cons phone weight) result))))
    (finally (return (nreverse result)))))

;;; Weights are language counts from a ~450-language typological survey: 425 of
;;; them have some /m/, 403 some voiceless velar plosive, and so on.
;;;
;;; Rows marked ;+ were added later to cover phones that consonants.csv has but
;;; the original transcription skipped — every postalveolar, glottal,
;;; labial-velar, labial-palatal, and epiglottal place, which between them
;;; include some of the most widespread consonants there are.  Without them
;;; those phones fall to FREQ-LOOKUP's default weight of 1 and never get picked.
;;; The /w/ and /h/ counts sit exactly in the gaps the original list left in its
;;; descending order; the rarer six are best estimates and worth a second look.

(defun initialize-frequencies ()
  (setf *consonant-frequencies*
    (collect-frequencies
     (list
      (cons (lookup-consonant :voicing 'voiced :place 'bilabial :manner 'nasal)  425)
      (cons (lookup-consonant :voicing 'voiceless :place 'velar :manner 'plosive)  403)
      (cons (lookup-consonant :voicing 'voiced :place 'palatal :manner 'approximant)  378)
      (cons (lookup-consonant :voicing 'voiceless :place 'bilabial :manner 'plosive)  375)
      (cons (lookup-consonant :voicing 'voiced :place 'labial-velar :manner 'approximant)  332) ;+
      (cons (lookup-consonant :voicing 'voiced :place 'bilabial :manner 'plosive)  287)
      (cons (lookup-consonant :voicing 'voiceless :place 'glottal :manner 'fricative)  279) ;+
      (cons (lookup-consonant :voicing 'voiced :place 'velar :manner 'plosive)  253)
      (cons (lookup-consonant :voicing 'voiced :place 'velar :manner 'nasal)  237)
      (cons (lookup-consonant :voicing 'voiced :place 'alveolar :manner 'nasal)  202)
      (cons (lookup-consonant :voicing 'voiceless :place 'alveolar :manner 'fricative)  196)
      (cons (lookup-consonant :voicing 'voiceless :place 'alveolar :manner 'plosive)  181)
      (cons (lookup-consonant :voicing 'voiceless :place 'labiodental :manner 'fricative)  180)
      (cons (lookup-consonant :voicing 'voiced :place 'alveolar :manner 'lateral-approximant)  174)
      (cons (lookup-consonant :voicing '|| :place 'glottal :manner 'plosive)  166) ;+
      (cons (lookup-consonant :voicing 'voiceless :place 'postalveolar :manner 'fricative)  146) ;+
      (cons (lookup-consonant :voicing 'voiced :place 'palatal :manner 'nasal)  141)
      (cons (lookup-consonant :voicing 'voiced :place 'alveolar :manner 'plosive)  120)
      (cons (lookup-consonant :voicing 'voiceless :place 'dental :manner 'plosive)  106)
      (cons (lookup-consonant :voicing 'voiced :place 'alveolar :manner 'trill)  95)
      (cons (lookup-consonant :voicing 'voiced :place 'labiodental :manner 'fricative)  95)
      (cons (lookup-consonant :voicing 'voiceless :place 'velar :manner 'fricative)  94)
      (cons (lookup-consonant :voicing 'voiced :place 'alveolar :manner 'flap)  91)
      (cons (lookup-consonant :voicing 'voiced :place 'dental :manner 'nasal)  83)
      (cons (lookup-consonant :voicing 'voiced :place 'dental :manner 'plosive)  80)
      (cons (lookup-consonant :voicing 'voiced :place 'postalveolar :manner 'fricative)  68) ;+
      (cons (lookup-consonant :voicing 'voiced :place 'alveolar :manner 'fricative)  62)
      (cons (lookup-consonant :voicing 'voiced :place 'velar :manner 'fricative)  55)
      (cons (lookup-consonant :voicing 'voiced :place 'bilabial :manner 'fricative)  54)
      (cons (lookup-consonant :voicing 'voiceless :place 'palatal :manner 'plosive)  54)
      (cons (lookup-consonant :voicing 'voiceless :place 'uvular :manner 'plosive)  52)
      (cons (lookup-consonant :voicing 'voiced :place 'glottal :manner 'fricative)  52) ;+
      (cons (lookup-consonant :voicing 'voiceless :place 'uvular :manner 'fricative)  44)
      (cons (lookup-consonant :voicing 'voiced :place 'palatal :manner 'plosive)  43)
      (cons (lookup-consonant :voicing 'voiceless :place 'dental :manner 'fricative)  42)
      (cons (lookup-consonant :voicing 'voiceless :place 'bilabial :manner 'fricative)  39)
      (cons (lookup-consonant :voicing 'voiced :place 'dental :manner 'lateral-approximant)  34)
      (cons (lookup-consonant :voicing 'voiceless :place 'retroflex :manner 'plosive)  34)
      (cons (lookup-consonant :voicing 'voiced :place 'retroflex :manner 'plosive)  27)
      (cons (lookup-consonant :voicing 'voiced :place 'retroflex :manner 'lateral-approximant)  27)
      (cons (lookup-consonant :voicing 'voiced :place 'retroflex :manner 'nasal)  24)
      (cons (lookup-consonant :voicing 'voiceless :place 'labial-velar :manner 'fricative)  24) ;+
      (cons (lookup-consonant :voicing 'voiceless :place 'retroflex :manner 'fricative)  23)
      (cons (lookup-consonant :voicing 'voiced :place 'dental :manner 'fricative)  22)
      (cons (lookup-consonant :voicing 'voiced :place 'uvular :manner 'fricative)  22)
      (cons (lookup-consonant :voicing 'voiceless :place 'alveolar :manner 'lateral-fricative)  22)
      (cons (lookup-consonant :voicing 'voiced :place 'palatal :manner 'lateral-approximant)  20)
      (cons (lookup-consonant :voicing 'voiced :place 'bilabial :manner 'approximant)  19)
      (cons (lookup-consonant :voicing 'voiceless :place 'dental :manner 'fricative)  18)
      (cons (lookup-consonant :voicing 'voiceless :place 'bilabial :manner 'nasal)  17)
      (cons (lookup-consonant :voicing 'voiced :place 'retroflex :manner 'approximant)  17)
      (cons (lookup-consonant :voicing 'voiced :place 'dental :manner 'fricative)  17)
      (cons (lookup-consonant :voicing 'voiced :place 'uvular :manner 'plosive)  14)
      (cons (lookup-consonant :voicing 'voiced :place 'retroflex :manner 'flap)  14)
      (cons (lookup-consonant :voicing 'voiceless :place 'palatal :manner 'affricate)  12)
      (cons (lookup-consonant :voicing 'voiced :place 'velar :manner 'approximant)  12)
      (cons (lookup-consonant :voicing 'voiced :place 'palatal :manner 'fricative)  12)
      (cons (lookup-consonant :voicing 'voiceless :place 'palatal :manner 'fricative)  11)
      (cons (lookup-consonant :voicing 'voiced :place 'alveolar :manner 'approximant)  11)
      (cons (lookup-consonant :voicing 'voiceless :place 'palatal :manner 'fricative)  9)
      (cons (lookup-consonant :voicing 'voiceless :place 'velar :manner 'nasal)  9)
      (cons (lookup-consonant :voicing 'voiceless :place 'palatal :manner 'approximant)  9)
      (cons (lookup-consonant :voicing 'voiced :place 'dental :manner 'trill)  9)
      (cons (lookup-consonant :voicing 'voiced :place 'retroflex :manner 'fricative)  9)
      (cons (lookup-consonant :voicing 'voiced :place 'labial-palatal :manner 'approximant)  9) ;+
      (cons (lookup-consonant :voicing 'voiced :place 'palatal :manner 'affricate)  8)
      (cons (lookup-consonant :voicing 'voiceless :place 'palatal :manner 'nasal)  8)
      (cons (lookup-consonant :voicing 'voiced :place 'alveolar :manner 'lateral-fricative)  8)
      (cons (lookup-consonant :voicing 'voiced :place 'alveolar :manner 'tap)  7)
      (cons (lookup-consonant :voicing 'voiced :place 'palatal :manner 'fricative)  7)
      (cons (lookup-consonant :voicing 'voiced :place 'labiodental :manner 'approximant)  6)
      (cons (lookup-consonant :voicing 'voiced :place 'retroflex :manner 'fricative)  4)
      (cons (lookup-consonant :voicing 'voiced :place 'uvular :manner 'trill)  4)
      (cons (lookup-consonant :voicing 'voiceless :place 'alveolar :manner 'nasal)  4)
      (cons (lookup-consonant :voicing 'voiceless :place 'uvular :manner 'affricate)  4)
      (cons (lookup-consonant :voicing 'voiced :place 'bilabial :manner 'trill)  4) ;+
      (cons (lookup-consonant :voicing 'voiced :place 'labiodental :manner 'affricate)  3)
      (cons (lookup-consonant :voicing 'voiceless :place 'labiodental :manner 'affricate)  3)
      (cons (lookup-consonant :voicing 'voiced :place 'uvular :manner 'approximant)  2)
      (cons (lookup-consonant :voicing 'voiceless :place 'alveolar :manner 'lateral-approximant)  2)
      (cons (lookup-consonant :voicing 'voiceless :place 'dental :manner 'nasal)  2)
      (cons (lookup-consonant :voicing 'voiceless :place 'alveolar :manner 'trill)  2)
      (cons (lookup-consonant :voicing 'voiceless :place 'alveolar :manner 'affricate)  2)
      (cons (lookup-consonant :voicing 'voiceless :place 'dental :manner 'affricate)  2)
      (cons (lookup-consonant :voicing 'voiceless :place 'epiglottal :manner 'fricative)  2) ;+
      (cons (lookup-consonant :voicing 'voiceless :place 'retroflex :manner 'fricative)  1)
      (cons (lookup-consonant :voicing 'voiced :place 'alveolar :manner 'fricative)  1)
      (cons (lookup-consonant :voicing 'voiced :place 'velar :manner 'lateral-approximant)  1)
      (cons (lookup-consonant :voicing 'voiceless :place 'bilabial :manner 'approximant)  1)
      (cons (lookup-consonant :voicing 'voiced :place 'labiodental :manner 'plosive)  1)
      (cons (lookup-consonant :voicing 'voiced :place 'dental :manner 'affricate)  1)
      (cons (lookup-consonant :voicing 'voiceless :place 'velar :manner 'lateral-fricative)  1)
      (cons (lookup-consonant :voicing 'voiceless :place 'retroflex :manner 'lateral-approximant)  1)
      (cons (lookup-consonant :voicing 'voiceless :place 'dental :manner 'lateral-approximant)  1)
      (cons (lookup-consonant :voicing 'voiceless :place 'dental :manner 'lateral-fricative)  1)
      (cons (lookup-consonant :voicing 'voiceless :place 'retroflex :manner 'nasal)  1)
      (cons (lookup-consonant :voicing 'voiceless :place 'velar :manner 'affricate)  1)
      (cons (lookup-consonant :voicing 'voiced :place 'retroflex :manner 'lateral-fricative)  1)
      (cons (lookup-consonant :voicing 'voiced :place 'dental :manner 'lateral-fricative)  1)
      (cons (lookup-consonant :voicing 'voiced :place 'labiodental :manner 'nasal)  1)
      (cons (lookup-consonant :voicing 'voiced :place 'uvular :manner 'nasal)  1)
      (cons (lookup-consonant :voicing 'voiced :place 'retroflex :manner 'trill)  1)
      (cons (lookup-consonant :voicing 'voiced :place 'dental :manner 'tap)  1)
      (cons (lookup-consonant :voicing 'voiced :place 'dental :manner 'flap)  1)
      (cons (lookup-consonant :voicing 'voiced :place 'palatal :manner 'trill)  1)
      (cons (lookup-consonant :voicing 'voiceless :place 'retroflex :manner 'affricate)  1)
      (cons (lookup-consonant :voicing 'voiced :place 'labiodental :manner 'flap)  1))))
  (setf *vowel-frequencies*
    (collect-frequencies
     (list
      (cons (car (filter-vowel *vowels* :height 'open :backness 'front :rounding 'unrounded)) 393)
      (cons (car (filter-vowel *vowels* :height 'close :backness 'central :rounding 'unrounded)) 392)
      (cons (car (filter-vowel *vowels* :height 'open :backness 'back :rounding 'rounded)) 369)
      (cons (car (filter-vowel *vowels* :height 'close-mid :backness 'front :rounding 'unrounded)) 186)
      (cons (car (filter-vowel *vowels* :height 'mid :backness 'back :rounding 'rounded)) 181)
      (cons (car (filter-vowel *vowels* :height 'mid :backness 'front :rounding 'unrounded)) 169)
      (cons (car (filter-vowel *vowels* :height 'close-mid :backness 'back :rounding 'rounded)) 162)
      (cons (car (filter-vowel *vowels* :height 'open-mid :backness 'back :rounding 'rounded)) 131)
      (cons (car (filter-vowel *vowels* :height 'open-mid :backness 'front :rounding 'unrounded)) 124)
      (cons (car (filter-vowel *vowels* :height 'mid :backness 'central :rounding '||)) 76)
      (cons (car (filter-vowel *vowels* :height 'near-open :backness 'front :rounding 'unrounded)) 74)
      (cons (car (filter-vowel *vowels* :height 'near-open :backness 'back :rounding 'rounded)) 66)
      (cons (car (filter-vowel *vowels* :height 'open :backness 'central :rounding 'unrounded)) 61)
      (cons (car (filter-vowel *vowels* :height 'open :backness 'back :rounding 'unrounded)) 41)
      (cons (car (filter-vowel *vowels* :height 'near-close :backness 'near-front :rounding 'unrounded)) 39)
      (cons (car (filter-vowel *vowels* :height 'close :backness 'front :rounding 'unrounded)) 26)
      (cons (car (filter-vowel *vowels* :height 'close :backness 'back :rounding 'unrounded)) 25)
      (cons (car (filter-vowel *vowels* :height 'open :backness 'front :rounding 'rounded)) 24)
      (cons (car (filter-vowel *vowels* :height 'open-mid :backness 'central :rounding 'unrounded)) 20)
      (cons (car (filter-vowel *vowels* :height 'close :backness 'back :rounding 'rounded)) 19)
      (cons (car (filter-vowel *vowels* :height 'close-mid :backness 'central :rounding 'unrounded)) 15)
      (cons (car (filter-vowel *vowels* :height 'near-close :backness 'central :rounding 'unrounded)) 14)
      (cons (car (filter-vowel *vowels* :height 'open-mid :backness 'back :rounding 'unrounded)) 12)
      (cons (car (filter-vowel *vowels* :height 'open-mid :backness 'front :rounding 'rounded)) 12)
      (cons (car (filter-vowel *vowels* :height 'close-mid :backness 'back :rounding 'unrounded)) 10)
      (cons (car (filter-vowel *vowels* :height 'mid :backness 'back :rounding 'unrounded)) 8)
      (cons (car (filter-vowel *vowels* :height 'close-mid :backness 'front :rounding 'rounded)) 8)
      (cons (car (filter-vowel *vowels* :height 'open :backness 'central :rounding 'rounded)) 6)
      (cons (car (filter-vowel *vowels* :height 'mid :backness 'central :rounding 'rounded)) 5)
      (cons (car (filter-vowel *vowels* :height 'near-open :backness 'back :rounding 'unrounded)) 5)
      (cons (car (filter-vowel *vowels* :height 'open-mid :backness 'central :rounding 'rounded)) 4)
      (cons (car (filter-vowel *vowels* :height 'near-open :backness 'central :rounding '||)) 4)
      (cons (car (filter-vowel *vowels* :height 'near-open :backness 'front :rounding 'rounded)) 4)
      (cons (car (filter-vowel *vowels* :height 'mid :backness 'front :rounding 'rounded)) 1)
      (cons (car (filter-vowel *vowels* :height 'close-mid :backness 'central :rounding 'rounded)) 1)
      (cons (car (filter-vowel *vowels* :height 'near-close :backness 'back :rounding 'rounded)) 1)
      (cons (car (filter-vowel *vowels* :height 'near-close :backness 'back :rounding 'unrounded)) 1)
      (cons (car (filter-vowel *vowels* :height 'near-open :backness 'central :rounding 'rounded)) 1)
      (cons (car (filter-vowel *vowels* :height 'open-mid :backness 'back :rounding 'rounded)) 1)))))
