(in-package #:lang)

(initialize)

;;; Tests for backward evolution: clause inversion, chain inversion, and the
;;; round-trip property (forward then back-derive recovers ancestor forms wherever
;;; the changes were not mergers).

(defvar *bd-pass* 0)
(defvar *bd-fail* 0)

(defun bd-check (label got expected)
  (if (equal got expected)
      (progn (incf *bd-pass*) (format t "  ok   ~a~%" label))
      (progn (incf *bd-fail*)
             (format t "  FAIL ~a~%       got:      ~s~%       expected: ~s~%"
                     label got expected))))

(defun bd-check-true (label got)
  (if got
      (progn (incf *bd-pass*) (format t "  ok   ~a~%" label))
      (progn (incf *bd-fail*) (format t "  FAIL ~a (was NIL)~%" label))))

(defun run-back-derive-tests ()
  (setf *bd-pass* 0 *bd-fail* 0)

  (format t "~%=== Test 1: invert a consonant feature clause ===~%")
  (bd-check "plosive->fricative inverts to fricative->plosive"
            (invert-spec-clause '(C :voicing voiced :manner plosive -> :manner fricative))
            '(C :voicing voiced :manner fricative -> :manner plosive))

  (format t "~%=== Test 2: invert a vowel feature clause ===~%")
  (bd-check "open->mid inverts to mid->open"
            (invert-spec-clause '(V :height open -> :height mid))
            '(V :height mid -> :height open))

  (format t "~%=== Test 3: invert an IPA-literal clause ===~%")
  (bd-check "(\"p\" -> \"f\") inverts to (\"f\" -> \"p\")"
            (invert-spec-clause '("p" -> "f"))
            '("f" -> "p"))

  (format t "~%=== Test 4: a merger is reported, not guessed ===~%")
  (multiple-value-bind (inv reason)
      (invert-spec-clause '(C :stress unstressed :manner plosive -> :voicing voiceless))
    (bd-check "merger clause yields no inverse" inv nil)
    (bd-check-true "merger clause yields a reason string" (stringp reason)))

  (format t "~%=== Test 5: a whole chain reverses order and inverts each clause ===~%")
  (multiple-value-bind (inv dropped)
      (invert-transformer-spec
       '((C :voicing voiceless :manner plosive -> :manner fricative)
         (V :height open -> :height mid)
         (C :stress unstressed :manner plosive -> :voicing voiceless)))
    (bd-check "invertible clauses returned in reversed order"
              inv
              '((V :height mid -> :height open)
                (C :voicing voiceless :manner fricative -> :manner plosive)))
    (bd-check "the merger clause is dropped" (length dropped) 1)
    (bd-check-true "dropped entry keeps the original clause"
                   (equal (car (first dropped))
                          '(C :stress unstressed :manner plosive -> :voicing voiceless))))

  (format t "~%=== Test 6: back-derive-language produces a reconstructed ancestor ===~%")
  (let* ((proto (halfling 12 :name "proto-x")))
    (generate-grammar proto :typology :mixed)
    (generate-lexicon proto (demo-vocabulary))
    (let* ((changes '((V :height open -> :height mid)
                      (V :backness back -> :backness central)))
           (modern (derive-language proto changes :name "modern-x"))
           (recon  (back-derive-language modern changes :name "recon-x")))
      (bd-check-true "reconstruction is a reconstructed-language"
                     (typep recon 'reconstructed-language))
      (bd-check-true "reconstruction source is the modern/target language"
                     (eq (source recon) modern))
      (bd-check-true "reconstruction lexicon is same size as the target"
                     (= (length (lexicon recon)) (length (lexicon modern))))
      ;; Round-trip property: for these cleanly-invertible vowel shifts, the
      ;; reconstruction should match the true proto on most forms.  (It is not
      ;; guaranteed to be 100%: raising/centralization can still merge two
      ;; source vowels onto one target vowel.)  Require a strong majority.
      (let ((total 0) (hits 0))
        (dolist (g *demo-words*)
          (let ((p (let ((e (lookup-word proto g))) (and e (form e))))
                (r (let ((e (lookup-word recon g))) (and e (form e)))))
            (when (and p r)
              (incf total)
              (when (equal (serialize-form p) (serialize-form r)) (incf hits)))))
        (format t "  round-trip: ~a/~a proto forms recovered exactly~%" hits total)
        (bd-check-true "cleanly-invertible chain recovers a majority of forms"
                       (and (plusp total) (>= (/ hits total) 1/2))))))

  (format t "~%=== back-derive tests: ~a passed, ~a failed ===~%" *bd-pass* *bd-fail*)
  (values *bd-pass* *bd-fail*))

(run-back-derive-tests)
