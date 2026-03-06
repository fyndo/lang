(in-package #:lang)

(initialize)

(defun run-disambiguate-tests ()
  (setf *random-state* (sb-ext:seed-random-state 42))

  (format t "~%=== Test 1: Particle-vs-particle collision ===~%")
  (let* ((proto (language-helper
                 (initialize-zipflist
                  (filter-consonant *safe-consonants*
                                    :place '(alveolar) :manner '(plosive nasal)))
                 :name "test-proto"
                 :num-vowels 3)))
    (generate-grammar proto :typology :synthetic)
    (define-word proto "dog" 'noun :syllables 1)
    (define-word proto "cat" 'noun :syllables 1)
    (define-word proto "run" 'verb :syllables 1)
    (generate-paradigms proto)

    (let* ((acc-rule (find-morpheme-rule proto :accusative :noun))
           (gen-rule (find-morpheme-rule proto :genitive :noun)))
      (format t "Before: acc strategy=~a marker=~a~%" (mrule-strategy acc-rule) (mrule-marker acc-rule))
      (format t "Before: gen strategy=~a marker=~a~%" (mrule-strategy gen-rule) (mrule-marker gen-rule))

      ;; Force both to particle-before with same marker
      (setf (mrule-strategy acc-rule) :particle-before)
      (setf (mrule-marker acc-rule) '("k" "a"))
      (setf (mrule-strategy gen-rule) :particle-before)
      (setf (mrule-marker gen-rule) '("k" "a"))

      (format t "Forced collision: acc=~a gen=~a~%" (mrule-marker acc-rule) (mrule-marker gen-rule))

      (let ((fixed (disambiguate-markers proto)))
        (format t "disambiguate-markers returned: ~a~%" fixed)
        (format t "After: acc strategy=~a marker=~a~%" (mrule-strategy acc-rule) (mrule-marker acc-rule))
        (format t "After: gen strategy=~a marker=~a~%" (mrule-strategy gen-rule) (mrule-marker gen-rule))
        ;; Particle collision: gen gets a fresh marker (no suffix to preserve)
        (assert fixed nil "Expected disambiguate-markers to return T")
        (assert (not (equal (mrule-marker acc-rule) (mrule-marker gen-rule)))
                nil "Expected markers to differ after disambiguation")
        ;; Strategies stay as particles
        (assert (eql (mrule-strategy gen-rule) :particle-before)
                nil "Expected gen to remain a particle strategy"))))

  (format t "~%=== Test 2: Bound-vs-bound collision (suffix) ===~%")
  (setf *random-state* (sb-ext:seed-random-state 99))
  (let* ((proto (language-helper
                 (initialize-zipflist
                  (filter-consonant *safe-consonants*
                                    :place '(alveolar) :manner '(plosive nasal)))
                 :name "test-proto2"
                 :num-vowels 3)))
    (generate-grammar proto :typology :synthetic)
    (define-word proto "dog" 'noun :syllables 1)
    (define-word proto "cat" 'noun :syllables 1)
    (define-word proto "tree" 'noun :syllables 1)
    (generate-paradigms proto)

    (let* ((acc-rule (find-morpheme-rule proto :accusative :noun))
           (gen-rule (find-morpheme-rule proto :genitive :noun)))
      (setf (mrule-strategy acc-rule) :suffix)
      (setf (mrule-marker acc-rule) '("n" "a"))
      (setf (mrule-strategy gen-rule) :suffix)
      (setf (mrule-marker gen-rule) '("n" "a"))
      (generate-paradigms proto)

      (format t "Forced bound collision: acc=~a gen=~a~%" (mrule-marker acc-rule) (mrule-marker gen-rule))

      (let ((rate (measure-paradigm-collision :accusative :genitive :noun proto)))
        (format t "Collision rate: ~a~%" rate)
        (assert (= rate 1) nil "Expected 100%% collision rate for identical suffixes"))

      (let ((fixed (disambiguate-markers proto)))
        (format t "disambiguate-markers returned: ~a~%" fixed)
        (format t "After: acc strategy=~a marker=~a~%" (mrule-strategy acc-rule) (mrule-marker acc-rule))
        (format t "After: gen strategy=~a marker=~a disambig-particle=~a~%"
                (mrule-strategy gen-rule) (mrule-marker gen-rule) (mrule-disambig gen-rule))
        (assert fixed nil "Expected disambiguate-markers to return T")
        ;; Gen should now be a compound disambig strategy
        (assert (member (mrule-strategy gen-rule) '(:disambig-suffix-before :disambig-suffix-after))
                nil "Expected gen-rule to be switched to disambig-suffix strategy")
        ;; Original merged marker preserved
        (assert (equal (mrule-marker gen-rule) '("n" "a"))
                nil "Expected gen-rule to keep merged marker")
        ;; Disambig particle added
        (assert (mrule-disambig gen-rule)
                nil "Expected gen-rule to have a disambig-particle")
        ;; Test inflect produces 2 words: particle + suffixed form
        (let* ((base (form (first (lexicon proto))))
               (inflected (inflect base gen-rule)))
          (format t "Inflect result: ~a words~%" (length inflected))
          (assert (= (length inflected) 2)
                  nil "Expected inflect to return 2 word-forms for compound strategy")))))

  (format t "~%=== Test 3: No collision - should return NIL ===~%")
  (setf *random-state* (sb-ext:seed-random-state 77))
  (let* ((proto (language-helper
                 (initialize-zipflist
                  (filter-consonant *safe-consonants*
                                    :place '(alveolar) :manner '(plosive nasal)))
                 :name "test-proto3"
                 :num-vowels 3)))
    (generate-grammar proto :typology :synthetic)
    (define-word proto "dog" 'noun :syllables 1)
    (generate-paradigms proto)

    (let* ((case-features '(:accusative :genitive :oblique-with :oblique-from
                            :oblique-to :oblique-on :oblique-at :oblique-over))
           ;; Give every case feature a unique, distinct marker
           (unique-markers '(("n" "a") ("t" "e") ("s" "i") ("d" "o")
                             ("n" "e") ("t" "a") ("s" "o") ("d" "i")))
           (tense-features '(:past :future))
           (tense-markers '(("n" "o") ("t" "i"))))
      ;; Set all case rules to suffix with distinct markers
      (iter (for feat in case-features)
        (for marker in unique-markers)
        (let ((rule (find-morpheme-rule proto feat :noun)))
          (when rule
            (setf (mrule-strategy rule) :suffix)
            (setf (mrule-marker rule) marker))))
      ;; Set verb tense rules to suffix with distinct markers
      (iter (for feat in tense-features)
        (for marker in tense-markers)
        (let ((rule (find-morpheme-rule proto feat :verb)))
          (when rule
            (setf (mrule-strategy rule) :suffix)
            (setf (mrule-marker rule) marker))))
      (generate-paradigms proto)

      (let ((rate (measure-paradigm-collision :accusative :genitive :noun proto)))
        (format t "Collision rate (should be 0): ~a~%" rate))

      (let ((fixed (disambiguate-markers proto)))
        (format t "disambiguate-markers returned (should be NIL): ~a~%" fixed)
        (assert (null fixed) nil "Expected no fixes when markers differ"))))

  (format t "~%=== Test 4: Mixed strategies (bound vs particle) - should skip ===~%")
  (setf *random-state* (sb-ext:seed-random-state 55))
  (let* ((proto (language-helper
                 (initialize-zipflist
                  (filter-consonant *safe-consonants*
                                    :place '(alveolar) :manner '(plosive nasal)))
                 :name "test-proto4"
                 :num-vowels 3)))
    (generate-grammar proto :typology :synthetic)
    (define-word proto "dog" 'noun :syllables 1)
    (generate-paradigms proto)

    (let* (;; Set acc to suffix and gen to particle (mixed — should be skipped)
           ;; But also set ALL other case features to distinct suffixes so they don't interfere
           (case-features '(:accusative :genitive :oblique-with :oblique-from
                            :oblique-to :oblique-on :oblique-at :oblique-over))
           (unique-markers '(("k" "a") ("k" "a") ("s" "i") ("d" "o")
                             ("n" "e") ("t" "a") ("s" "o") ("d" "i")))
           (tense-features '(:past :future))
           (tense-markers '(("n" "o") ("t" "i"))))
      ;; Set all case rules to suffix with distinct markers, except gen which is particle
      (iter (for feat in case-features)
        (for marker in unique-markers)
        (let ((rule (find-morpheme-rule proto feat :noun)))
          (when rule
            (if (eql feat :genitive)
                (progn
                  (setf (mrule-strategy rule) :particle-before)
                  (setf (mrule-marker rule) marker))
                (progn
                  (setf (mrule-strategy rule) :suffix)
                  (setf (mrule-marker rule) marker))))))
      ;; Set verb tense rules distinctly too
      (iter (for feat in tense-features)
        (for marker in tense-markers)
        (let ((rule (find-morpheme-rule proto feat :verb)))
          (when rule
            (setf (mrule-strategy rule) :suffix)
            (setf (mrule-marker rule) marker))))
      (generate-paradigms proto)

      (let ((fixed (disambiguate-markers proto)))
        (format t "disambiguate-markers returned (should be NIL for mixed): ~a~%" fixed)
        (assert (null fixed) nil "Expected no fixes for mixed bound/particle pair"))))

  (format t "~%=== Test 5: Verb tense collision ===~%")
  (setf *random-state* (sb-ext:seed-random-state 33))
  (let* ((proto (language-helper
                 (initialize-zipflist
                  (filter-consonant *safe-consonants*
                                    :place '(alveolar) :manner '(plosive nasal)))
                 :name "test-proto5"
                 :num-vowels 3)))
    (generate-grammar proto :typology :synthetic)
    (define-word proto "run" 'verb :syllables 1)
    (define-word proto "eat" 'verb :syllables 1)
    (generate-paradigms proto)

    (let* ((past-rule (find-morpheme-rule proto :past :verb))
           (future-rule (find-morpheme-rule proto :future :verb)))
      (when (and past-rule future-rule
                 (mrule-marker past-rule) (mrule-marker future-rule))
        ;; Force both to suffix with same marker
        (setf (mrule-strategy past-rule) :suffix)
        (setf (mrule-marker past-rule) '("d" "a"))
        (setf (mrule-strategy future-rule) :suffix)
        (setf (mrule-marker future-rule) '("d" "a"))
        (generate-paradigms proto)

        (format t "Forced verb tense collision: past=~a future=~a~%"
                (mrule-marker past-rule) (mrule-marker future-rule))
        (let ((fixed (disambiguate-markers proto)))
          (format t "disambiguate-markers returned: ~a~%" fixed)
          (format t "After: past strategy=~a marker=~a~%" (mrule-strategy past-rule) (mrule-marker past-rule))
          (format t "After: future strategy=~a marker=~a disambig-particle=~a~%"
                  (mrule-strategy future-rule) (mrule-marker future-rule) (mrule-disambig future-rule))
          (assert fixed nil "Expected verb tense collision to be fixed")
          ;; Future should now be a compound disambig strategy
          (assert (member (mrule-strategy future-rule) '(:disambig-suffix-before :disambig-suffix-after))
                  nil "Expected future-rule to be switched to disambig strategy")
          ;; Merged marker preserved
          (assert (equal (mrule-marker future-rule) '("d" "a"))
                  nil "Expected future-rule to keep merged marker")
          ;; Disambig particle present
          (assert (mrule-disambig future-rule)
                  nil "Expected future-rule to have disambig-particle")))))

  (format t "~%=== All tests passed! ===~%"))

(run-disambiguate-tests)
