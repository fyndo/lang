(in-package #:lang)

(initialize)

(defun run-borrow-tests ()
  (setf *random-state* (sb-ext:seed-random-state 42))

  (format t "~%=== Test 1: Borrow missing words between two languages ===~%")
  (let* ((lang-a (language-helper
                  (initialize-zipflist
                   (filter-consonant *safe-consonants*
                                     :place '(alveolar) :manner '(plosive nasal)))
                  :name "lang-a"
                  :num-vowels 3))
         (lang-b (language-helper
                  (initialize-zipflist
                   (filter-consonant *safe-consonants*
                                     :place '(bilabial) :manner '(plosive nasal)))
                  :name "lang-b"
                  :num-vowels 3)))
    (generate-grammar lang-a :typology :analytic)
    (generate-grammar lang-b :typology :analytic)

    ;; Give lang-a some words lang-b doesn't have
    (define-word lang-a "sun" 'noun :syllables 1)
    (define-word lang-a "moon" 'noun :syllables 1)
    (define-word lang-a "wolf" 'noun :syllables 1)

    ;; Give lang-b some words lang-a doesn't have
    (define-word lang-b "fire" 'noun :syllables 1)
    (define-word lang-b "stone" 'noun :syllables 1)

    ;; Give both a shared word
    (define-word lang-a "water" 'noun :syllables 1)
    (define-word lang-b "water" 'noun :syllables 1)

    (let ((glosses-a (mapcar #'gloss (lexicon lang-a)))
          (glosses-b (mapcar #'gloss (lexicon lang-b))))
      (format t "Before: lang-a has ~a glosses: ~a~%" (length glosses-a) glosses-a)
      (format t "Before: lang-b has ~a glosses: ~a~%" (length glosses-b) glosses-b)
      (format t "A has that B lacks: ~a~%" (set-difference glosses-a glosses-b :test #'equal))
      (format t "B has that A lacks: ~a~%" (set-difference glosses-b glosses-a :test #'equal)))

    (borrow-missing-words lang-a lang-b)

    (let ((glosses-a (mapcar #'gloss (lexicon lang-a)))
          (glosses-b (mapcar #'gloss (lexicon lang-b))))
      (format t "~%After: lang-a has ~a glosses: ~a~%" (length glosses-a) glosses-a)
      (format t "After: lang-b has ~a glosses: ~a~%" (length glosses-b) glosses-b)
      (format t "A has that B lacks: ~a~%" (set-difference glosses-a glosses-b :test #'equal))
      (format t "B has that A lacks: ~a~%" (set-difference glosses-b glosses-a :test #'equal)))

    ;; Verify no gaps remain
    (let ((glosses-a (mapcar #'gloss (lexicon lang-a)))
          (glosses-b (mapcar #'gloss (lexicon lang-b))))
      (assert (null (set-difference glosses-a glosses-b :test #'equal)))
      (assert (null (set-difference glosses-b glosses-a :test #'equal))))

    ;; Verify borrowed entries are marked as loans
    (let ((loans-a (remove-if-not (lambda (e) (and (consp (origin e)) (eq :loan (car (origin e)))))
                                  (lexicon lang-a))))
      (format t "~%Loans into lang-a: ~a~%" (mapcar #'gloss loans-a))
      (assert (= (length loans-a) 2) nil "Expected 2 loans into lang-a, got ~a" (length loans-a))
      (assert (every (lambda (l) (string= "lang-b" (cdr (origin l)))) loans-a)
              nil "All loans into lang-a should be from lang-b"))

    (let ((loans-b (remove-if-not (lambda (e) (and (consp (origin e)) (eq :loan (car (origin e)))))
                                  (lexicon lang-b))))
      (format t "Loans into lang-b: ~a~%" (mapcar #'gloss loans-b))
      (assert (= (length loans-b) 3) nil "Expected 3 loans into lang-b, got ~a" (length loans-b))
      (assert (every (lambda (l) (string= "lang-a" (cdr (origin l)))) loans-b)
              nil "All loans into lang-b should be from lang-a"))

    ;; Verify "water" was NOT borrowed (both already had it)
    (let ((water-a (find "water" (lexicon lang-a) :key #'gloss :test #'equal))
          (water-b (find "water" (lexicon lang-b) :key #'gloss :test #'equal)))
      (assert (not (and (consp (origin water-a)) (eq :loan (car (origin water-a)))))
              nil "water in lang-a should not be a loan")
      (assert (not (and (consp (origin water-b)) (eq :loan (car (origin water-b)))))
              nil "water in lang-b should not be a loan")))

  (format t "~%=== All borrow tests passed! ===~%"))

(run-borrow-tests)
