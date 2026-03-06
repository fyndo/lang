(in-package :lang)

(defmethod place-number ((c consonant))
  (cond
    ((equal (place c) 'bilabial) 1)
    ((equal (place c) 'labiodental) 2)
    ((equal (place c) 'dental) 3)
    ((equal (place c) 'alveolar) 4)
    ((equal (place c) 'postalveolar) 5)
    ((equal (place c) 'retroflex) 6)
    ((equal (place c) 'labial-palatal) 7)
    ((equal (place c) 'palatal) 7)
    ((equal (place c) 'labial-velar) 8)
    ((equal (place c) 'velar) 8)
    ((equal (place c) 'uvular) 9)
    ((equal (place c) 'epiglottal) 10)
    ((equal (place c) 'glottal) 10)
    (t -1)))

(defmethod height-number ((v vowel))
  (cond
    ((equal (height v) 'open) 1)
    ((equal (height v) 'near-open) 2)
    ((equal (height v) 'open-mid) 3)
    ((equal (height v) 'mid) 4)
    ((equal (height v) 'close-mid) 5)
    ((equal (height v) 'near-close) 6)
    ((equal (height v) 'close) 7)
    (t -1)))

(defmethod backness-number ((v vowel))
  (cond
    ((equal (backness v) 'front) 1)
    ((equal (backness v) 'near-front) 2)
    ((equal (backness v) 'central) 3)
    ((equal (backness v) 'near-back) 4)
    ((equal (backness v) 'back) 5)
    (t -1)))

(defmethod backness-number ((p phone-point))
  (backness-number (phone p)))

(defgeneric ensure-raw-phone (p))

(defmethod ensure-raw-phone ((p phone))
  p)

(defmethod ensure-raw-phone ((p phone-point))
  (phone p))

(defmethod ensure-raw-phone (p)
  p)

(defmethod place-number ((p phone-point))
  (place-number (phone p)))

(defmethod height-number ((p phone-point))
  (height-number (phone p)))

(defmethod consonant-distance (p1 p2)
  (let ((r1 (ensure-raw-phone p1))
        (r2 (ensure-raw-phone p2)))
    (if (and (consonant-p r1) (consonant-p r2))
        (consonant-distance r1 r2)
        20)))

(defmethod consonant-distance ((c1 consonant) (c2 consonant))
  (if (string= (ipa c1) (ipa c2)) 0
      (let ((delta-place (- (place-number c1) (place-number c2)))
            (delta-sonority (- (sonority c1) (sonority c2))))
        (* (if (equal (place c1) (place c2))
               1
               (* 2 (+ 1 (/ (* delta-place delta-place) 25.0))))
           (cond
             ((and (equal (manner c1) (manner c2))
                   (equal (voicing c1) (voicing c2)))
              1)
             ((equal (manner c1) (manner c2)) 2)
             (t (* 2 (+ 2 (/ (* delta-sonority delta-sonority) 42.25)))))))))

(defmethod vowel-distance (p1 p2)
  (let ((r1 (ensure-raw-phone p1))
        (r2 (ensure-raw-phone p2)))
    (if (and (vowel-p r1) (vowel-p r2))
        (vowel-distance r1 r2)
        0)))

(defmethod vowel-distance ((v1 vowel) (v2 vowel))
  (cond
    ((equal (roman v1) (roman v2)) 0)
    (t (* (if (equal (rounding v1) (rounding v2)) 3 1)
          (let ((dh (- (height-number v1) (height-number v2))))
            (/ (* (1+ dh) (1+ dh)) 6))))))

(defun consonant-string-similarity (l1 l2)
  (if (or l1 l2)
      (+ (consonant-distance (first l1) (first l2))
         (consonant-string-similarity (rest l1) (rest l2)))
      0))


(defun vowel-normalize (word)
  (labels ((ensure-v (word)
             (cond
               ((null word) (list nil))
               ((vowel-p (first word)) (cons (first word) (ensure-c (rest word))))
               ((consonant-p (first word))
                (cons nil (ensure-c word)))))
           (ensure-c (word)
             (cond
               ((null word) nil) 
               ((vowel-p (first word)) (ensure-c (rest word))) ;; V1V2 -> V1
               ((consonant-p (first word)) (cons (first word) (ensure-v (rest word)))))))
    (ensure-v word)))

(defun aligned-vowel-similarity (original-loanword original-testword)
  (labels ((recur (loanword testword unused-loan unused-test)
             (let ((loan (first loanword))
                   (test (first testword)))
               (cond
                 ((or (null loanword)
                      (null testword))
                  0)
                 ((and (null loan)
                       (null test))
                  (recur (rest loanword) (rest testword) nil nil))
                 ((and (consonant-p loan)
                       (consonant-p test))
                  (recur (rest loanword) (rest testword) unused-loan unused-test))
                 ((and (vowel-p loan)
                       (vowel-p test))
                  (+ (vowel-distance loan test)
                     (recur (rest loanword) (rest testword) nil nil)))
                 ((and (vowel-p loan)
                       (null test)
                       (null unused-test))
                  (recur (rest loanword) (rest testword) loan nil))
                 ((and (vowel-p loan)
                       (null test)
                       unused-test)
                  (+ (vowel-distance loan unused-test)
                     (recur (rest loanword) (rest testword) nil nil)))
                 ((and (vowel-p test)
                       (null loan)
                       (null unused-loan))
                  (recur (rest loanword) (rest testword) nil test))
                 ((and (vowel-p test)
                       (null loan)
                       unused-loan)
                  (+ (vowel-distance unused-loan test)
                     (recur (rest loanword) (rest testword) nil nil)))
                 (t (progn
                      (format t "Error: ~a ~a ~a ~a~%" test loan unused-test unused-loan)
                      0))))))
    (recur (vowel-normalize original-loanword) (vowel-normalize original-testword) nil nil)))


(defun strip-markers (word)
  (remove-if-not #'phone-p word))

(defun loanword-similarity-max-consonants (loanword testword)
  (let ((l (strip-markers loanword))
        (t* (strip-markers testword)))
    (+ (aligned-vowel-similarity l t*)
       (* 3 (consonant-string-similarity (remove-if-not #'consonant-p l)
                                         (remove-if-not #'consonant-p t*))))))

(defun tweak-syllable (syllable language)
  (iter
    (with test = syllable)
    (while (equal syllable test))
    (setf test
          (case (random 3)
            (0 (list (onset language) (second syllable) (third syllable)))
            (1 (list (first syllable) (nucleus language) (third syllable)))
            (2 (list (first syllable) (second syllable) (coda language)))))
    (finally (return test))))

(defun replace-syllable (slist language)
  (if (null slist) slist
   (let ((n (random (length slist))))
     (append (subseq slist 0 n)
             (list (tweak-syllable (nth n slist) language))
             (subseq slist (1+ n))))))

(defun add-syllable (slist language)
  (if (null slist)
      (list (deconstructed-syllable language))
      (let ((n (random (length slist))))
        (append (subseq slist 0 n)
                (list (deconstructed-syllable language))
                (subseq slist n)))))

(defun remove-syllable (slist language)
  (declare (ignore language))
  (if (null slist)
      slist
      (let ((n (random (length slist))))
        (append (subseq slist 0 n)
             (subseq slist (1+ n))))))

(defun tweak-word (slist language)
  (case (random 5)
    (0 (add-syllable slist language))
    (1 (remove-syllable slist language))
    (2 (replace-syllable slist language))
    (3 (replace-syllable slist language))
    (4 (replace-syllable slist language))))


(defun metropolis (steps temperature loanword seed language &key debug fitness-fn)
  (let* ((fitness (or fitness-fn
                      (lambda (candidate)
                        (loanword-similarity-max-consonants loanword (flatten candidate)))))
         (best seed)
         (best-match (funcall fitness best)))
    (iter
      (for i from 0 to steps)
      (with last = best)
      (with last-match = best-match)
      (while (> best-match 0))
      (let* ((test (tweak-word last language))
              (test-match (funcall fitness test)))
         (when (< test-match best-match)
           (setf best test)
           (setf best-match test-match))
         (when (or (< test-match last-match)
                   (> (exp (/ (- last-match test-match )
                              temperature))
                      (random 1.0)))
           (when (and debug (not (= test-match last-match)))
               (format t "~5d ~6,2f ~6,2f ~6,2f ~6,2f ~6,2f ~a ~a ~a~%"
                    i temperature test-match last-match
                    (exp (/ (- last-match test-match ) 2.0))
                    best-match
                    (romanize (flatten test))
                    (romanize (flatten last))
                    (romanize (flatten best))
                    ))
           (setf last test)
           (setf last-match test-match)))
      (finally (return best)))))

(defun anneal (schedule loanword seed language &key debug fitness-fn)
  (if (null schedule) seed
      (anneal (rest schedule)
          loanword
          (metropolis (car (first schedule))
                      (cdr (first schedule))
                      loanword
                      seed
                      language :debug debug :fitness-fn fitness-fn)
          language
          :debug debug :fitness-fn fitness-fn)))

(fmakunbound 'find-loanword)
(defgeneric find-loanword (loanword language))

(defmethod find-loanword (loanword (language proto-language))
  (let* ((flat-loanword (flatten loanword))
         (result
           (anneal (iter (for i from 0 to 40)
                     (collect (cons 25000 (* 10 (exp (- (/ i 2)))))))
                   flat-loanword
                   (list (deconstructed-syllable language))
                   language))
         (word (reanalyze result)))
    (format t "~a ~a ~f~%"
            (alt-print-word word)
            (alt-print-word loanword)
            (loanword-similarity-max-consonants (flatten result) flat-loanword))
    word))

(defmethod find-loanword (loanword (lang derived-language))
  (multiple-value-bind (proto chain) (collect-derivation-chain lang)
    (let* ((flat-loanword (flatten loanword))
           (evolve-candidate (lambda (candidate)
                               (evolve chain
                                       (mapcar #'ensure-phone-point
                                               (flatten candidate)))))
           (result
             (anneal (iter (for i from 0 to 40)
                       (collect (cons 25000 (* 10 (exp (- (/ i 2)))))))
                     flat-loanword
                     (list (deconstructed-syllable proto))
                     proto
                     :fitness-fn (lambda (candidate)
                                   (loanword-similarity-max-consonants
                                    flat-loanword
                                    (flatten (funcall evolve-candidate candidate))))))
           (evolved (funcall evolve-candidate result)))
      (format t "~a ~a ~f~%"
              (alt-print-word evolved)
              (alt-print-word loanword)
              (loanword-similarity-max-consonants (flatten evolved) flat-loanword))
      evolved)))

(defun borrow-word (target-language donor-language gloss)
  (let* ((donor-entry (lookup-word donor-language gloss))
         (donor-form (form donor-entry))
         (adapted (find-loanword donor-form target-language))
         (entry (make-instance 'lexical-entry
                               :gloss gloss
                               :form adapted
                               :category (category donor-entry)
                               :origin (cons :loan (lang-name donor-language)))))
    (push entry (lexicon target-language))
    entry))

(defun borrow-missing-words (lang-a lang-b)
  "Find words each language has that the other lacks, and borrow them."
  (let ((glosses-a (mapcar #'gloss (lexicon lang-a)))
        (glosses-b (mapcar #'gloss (lexicon lang-b))))
    (dolist (gloss (set-difference glosses-a glosses-b :test #'equal))
      (borrow-word lang-b lang-a gloss))
    (dolist (gloss (set-difference glosses-b glosses-a :test #'equal))
      (borrow-word lang-a lang-b gloss))))
