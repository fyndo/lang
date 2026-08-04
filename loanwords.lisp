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

(defun default-anneal-schedule (&key (stages 41) (steps 25000))
  "Annealing schedule: STAGES exponentially-cooling temperature stages of
   STEPS Metropolis steps each."
  (iter (for i from 0 below stages)
    (collect (cons steps (* 10 (exp (- (/ i 2))))))))

(defun back-form-word (target-form lang &key (schedule (default-anneal-schedule))
                                          debug (restarts 1))
  "Search the root of LANG's derivation chain for a form that evolves into
   something close to TARGET-FORM in LANG.  Returns
   (values root-word evolved-word distance); for a proto-language (empty
   chain) root-word and evolved-word are the same word.  This is the core of
   both loanword adaptation and English-target back-formation: the annealer
   proposes root-language words, but fitness is measured on the surface form
   after running the whole chain of sound changes forward.  The anneal is
   stochastic — RESTARTS > 1 runs it that many times and keeps the best
   result (each restart costs a full SCHEDULE run)."
  (multiple-value-bind (root chain) (collect-derivation-chain lang)
    (let* ((flat-target (flatten target-form))
           (evolve-candidate
             (lambda (candidate)
               (if chain
                   (evolve chain (mapcar #'ensure-phone-point (flatten candidate)))
                   (flatten candidate))))
           (best-root nil) (best-evolved nil) (best-distance nil))
      (iter (repeat (max 1 restarts))
        (let* ((result
                 (anneal schedule
                         flat-target
                         (list (deconstructed-syllable root))
                         root
                         :debug debug
                         ;; A candidate that evolves to nothing would score 0
                         ;; against any vowel-only target — reject it outright.
                         :fitness-fn (lambda (candidate)
                                       (let ((surface (flatten (funcall evolve-candidate candidate))))
                                         (if (remove-if-not #'phone-p surface)
                                             (loanword-similarity-max-consonants
                                              flat-target surface)
                                             most-positive-fixnum)))))
               (root-word (reanalyze result))
               (evolved (if chain
                            (funcall evolve-candidate result)
                            root-word))
               (distance (loanword-similarity-max-consonants
                          (flatten evolved) flat-target)))
          (when (or (null best-distance) (< distance best-distance))
            (setf best-root root-word
                  best-evolved evolved
                  best-distance distance))
          (until (zerop best-distance))))
      (values best-root best-evolved best-distance))))

(fmakunbound 'find-loanword)
(defgeneric find-loanword (loanword language &key schedule restarts))

(defmethod find-loanword (loanword (language proto-language)
                          &key (schedule (default-anneal-schedule)) (restarts 1))
  (multiple-value-bind (word evolved distance)
      (back-form-word loanword language :schedule schedule :restarts restarts)
    (declare (ignore evolved))
    (format t "~a ~a ~f~%"
            (alt-print-word word)
            (alt-print-word loanword)
            distance)
    (values word distance)))

(defmethod find-loanword (loanword (lang derived-language)
                          &key (schedule (default-anneal-schedule)) (restarts 1))
  (multiple-value-bind (word evolved distance)
      (back-form-word loanword lang :schedule schedule :restarts restarts)
    (declare (ignore word))
    (format t "~a ~a ~f~%"
            (alt-print-word evolved)
            (alt-print-word loanword)
            distance)
    (values evolved distance)))

(defun borrow-word (target-language donor-language gloss
                    &key (schedule (default-anneal-schedule)) (restarts 1)
                      (record t) (install t))
  "Adapt DONOR-LANGUAGE's word for GLOSS into TARGET-LANGUAGE's phonology.
   With INSTALL (the default), push it onto the target's lexicon and record
   the borrow event in the target's ledger (unless RECORD is NIL, as during
   replay) so it can be re-run after a lexicon refresh.  Returns
   (values entry adaptation-distance)."
  (let* ((donor-entry (lookup-word donor-language gloss))
         (donor-form (form donor-entry)))
    (multiple-value-bind (adapted distance)
        (find-loanword donor-form target-language
                       :schedule schedule :restarts restarts)
      (let ((entry (make-instance 'lexical-entry
                                  :gloss gloss
                                  :form adapted
                                  :category (category donor-entry)
                                  :origin (cons :loan (lang-name donor-language)))))
        (when install
          (push entry (lexicon target-language))
          (when record
            (setf (borrowings target-language)
                  (append (borrowings target-language)
                          (list (cons donor-language gloss))))))
        (values entry distance)))))

(defun borrow-word-from-best (target-language donors gloss
                              &key (schedule (default-anneal-schedule)) (restarts 1))
  "Audition each of DONORS as the source for GLOSS: adapt each donor's word
   into TARGET-LANGUAGE and install only the adaptation that survived most
   faithfully (smallest adaptation distance).  Donors lacking the gloss are
   skipped.  The winning donor is recorded in the borrow ledger, so replays
   keep the chosen etymology.  Returns (values entry distance donor)."
  (let ((best nil) (best-distance nil) (best-donor nil))
    (dolist (donor donors)
      (when (lookup-word donor gloss)
        (multiple-value-bind (entry distance)
            (borrow-word target-language donor gloss
                         :schedule schedule :restarts restarts :install nil)
          (when (or (null best-distance) (< distance best-distance))
            (setf best entry
                  best-distance distance
                  best-donor donor)))))
    (when best
      (push best (lexicon target-language))
      (setf (borrowings target-language)
            (append (borrowings target-language)
                    (list (cons best-donor gloss))))
      (format t "  best source for ~s: ~a (~,2f)~%"
              gloss (lang-name best-donor) best-distance))
    (values best best-distance best-donor)))

(defun borrow-words (target-language donor-language glosses
                     &key (schedule (default-anneal-schedule)))
  "Borrow each of GLOSSES from DONOR-LANGUAGE into TARGET-LANGUAGE — a loan
   influx.  Glosses the donor lacks are skipped with a warning."
  (iter (for gloss in glosses)
    (if (lookup-word donor-language gloss)
        (collect (borrow-word target-language donor-language gloss
                              :schedule schedule))
        (warn "~a has no word for ~s; skipping borrow."
              (lang-name donor-language) gloss))))

(defun replay-borrowings (lang &key (schedule (default-anneal-schedule)))
  "Re-run LANG's recorded borrow events in order, re-adapting each loan from
   the donor's CURRENT lexicon.  Call after rebuilding LANG's lexicon; if
   donors changed too, refresh them first so the replay sees current forms."
  (iter (for (donor . gloss) in (borrowings lang))
    (if (lookup-word donor gloss)
        (borrow-word lang donor gloss :schedule schedule :record nil)
        (warn "Replay: ~a no longer has ~s; loan dropped."
              (lang-name donor) gloss))))

(defun borrow-missing-words (lang-a lang-b)
  "Find words each language has that the other lacks, and borrow them."
  (let ((glosses-a (mapcar #'gloss (lexicon lang-a)))
        (glosses-b (mapcar #'gloss (lexicon lang-b))))
    (dolist (gloss (set-difference glosses-a glosses-b :test #'equal))
      (borrow-word lang-b lang-a gloss))
    (dolist (gloss (set-difference glosses-b glosses-a :test #'equal))
      (borrow-word lang-a lang-b gloss))))
