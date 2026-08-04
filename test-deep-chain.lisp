(in-package #:lang)

;;; Deep-chain scenario test.  Load demo.lisp and demo-english.lisp first.
;;;
;;;   proto-halfling ──→ river-halfling ──→ late-river-halfling ─┐ lexifier
;;;              └─────→ marsh-halfling                          │
;;;   proto-dwarvish ──loan "iron"──→ marsh-halfling             │
;;;   marsh-halfling ──loan "iron"──→ late-river-halfling        │
;;;   proto-orcish ────────────────────────────────── substrate ─┤
;;;                                            west-sea-creole ←─┘ (pidginize)
;;;                                                  │
;;;                                            early-imperial   (derive)
;;;                                                  │
;;;                                            middle-imperial  (derive + loan
;;;                    proto-gnomish ──influx──→    │            influx)
;;;                                            imperial-trade   (derive)
;;;
;;; Then: retrofit English targets through imperial-trade (chain root is the
;;; creole) and refresh the chain — the gnomish influx must survive the
;;; refresh via borrow replay.

(defparameter *deep-influx-glosses*
  '("trade" "gold" "merchant" "letter" "clerk" "harbor" "ship" "law")
  "Commerce vocabulary middle-imperial borrows wholesale from proto-gnomish.")

(defun show-word (label lang gloss)
  (let ((entry (lookup-word lang gloss)))
    (if entry
        (format t "  ~28a ~14a ~14a origin ~s~%"
                label
                (alt-print-word (form entry) nil #'anglicize)
                (alt-print-word (form entry))
                (origin entry))
        (format t "  ~28a (missing)~%" label))))

(defun run-deep-chain (&key (seed 42)
                         (schedule (default-anneal-schedule :stages 20 :steps 4000)))
  (setf *random-state* (sb-ext:seed-random-state seed))
  (initialize)
  (let* ((halfling (build-english-proto #'halfling 12 :mixed "proto-halfling"))
         (dwarvish (build-english-proto #'dwarvish 12 :mixed "proto-dwarvish"))
         (orcish (build-english-proto #'orcish 10 :synthetic "proto-orcish"))
         (gnomish (build-english-proto #'gnomish 14 :analytic "proto-gnomish"))
         (corpus (append (demo-phrases) (sample-phrases)))
         ;; proto-halfling splits
         (river (derive-language halfling
                                 '((C :manner fricative -> :manner plosive)
                                   (V :backness central -> :backness back))
                                 :name "river-halfling"))
         (marsh (derive-language halfling
                                 '((C :voicing voiced :manner plosive -> :manner nasal)
                                   (V :height close -> :height close-mid))
                                 :name "marsh-halfling")))
    ;; marsh borrows "iron" from dwarvish
    (format t "~%--- marsh-halfling borrows 'iron' from proto-dwarvish ---~%")
    (borrow-word marsh dwarvish "iron" :schedule schedule)
    ;; river evolves further; the loan travels laterally into the later stage
    (let ((late-river (derive-language river
                                       '((C :place uvular -> :place velar)
                                         (V :height near-close -> :height close))
                                       :name "late-river-halfling")))
      (format t "~%--- late-river-halfling borrows 'iron' from marsh-halfling ---~%")
      (borrow-word late-river marsh "iron" :schedule schedule)
      ;; the creole: late river halfling lexifier over an orcish substrate
      (let ((creole (pidginize late-river orcish corpus :name "west-sea-creole")))
        (apply-english-grammar creole)
        (let* ((early (derive-language creole
                                       '((C :place retroflex -> :place alveolar))
                                       :name "early-imperial"
                                       :cliticization-rate 0))
               (middle (derive-language early
                                        '((C :manner lateral-fricative -> :manner fricative))
                                        :name "middle-imperial"
                                        :cliticization-rate 0)))
          ;; the gnomish influx defines middle imperial
          (format t "~%--- middle-imperial: loan influx from proto-gnomish ---~%")
          (borrow-words middle gnomish *deep-influx-glosses* :schedule schedule)
          ;; multiple possible sources: audition donors in priority order —
          ;; gnomish first (the influx language middle-imperial defaults to);
          ;; a lower-priority donor must fit substantially better to win.
          (format t "~%--- middle-imperial borrows 'moon': gnomish default, dwarvish/orcish challengers ---~%")
          (multiple-value-bind (entry distance donor)
              (borrow-word-from-best middle (list gnomish dwarvish orcish) "moon"
                                     :schedule schedule)
            (declare (ignore distance))
            (assert entry () "borrow-word-from-best found no source for 'moon'")
            (assert (member donor (list dwarvish gnomish orcish)) ()
                    "winning donor not among the candidates"))
          (let ((trade (derive-language middle
                                        '((C :place uvular -> :place velar))
                                        :name "imperial-trade"
                                        :cliticization-rate 0)))
            ;; ── the word trail before any retrofit ──
            (format t "~%=== 'iron' through the family ===~%")
            (dolist (pair (list (cons "proto-dwarvish" dwarvish)
                                (cons "marsh-halfling" marsh)
                                (cons "late-river-halfling" late-river)
                                (cons "west-sea-creole" creole)
                                (cons "early-imperial" early)
                                (cons "middle-imperial" middle)
                                (cons "imperial-trade" trade)))
              (show-word (car pair) (cdr pair) "iron"))
            (format t "~%=== gnomish influx in middle-imperial and imperial-trade ===~%")
            (dolist (gloss *deep-influx-glosses*)
              (show-word (format nil "middle ~a" gloss) middle gloss)
              (show-word (format nil "trade  ~a" gloss) trade gloss))
            ;; ── retrofit English through the three-stage chain, refresh ──
            (format t "~%=== retrofit English targets through imperial-trade ===~%")
            (retrofit-english-lexicon trade
                                      '("water" "king" "man" "fire" "speak" "truth")
                                      :schedule schedule)
            (finish-creole-lexicon creole
                                   :exclude '("water" "king" "man" "fire" "speak" "truth"))
            (refresh-derivation-chain trade :schedule schedule)
            ;; ── verify: retrofits arrived AND the influx survived ──
            (format t "~%=== after refresh: retrofitted words in imperial-trade ===~%")
            (dolist (gloss '("water" "king" "man" "fire" "speak" "truth"))
              (show-word gloss trade gloss))
            (format t "~%=== after refresh: influx must have survived ===~%")
            (dolist (gloss *deep-influx-glosses*)
              (let ((m (lookup-word middle gloss))
                    (tr (lookup-word trade gloss)))
                (assert (and m (eql (car (origin m)) :loan)) ()
                        "middle-imperial lost influx word ~s" gloss)
                (assert tr () "imperial-trade lost influx word ~s" gloss)
                (show-word (format nil "middle ~a" gloss) middle gloss)
                (show-word (format nil "trade  ~a" gloss) trade gloss)))
            (assert (lookup-word trade "iron") () "imperial-trade lost 'iron'")
            (show-word "trade iron (post-refresh)" trade "iron")
            ;; the multi-source loan must survive refresh with its chosen donor
            (let ((moon (lookup-word middle "moon")))
              (assert (and moon (eql (car (origin moon)) :loan)) ()
                      "middle-imperial lost multi-source loan 'moon'")
              (show-word "middle moon (post-refresh)" middle "moon"))
            (format t "~%DEEP CHAIN OK~%")
            (list trade middle early creole late-river marsh river
                  halfling dwarvish orcish gnomish)))))))
