(in-package #:lang)

;;; Imperial trade language demo — English-target back-formation through a
;;; creole.  Load demo.lisp first (for demo-vocabulary, demo-phrases,
;;; translate-phrases).
;;;
;;; The chain:
;;;   proto-halfling ─┐
;;;                   ├─ pidginize ─→ imperial-creole ─→ imperial-trade
;;;   proto-orcish  ──┘                       └────────→ provincial-trade
;;;
;;; English targets are back-formed through the creole→imperial chain and
;;; installed in the creole's lexicon, then both daughters are re-derived, so
;;; imperial-trade comes out English-like while provincial-trade gets
;;; consistent cognates for free.

;;; Sound changes creole → imperial-trade: mild, chosen to trim the phones
;;; English lacks so the annealer has an easier target.

(defparameter *imperial-changes*
  '((C :place uvular -> :place velar)
    (C :place retroflex -> :place alveolar)
    (C :manner lateral-fricative -> :manner fricative)))

;;; Sound changes creole → provincial-trade: harsher, for audible divergence.

(defparameter *provincial-changes*
  '((C :voicing voiceless :manner plosive -> :manner fricative)
    (C :manner trill -> :manner tap)
    (V :height close-mid -> :height open-mid)))

(defun build-english-proto (ctor n typology name &key (vocabulary (demo-vocabulary)))
  "Build one proto-language with grammar, lexicon, and paradigms, retrying on
   generation failures the same way make-world does."
  (loop for attempt below 50
        for candidate = (handler-case
                            (let ((l (funcall ctor n :name name)))
                              (generate-grammar l :typology typology)
                              (generate-lexicon l vocabulary)
                              (generate-pronoun-lexicon l)
                              (generate-modal-lexicon l)
                              (generate-wh-lexicon l)
                              (generate-derived-lexicon l)
                              (generate-compound-lexicon l *compound-specs*)
                              (apply-compound-derivations l *compound-derivations*)
                              (generate-paradigms l)
                              l)
                          (error (e)
                            (when (= attempt 49)
                              (error "~a failed after 50 attempts: ~a" name e))
                            nil))
        when candidate return candidate))

(defun make-imperial-chain (&key (vocabulary (demo-vocabulary)) corpus
                              (english-grammar t))
  "Build the halfling/orc → creole → imperial/provincial chain.  The creole's
   derived/compound lexicon is NOT yet generated — do that after retrofitting
   so compounds are built from the retrofitted stems.  With ENGLISH-GRAMMAR
   (the default), the creole gets the hand-built English grammar in place of
   the creolized substrate grammar, and imperial-trade derives with zero
   strategy drift so the hand-picked grammar survives intact.
   Returns (values imperial provincial creole halfling orcish)."
  (let* ((halfling (build-english-proto #'halfling 12 :mixed "proto-halfling"
                                        :vocabulary vocabulary))
         (orcish (build-english-proto #'orcish 10 :synthetic "proto-orcish"
                                      :vocabulary vocabulary))
         (corpus (or corpus (append (demo-phrases) (sample-phrases))))
         (creole (pidginize halfling orcish corpus :name "imperial-creole")))
    (when english-grammar
      (apply-english-grammar creole))
    (let ((imperial (derive-language creole *imperial-changes*
                                     :name "imperial-trade"
                                     :cliticization-rate (if english-grammar 0 0.15)))
          (provincial (derive-language creole *provincial-changes*
                                       :name "provincial-trade")))
      (values imperial provincial creole halfling orcish))))

(defun finish-creole-lexicon (creole &key exclude)
  "Generate the creole's derived words, compounds, and paradigms.  Run AFTER
   retrofitting so everything is built from the retrofitted stems.  EXCLUDE
   lists glosses whose roots must not be replaced by compound derivations
   (i.e. the retrofitted ones)."
  (generate-derived-lexicon creole)
  (generate-compound-lexicon creole *compound-specs*)
  (apply-compound-derivations creole
                              (remove-if (lambda (spec)
                                           (member (first spec) exclude
                                                   :test #'equal))
                                         *compound-derivations*))
  (generate-paradigms creole))

(defun run-english (&key (seed 42)
                      (glosses (english-glosses))
                      (schedule (default-anneal-schedule :stages 25 :steps 5000))
                      (phrases (demo-phrases)))
  "Build the imperial chain, back-form English targets into the creole,
   re-derive the daughters, and translate PHRASES across the whole family.
   GLOSSES defaults to every English target; pass a subset for a quick run.
   SCHEDULE defaults to a light annealing schedule — pass
   (default-anneal-schedule) for full-quality (slow) back-formation."
  (setf *random-state* (sb-ext:seed-random-state seed))
  (initialize)
  (multiple-value-bind (imperial provincial creole halfling orcish)
      (make-imperial-chain)
    (format t "~%=== English phone coverage ===~%")
    (english-coverage-report imperial)
    (format t "~%=== Back-forming ~a English targets through ~a ===~%"
            (length glosses) (lang-name imperial))
    (retrofit-english-lexicon imperial glosses :schedule schedule)
    (finish-creole-lexicon creole :exclude glosses)
    (refresh-derivation-chain imperial)
    (refresh-derivation-chain provincial)
    ;; Cognate table: how the same root surfaces across the family
    (format t "~%~%=== COGNATES (gloss | creole | imperial | provincial) ===~%")
    (dolist (gloss glosses)
      (let ((c (lookup-word creole gloss))
            (i (lookup-word imperial gloss))
            (p (lookup-word provincial gloss)))
        (when (and c i p)
          (format t "~15a ~15a ~15a ~15a~%"
                  gloss
                  (alt-print-word (form c) nil #'anglicize)
                  (alt-print-word (form i) nil #'anglicize)
                  (alt-print-word (form p) nil #'anglicize)))))
    (translate-phrases (list halfling orcish creole imperial provincial)
                       phrases
                       "IMPERIAL TRADE TRANSLATIONS")
    (list imperial provincial creole halfling orcish)))
