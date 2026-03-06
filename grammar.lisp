;;;; grammar.lisp — render methods bridging semantics to language grammars

(in-package #:lang)

(defun class-gloss-tag (noun-class)
  "Return gloss tag for a noun class, e.g. :class-3 → \"III\"."
  (when noun-class
    (let ((n (parse-integer (subseq (symbol-name noun-class) 6))))
      (case n (1 "I") (2 "II") (3 "III") (4 "IV") (5 "V")
             (6 "VI") (7 "VII") (8 "VIII") (t (format nil "~d" n))))))

(defun collect-bound-features (features lang word-class)
  "From a list of features, return those with suffix/prefix rules, in order."
  (remove-if-not (lambda (f)
                   (let ((rule (find-morpheme-rule lang f word-class)))
                     (and rule (bound-rule-p rule))))
                 features))

(defun apply-paradigm-to-words (entry words paradigm-form)
  "Replace the base form in words with paradigm-form.
   The base form is identified by eq with (form entry)."
  (if paradigm-form
      (mapcar (lambda (w) (if (eq w (form entry)) paradigm-form w)) words)
      words))

(defun apply-inflection (words rule &key gloss lang feature applies-to)
  "Apply a single morpheme rule to a list of word-forms.
   For particle strategies, add the particle once (before/after the word list).
   For bound strategies (suffix/prefix), apply to each word form.
   When GLOSS is true, delegates to apply-gloss-inflection instead."
  (if gloss
      (apply-gloss-inflection words lang feature applies-to)
      (if (particle-strategy-p rule)
          (let ((marker (reanalyze (mapcar #'ensure-phone (mrule-marker rule)))))
            (case (mrule-strategy rule)
              (:particle-before (cons marker words))
              (:particle-after  (append words (list marker)))))
          (iter (for w in words) (appending (inflect w rule))))))

(defun inflect-entry (lang entry words features word-class &key gloss)
  "Apply morphological features to an entry's word-forms, using paradigm lookup
   when available, falling back to runtime inflection.
   When GLOSS is true, applies gloss inflection instead of phonological inflection."
  (if gloss
      (progn
        (dolist (feat features)
          (setf words (apply-gloss-inflection words lang feat word-class)))
        words)
      (let* ((bound-feats (when features
                            (collect-bound-features features lang word-class)))
             (paradigm-form (when (and entry bound-feats (inflected-forms entry))
                              (lookup-inflected entry bound-feats))))
        (if paradigm-form
            (progn
              (setf words (apply-paradigm-to-words entry words paradigm-form))
              (iter (for feat in features)
                (unless (member feat bound-feats)
                  (let ((rule (find-morpheme-rule lang feat word-class)))
                    (when rule
                      (setf words (apply-inflection words rule))))))
              words)
            (progn
              (iter (for feat in features)
                (let ((rule (find-morpheme-rule lang feat word-class)))
                  (when rule
                    (setf words (apply-inflection words rule)))))
              words)))))

(defun make-topic-word (lang)
  "Build the topic-particle word-form."
  (let ((tp (gfeature lang :topic-particle)))
    (when tp (reanalyze (mapcar #'ensure-phone tp)))))

(defun emit-base (lang name &key gloss entry)
  "Base word-forms (render) or gloss tokens (gloss mode)."
  (if gloss
      (derivation-gloss lang name)
      (if entry (entry-words lang entry) nil)))

(defun inflect-verb-entry (lang verb-entry verb-words tense agr-rule agr-feat needs-passive
                           &key skip-tense gloss agr-person agr-number)
  "Inflect verb words via paradigm lookup or runtime rules. Returns inflected words.
   Reuses pre-computed AGR-RULE instead of re-looking it up.
   When SKIP-TENSE is true, skip tense inflection (for topic-drop).
   When GLOSS is true, produces gloss tokens instead of phonological forms.
   AGR-PERSON and AGR-NUMBER are required in gloss mode for agreement tags."
  (if gloss
      ;; Gloss mode: apply tense and agreement as gloss annotations
      (let* ((effective-tense (if skip-tense nil tense)))
        ;; Tense gloss
        (when effective-tense
          (setf verb-words (apply-gloss-inflection verb-words lang effective-tense :verb)))
        ;; Agreement gloss
        (when agr-rule
          (let ((agr-tag (agreement-gloss-tag agr-person agr-number))
                (strategy (mrule-strategy agr-rule)))
            (cond
              ((eql strategy :none) nil)
              ((member strategy '(:particle-before :particle-after))
               (if (eql strategy :particle-before)
                   (setf verb-words (cons agr-tag verb-words))
                   (setf verb-words (append verb-words (list agr-tag)))))
              (t (let ((last-g (car (last verb-words))))
                   (when last-g
                     (setf (car (last verb-words))
                           (format nil "~a.~a" last-g agr-tag))))))))
        ;; Passive gloss
        (when needs-passive
          (setf verb-words (apply-gloss-inflection verb-words lang :passive :verb)))
        verb-words)
      ;; Render mode: existing paradigm + runtime code
      (let* ((effective-tense (if skip-tense nil tense))
             (verb-feats (remove nil (list effective-tense agr-feat
                                           (when needs-passive :passive))))
             (bound-feats (when verb-feats
                            (collect-bound-features verb-feats lang :verb)))
             (paradigm-form (when (and verb-entry bound-feats
                                       (inflected-forms verb-entry))
                              (lookup-inflected verb-entry bound-feats))))
        (if paradigm-form
            ;; Use paradigm form, apply particle features at runtime
            (progn
              (setf verb-words (apply-paradigm-to-words
                                verb-entry verb-words paradigm-form))
              (iter (for feat in verb-feats)
                (unless (member feat bound-feats)
                  (let ((rule (if (eq feat agr-feat)
                                  agr-rule
                                  (find-morpheme-rule lang feat :verb))))
                    (when rule
                      (setf verb-words (apply-inflection verb-words rule))))))
              verb-words)
            ;; Fall back to runtime inflect for all features
            (progn
              (let ((tense-rule (when effective-tense (find-morpheme-rule lang effective-tense :verb))))
                (when tense-rule
                  (setf verb-words (apply-inflection verb-words tense-rule))))
              (when agr-rule
                (setf verb-words (apply-inflection verb-words agr-rule)))
              (when needs-passive
                (let ((passive-rule (find-morpheme-rule lang :passive :verb)))
                  (when passive-rule
                    (setf verb-words (apply-inflection verb-words passive-rule)))))
              verb-words)))))

(defun apply-demote (lang demote-strategy demote words case-feature &key gloss)
  "Apply info-structure demotion to an NP's words.
   Returns (values new-words needs-passive-p)."
  (if (not demote)
      (values words nil)
      (case demote-strategy
        (:pro-drop (values nil nil))
        (:impersonal
         (if gloss
             (values (list "3PL") nil)
             (values (render lang (pro :3rd :plural) :features (list case-feature)) nil)))
        (:verb-morphology
         (let* ((passive-rule (find-morpheme-rule lang :passive :verb))
                (needs-passive (and passive-rule
                                   (not (eql (mrule-strategy passive-rule) :none)))))
           (values nil needs-passive)))
        (t (values words nil)))))

(defun apply-promote (lang promote-strategy subj-promote obj-promote
                      subject-words object-words effective-order &key gloss)
  "Apply info-structure promotion. Returns (values subject-words object-words effective-order)."
  (case promote-strategy
    (:fronting
     (when subj-promote
       (setf effective-order '(:subject :verb :object)))
     (when obj-promote
       (setf effective-order '(:object :verb :subject))))
    (:particle
     (let ((tp-word (if gloss "TOP" (make-topic-word lang))))
       (when tp-word
         (when subj-promote
           (setf subject-words (append subject-words (list tp-word))))
         (when obj-promote
           (setf object-words (append object-words (list tp-word))))))))
  (values subject-words object-words effective-order))

;;; Topic-drop — discourse context tracking

(defvar *discourse-referents* nil
  "When non-nil, a list wrapping an alist of (person . number) for established referents.")
(defvar *discourse-tense* nil
  "When non-nil, a list wrapping the current established tense keyword.")
(defvar *discourse-case* nil
  "When non-nil, a list wrapping an alist of (referent-key . case-feature) for established case.")

(defun discourse-active-p ()
  "True when discourse tracking is active (bound to a list, possibly empty alist)."
  (and *discourse-referents* (listp *discourse-referents*)))

(defun register-referent (person number)
  "Register a subject referent in the current discourse scope."
  (when (discourse-active-p)
    (let ((key (cons person number)))
      (unless (assoc person (first *discourse-referents*)
                     :test #'eql)
        (push key (first *discourse-referents*))))))

(defun referent-established-p (person number)
  "True if the given person+number referent has been established in current discourse."
  (declare (ignore number))
  (when (discourse-active-p)
    (assoc person (first *discourse-referents*) :test #'eql)))

(defun register-tense (tense)
  "Register a tense in the current discourse scope."
  (when (and (discourse-active-p) tense)
    (setf (first *discourse-tense*) tense)))

(defun tense-established-p (tense)
  "True if the given tense has been established in current discourse."
  (when (and (discourse-active-p) tense)
    (eql (first *discourse-tense*) tense)))

(defun register-case (referent-key case-feature)
  "Register a case feature for a referent in the current discourse scope."
  (when (discourse-active-p)
    (let ((existing (assoc referent-key (first *discourse-case*) :test #'equal)))
      (if existing
          (setf (cdr existing) case-feature)
          (push (cons referent-key case-feature) (first *discourse-case*))))))

(defun case-established-p (referent-key case-feature)
  "True if the given case for a referent has been established in current discourse."
  (when (discourse-active-p)
    (let ((existing (assoc referent-key (first *discourse-case*) :test #'equal)))
      (and existing (eql (cdr existing) case-feature)))))

(defun particle-strategy-p (rule)
  "True if a morpheme rule uses a particle strategy."
  (and rule (member (mrule-strategy rule)
                    '(:particle-before :particle-after))))

(defun topic-drop-skip-position (strategy n)
  "Given a morpheme strategy and N conjuncts, return a list of indices that should skip.
   particle-after → all but last; otherwise → all but first."
  (when (> n 0)
    (if (eql strategy :particle-after)
        (loop for i below (1- n) collect i)
        (loop for i from 1 below n collect i))))

(defmacro with-fresh-discourse-scope (&body body)
  "Bind fresh discourse tracking variables for a new scope.
Inherits established referents/tense from enclosing scope so nested clauses
can benefit from cross-clause topic drop."
  `(let ((*discourse-referents*
           (list (if (discourse-active-p)
                     (copy-alist (first *discourse-referents*))
                     nil)))
         (*discourse-tense*
           (list (if (discourse-active-p)
                     (first *discourse-tense*)
                     nil)))
         (*discourse-case*
           (list (if (discourse-active-p)
                     (copy-alist (first *discourse-case*))
                     nil))))
     ,@body))

(defmacro with-ensure-discourse-scope (&body body)
  "Ensure a discourse scope exists. If one is already active, just run body.
If not, create a fresh scope so register-referent/register-tense work."
  (let ((active (gensym "ACTIVE")))
    `(let ((,active (discourse-active-p)))
       (if ,active
           (progn ,@body)
           (with-fresh-discourse-scope ,@body)))))

(defgeneric render (language message &key features gloss &allow-other-keys))

(defmethod render ((lang language) (v verb) &key features skip-tense skip-subject gloss)
  (declare (ignore features))
  (with-ensure-discourse-scope
  (let* ((tense (tense v))
         (verb-entry (unless gloss (lookup-word lang (name v))))
         (verb-words (if gloss (list (name v))
                        (if verb-entry (entry-words lang verb-entry) nil)))
         (subject-words (when (and (subject v) (not skip-subject))
                          (render lang (subject v) :features '(:nominative) :gloss gloss))))
    (multiple-value-bind (agr-person agr-number)
        (extract-agreement (subject v))
      (let* ((agr-rule (find-agreement-rule lang agr-person agr-number))
             (agr-feat (when agr-rule (mrule-feature agr-rule))))
        ;; Pro-drop: elide pronoun subjects when verb is agreement-marked
        (when (and (not skip-subject)
                   (gfeature lang :pro-drop)
                   (typep (subject v) 'pronoun)
                   agr-rule)
          (setf subject-words nil))
        ;; Discourse-driven subject-pronoun drop (check BEFORE registering)
        (when (and (not skip-subject)
                   subject-words
                   (getf (gfeature lang :topic-drop) :subject-pronoun)
                   (typep (subject v) 'pronoun)
                   (referent-established-p (pronoun-person (subject v))
                                           (pro-number (subject v))))
          (let ((nom-rule (find-morpheme-rule lang :nominative :noun)))
            (when (or (not nom-rule) (particle-strategy-p nom-rule))
              (setf subject-words nil))))
        ;; Register subject AFTER drop check, BEFORE object rendering
        (when (subject v)
          (register-referent agr-person agr-number))
        (let* ((object-words (when (direct-object v)
                               (render lang (direct-object v) :features '(:accusative) :gloss gloss)))
               ;; Complementizer: mark clause-objects at the boundary
               (object-words (if (and object-words (typep (direct-object v) 'verb-like))
                                 (let ((comp-rule (find-morpheme-rule lang :complementizer :clause)))
                                   (if comp-rule
                                       (append (inflect (first object-words) comp-rule
                                                        :gloss gloss :lang lang
                                                        :feature :complementizer :applies-to :clause)
                                               (rest object-words))
                                       object-words))
                                 object-words))
               (effective-order (gfeature lang :clause-order))
               (demote-strategy (gfeature lang :demote-strategy))
               (subj-info (extract-info-structure (subject v)))
               (subj-promote (car subj-info)) (subj-demote (cdr subj-info))
               (obj-info (extract-info-structure (direct-object v)))
               (obj-promote (car obj-info)) (obj-demote (cdr obj-info)))
        ;; Apply demote
        (multiple-value-bind (subj-demoted needs-passive)
            (apply-demote lang demote-strategy subj-demote subject-words :nominative :gloss gloss)
          (setf subject-words subj-demoted)
          (setf object-words
                (nth-value 0 (apply-demote lang demote-strategy obj-demote
                                           object-words :accusative :gloss gloss)))
          ;; Gloss mode: handle verb-morphology passive annotation
          (when (and gloss needs-passive)
            (let ((last-g (car (last verb-words))))
              (when last-g
                (setf (car (last verb-words))
                      (format nil "~a.PASS" last-g)))))
          ;; Inflect verb (with discourse-driven tense suppression)
          (let ((discourse-skip-tense
                  (when (and (not skip-tense)
                             tense
                             (getf (gfeature lang :topic-drop) :shared-tense)
                             (tense-established-p tense))
                    (let ((tense-rule (find-morpheme-rule lang tense :verb)))
                      (particle-strategy-p tense-rule)))))
            (setf verb-words (inflect-verb-entry lang verb-entry verb-words
                                                 tense agr-rule agr-feat needs-passive
                                                 :skip-tense (or skip-tense discourse-skip-tense)
                                                 :gloss gloss
                                                 :agr-person agr-person
                                                 :agr-number agr-number)))
          ;; Register tense AFTER inflection (avoids self-suppression)
          (register-tense tense)
          ;; Promote
          (multiple-value-setq (subject-words object-words effective-order)
            (apply-promote lang (gfeature lang :promote-strategy)
                           subj-promote obj-promote
                           subject-words object-words effective-order
                           :gloss gloss))
          ;; Linearize
          (let ((clause-words (linearize effective-order
                                         (list :subject subject-words
                                               :verb verb-words
                                               :object object-words)))
                (oblique-words (iter (for ob in (obliques v))
                                 (appending (render lang ob :gloss gloss)))))
            (append clause-words oblique-words)))))))))

(defmethod render ((lang language) (n noun) &key features gloss)
  (let* ((entry (unless gloss (lookup-word lang (name n))))
         (nc (when entry (noun-class entry))))
    (if gloss
        ;; Gloss mode
        (let* ((base (name n))
               (tokens (derivation-gloss lang base))
               (gloss-entry (lookup-word lang (name n)))
               (gloss-nc (when gloss-entry (noun-class gloss-entry))))
          ;; Definiteness
          (let ((def (definiteness n)))
            (when def
              (let* ((class-def-feat (when gloss-nc
                                       (intern (format nil "~a-~a" (symbol-name def)
                                                       (symbol-name gloss-nc))
                                               :keyword)))
                     (effective-feat (if (and class-def-feat
                                              (find-morpheme-rule lang class-def-feat :noun))
                                         class-def-feat
                                         def))
                     (rule (find-morpheme-rule lang effective-feat :noun)))
                (when rule
                  (let ((class-tag (class-gloss-tag gloss-nc))
                        (base-gloss-tokens (morpheme-gloss lang effective-feat :noun (first tokens))))
                    (when class-tag
                      (let ((first-g (first base-gloss-tokens)))
                        (when (and first-g (not (eql effective-feat def)))
                          (setf (first base-gloss-tokens)
                                (format nil "~a.~a" first-g class-tag)))))
                    (setf tokens (append base-gloss-tokens (rest tokens))))))))
          ;; Positional features (accusative, etc.)
          (iter (for feat in features)
            (setf tokens (apply-gloss-inflection tokens lang feat :noun)))
          ;; Traits (plural, etc.)
          (iter (for trait in (traits n))
            (setf tokens (apply-gloss-inflection tokens lang trait :noun)))
          tokens)
        ;; Render mode
        (let ((words (if entry (entry-words lang entry) nil)))
          ;; Resolve definiteness feature (class-specific or base)
          (let* ((def (definiteness n))
                 (resolved-def
                   (when def
                     (let* ((class-def-feat (when nc
                                              (intern (format nil "~a-~a" (symbol-name def)
                                                              (symbol-name nc))
                                                      :keyword)))
                            (rule (or (when class-def-feat
                                        (find-morpheme-rule lang class-def-feat :noun))
                                      (find-morpheme-rule lang def :noun))))
                       (when rule
                         (if (and class-def-feat
                                  (find-morpheme-rule lang class-def-feat :noun))
                             class-def-feat
                             def)))))
                 ;; Collect all features in canonical order
                 (all-feats (remove nil (append (when resolved-def (list resolved-def))
                                                features
                                                (traits n)))))
            (inflect-entry lang entry words all-feats :noun))))))

(defmethod render ((lang language) (p pronoun) &key features gloss)
  (if gloss
      ;; Gloss mode
      (let* ((person (pronoun-person p))
             (number (pro-number p))
             (nc (when (eql person :3rd)
                   (find-if (lambda (k) (member k (gfeature lang :noun-classes)))
                            (traits p))))
             (class-tag (class-gloss-tag nc))
             (base-tag (if class-tag
                           (format nil "~a~a.~a"
                                   (case person (:1st "1") (:2nd "2") (:3rd "3"))
                                   (if (eql number :plural) "PL" "SG")
                                   class-tag)
                           (format nil "~a~a"
                                   (case person (:1st "1") (:2nd "2") (:3rd "3"))
                                   (if (eql number :plural) "PL" "SG"))))
             (tokens (list base-tag)))
        ;; Apply case features
        (iter (for feat in features)
          (setf tokens (apply-gloss-inflection tokens lang feat :noun)))
        ;; Plural disambiguation
        (when (and (eql number :plural)
                   (needs-plural-disambiguation person (gfeature lang :pronoun-collapse)))
          (case (gfeature lang :plural-pronoun-strategy)
            (:reduplication (setf tokens (append tokens (list base-tag))))
            ((:quantifier-before) (setf tokens (cons "PL.Q" tokens)))
            ((:quantifier-after) (setf tokens (append tokens (list "PL.Q"))))))
        tokens)
      ;; Render mode
      (let* ((nc (when (eql (pronoun-person p) :3rd)
                   (find-if (lambda (k) (member k (gfeature lang :noun-classes)))
                            (traits p))))
             (pro-gloss (if nc
                            (format nil "~a-~a" (name p) (symbol-name nc))
                            (name p)))
             (entry (or (lookup-word lang pro-gloss)
                        (lookup-word lang (name p))))
             (words (if entry (entry-words lang entry) nil)))
        (setf words (inflect-entry lang entry words features :noun))
        ;; Apply plural disambiguation if needed
        (when (and (eql (pro-number p) :plural)
                   (needs-plural-disambiguation (pronoun-person p)
                                                (gfeature lang :pronoun-collapse)))
          (let ((strategy (gfeature lang :plural-pronoun-strategy)))
            (case strategy
              (:quantifier-after
               (let ((q-entry (lookup-word lang "plural-quantifier")))
                 (when q-entry
                   (setf words (append words (list (form q-entry)))))))
              (:quantifier-before
               (let ((q-entry (lookup-word lang "plural-quantifier")))
                 (when q-entry
                   (setf words (append (list (form q-entry)) words)))))
              (:reduplication
               (when words
                 (setf words (list (reanalyze (append (strip-markers (first words))
                                                      (strip-markers (first words)))))))))))
        words)))

;;; Compound word — on-the-fly rendering for non-lexicalized compounds

(defmethod render ((lang language) (cw compound-word) &key features gloss)
  (if gloss
      (list (format nil "~a-~a" (compound-modifier cw) (compound-head cw)))
      (let* ((strategy (gfeature lang :compound-strategy))
             (order (gfeature lang :compound-order))
             (head-gloss (compound-head cw))
             (mod-gloss (compound-modifier cw))
             ;; Check if this compound is already in the lexicon as a fused entry
             (compound-gloss (format nil "~a-~a" mod-gloss head-gloss))
             (existing (lookup-word lang compound-gloss)))
        (if existing
            ;; Pre-lexicalized compound — render as a plain noun
            (let ((words (entry-words lang existing)))
              (inflect-entry lang existing words features :noun))
            ;; On-the-fly compound — fuse head and modifier per strategy
            (let* ((head-entry (lookup-word lang head-gloss))
                   (mod-entry (lookup-word lang mod-gloss))
                   (head-form (when head-entry (strip-markers (form head-entry))))
                   (mod-form (when mod-entry (strip-markers (form mod-entry)))))
              (when (and head-form mod-form)
                (let* ((ordered (if (eql order :head-final)
                                    (list mod-form head-form)
                                    (list head-form mod-form)))
                       (fused (case strategy
                                (:juxtapose
                                 (reanalyze (append (first ordered) (second ordered))))
                                (:linking
                                 (let ((linker (mapcar #'ensure-phone
                                                       (gfeature lang :compound-linker))))
                                   (reanalyze (append (first ordered) linker
                                                      (second ordered)))))
                                (:genitive
                                 (let* ((gen-rule (find-morpheme-rule lang :genitive :noun))
                                        (mod-inflected (if gen-rule
                                                          (strip-markers
                                                           (first (inflect (reanalyze mod-form)
                                                                           gen-rule)))
                                                          mod-form)))
                                   (reanalyze (if (eql order :head-final)
                                                  (append mod-inflected head-form)
                                                  (append head-form mod-inflected)))))))
                       (words (list fused)))
                  ;; Apply noun features to the fused form
                  (iter (for feat in features)
                    (let ((rule (find-morpheme-rule lang feat :noun)))
                      (when rule
                        (setf words (apply-inflection words rule)))))
                  words)))))))

(defmethod render ((lang language) (np noun-phrase) &key features gloss)
  (let* ((np-order (gfeature lang :np-order))
         (parts (components np))
         (noun-part (find-if (lambda (c) (typep c 'noun-like)) parts))
         (adjs (remove-if-not (lambda (c) (typep c 'adjective)) parts))
         (noun-words (render lang noun-part :features features :gloss gloss))
         (nc (when (typep noun-part 'noun-like)
               (extract-noun-class lang noun-part)))
         (adj-features (when nc (list nc)))
         (adj-words (iter (for a in adjs)
                      (appending (render lang a :features adj-features :gloss gloss)))))
    (linearize np-order
               (list :adjective adj-words
                     :noun noun-words))))

(defmethod render ((lang language) (a adjective) &key features gloss)
  (if gloss
      (let ((tokens (list (name a))))
        (iter (for feat in features)
          (let ((rule (find-morpheme-rule lang feat :adjective)))
            (when rule
              (let ((class-tag (class-gloss-tag feat)))
                (if class-tag
                    (if (particle-strategy-p rule)
                        (let ((tag class-tag))
                          (if (eql (mrule-strategy rule) :particle-before)
                              (setf tokens (cons tag tokens))
                              (setf tokens (append tokens (list tag)))))
                        (setf tokens (list (format nil "~a.~a" (first tokens) class-tag))))
                    (setf tokens (apply-gloss-inflection tokens lang feat :adjective)))))))
        tokens)
      (let* ((entry (lookup-word lang (name a)))
             (words (if entry (entry-words lang entry) nil)))
        (inflect-entry lang entry words features :adjective))))

(defmethod render ((lang language) (a adverb) &key features gloss)
  (declare (ignore features))
  (if gloss
      (list (name a))
      (let ((entry (lookup-word lang (name a))))
        (if entry (entry-words lang entry) nil))))

(defmethod render ((lang language) (vp verb-phrase) &key features gloss)
  (let* ((vp-order (gfeature lang :vp-order))
         (parts (components vp))
         (verb-part (find-if (lambda (c) (typep c 'verb-like)) parts))
         (advs (remove-if-not (lambda (c) (typep c 'adverb)) parts))
         (verb-words (render lang verb-part :features features :gloss gloss))
         (adv-words (iter (for a in advs) (appending (render lang a :features nil :gloss gloss)))))
    (linearize vp-order
               (list :adverb adv-words
                     :verb verb-words))))

(defmethod render ((lang language) (p possessive) &key features gloss)
  (let* ((gen-order (gfeature lang :gen-order))
         (topic-drop (gfeature lang :topic-drop))
         (possessor-words (render lang (possessor p) :features '(:genitive) :gloss gloss))
         (possessed-words (render lang (possessed p) :features features :gloss gloss)))
    ;; Possessor-pronoun drop: elide coreferent possessor pronoun
    (when (and (getf topic-drop :possessor-pronoun)
               (typep (possessor p) 'pronoun)
               (referent-established-p (pronoun-person (possessor p))
                                       (pro-number (possessor p))))
      (setf possessor-words nil))
    (if (eql gen-order :possessor-first)
        (append possessor-words possessed-words)
        (append possessed-words possessor-words))))

(defmethod render ((lang language) (neg negation) &key features gloss)
  (let* ((tgt (target neg))
         (applies-to (if (typep tgt 'verb-like) :verb :noun))
         (neg-rule (find-morpheme-rule lang :negation applies-to))
         (target-words (render lang tgt :features features :gloss gloss)))
    (if neg-rule
        (if gloss
            (append (morpheme-gloss lang :negation applies-to (first target-words))
                    (rest target-words))
            (let ((negated (inflect (first target-words) neg-rule)))
              (append negated (rest target-words))))
        target-words)))

;;; Oblique — "with X", "from X"

(defmethod render ((lang language) (ob oblique) &key features gloss)
  (declare (ignore features))
  (let* ((role-feature (intern (format nil "OBLIQUE-~a" (role ob)) :keyword))
         (rule (find-morpheme-rule lang role-feature :noun))
         (arg-words (render lang (argument ob) :gloss gloss)))
    (if gloss
        ;; Gloss mode
        (let ((role-label (string-upcase (symbol-name (role ob)))))
          (if (and rule (not (eql (mrule-strategy rule) :none)))
              (let ((strategy (mrule-strategy rule)))
                (cond
                  ((member strategy '(:particle-before :particle-after))
                   (if (eql strategy :particle-before)
                       (cons role-label arg-words)
                       (append arg-words (list role-label))))
                  (t ; suffix/prefix — annotate first word
                   (let ((tagged (format nil "~a.~a" (first arg-words) role-label)))
                     (cons tagged (rest arg-words))))))
              arg-words))
        ;; Render mode
        (if (and rule arg-words (particle-strategy-p rule))
            (let ((marker (reanalyze (mapcar #'ensure-phone (mrule-marker rule)))))
              (if (eql (mrule-strategy rule) :particle-before)
                  (cons marker arg-words)
                  (append arg-words (list marker))))
            (if (and rule arg-words)
                (let ((marked (inflect (first arg-words) rule)))
                  (append marked (rest arg-words)))
                arg-words)))))

;;; Copula / Predication — "X is Y"

(defmethod render ((lang language) (p predication) &key features gloss)
  (declare (ignore features))
  (with-ensure-discourse-scope
  (let* ((strategy (gfeature lang :copula-strategy))
         (subj-words (render lang (subject p) :features '(:nominative) :gloss gloss))
         (pred-words (render lang (predicate p) :gloss gloss)))
    ;; Pro-drop: elide pronoun subjects in copula clauses
    (when (and (gfeature lang :pro-drop) (typep (subject p) 'pronoun))
      (setf subj-words nil))
    ;; Discourse-driven subject-pronoun drop
    (when (and subj-words
               (getf (gfeature lang :topic-drop) :subject-pronoun)
               (typep (subject p) 'pronoun)
               (referent-established-p (pronoun-person (subject p))
                                       (pro-number (subject p))))
      (let ((nom-rule (find-morpheme-rule lang :nominative :noun)))
        (when (or (not nom-rule) (particle-strategy-p nom-rule))
          (setf subj-words nil))))
    ;; Register discourse context
    (when (typep (subject p) 'pronoun)
      (register-referent (pronoun-person (subject p))
                         (pro-number (subject p))))
    (when (tense p) (register-tense (tense p)))
    (let ((copula-words (case strategy
                          (:zero nil)
                          (:particle (if gloss
                                         (list "COP")
                                         (when (gfeature lang :copula-marker)
                                           (list (reanalyze (mapcar #'ensure-phone
                                                                    (gfeature lang :copula-marker)))))))
                          (:verb (if gloss
                                     (list "be")
                                     (let ((entry (lookup-word lang "be")))
                                       (if entry (entry-words lang entry) nil)))))))
      ;; Apply tense to copula
      (when (and (tense p) copula-words)
        (if gloss
            (setf copula-words (morpheme-gloss lang (tense p) :verb (first copula-words)))
            (let ((tense-rule (find-morpheme-rule lang (tense p) :verb)))
              (when tense-rule
                (setf copula-words (apply-inflection copula-words tense-rule))))))
      ;; Info-structure: promote on copula subject
      (when (and (car (extract-info-structure (subject p)))
                 (eql (gfeature lang :promote-strategy) :particle))
        (setf subj-words (append subj-words (list (if gloss "TOP" (make-topic-word lang))))))
      (linearize (gfeature lang :copula-order)
                 (list :subject subj-words
                       :copula copula-words
                       :predicate pred-words))))))

;;; Conditional — "if X then Y"

(defmethod render ((lang language) (c conditional) &key features gloss)
  (declare (ignore features))
  (with-fresh-discourse-scope
  (let* ((strategy (gfeature lang :conditional-strategy))
         (protasis-words (render lang (protasis c) :gloss gloss))
         (apodosis-words (render lang (apodosis c) :gloss gloss)))
    ;; Apply conditional marking to protasis
    (when (member strategy '(:particle :verb-morphology))
      (let ((rule (find-morpheme-rule lang :conditional :clause)))
        (when (and rule protasis-words)
          (setf protasis-words
                (append (inflect (first protasis-words) rule
                                 :gloss gloss :lang lang
                                 :feature :conditional :applies-to :clause)
                        (rest protasis-words))))))
    (linearize (gfeature lang :conditional-order)
               (list :protasis protasis-words
                     :apodosis apodosis-words)))))

;;; Imperative — "Don't X!", "Fear the wolf!"

(defmethod render ((lang language) (imp imperative) &key features gloss)
  (declare (ignore features))
  (let* ((inner (command imp))
         (inner-words (render lang inner :gloss gloss))
         (imp-rule (find-morpheme-rule lang :imperative :verb)))
    (if (and imp-rule inner-words)
        (append (inflect (first inner-words) imp-rule
                         :gloss gloss :lang lang
                         :feature :imperative :applies-to :verb)
                (rest inner-words))
        inner-words)))

;;; Comparative — "X-er than Y"

(defmethod render ((lang language) (c comparative) &key features gloss)
  (declare (ignore features))
  (let* ((adj-words (if gloss
                        (morpheme-gloss lang :comparative :adjective (quality c))
                        (let* ((entry (lookup-word lang (quality c)))
                               (words (if entry (entry-words lang entry) nil))
                               (comp-rule (find-morpheme-rule lang :comparative :adjective)))
                          (when comp-rule
                            (setf words (apply-inflection words comp-rule)))
                          words)))
         (target-words (when (comp-target c)
                         (render lang (comp-target c) :gloss gloss)))
         (std-words (when (comp-standard c)
                      (let ((words (render lang (comp-standard c) :gloss gloss))
                            (std-rule (find-morpheme-rule lang :comp-standard :noun)))
                        (if std-rule
                            (if gloss
                                (append (morpheme-gloss lang :comp-standard :noun (first words))
                                        (rest words))
                                (apply-inflection words std-rule))
                            words)))))
    (linearize (gfeature lang :comp-order)
               (list :target target-words
                     :quality adj-words
                     :standard std-words))))

;;; Conjunction — "X and Y"

(defmethod render ((lang language) (c conj-np) &key features gloss)
  (render-conjunction lang (conjuncts c) features :conj-type :np :gloss gloss))

(defmethod render ((lang language) (c conj-vp) &key features gloss)
  (declare (ignore features))
  (render-conjunction lang (conjuncts c) nil :conj-type :vp :gloss gloss))

(defun render-conjunction (lang conjuncts features &key (conj-type :np) gloss)
  (let* ((topic-drop (gfeature lang :topic-drop))
         (strategy (gfeature lang :conjunction-strategy))
         (marker-form (if gloss "AND"
                          (when (gfeature lang :conjunction-marker)
                            (reanalyze (mapcar #'ensure-phone
                                               (gfeature lang :conjunction-marker))))))
         (n (length conjuncts))
         (skip-subject-indices
           (when (and (eql conj-type :vp) (getf topic-drop :subject-pronoun))
             (let ((rule (find-morpheme-rule lang :nominative :noun)))
               (topic-drop-skip-position
                (if (and rule (particle-strategy-p rule))
                    (mrule-strategy rule)
                    :suffix)
                n))))
         (skip-tense-indices
           (when (and (eql conj-type :vp) (getf topic-drop :shared-tense))
             (let* ((tense (when (first conjuncts)
                             (and (typep (first conjuncts) 'verb-like)
                                  (typep (first conjuncts) 'verb)
                                  (tense (first conjuncts)))))
                    (rule (when tense (find-morpheme-rule lang tense :verb))))
               (when rule
                 (topic-drop-skip-position (mrule-strategy rule) n)))))
         (skip-case-indices
           (when (and (eql conj-type :np) (getf topic-drop :shared-case) features)
             (let* ((case-feat (first features))
                    (rule (find-morpheme-rule lang case-feat :noun)))
               (when (and rule (particle-strategy-p rule))
                 (topic-drop-skip-position (mrule-strategy rule) n)))))
         (drop-conjunction (getf topic-drop :conjunction-marker))
         (rendered
           (with-fresh-discourse-scope
             (iter (for item in conjuncts)
               (for i upfrom 0)
               (let ((skip-subj (member i skip-subject-indices))
                     (skip-t (member i skip-tense-indices))
                     (skip-case-feats (when (member i skip-case-indices)
                                        (remove (first features) features))))
                 (collect
                   (if (and (eql conj-type :vp) (or skip-subj skip-t))
                       (render lang item :features features
                                         :skip-tense skip-t
                                         :skip-subject skip-subj
                                         :gloss gloss)
                       (if skip-case-feats
                           (render lang item :features skip-case-feats :gloss gloss)
                           (render lang item :features features :gloss gloss)))))))))
    (case strategy
      (:medial
       (iter (for words in rendered)
         (for i upfrom 0)
         (when (and (> i 0) marker-form
                    (not (and drop-conjunction (> i 1))))
           (appending (list marker-form)))
         (appending words)))
      (:each
       (iter (for words in rendered)
         (for i upfrom 0)
         (appending words)
         (when (and marker-form
                    (not (and drop-conjunction (> i 0))))
           (appending (list marker-form)))))
      (:juxtaposition
       (iter (for words in rendered)
         (appending words)))
      (t (iter (for words in rendered)
           (appending words))))))

;;; Temporal subordination — "before X", "when X"

(defmethod render ((lang language) (tc temporal-clause) &key features gloss)
  (declare (ignore features))
  (with-fresh-discourse-scope
    (let* ((rel (relation tc))
           (feat (intern (format nil "TEMPORAL-~a" rel) :keyword))
           (rule (find-morpheme-rule lang feat :clause))
           (sub-words (render lang (subordinate tc) :gloss gloss))
           (main-words (render lang (main tc) :gloss gloss)))
      (when (and rule sub-words)
        (setf sub-words
              (append (inflect (first sub-words) rule
                               :gloss gloss :lang lang
                               :feature feat :applies-to :clause)
                      (rest sub-words))))
      (append sub-words main-words))))

;;; Relative clause — "the man who fears death"

(defun render-gapped-clause (lang clause role &key gloss)
  "Render a clause with the constituent matching ROLE suppressed."
  (typecase clause
    (verb
     (let ((tense (tense clause))
           (verb-words (if gloss
                           (list (name clause))
                           (let ((e (lookup-word lang (name clause))))
                             (if e (entry-words lang e) nil))))
           (subject-words (when (and (subject clause) (not (eql role :subject)))
                            (render lang (subject clause) :features '(:nominative) :gloss gloss)))
           (object-words (when (and (direct-object clause) (not (eql role :object)))
                           (render lang (direct-object clause) :features '(:accusative) :gloss gloss))))
       ;; Apply tense
       (when tense
         (if gloss
             (setf verb-words (apply-gloss-inflection verb-words lang tense :verb))
             (let ((tense-rule (find-morpheme-rule lang tense :verb)))
               (when tense-rule
                 (setf verb-words (apply-inflection verb-words tense-rule))))))
       ;; Agreement
       (when (and (subject clause) (not (eql (gfeature lang :agreement-type) :none)))
         (multiple-value-bind (agr-person agr-number)
             (extract-agreement (subject clause))
           (let ((agr-rule (find-agreement-rule lang agr-person agr-number)))
             (when agr-rule
               (if gloss
                   (let ((agr-tag (agreement-gloss-tag agr-person agr-number))
                         (strategy (mrule-strategy agr-rule)))
                     (cond
                       ((eql strategy :none) nil)
                       ((member strategy '(:particle-before :particle-after))
                        (if (eql strategy :particle-before)
                            (setf verb-words (cons agr-tag verb-words))
                            (setf verb-words (append verb-words (list agr-tag)))))
                       (t (let ((last-g (car (last verb-words))))
                            (setf (car (last verb-words))
                                  (format nil "~a.~a" last-g agr-tag))))))
                   (setf verb-words (apply-inflection verb-words agr-rule)))))))
       (let ((clause-words (linearize (gfeature lang :clause-order)
                                      (list :subject subject-words
                                            :verb verb-words
                                            :object object-words)))
             (oblique-words (iter (for ob in (obliques clause))
                              (appending (render lang ob :gloss gloss)))))
         (append clause-words oblique-words))))
    (t (render lang clause :gloss gloss))))

(defmethod render ((lang language) (rc relative-clause) &key features gloss)
  (with-fresh-discourse-scope
    (let* ((strategy (gfeature lang :relative-strategy))
           (order (gfeature lang :relative-order))
           (head-words (render lang (head-noun rc) :features features :gloss gloss))
           (clause-words (render-gapped-clause lang (rel-clause rc) (rel-role rc) :gloss gloss)))
      (when (and (eql strategy :particle) clause-words)
        (let ((rule (find-morpheme-rule lang :relative :clause)))
          (when rule
            (setf clause-words
                  (append (inflect (first clause-words) rule
                                   :gloss gloss :lang lang
                                   :feature :relative :applies-to :clause)
                          (rest clause-words))))))
      (linearize order
                 (list :head head-words
                       :clause clause-words)))))

;;; Purpose clause — "I write so that her children know"

(defmethod render ((lang language) (pc purpose-clause) &key features gloss)
  (declare (ignore features))
  (with-fresh-discourse-scope
    (let* ((order (gfeature lang :purpose-order))
           (main-words (render lang (main pc) :gloss gloss))
           (purpose-words (render lang (purpose pc) :gloss gloss)))
      (when purpose-words
        (let ((rule (find-morpheme-rule lang :purpose :clause)))
          (when rule
            (setf purpose-words
                  (append (inflect (first purpose-words) rule
                                   :gloss gloss :lang lang
                                   :feature :purpose :applies-to :clause)
                          (rest purpose-words))))))
      (linearize order
                 (list :main main-words
                       :purpose purpose-words)))))

;;; Causative — "the chains made us free"

(defmethod render ((lang language) (c causative) &key features gloss)
  (declare (ignore features))
  (with-fresh-discourse-scope
    (let* ((strategy (gfeature lang :causative-strategy))
           (order (gfeature lang :causative-order))
           (causer-words (render lang (causer c) :gloss gloss))
           (caused-words (render lang (caused c) :gloss gloss)))
      (case strategy
        (:analytic
         (let ((make-words (if gloss
                               (list "make")
                               (let ((make-entry (lookup-word lang "make")))
                                 (if make-entry (entry-words lang make-entry) nil)))))
           (linearize order
                      (list :causer (append causer-words make-words)
                            :caused caused-words))))
        (:morpheme
         (let ((rule (find-morpheme-rule lang :causative :verb)))
           (when (and rule caused-words)
             (setf caused-words
                   (append (inflect (first caused-words) rule
                                    :gloss gloss :lang lang
                                    :feature :causative :applies-to :verb)
                           (rest caused-words)))))
         (linearize order
                    (list :causer causer-words
                          :caused caused-words)))
        (t
         (linearize order
                    (list :causer causer-words
                          :caused caused-words)))))))

;;; Reported speech — "she said: write this"

(defmethod render ((lang language) (q quotation) &key features gloss)
  (declare (ignore features))
  (let* ((order (gfeature lang :quotation-order))
         (speaker-words (render lang (speaker q) :gloss gloss))
         (speak-words (if gloss
                          (if (tense q)
                              (morpheme-gloss lang (tense q) :verb "speak")
                              (list "speak"))
                          (let ((speak-entry (lookup-word lang "speak")))
                            (if speak-entry (entry-words lang speak-entry) nil))))
         (content-words (with-fresh-discourse-scope
                          (render lang (content q) :gloss gloss))))
    ;; Apply tense to speak verb (render mode only, gloss already handled above)
    (when (and (not gloss) (tense q))
      (let ((tense-rule (find-morpheme-rule lang (tense q) :verb)))
        (when tense-rule
          (setf speak-words (apply-inflection speak-words tense-rule)))))
    ;; Quotative marker
    (let ((rule (find-morpheme-rule lang :quotative :clause)))
      (when (and rule content-words)
        (setf content-words
              (append (inflect (first content-words) rule
                               :gloss gloss :lang lang
                               :feature :quotative :applies-to :clause)
                      (rest content-words)))))
    (linearize order
               (list :speaker speaker-words
                     :verb speak-words
                     :content content-words))))

;;; Complement clause — "I think that X"

(defmethod render ((lang language) (cc complement-clause) &key features gloss)
  (declare (ignore features))
  (let* ((order (gfeature lang :complement-order))
         (subj-words (render lang (comp-subject cc) :features '(:nominative) :gloss gloss))
         (verb-words (if gloss
                         (if (comp-tense cc)
                             (morpheme-gloss lang (comp-tense cc) :verb (comp-verb cc))
                             (list (comp-verb cc)))
                         (let ((verb-entry (lookup-word lang (comp-verb cc))))
                           (if verb-entry (entry-words lang verb-entry) nil))))
         (content-words (with-fresh-discourse-scope
                          (render lang (comp-content cc) :gloss gloss))))
    ;; Tense on matrix verb (render mode)
    (when (and (not gloss) (comp-tense cc))
      (let ((tense-rule (find-morpheme-rule lang (comp-tense cc) :verb)))
        (when tense-rule
          (setf verb-words (apply-inflection verb-words tense-rule)))))
    ;; Complementizer marker on content
    (let ((rule (find-morpheme-rule lang :complementizer :clause)))
      (when (and rule content-words)
        (setf content-words
              (append (inflect (first content-words) rule
                               :gloss gloss :lang lang
                               :feature :complementizer :applies-to :clause)
                      (rest content-words)))))
    (linearize order
               (list :subject subj-words
                     :verb verb-words
                     :content content-words))))

;;; Adversative conjunction — "X but Y"

(defmethod render ((lang language) (a adversative) &key features gloss)
  (declare (ignore features))
  (with-fresh-discourse-scope
    (let* ((strategy (gfeature lang :adversative-strategy))
           (order (gfeature lang :adversative-order))
           (conceded-words (render lang (conceded a) :gloss gloss))
           (asserted-words (render lang (asserted a) :gloss gloss)))
      (case strategy
        (:particle
         (let ((rule (find-morpheme-rule lang :adversative :clause)))
           (when (and rule asserted-words)
             (setf asserted-words
                   (append (inflect (first asserted-words) rule
                                    :gloss gloss :lang lang
                                    :feature :adversative :applies-to :clause)
                           (rest asserted-words))))))
        (:each
         (let ((rule (find-morpheme-rule lang :adversative :clause)))
           (when rule
             (when conceded-words
               (setf conceded-words
                     (append (inflect (first conceded-words) rule
                                      :gloss gloss :lang lang
                                      :feature :adversative :applies-to :clause)
                             (rest conceded-words))))
             (when asserted-words
               (setf asserted-words
                     (append (inflect (first asserted-words) rule
                                      :gloss gloss :lang lang
                                      :feature :adversative :applies-to :clause)
                             (rest asserted-words))))))))
      (linearize order
                 (list :conceded conceded-words
                       :asserted asserted-words)))))

;;; Exception — "no X except/but Y"

(defmethod render ((lang language) (e exception-clause) &key features gloss)
  (with-fresh-discourse-scope
    (let* ((excluded-words (render lang (excluded e) :features features :gloss gloss))
           (excepted-words (render lang (excepted e) :features features :gloss gloss))
           (rule (find-morpheme-rule lang :exception :noun)))
      (when (and rule excepted-words)
        (setf excepted-words
              (append (inflect (first excepted-words) rule
                               :gloss gloss :lang lang
                               :feature :exception :applies-to :noun)
                      (rest excepted-words))))
      (append excluded-words excepted-words))))

;;; Degree/result — "so X that Y"

(defmethod render ((lang language) (d degree-clause) &key features gloss)
  (declare (ignore features))
  (with-fresh-discourse-scope
    (let* ((order (gfeature lang :degree-order))
           (quality-words (render lang (degree-quality d) :gloss gloss))
           (result-words (render lang (degree-result d) :gloss gloss)))
      (let ((rule (find-morpheme-rule lang :degree :clause)))
        (when (and rule quality-words)
          (setf quality-words
                (append (inflect (first quality-words) rule
                                 :gloss gloss :lang lang
                                 :feature :degree :applies-to :clause)
                        (rest quality-words)))))
      (linearize order
                 (list :quality quality-words
                       :result result-words)))))

;;; Question (yes/no interrogative)

(defmethod render ((lang language) (q question) &key features gloss)
  (declare (ignore features))
  (let* ((strategy (gfeature lang :question-strategy))
         (inner-words (render lang (content q) :gloss gloss)))
    (if gloss
        (case strategy
          (:particle
           (let ((pos (gfeature lang :question-particle-position)))
             (if (eql pos :initial)
                 (cons "Q" inner-words)
                 (append inner-words (list "Q")))))
          (:verb-morphology
           (if inner-words
               (append (morpheme-gloss lang :interrogative :verb (first inner-words))
                       (rest inner-words))
               inner-words))
          (t ; :intonation
           (append inner-words (list "[?]"))))
        (case strategy
          (:particle
           (let* ((raw (gfeature lang :question-particle))
                  (particle (when raw (reanalyze (mapcar #'ensure-phone raw))))
                  (pos (gfeature lang :question-particle-position)))
             (if particle
                 (if (eql pos :initial)
                     (cons particle inner-words)
                     (append inner-words (list particle)))
                 inner-words)))
          (:verb-morphology
           (let ((rule (find-morpheme-rule lang :interrogative :verb)))
             (if (and rule inner-words)
                 (let ((marked (inflect (first inner-words) rule)))
                   (append marked (rest inner-words)))
                 inner-words)))
          (t inner-words)))))

;;; Modal — ability / obligation

(defmethod render ((lang language) (m modal) &key features gloss)
  (declare (ignore features))
  (let* ((strategy (gfeature lang :modal-strategy))
         (mod-type (modality m))
         (inner-words (render lang (content m) :gloss gloss)))
    (case strategy
      (:auxiliary
       (let* ((aux-gloss (if (eql mod-type :ability) "can" "must"))
              (aux-words (if gloss
                             (list (string-upcase aux-gloss))
                             (let ((entry (lookup-word lang aux-gloss)))
                               (if entry (entry-words lang entry) nil))))
              (order (gfeature lang :modal-order)))
         (if (eql order :modal-first)
             (append aux-words inner-words)
             (append inner-words aux-words))))
      ((:morpheme :particle)
       (let* ((feature (if (eql mod-type :ability) :modal-ability :modal-obligation))
              (rule (find-morpheme-rule lang feature :verb)))
         (if (and rule inner-words)
             (append (inflect (first inner-words) rule
                              :gloss gloss :lang lang
                              :feature feature :applies-to :verb)
                     (rest inner-words))
             inner-words)))
      (t inner-words))))

(defun render-string (language message &key (orthography #'roman))
  "Render a semantic message to a string. ORTHOGRAPHY is #'roman (default),
#'anglicize, or #'ipa."
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (word-form) (alt-print-word word-form nil orthography))
                  (render language message))))

;;; Gloss rendering — parallel to render but outputs annotated English glosses

(defgeneric render-gloss (language message &key features &allow-other-keys))

(defmethod render-gloss ((lang language) (obj t) &rest args &key &allow-other-keys)
  (apply #'render lang obj :gloss t args))

(defun gloss-tag (base &rest tags)
  "Build a gloss string like \"hunt.AGT\" or \"fear.PRS\"."
  (let ((active-tags (remove nil tags)))
    (if active-tags
        (format nil "~a.~{~a~^.~}" base (mapcar #'string-upcase
                                                  (mapcar #'symbol-name active-tags)))
        base)))

(defun derivation-gloss (lang gloss)
  "If GLOSS names a particle-derived entry, return gloss tokens reflecting the particle.
   Otherwise return (list gloss)."
  (let* ((entry (lookup-word lang gloss)))
    (if (and entry (listp (origin entry))
             (eql (first (origin entry)) :derived-from))
        (let* ((feature (third (origin entry)))
               (rule (find-if (lambda (r) (eql (mrule-feature r) feature))
                              (gfeature lang :morphology))))
          (if (and rule (member (mrule-strategy rule) '(:particle-before :particle-after)))
              (let ((tag (string-upcase (symbol-name feature))))
                (if (eql (mrule-strategy rule) :particle-before)
                    (list tag gloss)
                    (list gloss tag)))
              (list gloss)))
        (list gloss))))

(defun apply-gloss-inflection (tokens lang feature applies-to)
  "Apply a morpheme gloss to a token list.
   For particle strategies, add the tag at the list level (before/after).
   For bound strategies, annotate the base token via morpheme-gloss."
  (let ((rule (find-morpheme-rule lang feature applies-to)))
    (if (not rule)
        tokens
        (if (particle-strategy-p rule)
            (let ((tag (string-upcase (symbol-name feature))))
              (if (eql (mrule-strategy rule) :particle-before)
                  (cons tag tokens)
                  (append tokens (list tag))))
            (append (morpheme-gloss lang feature applies-to (first tokens))
                    (rest tokens))))))


(defun morpheme-gloss (lang feature applies-to base-gloss)
  "Return gloss tokens for a morpheme application: either annotated base or base + particle.
   Rules with :none strategy are invisible — they don't mark the word."
  (let ((rule (find-morpheme-rule lang feature applies-to)))
    (if (and rule (eql (mrule-strategy rule) :none))
        (list base-gloss)
    (if (and rule (member (mrule-strategy rule) '(:particle-before :particle-after)))
        (if (eql (mrule-strategy rule) :particle-before)
            (list (string-upcase (symbol-name feature)) base-gloss)
            (list base-gloss (string-upcase (symbol-name feature))))
    (if (and rule (member (mrule-strategy rule) '(:disambig-suffix-before :disambig-suffix-after)))
        ;; Compound: particle gloss + suffixed form gloss
        (if (eql (mrule-strategy rule) :disambig-suffix-before)
            (list (string-upcase (symbol-name feature)) (gloss-tag base-gloss feature))
            (list (gloss-tag base-gloss feature) (string-upcase (symbol-name feature))))
        (if rule
            (list (gloss-tag base-gloss feature))
            (list base-gloss)))))))


(defun render-gloss-string (language message)
  "Render a semantic message to a gloss string showing word-by-word meaning."
  (format nil "~{~a~^ ~}" (render-gloss language message)))
