(in-package #:lang)

(defclass message ()
  ())

(defclass phrase ()
  ((components :initform nil :accessor components :initarg :components)))

(defclass terminal ()
  ((name :accessor name :initarg :name)))

(defclass noun-like (message)
  ())

(defclass noun (noun-like terminal)
  ((traits :initform nil :accessor traits :initarg :traits)
   (definiteness :initform :definite :accessor definiteness :initarg :definiteness)))

(defclass pronoun (noun-like terminal)
  ((person :initform :3rd :accessor pronoun-person :initarg :person)
   (pro-number :initform :singular :accessor pro-number :initarg :number)
   (traits :initform nil :accessor traits :initarg :traits)))

(defclass noun-phrase (noun-like phrase)
  ())

(defclass adjective (message terminal)
  ())

(defclass adverb (message terminal)
  ())

(defclass verb-like (message)
  ())

(defclass verb-phrase (verb-like phrase)
  ())

(defclass verb (terminal verb-like)
  ((tense :initarg :tense :accessor tense :initform nil)
   (subject :initarg :subject :accessor subject :initform nil)
   (direct-object :initarg :direct-object :accessor direct-object :initform nil)
   (indirect-object :initarg :indirect-object :accessor indirect-object :initform nil)
   (obliques :initarg :obliques :accessor obliques :initform nil)))

(defmethod n (name &rest traits)
  (let ((def (if (member :indefinite traits) :indefinite :definite)))
    (make-instance 'noun :name name
                         :traits (remove :indefinite traits)
                         :definiteness def)))

(defun pro (person number &rest traits)
  (make-instance 'pronoun
    :name (format nil "~a~a"
                  (string-downcase (symbol-name person))
                  (string-downcase (symbol-name number)))
    :person person :number number :traits traits))

(defmethod adj (name (n noun))
  (make-instance 'noun-phrase :components (list (make-instance 'adjective :name name)
                                                n)))
(defmethod adj (name (n noun-phrase))
  (push (make-instance 'adjective :name name) (components n)))

(defun vb (name tense &key subject object indirect-object obliques)
  (make-instance 'verb :name name :tense tense
                       :subject subject :direct-object object
                       :indirect-object indirect-object
                       :obliques obliques))

(defmethod adv (name (v verb))
  (make-instance 'verb-phrase :components (list (make-instance 'adverb :name name)
                                                v)))

(defmethod adv (name (v verb-phrase))
  (push (make-instance 'adverb :name name) (components v)))

;;; Compound word — "blood-king", "sword-song"

(defclass compound-word (noun-like)
  ((compound-head     :accessor compound-head     :initarg :head)
   (compound-modifier :accessor compound-modifier :initarg :modifier)))

(defun compound (modifier head)
  (make-instance 'compound-word :head head :modifier modifier))

(defclass possessive (noun-like)
  ((possessor :accessor possessor :initarg :possessor)
   (possessed :accessor possessed :initarg :possessed)))

(defclass negation (message)
  ((target :accessor target :initarg :target)))

(defun poss (possessor possessed)
  (make-instance 'possessive :possessor possessor :possessed possessed))

(defun neg (target)
  (make-instance 'negation :target target))

;;; Bare adjective (for copula predicates)

(defun a (name)
  "Bare adjective, for use as copula predicate."
  (make-instance 'adjective :name name))

;;; Copula / Predication — "X is Y"

(defclass predication (verb-like)
  ((subject   :accessor subject   :initarg :subject)
   (predicate :accessor predicate :initarg :predicate)
   (tense     :accessor tense     :initarg :tense :initform nil)))

(defun copula (subject predicate &optional tense)
  (make-instance 'predication :subject subject :predicate predicate :tense tense))

;;; Oblique — "with X", "from X"

(defclass oblique (noun-like)
  ((role     :accessor role     :initarg :role)
   (argument :accessor argument :initarg :argument)))

(defun obl (role argument)
  (make-instance 'oblique :role role :argument argument))

;;; Conditional — "if X then Y"

(defclass conditional (verb-like)
  ((protasis  :accessor protasis  :initarg :protasis)
   (apodosis  :accessor apodosis  :initarg :apodosis)))

(defun cond-if (protasis apodosis)
  (make-instance 'conditional :protasis protasis :apodosis apodosis))

;;; Imperative — "Don't X!", "Fear the wolf!"

(defclass imperative (verb-like)
  ((command :accessor command :initarg :command)))

(defun imp (command)
  (make-instance 'imperative :command command))

;;; Comparative — "X-er than Y"

(defclass comparative (message)
  ((quality  :accessor quality  :initarg :quality)
   (comp-target :accessor comp-target :initarg :target :initform nil)
   (comp-standard :accessor comp-standard :initarg :standard :initform nil)))

(defun comp (quality &key target than)
  (make-instance 'comparative :quality quality :target target :standard than))

;;; Conjunction — "X and Y"

(defclass conj-np (noun-like)
  ((conjuncts :accessor conjuncts :initarg :conjuncts)))

(defclass conj-vp (verb-like)
  ((conjuncts :accessor conjuncts :initarg :conjuncts)))

(defun conj (&rest items)
  (if (every (lambda (x) (typep x 'noun-like)) items)
      (make-instance 'conj-np :conjuncts items)
      (make-instance 'conj-vp :conjuncts items)))

;;; Disjunction — "X or Y"

(defclass disj-np (noun-like)
  ((disjuncts :accessor disjuncts :initarg :disjuncts)))

(defclass disj-vp (verb-like)
  ((disjuncts :accessor disjuncts :initarg :disjuncts)))

(defun disj (&rest items)
  (if (every (lambda (x) (typep x 'noun-like)) items)
      (make-instance 'disj-np :disjuncts items)
      (make-instance 'disj-vp :disjuncts items)))

;;; Temporal subordination — "before X", "when X"

(defclass temporal-clause (verb-like)
  ((relation    :accessor relation    :initarg :relation)
   (subordinate :accessor subordinate :initarg :subordinate)
   (main        :accessor main        :initarg :main)))

(defun temporal (relation subordinate main)
  (make-instance 'temporal-clause :relation relation
                                  :subordinate subordinate :main main))

;;; Relative clause — "the man who fears death"

(defclass relative-clause (noun-like)
  ((head-noun :accessor head-noun :initarg :head)
   (clause    :accessor rel-clause :initarg :clause)
   (role      :accessor rel-role :initarg :role :initform :subject)))

(defun rel (head clause &key (role :subject))
  (make-instance 'relative-clause :head head :clause clause :role role))

;;; Purpose clause — "I write so that her children know"

(defclass purpose-clause (verb-like)
  ((main    :accessor main    :initarg :main)
   (purpose :accessor purpose :initarg :purpose)))

(defun in-order-to (main purpose)
  (make-instance 'purpose-clause :main main :purpose purpose))

;;; Causative — "the chains made us free"

(defclass causative (verb-like)
  ((causer :accessor causer :initarg :causer)
   (caused :accessor caused :initarg :caused)))

(defun cause (causer caused)
  (make-instance 'causative :causer causer :caused caused))

;;; Reported speech — "she said: write this"

(defclass quotation (verb-like)
  ((speaker :accessor speaker :initarg :speaker)
   (content :accessor content :initarg :content)
   (tense   :accessor tense   :initarg :tense :initform :past)))

(defun quote-speech (speaker content &key (tense :past))
  (make-instance 'quotation :speaker speaker :content content :tense tense))

;;; Adversative conjunction — "X but Y"

(defclass adversative (verb-like)
  ((conceded :accessor conceded :initarg :conceded)
   (asserted :accessor asserted :initarg :asserted)))

(defun but-conj (conceded asserted)
  (make-instance 'adversative :conceded conceded :asserted asserted))

;;; Exception — "no X except/but Y"

(defclass exception-clause (noun-like)
  ((excluded :accessor excluded :initarg :excluded)
   (excepted :accessor excepted :initarg :excepted)))

(defun except-for (excluded excepted)
  (make-instance 'exception-clause :excluded excluded :excepted excepted))

;;; Degree/result — "so X that Y"

(defclass degree-clause (verb-like)
  ((degree-quality :accessor degree-quality :initarg :quality)
   (degree-result  :accessor degree-result  :initarg :result)))

(defun so-that (quality result)
  (make-instance 'degree-clause :quality quality :result result))

;;; Question (yes/no interrogative)

(defclass question (verb-like)
  ((content :accessor content :initarg :content)))

(defun ask (content)
  (make-instance 'question :content content))

;;; Modal — ability / obligation

(defclass modal (verb-like)
  ((modality :accessor modality :initarg :modality)   ; :ability | :obligation
   (content  :accessor content  :initarg :content)))

(defun mdl (modality content)
  (make-instance 'modal :modality modality :content content))

;;; Reflexive — "myself", "themselves"

(defclass reflexive (noun-like)
  ((ref-person :accessor ref-person :initarg :person)
   (ref-number :accessor ref-number :initarg :number)))

(defun refl (person number)
  (make-instance 'reflexive :person person :number number))

;;; Causal clause — "because X, Y"

(defclass cause-clause (verb-like)
  ((reason       :accessor reason       :initarg :reason)
   (cause-result :accessor cause-result :initarg :result)))

(defun because-of (reason result)
  (make-instance 'cause-clause :reason reason :result result))

;;; Exclamation — "Aha! X"

(defclass exclamation (message)
  ((excl-content :accessor excl-content :initarg :content)))

(defun excl (content)
  (make-instance 'exclamation :content content))

;;; Simile — "X like Y"

(defclass simile (message)
  ((compared    :accessor compared    :initarg :compared)
   (sim-standard :accessor sim-standard :initarg :standard)))

(defun like-as (compared standard)
  (make-instance 'simile :compared compared :standard standard))

;;; Wh-question — "when/where/how/what/why X?"

(defclass wh-question (verb-like)
  ((wh-word :accessor wh-word :initarg :wh-word)
   (content :accessor content :initarg :content)))

(defun ask-wh (word content)
  (make-instance 'wh-question :wh-word word :content content))

;;; Complement clause — "I think that X"

(defclass complement-clause (verb-like)
  ((comp-subject :accessor comp-subject :initarg :subject)
   (comp-verb    :accessor comp-verb    :initarg :verb-name)
   (comp-tense   :accessor comp-tense   :initarg :tense :initform :present)
   (comp-content :accessor comp-content :initarg :content)))

(defun think (subject verb-name content &key (tense :present))
  (make-instance 'complement-clause
    :subject subject :verb-name verb-name
    :tense tense :content content))

;;; Optative — wish/blessing ("May the sun shine")

(defclass optative (verb-like)
  ((content :accessor content :initarg :content)))

(defun wish (content)
  (make-instance 'optative :content content))

;;; Concessive — "though X, Y"

(defclass concessive (verb-like)
  ((conceded :accessor conceded :initarg :conceded)
   (asserted :accessor asserted :initarg :asserted)))

(defun although (conceded asserted)
  (make-instance 'concessive :conceded conceded :asserted asserted))

;;; Reciprocal — "each other"

(defclass reciprocal (noun-like)
  ((recip-person :accessor recip-person :initarg :person)
   (recip-number :accessor recip-number :initarg :number)))

(defun recip (person number)
  (make-instance 'reciprocal :person person :number number))

;;; Nominalization — verb-to-noun lifting

(defclass nominalization (noun-like)
  ((nom-verb :accessor nom-verb :initarg :verb)))

(defun nmlz (verb)
  (make-instance 'nominalization :verb verb))

;;; Distributive — universal quantifier ("every X")

(defclass distributive (noun-like)
  ((dist-target :accessor dist-target :initarg :target)))

(defun every-np (target)
  (make-instance 'distributive :target target))

;;; Counterfactual — "had X, would Y"

(defclass counterfactual (verb-like)
  ((cf-protasis  :accessor cf-protasis  :initarg :protasis)
   (cf-apodosis  :accessor cf-apodosis  :initarg :apodosis)))

(defun counter-if (protasis apodosis)
  (make-instance 'counterfactual :protasis protasis :apodosis apodosis))

