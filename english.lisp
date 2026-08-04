(in-package :lang)

;;; English-target back-formation.
;;;
;;; The goal: make a language deep in a derivation chain (e.g. an imperial
;;; trade language descended from a halfling/orc creole) come out close
;;; enough to English that in-world text needs no translation.  Rather than
;;; inverting the sound changes analytically, we reuse the forward-chaining
;;; loanword machinery: BACK-FORM-WORD anneals a root-language form whose
;;; fitness is measured on the surface form after evolving through the whole
;;; chain.  RETROFIT-ENGLISH-LEXICON does that for a set of glosses and
;;; installs the winners in the chain root's lexicon; REFRESH-DERIVATION-CHAIN
;;; then re-derives every language between root and target so all siblings
;;; inherit consistent cognates.

;;; General American English targets, broad transcription, restricted to the
;;; phone inventory in consonants.csv/vowels.csv: diphthongs are vowel
;;; sequences, affricates are stop+fricative sequences, /ɚ/ is ("ə" "ɹ"),
;;; and length is ignored.  Keys match the gloss names in *vocabulary* /
;;; *demo-words*, plus the generated pronoun/modal/wh glosses.

(defparameter *english-ipa*
  '(;; content vocabulary
    ("alone"    "ə" "l" "o" "ʊ" "n")
    ("always"   "ɔ" "l" "w" "e" "ɪ" "z")
    ("ask"      "æ" "s" "k")
    ("bad"      "b" "æ" "d")
    ("believe"  "b" "ɪ" "l" "i" "v")
    ("bird"     "b" "ə" "ɹ" "d")
    ("bitter"   "b" "ɪ" "t" "ə" "ɹ")
    ("blade"    "b" "l" "e" "ɪ" "d")
    ("blind"    "b" "l" "a" "ɪ" "n" "d")
    ("blood"    "b" "l" "ʌ" "d")
    ("brave"    "b" "ɹ" "e" "ɪ" "v")
    ("break"    "b" "ɹ" "e" "ɪ" "k")
    ("bright"   "b" "ɹ" "a" "ɪ" "t")
    ("bring"    "b" "ɹ" "ɪ" "ŋ")
    ("brother"  "b" "ɹ" "ʌ" "ð" "ə" "ɹ")
    ("burn"     "b" "ə" "ɹ" "n")
    ("call"     "k" "ɔ" "l")
    ("chain"    "t" "ʃ" "e" "ɪ" "n")
    ("charge"   "t" "ʃ" "ɑ" "ɹ" "d" "ʒ")
    ("child"    "t" "ʃ" "a" "ɪ" "l" "d")
    ("city"     "s" "ɪ" "t" "i")
    ("clerk"    "k" "l" "ə" "ɹ" "k")
    ("cloth"    "k" "l" "ɔ" "θ")
    ("cold"     "k" "o" "ʊ" "l" "d")
    ("come"     "k" "ʌ" "m")
    ("cook"     "k" "ʊ" "k")
    ("council"  "k" "a" "ʊ" "n" "s" "ə" "l")
    ("dark"     "d" "ɑ" "ɹ" "k")
    ("daughter" "d" "ɔ" "t" "ə" "ɹ")
    ("dawn"     "d" "ɔ" "n")
    ("day"      "d" "e" "ɪ")
    ("dead"     "d" "ɛ" "d")
    ("death"    "d" "ɛ" "θ")
    ("deep"     "d" "i" "p")
    ("die"      "d" "a" "ɪ")
    ("dock"     "d" "ɑ" "k")
    ("dog"      "d" "ɔ" "ɡ")
    ("door"     "d" "ɔ" "ɹ")
    ("dress"    "d" "ɹ" "ɛ" "s")
    ("earth"    "ə" "ɹ" "θ")
    ("eat"      "i" "t")
    ("empire"   "ɛ" "m" "p" "a" "ɪ" "ə" "ɹ")
    ("empty"    "ɛ" "m" "p" "t" "i")
    ("end"      "ɛ" "n" "d")
    ("enemy"    "ɛ" "n" "ə" "m" "i")
    ("fall"     "f" "ɔ" "l")
    ("fast"     "f" "æ" "s" "t")
    ("father"   "f" "ɑ" "ð" "ə" "ɹ")
    ("fear"     "f" "ɪ" "ɹ")
    ("fight"    "f" "a" "ɪ" "t")
    ("find"     "f" "a" "ɪ" "n" "d")
    ("fire"     "f" "a" "ɪ" "ə" "ɹ")
    ("fleet"    "f" "l" "i" "t")
    ("flower"   "f" "l" "a" "ʊ" "ə" "ɹ")
    ("follow"   "f" "ɑ" "l" "o" "ʊ")
    ("food"     "f" "u" "d")
    ("forest"   "f" "ɔ" "ɹ" "ə" "s" "t")
    ("free"     "f" "ɹ" "i")
    ("friend"   "f" "ɹ" "ɛ" "n" "d")
    ("full"     "f" "ʊ" "l")
    ("give"     "ɡ" "ɪ" "v")
    ("go"       "ɡ" "o" "ʊ")
    ("god"      "ɡ" "ɑ" "d")
    ("gold"     "ɡ" "o" "ʊ" "l" "d")
    ("good"     "ɡ" "ʊ" "d")
    ("goods"    "ɡ" "ʊ" "d" "z")
    ("grow"     "ɡ" "ɹ" "o" "ʊ")
    ("guard"    "ɡ" "ɑ" "ɹ" "d")
    ("hand"     "h" "æ" "n" "d")
    ("happy"    "h" "æ" "p" "i")
    ("harbor"   "h" "ɑ" "ɹ" "b" "ə" "ɹ")
    ("hear"     "h" "ɪ" "ɹ")
    ("hide"     "h" "a" "ɪ" "d")
    ("hold"     "h" "o" "ʊ" "l" "d")
    ("home"     "h" "o" "ʊ" "m")
    ("hope"     "h" "o" "ʊ" "p")
    ("horse"    "h" "ɔ" "ɹ" "s")
    ("hot"      "h" "ɑ" "t")
    ("house"    "h" "a" "ʊ" "s")
    ("hunt"     "h" "ʌ" "n" "t")
    ("iron"     "a" "ɪ" "ə" "ɹ" "n")
    ("judge"    "d" "ʒ" "ʌ" "d" "ʒ")
    ("kill"     "k" "ɪ" "l")
    ("king"     "k" "ɪ" "ŋ")
    ("knife"    "n" "a" "ɪ" "f")
    ("know"     "n" "o" "ʊ")
    ("laugh"    "l" "æ" "f")
    ("law"      "l" "ɔ")
    ("lead"     "l" "i" "d")
    ("leaf"     "l" "i" "f")
    ("letter"   "l" "ɛ" "t" "ə" "ɹ")
    ("live"     "l" "ɪ" "v")
    ("long"     "l" "ɔ" "ŋ")
    ("lord"     "l" "ɔ" "ɹ" "d")
    ("lose"     "l" "u" "z")
    ("love"     "l" "ʌ" "v")
    ("make"     "m" "e" "ɪ" "k")
    ("man"      "m" "æ" "n")
    ("merchant" "m" "ə" "ɹ" "t" "ʃ" "ə" "n" "t")
    ("mercy"    "m" "ə" "ɹ" "s" "i")
    ("moon"     "m" "u" "n")
    ("mother"   "m" "ʌ" "ð" "ə" "ɹ")
    ("mountain" "m" "a" "ʊ" "n" "t" "ə" "n")
    ("name"     "n" "e" "ɪ" "m")
    ("never"    "n" "ɛ" "v" "ə" "ɹ")
    ("new"      "n" "u")
    ("not"      "n" "ɑ" "t")
    ("obey"     "o" "ʊ" "b" "e" "ɪ")
    ("old"      "o" "ʊ" "l" "d")
    ("open"     "o" "ʊ" "p" "ə" "n")
    ("own"      "o" "ʊ" "n")
    ("palace"   "p" "æ" "l" "ə" "s")
    ("path"     "p" "æ" "θ")
    ("people"   "p" "i" "p" "ə" "l")
    ("play"     "p" "l" "e" "ɪ")
    ("pride"    "p" "ɹ" "a" "ɪ" "d")
    ("promise"  "p" "ɹ" "ɑ" "m" "ə" "s")
    ("protect"  "p" "ɹ" "ə" "t" "ɛ" "k" "t")
    ("proud"    "p" "ɹ" "a" "ʊ" "d")
    ("rain"     "ɹ" "e" "ɪ" "n")
    ("refuse"   "ɹ" "ə" "f" "j" "u" "z")
    ("return"   "ɹ" "ɪ" "t" "ə" "ɹ" "n")
    ("rich"     "ɹ" "ɪ" "t" "ʃ")
    ("rise"     "ɹ" "a" "ɪ" "z")
    ("river"    "ɹ" "ɪ" "v" "ə" "ɹ")
    ("road"     "ɹ" "o" "ʊ" "d")
    ("root"     "ɹ" "u" "t")
    ("rule"     "ɹ" "u" "l")
    ("run"      "ɹ" "ʌ" "n")
    ("sail"     "s" "e" "ɪ" "l")
    ("see"      "s" "i")
    ("seed"     "s" "i" "d")
    ("sell"     "s" "ɛ" "l")
    ("send"     "s" "ɛ" "n" "d")
    ("serve"    "s" "ə" "ɹ" "v")
    ("sharp"    "ʃ" "ɑ" "ɹ" "p")
    ("shield"   "ʃ" "i" "l" "d")
    ("shine"    "ʃ" "a" "ɪ" "n")
    ("ship"     "ʃ" "ɪ" "p")
    ("short"    "ʃ" "ɔ" "ɹ" "t")
    ("shout"    "ʃ" "a" "ʊ" "t")
    ("silence"  "s" "a" "ɪ" "l" "ə" "n" "s")
    ("silent"   "s" "a" "ɪ" "l" "ə" "n" "t")
    ("sing"     "s" "ɪ" "ŋ")
    ("sit"      "s" "ɪ" "t")
    ("sky"      "s" "k" "a" "ɪ")
    ("slave"    "s" "l" "e" "ɪ" "v")
    ("sleep"    "s" "l" "i" "p")
    ("small"    "s" "m" "ɔ" "l")
    ("snow"     "s" "n" "o" "ʊ")
    ("song"     "s" "ɔ" "ŋ")
    ("speak"    "s" "p" "i" "k")
    ("spirit"   "s" "p" "ɪ" "ɹ" "ə" "t")
    ("square"   "s" "k" "w" "ɛ" "ɹ")
    ("stand"    "s" "t" "æ" "n" "d")
    ("star"     "s" "t" "ɑ" "ɹ")
    ("stone"    "s" "t" "o" "ʊ" "n")
    ("stop"     "s" "t" "ɑ" "p")
    ("story"    "s" "t" "ɔ" "ɹ" "i")
    ("strange"  "s" "t" "ɹ" "e" "ɪ" "n" "d" "ʒ")
    ("strong"   "s" "t" "ɹ" "ɔ" "ŋ")
    ("sun"      "s" "ʌ" "n")
    ("sweet"    "s" "w" "i" "t")
    ("sword"    "s" "ɔ" "ɹ" "d")
    ("take"     "t" "e" "ɪ" "k")
    ("think"    "θ" "ɪ" "ŋ" "k")
    ("tongue"   "t" "ʌ" "ŋ")
    ("trade"    "t" "ɹ" "e" "ɪ" "d")
    ("traitor"  "t" "ɹ" "e" "ɪ" "t" "ə" "ɹ")
    ("tree"     "t" "ɹ" "i")
    ("truth"    "t" "ɹ" "u" "θ")
    ("wait"     "w" "e" "ɪ" "t")
    ("walk"     "w" "ɔ" "k")
    ("wall"     "w" "ɔ" "l")
    ("want"     "w" "ɑ" "n" "t")
    ("war"      "w" "ɔ" "ɹ")
    ("warm"     "w" "ɔ" "ɹ" "m")
    ("water"    "w" "ɔ" "t" "ə" "ɹ")
    ("weep"     "w" "i" "p")
    ("wife"     "w" "a" "ɪ" "f")
    ("wind"     "w" "ɪ" "n" "d")
    ("wise"     "w" "a" "ɪ" "z")
    ("wolf"     "w" "ʊ" "l" "f")
    ("woman"    "w" "ʊ" "m" "ə" "n")
    ("word"     "w" "ə" "ɹ" "d")
    ("write"    "ɹ" "a" "ɪ" "t")
    ("young"    "j" "ʌ" "ŋ")
    ;; pronouns (glosses from generate-pronoun-lexicon)
    ("1stsingular" "a" "ɪ")
    ("2ndsingular" "j" "u")
    ("3rdsingular" "h" "i")
    ("1stplural"   "w" "i")
    ("2ndplural"   "j" "u")
    ("3rdplural"   "ð" "e" "ɪ")
    ;; modals (glosses from generate-modal-lexicon)
    ("can"  "k" "æ" "n")
    ("must" "m" "ʌ" "s" "t")
    ;; wh-words (glosses from generate-wh-lexicon)
    ("wh-what"  "w" "ʌ" "t")
    ("wh-where" "w" "ɛ" "ɹ")
    ("wh-when"  "w" "ɛ" "n")
    ("wh-why"   "w" "a" "ɪ")
    ("wh-how"   "h" "a" "ʊ")
    ;; plural disambiguator ("you all") when pronoun-collapse merges number
    ("plural-quantifier" "ɔ" "l"))
  "English (General American) IPA targets keyed by gloss.")

;;; Hand-built English-like grammar
;;;
;;; A complete :features plist for GENERATE-GRAMMAR pinning every grammar
;;; decision to its English value, with English function words (in IPA) as
;;; the markers.  Apply to a proto- or contact language with
;;; APPLY-ENGLISH-GRAMMAR; derive daughters with :cliticization-rate 0 to
;;; keep the strategies from drifting.  Edit freely — this is a starting
;;; point, not canon.

(defparameter *english-grammar-features*
  (list
   ;; word order: SVO, adjective-noun, adverb-verb, possessor-first
   :clause-order '(:subject :verb :object)
   :np-order '(:adjective :noun)
   :vp-order '(:adverb :verb)
   :gen-order :possessor-first
   :head-final-p nil
   ;; copula: "is"
   :copula-strategy :particle
   :copula-marker '("ɪ" "z")
   :copula-order '(:subject :copula :predicate)
   ;; conjunction "and", disjunction "or"
   :conjunction-strategy :medial
   :conjunction-marker '("æ" "n" "d")
   :disjunction-strategy :medial
   :disjunction-marker '("ɔ" "ɹ")
   :adposition-strategy :preposition
   ;; clause combining
   :conditional-strategy :particle
   :conditional-order '(:protasis :apodosis)
   :comp-order '(:target :quality :standard)
   :relative-strategy :particle
   :relative-order '(:head :clause)
   :purpose-order '(:main :purpose)
   :causative-strategy :analytic
   :causative-order '(:causer :caused)
   :quotation-order '(:speaker :verb :content)
   :adversative-strategy :particle
   :adversative-order '(:conceded :asserted)
   :degree-order '(:quality :result)
   :causal-strategy :particle
   :causal-order '(:result :reason)
   :concessive-strategy :particle
   :concessive-order '(:conceded :asserted)
   :complement-order '(:subject :verb :content)
   ;; reflexive "self", reciprocal "each other", "each", "-ing"
   :reflexive-strategy :separate-word
   :reflexive-marker '("s" "ɛ" "l" "f")
   :reciprocal-strategy :separate-word
   :reciprocal-marker '("i" "t" "ʃ" "ʌ" "ð" "ə" "ɹ")
   :nominalization-strategy :suffix
   :nominalization-marker '("ɪ" "ŋ")
   :distributive-strategy :particle-before
   :distributive-marker '("i" "t" "ʃ")
   ;; exclamation "oh", simile "like"
   :exclamation-strategy :particle
   :exclamation-particle '("o" "ʊ")
   :exclamation-position :initial
   :simile-strategy :particle
   :simile-marker '("l" "a" "ɪ" "k")
   :simile-order '(:compared :standard)
   ;; questions: fronted wh-words; yes/no with initial "do"
   :wh-strategy :dedicated
   :wh-position :initial
   :question-strategy :particle
   :question-particle '("d" "u")
   :question-particle-position :initial
   ;; modals as auxiliaries ("can", "must" from the lexicon), modal first
   :modal-strategy :auxiliary
   :modal-order :modal-first
   ;; compounds: bare juxtaposition, modifier before head
   :compound-strategy :juxtapose
   :compound-order :head-final
   :compound-linker nil
   ;; "the king's sword", not "the king's the sword"
   :possessive-determiner :complementary
   ;; pronouns: "you" for singular and plural, "you all" to disambiguate
   :pronoun-collapse :collapse-2
   :plural-pronoun-strategy :quantifier-after
   ;; no noun classes, no agreement, no pro-drop, no topic-drop
   :noun-class-count 0
   :noun-classes nil
   :noun-class-scheme nil
   :agreement-type :none
   :articles :both
   :pro-drop nil
   :topic-drop nil
   ;; info structure: topic fronting, impersonal demotion
   :promote-strategy :fronting
   :topic-particle nil
   :demote-strategy :impersonal
   ;; per-feature morphology: strategies and English markers
   :morphology
   (list
    :accusative '(:strategy :none)
    :genitive '(:strategy :suffix :marker ("z"))            ; 's
    :plural '(:strategy :suffix :marker ("s"))              ; -s
    :past '(:strategy :suffix :marker ("d"))                ; -ed
    :present '(:strategy :none)
    :future '(:strategy :particle-before :marker ("w" "ɪ" "l"))
    :negation '(:strategy :particle-before :marker ("n" "ɑ" "t"))
    :imperative '(:strategy :none)
    :comparative '(:strategy :particle-before :marker ("m" "ɔ" "ɹ"))
    :comp-standard '(:strategy :particle-before :marker ("ð" "æ" "n"))
    :conditional '(:strategy :particle-before :marker ("ɪ" "f"))
    :oblique-with '(:strategy :particle-before :marker ("w" "ɪ" "θ"))
    :oblique-from '(:strategy :particle-before :marker ("f" "ɹ" "ʌ" "m"))
    :oblique-to '(:strategy :particle-before :marker ("t" "u"))
    :oblique-on '(:strategy :particle-before :marker ("ɑ" "n"))
    :oblique-at '(:strategy :particle-before :marker ("æ" "t"))
    :oblique-over '(:strategy :particle-before :marker ("o" "ʊ" "v" "ə" "ɹ"))
    :temporal-before '(:strategy :particle-before :marker ("b" "ɪ" "f" "ɔ" "ɹ"))
    :temporal-after '(:strategy :particle-before :marker ("æ" "f" "t" "ə" "ɹ"))
    :temporal-when '(:strategy :particle-before :marker ("w" "ɛ" "n"))
    :relative '(:strategy :particle-before :marker ("h" "u"))
    :purpose '(:strategy :particle-before :marker ("t" "u"))
    :causative '(:strategy :particle-before :marker ("m" "e" "ɪ" "k"))
    :quotative '(:strategy :particle-before :marker ("ð" "æ" "t"))
    :adversative '(:strategy :particle-before :marker ("b" "ʌ" "t"))
    :exception '(:strategy :particle-before :marker ("ɪ" "k" "s" "ɛ" "p" "t"))
    :degree '(:strategy :particle-before :marker ("s" "o" "ʊ"))
    :interrogative '(:strategy :none)
    :modal-ability '(:strategy :none)
    :modal-obligation '(:strategy :none)
    :causal '(:strategy :particle-before :marker ("b" "ɪ" "k" "ɔ" "z"))
    :exclamation '(:strategy :none)
    :simile '(:strategy :none)
    :complementizer '(:strategy :particle-before :marker ("ð" "æ" "t"))
    :passive '(:strategy :none)
    :optative '(:strategy :particle-before :marker ("m" "e" "ɪ"))
    :concessive '(:strategy :particle-before :marker ("ð" "o" "ʊ"))
    :counterfactual '(:strategy :particle-before :marker ("w" "ʊ" "d"))
    :definite '(:strategy :particle-before :marker ("ð" "ə"))
    :indefinite '(:strategy :particle-before :marker ("ə"))
    :adverbialize '(:strategy :suffix :marker ("l" "i"))     ; -ly
    :agentive '(:strategy :suffix :marker ("ə" "ɹ"))         ; -er
    :adj-from-noun '(:strategy :suffix :marker ("i"))        ; -y
    :noun-from-adj '(:strategy :suffix :marker ("n" "ə" "s")))) ; -ness
  "Hand-built English-like grammar :features plist for GENERATE-GRAMMAR.")

(defun apply-english-grammar (language)
  "Replace LANGUAGE's grammar with the hand-built English-like grammar and
   regenerate everything that depends on it (function-word lexicon entries
   and inflectional paradigms).  Typically applied to the chain root (e.g. a
   creole) before retrofitting; derive daughters with :cliticization-rate 0
   so the hand-picked strategies survive."
  (generate-grammar language :typology :analytic
                             :features *english-grammar-features*)
  ;; The grammar switch may activate function words the old grammar lacked
  ;; (auxiliary modals, the plural quantifier).  Only add what's missing.
  (unless (lookup-word language "can")
    (generate-modal-lexicon language))
  (unless (lookup-word language "plural-quantifier")
    (define-word language "plural-quantifier" 'particle :syllables 1))
  (generate-paradigms language)
  language)

;;; Phonology coverage — which English phones can this language reach?

(defun evolved-inventory (lang)
  "The phone inventory of LANG: its root's inventory pushed through the
   derivation chain (a phone's image under each sound change in order)."
  (multiple-value-bind (root chain) (collect-derivation-chain lang)
    (let ((phones (append (mapcar #'car (consonant-frequencies root))
                          (mapcar #'car (vowel-frequencies root)))))
      (remove-duplicates
       (iter (for p in phones)
         (let* ((evolved (if chain
                             (evolve chain (list (ensure-phone-point p)))
                             (list p)))
                (ph (find-if #'phone-p (flatten evolved))))
           (when ph (collect (ensure-raw-phone ph)))))
       :key #'ipa :test #'equal))))

(defun english-phones ()
  "All distinct IPA symbols used by the English targets."
  (remove-duplicates (apply #'append (mapcar #'rest *english-ipa*))
                     :test #'equal))

(defun english-coverage-report (lang &key (near-threshold 4.0))
  "Report how well LANG's evolved inventory covers the phones the English
   targets need.  For each needed phone: EXACT if present, NEAR (with the
   substitute) if something within NEAR-THRESHOLD exists, MISSING otherwise.
   Use this to choose parent inventories and sound changes: back-formation
   can only reach English words built from EXACT/NEAR phones."
  (let ((inventory (evolved-inventory lang))
        (exact 0) (near 0) (missing 0))
    (format t "~&English phone coverage for ~a (~a phones in inventory):~%"
            (lang-name lang) (length inventory))
    (dolist (sym (sort (english-phones) #'string<))
      (let ((target (ensure-phone sym)))
        (if (member sym inventory :key #'ipa :test #'equal)
            (incf exact)
            (let ((best nil) (best-dist most-positive-fixnum))
              (dolist (candidate inventory)
                (let ((d (cond ((and (consonant-p target) (consonant-p candidate))
                                (consonant-distance target candidate))
                               ((and (vowel-p target) (vowel-p candidate))
                                (vowel-distance target candidate))
                               (t most-positive-fixnum))))
                  (when (< d best-dist)
                    (setf best candidate best-dist d))))
              (cond
                ((and best (<= best-dist near-threshold))
                 (incf near)
                 (format t "  NEAR    ~a -> ~a (~,2f)~%" sym (ipa best) best-dist))
                (t
                 (incf missing)
                 (format t "  MISSING ~a~@[ (closest ~a at ~,2f)~]~%"
                         sym (when best (ipa best)) (when best best-dist))))))))
    (format t "  ~a exact, ~a near, ~a missing~%" exact near missing)
    (values exact near missing)))

(defun english-form (gloss)
  "Return the English target for GLOSS as a reanalyzed word, or NIL."
  (let ((entry (assoc gloss *english-ipa* :test #'string=)))
    (when entry
      (deserialize-form (rest entry)))))

(defun english-glosses ()
  (mapcar #'first *english-ipa*))

;;; Installing back-formed words in the chain root

(defun install-root-word (root gloss form &key (category 'noun) origin)
  "Insert FORM into ROOT's lexicon under GLOSS, replacing the form of an
   existing entry with that gloss if there is one (its category, domain, and
   noun class are kept; stale inflected forms are cleared — re-run
   GENERATE-PARADIGMS afterwards)."
  (let ((existing (find gloss (lexicon root) :key #'gloss :test #'equal)))
    (if existing
        (progn
          (setf (form existing) form
                (origin existing) (or origin (origin existing))
                (inflected-forms existing) nil)
          existing)
        (let ((entry (make-instance 'lexical-entry
                                    :gloss gloss
                                    :form form
                                    :category category
                                    :origin (or origin :retrofit))))
          (push entry (lexicon root))
          entry))))

(defun retrofit-english-lexicon (lang glosses &key (schedule (default-anneal-schedule))
                                                debug (verbose t))
  "Back-form an English-like word for each of GLOSSES through LANG's
   derivation chain and install the results in the chain root's lexicon,
   so that the root form legitimately evolves into an English-like surface
   form in LANG.  Glosses without an entry in *english-ipa* are skipped with
   a warning.  Rebuilds the root's paradigms afterwards.  Returns the root.
   NOTE: derived languages between root and LANG hold snapshots — call
   REFRESH-DERIVATION-CHAIN on each descendant afterwards."
  (multiple-value-bind (root chain) (collect-derivation-chain lang)
    (declare (ignore chain))
    (dolist (gloss glosses)
      (let ((target (english-form gloss)))
        (if (null target)
            (warn "No English IPA target for ~s; skipping." gloss)
            (multiple-value-bind (root-word evolved distance)
                (back-form-word target lang :schedule schedule :debug debug)
              (install-root-word root gloss root-word
                                 :category (let ((old (lookup-word root gloss)))
                                             (if old (category old) 'noun))
                                 :origin '(:retrofit . :english))
              (when verbose
                (format t "~12a  en /~a/  root ~a  surface ~a (~,2f) ~s~%"
                        gloss
                        (alt-print-word target)
                        (alt-print-word root-word)
                        (alt-print-word evolved)
                        distance
                        (alt-print-word evolved nil #'anglicize)))))))
    (generate-paradigms root)
    root))

;;; Re-deriving after the root lexicon changes

(defun refresh-derived-lexicon (derived &key (schedule (default-anneal-schedule)))
  "Recompute DERIVED's lexicon from its source using its stored transformers,
   leaving its grammar untouched, then replay DERIVED's recorded borrow
   events (re-adapting each loan from its donor's current lexicon, using
   SCHEDULE for the adaptation annealing)."
  (setf (lexicon derived)
        (iter (for entry in (lexicon (source derived)))
          (collect (make-instance 'lexical-entry
                                  :gloss (gloss entry)
                                  :form (evolve (transformers derived) (form entry))
                                  :category (category entry)
                                  :origin (cons :inherited (lang-name (source derived)))
                                  :domain (domain entry)
                                  :noun-class (noun-class entry)
                                  :inflected-forms
                                  (mapcar (lambda (pair)
                                            (cons (car pair)
                                                  (evolve (transformers derived)
                                                          (cdr pair))))
                                          (inflected-forms entry))))))
  (replay-borrowings derived :schedule schedule)
  derived)

(defun refresh-derivation-chain (lang &key (schedule (default-anneal-schedule)))
  "Refresh the lexicon of every derived language from LANG's chain root down
   to LANG itself, in root-to-leaf order, replaying each stage's recorded
   loans.  Call after editing the root lexicon (e.g. via
   RETROFIT-ENGLISH-LEXICON).  Loans whose donors sit on other branches are
   re-adapted from the donors' current lexicons — refresh those branches
   first if they changed too.  Returns LANG."
  (labels ((path (l)
             (if (typep l 'derived-language)
                 (append (path (source l)) (list l))
                 nil)))
    (dolist (l (path lang))
      (refresh-derived-lexicon l :schedule schedule)))
  lang)
