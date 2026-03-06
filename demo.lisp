(in-package #:lang)

;;; Sound change specs for deriving variant languages
;;; Each pair is (name . transformer-specs)

(defparameter *variant-changes*
  '(;; Elvish variants
    (:high-elvish . ((C :voicing voiced :manner plosive -> :manner fricative)
                     (V :height open -> :height mid)))
    (:wood-elvish . ((C :manner fricative :place alveolar -> :place palatal)
                     (V :backness back -> :backness central)
                     (C :stress unstressed :manner plosive -> :voicing voiceless)
                     ))
    ;; Goblin variants
    (:marsh-goblin . ((C :voicing voiced :manner plosive -> :manner nasal)
                      (V :height close -> :height close-mid)))
    (:cave-goblin . ((C :manner nasal -> :manner plosive)
                     (V :rounding unrounded :height open -> :rounding rounded)))
    ;; Orcish variants
    (:black-orc . ((C :voicing voiceless :manner plosive -> :manner fricative)
                   (V :height mid -> :height open)))
    (:mountain-orc . ((C :manner fricative :voicing voiced -> :voicing voiceless)
                      (V :backness front -> :backness central)))
    ;; Dwarvish variants
    (:deep-dwarvish . ((C :place velar -> :place uvular)
                       (V :height close -> :height close-mid)))
    (:iron-dwarvish . ((C :manner fricative -> :manner plosive)
                       (V :rounding rounded -> :rounding unrounded)))
    ;; Gnomish variants
    (:tinker-gnomish . ((C :voicing voiceless :manner plosive -> :manner fricative)
                        (V :backness back -> :backness front)))
    (:garden-gnomish . ((C :manner nasal :place alveolar -> :place bilabial)
                        (V :height open -> :height open-mid)))
    ;; Halfling variants
    (:shire-halfling . ((C :manner plosive :voicing voiced -> :manner approximant)
                        (V :height close-mid -> :height open-mid)))
    (:river-halfling . ((C :manner fricative -> :manner plosive)
                        (V :backness central -> :backness back)))))

(defparameter *demo-words*
  '("alone" "always" "ask" "bad" "believe" "bird" "bitter" "blind" "blood"
    "brave" "break" "bright" "bring" "brother" "burn" "call" "chain" "charge"
    "child" "city" "clerk" "cloth" "cold" "come" "cook" "council" "dark"
    "daughter" "dawn" "day" "dead" "death" "deep" "die" "dock" "dog" "door"
    "dress" "earth" "eat" "empire" "empty" "end" "enemy" "fall" "fast" "father"
    "fear" "fight" "find" "fire" "fleet" "flower" "follow" "food" "forest"
    "free" "friend" "full" "give" "go" "gold" "good" "goods" "grow" "guard"
    "hand" "happy" "harbor" "hear" "hide" "hold" "home" "hope" "horse" "hot"
    "house" "hunt" "iron" "judge" "kill" "king" "know" "laugh" "law" "lead"
    "leaf" "letter" "live" "long" "lord" "lose" "love" "make" "man" "merchant"
    "mercy" "moon" "mother" "mountain" "name" "never" "new" "not" "obey" "old"
    "open" "own" "palace" "path" "people" "play" "pride" "promise" "protect"
    "proud" "rain" "refuse" "return" "rich" "rise" "river" "road" "root" "rule"
    "run" "sail" "see" "seed" "sell" "send" "serve" "sharp" "shield" "shine"
    "ship" "short" "shout" "silence" "silent" "sing" "sit" "sky" "slave"
    "sleep" "small" "snow" "song" "speak" "square" "stand" "star" "stone"
    "stop" "story" "strange" "strong" "sun" "sweet" "sword" "take" "think"
    "tongue" "trade" "traitor" "tree" "truth" "wait" "walk" "wall" "want"
    "war" "warm" "water" "weep" "wife" "wind" "wise" "wolf" "woman" "word"
    "write" "young")
  "Subset of word glosses actually referenced in demo/sample/stress phrases.")

(defun demo-vocabulary ()
  "Filter *vocabulary* to only the words used by the demo phrases."
  (remove-if-not (lambda (spec) (member (first spec) *demo-words* :test #'string=))
                 *vocabulary*))

(defun make-world (&key (vocabulary (demo-vocabulary)))
  "Generate all proto-languages, their grammars, lexicons, and derived variants."
  (let ((protos nil)
        (all-langs nil))
    ;; Generate proto-languages
    (dolist (spec '((elvish 14 :analytic)
                    (goblin 12 :synthetic)
                    (orcish 10 :synthetic)
                    (dwarvish 12 :mixed)
                    (gnomish 14 :analytic)
                    (halfling 12 :mixed)))
      (let* ((name-sym (first spec))
             (n (second spec))
             (typology (third spec))
             (proto-name (string-downcase (format nil "proto-~a" name-sym)))
             (lang (loop for attempt below 50
                         for candidate = (handler-case
                                             (let ((l (funcall name-sym n :name proto-name)))
                                               (generate-grammar l :typology typology)
                                               (generate-lexicon l vocabulary)
                                               (generate-pronoun-lexicon l)
                                               (generate-modal-lexicon l)
                                               (generate-wh-lexicon l)
                                               (generate-derived-lexicon l)
                                               (generate-compound-lexicon l *compound-specs*)
                                               (generate-paradigms l)
                                               l)
                                           (error (e)
                                             (when (= attempt 49)
                                               (format t "FAILED ~a after 50 attempts: ~a~%" proto-name e))
                                             nil))
                         when candidate return candidate)))
        (when lang
          (push (cons name-sym lang) protos)
          (push lang all-langs))))
    ;; Generate derived languages
    (dolist (variant-spec *variant-changes*)
      (let* ((variant-name (car variant-spec))
             (changes (cdr variant-spec))
             ;; figure out which proto this belongs to
             (proto-key (cond
                          ((member variant-name '(:high-elvish :wood-elvish)) 'elvish)
                          ((member variant-name '(:marsh-goblin :cave-goblin)) 'goblin)
                          ((member variant-name '(:black-orc :mountain-orc)) 'orcish)
                          ((member variant-name '(:deep-dwarvish :iron-dwarvish)) 'dwarvish)
                          ((member variant-name '(:tinker-gnomish :garden-gnomish)) 'gnomish)
                          ((member variant-name '(:shire-halfling :river-halfling)) 'halfling)))
             (proto (cdr (assoc proto-key protos))))
        (when proto
          (let ((derived (derive-language proto changes
                                          :name (string-downcase (symbol-name variant-name)))))
            (push derived all-langs)))))
    ;; Generate contact languages (pidgins)
    (let ((corpus (append (demo-phrases) (sample-phrases) (sample-text) (stress-sentences))))
      (let ((elvish-proto (cdr (assoc 'elvish protos)))
            (goblin-proto (cdr (assoc 'goblin protos))))
        (when (and elvish-proto goblin-proto)
          (handler-case
              (let ((pidgin (pidginize elvish-proto goblin-proto corpus
                                       :name "elvish-goblin-trade")))
                (generate-derived-lexicon pidgin)
                (generate-compound-lexicon pidgin *compound-specs*)
                (generate-paradigms pidgin)
                (push pidgin all-langs))
            (error (e)
              (format t "FAILED elvish-goblin pidgin: ~a~%" e)))))
      (let ((orcish-proto (cdr (assoc 'orcish protos)))
            (dwarvish-proto (cdr (assoc 'dwarvish protos))))
        (when (and orcish-proto dwarvish-proto)
          (handler-case
              (let ((pidgin (pidginize orcish-proto dwarvish-proto corpus
                                       :name "orc-dwarf-border")))
                (generate-derived-lexicon pidgin)
                (generate-compound-lexicon pidgin *compound-specs*)
                (generate-paradigms pidgin)
                (push pidgin all-langs))
            (error (e)
              (format t "FAILED orc-dwarf pidgin: ~a~%" e))))))
    (remove nil (nreverse all-langs))))

;;; Phrases to translate

(defun demo-phrases ()
  "Return a list of (description . semantic-form) pairs."
  (list
   (cons "The wise wolf hunts the king"
         (vb "hunt" :present :subject (adj "wise" (n "wolf")) :object (n "king")))
   (cons "The old sword breaks"
         (vb "break" :present :subject (adj "old" (n "sword"))))
   (cons "No man's death is silent"
         (vb "live" :present :subject (neg (poss (n "man") (n "death")))))
   (cons "The strong fear the wise"
         (vb "fear" :present :subject (adj "strong" (n "man")) :object (adj "wise" (n "enemy"))))
   (cons "Fire burns the dark forest"
         (vb "burn" :present :subject (n "fire") :object (adj "dark" (n "forest"))))
   (cons "The child follows the bright star"
         (vb "follow" :present :subject (n "child") :object (adj "bright" (n "star"))))
   (cons "The dead king's sword falls"
         (vb "fall" :present :subject (poss (adj "dead" (n "king")) (n "sword"))))
   (cons "A blind wolf finds the path"
         (vb "find" :present :subject (adj "blind" (n "wolf")) :object (n "path")))
   (cons "Blood and gold rule the people"
         (vb "rule" :present :subject (conj (n "blood") (n "gold")) :object (n "people")))
   (cons "The bitter wind comes from the mountain"
         (vb "come" :present :subject (adj "bitter" (n "wind"))))
   ;; New proverb phrases using expanded semantic types
   (cons "He who fears death never lives"
         (adv "never" (vb "live" :present
           :subject (rel (n "man") (vb "fear" :present :object (n "death"))))))
   (cons "Better a sharp sword than a wise tongue"
         (comp "good" :target (adj "sharp" (n "sword")) :than (adj "wise" (n "tongue"))))
   (cons "Death is silent"
         (copula (n "death") (a "silent")))
   (cons "Fear not the dark"
         (imp (neg (vb "fear" nil :object (n "dark")))))
   (cons "Before the wolf comes, the dog sleeps"
         (temporal :before
                   (vb "come" :present :subject (n "wolf"))
                   (vb "sleep" :present :subject (n "dog"))))
   (cons "The man who sleeps with wolves rises with blood"
         (vb "rise" :present
           :subject (rel (n "man")
                         (vb "sleep" :present :obliques (list (obl :with (n "wolf")))))
           :obliques (list (obl :with (n "blood")))))
   ;; Derived word phrases
   (cons "The hunter fears the darkness"
         (vb "fear" :present :subject (n "hunt-er") :object (n "dark-ness")))
   (cons "The blood-king rules wisely"
         (adv "wise-ly" (vb "rule" :present :subject (n "blood-king"))))
   (cons "Strength is the wisdom of the brave"
         (copula (n "strong-ness") (poss (adj "brave" (n "man")) (n "wise-ness"))))
   (cons "The sword-song ends the silence"
         (vb "end" :present :subject (n "sword-song") :object (n "silence")))
   ;; Pronoun phrases
   (cons "I fear no man"
         (vb "fear" :present :subject (pro :1st :singular) :object (neg (n "man"))))
   (cons "You speak the truth"
         (vb "speak" :present :subject (pro :2nd :singular) :object (n "truth")))
   (cons "We hunt the wolf"
         (vb "hunt" :present :subject (pro :1st :plural) :object (n "wolf")))
   (cons "They rule with iron"
         (vb "rule" :present :subject (pro :3rd :plural)
              :obliques (list (obl :with (n "iron")))))
   (cons "Your mother is a dog"
         (copula (poss (pro :2nd :singular) (n "mother")) (n "dog" :indefinite)))
   (cons "Speak, friend!"
         (imp (vb "speak" nil :object (n "friend"))))
   (cons "I'll fight you(sg)"
         (vb "fight" :present :subject (pro :1st :singular) :object (pro :2nd :singular)))
   (cons "I'll fight you(pl)"
         (vb "fight" :present :subject (pro :1st :singular) :object (pro :2nd :plural)))))

;;; Sample text phrases — distilled from cloth-merchant narrative

(defun sample-phrases ()
  "Return a list of (description . semantic-form) pairs for the sample text."
  (list
   ;; 1. "I was a cloth-merchant"
   (cons "I was a cloth-merchant"
         (copula (pro :1st :singular) (n "cloth-merchant" :indefinite) :past))
   ;; 2. "My father sold cloth before me"
   (cons "My father sold cloth before me"
         (temporal :before
                   (pro :1st :singular)
                   (vb "sell" :past :subject (poss (pro :1st :singular) (n "father"))
                        :object (n "cloth" :indefinite))))
   ;; 3. "We were free people"
   (cons "We were free people"
         (copula (pro :1st :plural) (adj "free" (n "people" :indefinite)) :past))
   ;; 4. "The lord broke the law"
   (cons "The lord broke the law"
         (vb "break" :past :subject (n "lord") :object (n "law")))
   ;; 5. "The fleet came at dawn"
   (cons "The fleet came at dawn"
         (vb "come" :past :subject (n "fleet")
              :obliques (list (obl :at (n "dawn" :indefinite)))))
   ;; 6. "My wife wept"
   (cons "My wife wept"
         (vb "weep" :past :subject (poss (pro :1st :singular) (n "wife"))))
   ;; 7. "They chained the people"
   (cons "They chained the people"
         (vb "chain" :past :subject (pro :3rd :plural) :object (n "people")))
   ;; 8. "We stood in chains"
   (cons "We stood in chains"
         (vb "stand" :past :subject (pro :1st :plural)
              :obliques (list (obl :with (n "chain" :indefinite :plural)))))
   ;; 9. "The ship sailed to the city"
   (cons "The ship sailed to the city"
         (vb "sail" :past :subject (n "ship")
              :obliques (list (obl :to (n "city")))))
   ;; 10. "I write this for my daughter"
   (cons "I write this for my daughter"
         (vb "write" :present :subject (pro :1st :singular)
              :obliques (list (obl :to (poss (pro :1st :singular) (n "daughter"))))))
   ;; 11. "No lord holds us"
   (cons "No lord holds us"
         (vb "hold" :present :subject (neg (n "lord" :indefinite))
              :object (pro :1st :plural)))
   ;; 12. "We serve the king alone"
   (cons "We serve the king alone"
         (adv "alone" (vb "serve" :present :subject (pro :1st :plural) :object (n "king"))))
   ;; 13. "She speaks with pride"
   (cons "She speaks with pride"
         (vb "speak" :present :subject (pro :3rd :singular)
              :obliques (list (obl :with (n "pride" :indefinite)))))
   ;; 14. "If freedom is dead, freedom is new"
   (cons "If freedom is dead, freedom is new"
         (cond-if (copula (n "free-ness") (a "dead"))
                  (copula (n "free-ness") (a "new"))))
   ;; 15. "My daughter came to the city"
   (cons "My daughter came to the city"
         (vb "come" :past :subject (poss (pro :1st :singular) (n "daughter"))
              :obliques (list (obl :to (n "city")))))
   ;; 16. "The chain-man's daughter knows no fear"
   (cons "The chain-man's daughter knows no fear"
         (vb "know" :present :subject (poss (n "chain-man") (n "daughter"))
              :object (neg (n "fear" :indefinite))))
   ;; 17. "Strength is the wisdom of the free"
   (cons "Strength is the wisdom of the free"
         (copula (n "strong-ness")
                 (poss (adj "free" (n "people")) (n "wise-ness"))))))

;;; Full narrative translation — all paragraphs of sample.txt

(defun sample-text ()
  "Return (description . semantic-form) pairs for the full sample text narrative.
   Follows sample.txt paragraph by paragraph."
  (list
   ;; ═══════════════════════════════════════════
   ;; I. Prologue (para 1)
   ;; ═══════════════════════════════════════════

   ;; "I am setting down this account at the request of my daughter"
   (cons "My daughter asked me to write this"
         (vb "ask" :past :subject (poss (pro :1st :singular) (n "daughter"))
              :object (pro :1st :singular)
              :obliques (list (obl :to (vb "write" nil)))))
   ;; "who says that if I do not write it, her children will have only
   ;;  rumors and tavern stories to explain how their grandfather came
   ;;  to the imperial city in chains"
   (cons "She said: if you do not write, your children will hear stories, not truth"
         (quote-speech (pro :3rd :singular)
                       (cond-if (neg (vb "write" :present :subject (pro :2nd :singular)))
                                (vb "hear" :future
                                    :subject (poss (pro :2nd :singular) (n "child" :plural))
                                    :object (conj (n "story" :plural) (neg (n "truth")))))))
   ;; "how their grandfather came to the imperial city in chains"
   (cons "They will not know how I came to the city in chains"
         (adv "not" (vb "know" :future
                        :subject (pro :3rd :plural)
                        :object (rel (pro :1st :singular)
                                     (vb "come" :past
                                          :obliques (list (obl :to (n "city"))
                                                          (obl :with (n "chain" :indefinite :plural))))))))
   ;; "She is right, though I wish she were not."
   (cons "She speaks the truth"
         (vb "speak" :present :subject (pro :3rd :singular) :object (n "truth")))

   ;; ═══════════════════════════════════════════
   ;; II. The Merchant Days (para 2)
   ;; ═══════════════════════════════════════════

   ;; "I was a cloth merchant... comfortable enough to own my own warehouse"
   (cons "I was a cloth-merchant, rich enough that I owned a house and goods"
         (but-conj (copula (pro :1st :singular) (n "cloth-merchant" :indefinite) :past)
                   (so-that (copula (pro :1st :singular) (a "rich"))
                            (vb "own" :past :subject (pro :1st :singular)
                                 :object (conj (n "house" :indefinite)
                                               (n "goods" :indefinite))))))
   ;; "My father had traded under it before me."
   (cons "My father sold cloth before me"
         (temporal :before
                   (pro :1st :singular)
                   (vb "sell" :past :subject (poss (pro :1st :singular) (n "father"))
                        :object (n "cloth" :indefinite))))
   ;; "We understood ourselves to be free citizens of the empire,
   ;;  protected by our charter and answerable to no lord but the emperor"
   ;; one thought: free citizens, protected by law, answerable to king alone
   (cons "We believed we were free people who the law protected, and that we obeyed the king alone"
         (vb "believe" :past :subject (pro :1st :plural)
              :object (conj (copula (pro :1st :plural)
                                    (rel (adj "free" (n "people" :indefinite))
                                         (vb "protect" :past :subject (n "law"))
                                         :role :object))
                            (adv "alone" (vb "obey" :past :subject (pro :1st :plural)
                                              :object (n "king"))))))
   ;; "This understanding, as it turned out, was worth precisely
   ;;  as much as the conclave chose to honor it, which is to say nothing"
   (cons "The promise that protected us was empty"
         (copula (rel (n "promise")
                      (vb "protect" :past :object (pro :1st :plural))
                      :role :object)
                 (a "empty") :past))

   ;; ═══════════════════════════════════════════
   ;; III. The Dissolution (para 3)
   ;; ═══════════════════════════════════════════

   ;; "the conclave had demanded the dissolution of the free cities"
   (cons "The lords asked the king to break the free cities"
         (vb "ask" :past :subject (n "lord" :plural)
              :object (n "king")
              :obliques (list (obl :to (vb "break" nil
                                            :object (adj "free" (n "city" :plural)))))))
   ;; "we did not believe it at first"
   (cons "We did not believe it"
         (adv "not" (vb "believe" :past :subject (pro :1st :plural))))
   ;; "The cities were the empire's engines. Half the goods that moved
   ;;  along the coast passed through our warehouses."
   (cons "The goods that made the empire strong came through our harbor"
         (vb "come" :past
              :subject (rel (n "goods")
                            (cause (pro :3rd :plural) (copula (n "empire") (a "strong"))))
              :obliques (list (obl :to (poss (pro :1st :plural) (n "harbor"))))))
   ;; "The notion that we should be handed over to whatever petty lord
   ;;  claimed the land beneath our docks seemed so obviously ruinous"
   (cons "This was so bad that we believed a lord who took the dock would break the trade"
         (so-that (copula (pro :3rd :singular) (a "bad"))
                  (vb "believe" :past :subject (pro :1st :plural)
                       :object (vb "break" :future
                                   :subject (rel (n "lord" :indefinite)
                                                 (vb "take" :past :object (n "dock")))
                                   :object (n "trade")))))
   ;; "we assumed cooler heads would prevail"
   (cons "We believed wise men would protect us"
         (vb "believe" :past :subject (pro :1st :plural)
              :object (vb "protect" :future
                          :subject (adj "wise" (n "man" :plural))
                          :object (pro :1st :plural))))
   ;; "We were wrong about that"
   (cons "We were blind"
         (copula (pro :1st :plural) (a "blind") :past))

   ;; ═══════════════════════════════════════════
   ;; IV. The Council's Refusal (para 4)
   ;; ═══════════════════════════════════════════

   ;; "The city council voted to refuse dissolution"
   (cons "The council refused the lords"
         (vb "refuse" :past :subject (n "council") :object (n "lord" :plural)))
   ;; "I supported this, as did nearly every merchant I knew"
   (cons "Every merchant who sold goods in the city stood with the council"
         (vb "stand" :past
              :subject (rel (n "merchant" :indefinite)
                            (vb "sell" :past :object (n "goods" :indefinite)
                                 :obliques (list (obl :with (n "city")))))
              :obliques (list (obl :with (n "council")))))
   ;; "We thought the emperor would intervene on our behalf...
   ;;  We had always understood the emperor to be our protector"
   (cons "We believed the king who we obeyed would protect us"
         (vb "believe" :past :subject (pro :1st :plural)
              :object (vb "protect" :future
                          :subject (rel (n "king")
                                        (vb "obey" :past :object (pro :1st :plural))
                                        :role :object)
                          :object (pro :1st :plural))))
   ;; "Instead, we heard nothing for six weeks"
   (cons "The king was silent"
         (copula (n "king") (a "silent") :past))
   ;; "and then the fleet appeared in the harbor"
   (cons "We waited, and the fleet came to the harbor"
         (temporal :after
                   (vb "wait" :past :subject (pro :1st :plural))
                   (vb "come" :past :subject (n "fleet")
                        :obliques (list (obl :to (n "harbor"))))))

   ;; ═══════════════════════════════════════════
   ;; V. The Taking (para 5)
   ;; ═══════════════════════════════════════════

   ;; "There was no battle. We had no army, only dock guards"
   (cons "We did not fight — we had no army, only dock guards"
         (adv "not" (vb "fight" :past :subject (pro :1st :plural))))
   ;; "The imperial marines came ashore at dawn"
   (cons "The fleet came at dawn"
         (vb "come" :past :subject (n "fleet")
              :obliques (list (obl :at (n "dawn" :indefinite)))))
   ;; "and by noon the council members were in irons"
   (cons "They chained the council before the day ended"
         (temporal :before
                   (vb "end" :past :subject (n "day"))
                   (vb "chain" :past :subject (pro :3rd :plural)
                        :object (n "council"))))
   ;; "the marines were working from lists... every guild master,
   ;;  every major merchant, every shipowner — anyone whose name
   ;;  appeared in the city's commercial registry"
   (cons "They took every merchant whose name was on the list"
         (vb "take" :past :subject (pro :3rd :plural)
              :object (rel (n "merchant" :indefinite)
                           (vb "hold" :past :object (n "name" :indefinite)))))
   ;; "I was not important enough to be taken on the first day.
   ;;  They came for me on the third."
   (cons "They came for me"
         (vb "come" :past :subject (pro :3rd :plural)
              :obliques (list (obl :to (pro :1st :singular)))))

   ;; ═══════════════════════════════════════════
   ;; VI. The Family (para 6)
   ;; ═══════════════════════════════════════════

   ;; "My wife wept. My children did not understand."
   (cons "My wife wept and my children did not know what was happening"
         (conj (vb "weep" :past :subject (poss (pro :1st :singular) (n "wife")))
               (adv "not" (vb "know" :past
                               :subject (poss (pro :1st :singular) (n "child" :plural))))))
   ;; "I was permitted to bring nothing"
   (cons "I could not bring my goods"
         (adv "not" (vb "bring" :past :subject (pro :1st :singular)
                        :object (poss (pro :1st :singular) (n "goods")))))
   ;; "A marine sergeant... told me that I should not expect to see
   ;;  my warehouse again"
   (cons "A guard said: you will not see your house again"
         (quote-speech (n "guard" :indefinite)
                       (adv "not" (vb "see" :future :subject (pro :2nd :singular)
                                      :object (poss (pro :2nd :singular) (n "house"))))))
   ;; "I spent that night writing letters to my brother, who had
   ;;  married into a farming family inland"
   (cons "I wrote letters to my brother who lived on the land"
         (vb "write" :past :subject (pro :1st :singular)
              :object (n "letter" :indefinite :plural)
              :obliques (list (obl :to (rel (poss (pro :1st :singular) (n "brother"))
                                            (vb "live" :past
                                                 :obliques (list (obl :on (n "earth")))))))))

   ;; ═══════════════════════════════════════════
   ;; VII. The Voyage (para 7)
   ;; ═══════════════════════════════════════════

   ;; "The voyage to the imperial city took eleven days.
   ;;  We were held below decks, which was miserable, but we were fed"
   (cons "They held us in the ship, but they gave us food"
         (but-conj (vb "hold" :past :subject (pro :3rd :plural)
                       :object (pro :1st :plural)
                       :obliques (list (obl :with (n "ship"))))
                   (vb "give" :past :subject (pro :3rd :plural)
                        :object (n "food" :indefinite)
                        :obliques (list (obl :to (pro :1st :plural))))))
   ;; "The voyage... was long"
   (cons "The road was long"
         (copula (n "road") (a "long") :past))
   ;; "One of them, when pressed by a guild master who demanded to
   ;;  know the charges, said only that we were traitors"
   (cons "A merchant asked the guard to speak the charges"
         (vb "ask" :past
              :subject (n "merchant" :indefinite)
              :object (n "guard")
              :obliques (list (obl :to (vb "speak" nil :object (n "charge" :plural))))))
   (cons "The guard said: you are traitors — the king will judge you"
         (quote-speech (n "guard")
                       (conj (copula (pro :2nd :plural)
                                     (n "traitor" :indefinite :plural))
                             (vb "judge" :future :subject (n "king")
                                  :object (pro :2nd :plural)))))

   ;; ═══════════════════════════════════════════
   ;; VIII. The Sentencing (para 8)
   ;; ═══════════════════════════════════════════

   ;; "We stood in rows — merchants, councilors... the entire
   ;;  commercial class of six cities, filthy from the ships"
   (cons "Merchants and people from the cities stood in chains in the square before the palace"
         (vb "stand" :past
              :subject (conj (n "merchant" :plural) (n "people"))
              :obliques (list (obl :with (n "chain" :indefinite :plural))
                              (obl :at (n "square"))
                              (obl :at (n "palace")))))
   ;; "The emperor's chancellor read the judgment. Treason against
   ;;  the empire. Slavery in perpetuity"
   (cons "The king's man said: you are traitors, you and your children are the king's slaves forever"
         (quote-speech (poss (n "king") (n "man"))
                       (conj (copula (pro :2nd :plural) (n "traitor" :indefinite :plural))
                             (copula (conj (pro :2nd :plural)
                                          (poss (pro :2nd :plural) (n "child" :plural)))
                                     (poss (n "king") (n "slave" :indefinite :plural))))))
   ;; "A woman beside me fainted. Several men shouted.
   ;;  Most of us simply stood, too stunned..."
   (cons "A woman fell and men shouted, but fear made the people silent"
         (but-conj (conj (vb "fall" :past :subject (n "woman" :indefinite))
                         (vb "shout" :past :subject (n "man" :plural)))
                   (cause (n "fear") (copula (n "people") (a "silent")))))

   ;; ═══════════════════════════════════════════
   ;; IX. The Return of Goods (para 9)
   ;; ═══════════════════════════════════════════

   ;; "We were assigned quarters. Not cells — quarters."
   (cons "They did not chain us but gave us houses"
         (but-conj (adv "not" (vb "chain" :past :subject (pro :3rd :plural)
                                   :object (pro :1st :plural)))
                   (vb "give" :past :subject (pro :3rd :plural)
                        :object (n "house" :indefinite :plural)
                        :obliques (list (obl :to (pro :1st :plural))))))
   ;; "Every item seized from our warehouses had been catalogued.
   ;;  We were asked to... sign for the return of our goods."
   (cons "Clerks came and returned the goods that they had taken from us"
         (conj (vb "come" :past :subject (n "clerk" :plural))
               (vb "return" :past :subject (pro :3rd :plural)
                    :object (rel (n "goods")
                                 (vb "take" :past :object (pro :1st :plural)
                                      :obliques (list (obl :from (pro :1st :plural))))
                                 :role :object))))
   ;; "I remember staring at the clerk... and asking him whether this
   ;;  was a joke"
   (cons "I asked the clerk if this was true"
         (vb "ask" :past :subject (pro :1st :singular) :object (n "clerk")))
   ;; "He looked at me with complete sincerity and said, 'The emperor
   ;;  requires productive slaves, not idle ones. You will resume your trades.'"
   (cons "The clerk said: the king wants slaves who sell, not slaves who sit — you will sell your goods"
         (quote-speech (n "clerk")
                       (conj (vb "want" :present :subject (n "king")
                                  :object (conj
                                           (rel (n "slave" :plural) (vb "sell" :present))
                                           (neg (rel (n "slave" :plural) (vb "sit" :present)))))
                             (vb "sell" :future :subject (pro :2nd :plural)
                                  :object (poss (pro :2nd :plural) (n "goods"))))))

   ;; ═══════════════════════════════════════════
   ;; X. The Ironic Freedom (para 10)
   ;; ═══════════════════════════════════════════

   ;; "And so we did. What else could we do?"
   (cons "We sold cloth — what else could we do?"
         (vb "sell" :past :subject (pro :1st :plural)
              :object (n "cloth" :indefinite)))
   ;; "We were slaves who owned our own stock, worked their own
   ;;  trades, and governed themselves through councils"
   (cons "We were slaves who owned our goods, sold our cloth, and ruled ourselves through a council"
         (copula (pro :1st :plural)
                 (rel (n "slave" :indefinite :plural)
                      (conj (vb "own" :past :object (poss (pro :1st :plural) (n "goods")))
                            (vb "sell" :past :object (poss (pro :1st :plural) (n "cloth")))
                            (vb "rule" :past :object (pro :1st :plural)
                                 :obliques (list (obl :with (n "council" :indefinite))))))
                 :past))
   ;; "It took me... a year to understand that this was not mercy but strategy"
   (cons "This was not mercy but wisdom"
         (copula (pro :3rd :singular)
                 (except-for (neg (n "mercy" :indefinite)) (n "wise-ness"))
                 :past))
   ;; "we were, in our chains, freer than we had ever been — because
   ;;  now no lord, no conclave, no petty noble with a claim to our
   ;;  dockside could touch us"
   (cons "We were slaves, but in our chains we were free"
         (but-conj (copula (pro :1st :plural) (n "slave" :indefinite :plural) :past)
                   (cause (n "chain" :plural)
                          (copula (pro :1st :plural) (a "free")))))
   (cons "No lord except the king could hold us"
         (vb "hold" :present
              :subject (neg (except-for (n "lord" :indefinite) (n "king")))
              :object (pro :1st :plural)))
   ;; "We belonged to the emperor alone, and the emperor... had very
   ;;  little interest in telling us what to do."
   (cons "We obeyed no lord except the king, and the king did not want to rule us"
         (but-conj (vb "obey" :past :subject (pro :1st :plural)
                       :object (except-for (neg (n "lord" :indefinite)) (n "king")))
                   (adv "not" (vb "want" :present :subject (n "king")
                                   :object (vb "rule" nil :object (pro :1st :plural))))))

   ;; ═══════════════════════════════════════════
   ;; XI. The Daughter (para 11)
   ;; ═══════════════════════════════════════════

   ;; "My daughter was born here, in the imperial city.
   ;;  She has never known another home."
   (cons "My daughter was born in the city — she has never known another home"
         (conj (copula (poss (pro :1st :singular) (n "daughter"))
                       (poss (n "city") (n "child" :indefinite)) :past)
               (adv "never" (vb "know" :past :subject (pro :3rd :singular)
                                 :object (n "home" :indefinite)))))
   ;; "She calls herself a slave with the same tone her mother once
   ;;  used to call herself a free citizen — with pride and defiance"
   (cons "Her mother once said: I am a free woman"
         (quote-speech (poss (pro :3rd :singular) (n "mother"))
                       (copula (pro :1st :singular)
                               (adj "free" (n "woman" :indefinite)))))
   (cons "My daughter says the same words: I am a proud slave"
         (quote-speech (poss (pro :1st :singular) (n "daughter"))
                       (copula (pro :1st :singular)
                               (adj "proud" (n "slave" :indefinite)))
                       :tense :present))
   ;; "She speaks... with pride and a certain defiance"
   (cons "She speaks with pride"
         (vb "speak" :present :subject (pro :3rd :singular)
              :obliques (list (obl :with (n "pride" :indefinite)))))
   ;; "I find this strange and wonderful and sad, all at once."
   (cons "I find this strange and good and sad"
         (copula (pro :3rd :singular) (a "strange")))

   ;; ═══════════════════════════════════════════
   ;; XII. The Reflection (para 12)
   ;; ═══════════════════════════════════════════

   ;; "What I know is that I was a free merchant who lost everything"
   (cons "I was a free merchant who lost everything"
         (copula (pro :1st :singular)
                 (rel (adj "free" (n "merchant" :indefinite))
                      (vb "lose" :past :object (n "home")))
                 :past))
   ;; "and then a slave who got it back"
   (cons "Then I was a slave who found it again"
         (copula (pro :1st :singular)
                 (rel (n "slave" :indefinite)
                      (vb "find" :past :object (n "home")))
                 :past))
   ;; "my granddaughter will inherit a trading house that pays no
   ;;  tariffs to any lord in the empire"
   (cons "My daughter's daughter will own a trade-house that gives no gold to any lord"
         (vb "own" :future
              :subject (poss (poss (pro :1st :singular) (n "daughter")) (n "daughter"))
              :object (rel (n "trade-house" :indefinite)
                           (vb "give" :present :object (neg (n "gold" :indefinite))
                                :obliques (list (obl :to (n "lord" :indefinite)))))))
   ;; "If this is slavery, it is a peculiar kind.
   ;;  But then, this is a peculiar empire."
   (cons "If this is slavery, it is a strange slavery — and this is a strange empire"
         (conj (cond-if (copula (pro :3rd :singular) (n "chain" :indefinite))
                        (copula (pro :3rd :singular) (a "strange")))
               (copula (pro :3rd :singular) (adj "strange" (n "empire" :indefinite)))))))

;;; Stress-test sentences — adapted from sentences.txt
;;; Exercises every semantic construct across ~55 sentences

(defun stress-sentences ()
  "Return (description . semantic-form) pairs stress-testing every construct."
  (list
   ;; ═══════════════════════════════════════════
   ;; Simple verbs — present, past, future
   ;; ═══════════════════════════════════════════

   ;; #1 The sun shines.
   (cons "[1] The sun shines"
         (vb "shine" :present :subject (n "sun")))
   ;; #3 The sun shone.
   (cons "[3] The sun shone"
         (vb "shine" :past :subject (n "sun")))
   ;; #4 The sun will shine.
   (cons "[4] The sun will shine"
         (vb "shine" :future :subject (n "sun")))
   ;; #9 The bright sun shines.
   (cons "[9] The bright sun shines"
         (vb "shine" :present :subject (adj "bright" (n "sun"))))
   ;; #10 The sun is rising now.
   (cons "[10] The sun rises"
         (vb "rise" :present :subject (n "sun")))
   ;; #11 All the people shouted.
   (cons "[11] The people shouted"
         (vb "shout" :past :subject (n "people")))
   ;; #19 The rain came down.
   (cons "[19] The rain fell"
         (vb "fall" :past :subject (n "rain")))
   ;; #64 The man laughed.
   (cons "[64] The man laughed"
         (vb "laugh" :past :subject (n "man")))

   ;; ═══════════════════════════════════════════
   ;; Copula — adjective/noun predicates, all tenses
   ;; ═══════════════════════════════════════════

   ;; #82 I am very happy.
   (cons "[82] I am happy"
         (copula (pro :1st :singular) (a "happy")))
   ;; #85 The streets are full of people.
   (cons "[85] The road is full"
         (copula (n "road") (a "full")))
   ;; #86 The food tastes sweet.
   (cons "[86] The food is sweet"
         (copula (n "food") (a "sweet")))
   ;; #87 The fire feels hot.
   (cons "[87] The fire is hot"
         (copula (n "fire") (a "hot")))
   ;; #88 The little girl seemed lonely.
   (cons "[88] The child was alone"
         (copula (n "child") (a "silent") :past))
   ;; #89 The boy's father had been a sailor.
   (cons "[89] The child's father was a man who sailed"
         (copula (poss (n "child") (n "father"))
                 (rel (n "man" :indefinite) (vb "sail" :past))
                 :past))

   ;; ═══════════════════════════════════════════
   ;; Negation
   ;; ═══════════════════════════════════════════

   ;; #21 The rain has stopped.
   (cons "[21] The rain does not fall"
         (adv "not" (vb "fall" :present :subject (n "rain"))))
   ;; #22 Soon the rain will stop.
   (cons "[22] The rain will stop"
         (vb "stop" :future :subject (n "rain")))
   ;; #148 They looked but saw nothing.
   (cons "[148] They see nothing"
         (vb "see" :present :subject (pro :3rd :plural)
              :object (neg (n "path" :indefinite))))

   ;; ═══════════════════════════════════════════
   ;; Conjunction (NP and VP), adversative
   ;; ═══════════════════════════════════════════

   ;; #111 The man and the woman are brother and sister.
   (cons "[111] The man and the woman are brother and sister"
         (copula (conj (n "man") (n "woman"))
                 (conj (n "brother" :indefinite) (n "wife" :indefinite))))
   ;; #112 You and I will go together.
   (cons "[112] You and I will go"
         (vb "go" :future :subject (conj (pro :2nd :singular) (pro :1st :singular))))
   ;; #113 They opened all the doors.
   (cons "[113] They opened the door"
         (vb "open" :past :subject (pro :3rd :plural) :object (n "door")))
   ;; #114 He is small, but strong.
   (cons "[114] He is small but strong"
         (but-conj (copula (pro :3rd :singular) (a "small"))
                   (copula (pro :3rd :singular) (a "strong"))))
   ;; #124 The child laughed and played.
   (cons "[124] The child laughed and played"
         (conj (vb "laugh" :past :subject (n "child"))
               (vb "play" :past :subject (n "child"))))

   ;; ═══════════════════════════════════════════
   ;; Possessive
   ;; ═══════════════════════════════════════════

   ;; #39 The man's dog is lost.
   (cons "[39] The man's dog is lost"
         (copula (poss (n "man") (n "dog")) (a "blind")))
   ;; #145 Our bird's name is bright.
   (cons "[145] The bird's name is old"
         (copula (poss (n "bird") (n "name")) (a "old")))

   ;; ═══════════════════════════════════════════
   ;; Comparative
   ;; ═══════════════════════════════════════════

   ;; #202 She is stronger than her brother.
   (cons "[202] She is stronger than her brother"
         (comp "strong" :target (pro :3rd :singular)
                :than (poss (pro :3rd :singular) (n "brother"))))
   ;; #204 Light travels faster than sound.
   (cons "[204] The horse is faster than the dog"
         (comp "fast" :target (n "horse") :than (n "dog")))
   ;; #206 She has more friends than enemies.
   (cons "[206] He has more friends than enemies"
         (comp "good" :target (n "friend" :plural) :than (n "enemy" :plural)))

   ;; ═══════════════════════════════════════════
   ;; Temporal clauses — before, after, when
   ;; ═══════════════════════════════════════════

   ;; #162 Think first and then act.
   (cons "[162] Before you fight, know your enemy"
         (temporal :before
                   (vb "fight" :present :subject (pro :2nd :singular))
                   (vb "know" :present :subject (pro :2nd :singular)
                        :object (poss (pro :2nd :singular) (n "enemy")))))
   ;; #190 When he saw me, he stopped.
   (cons "[190] When he saw me, he stopped"
         (temporal :when
                   (vb "see" :past :subject (pro :3rd :singular)
                        :object (pro :1st :singular))
                   (vb "stop" :past :subject (pro :3rd :singular))))
   ;; after
   (cons "[after] After the war ended, the people sang"
         (temporal :after
                   (vb "end" :past :subject (n "war"))
                   (vb "sing" :past :subject (n "people"))))

   ;; ═══════════════════════════════════════════
   ;; Conditional — if/then
   ;; ═══════════════════════════════════════════

   ;; #189 I shall stay at home if it rains.
   (cons "[189] If the rain falls, I will wait at home"
         (cond-if (vb "fall" :present :subject (n "rain"))
                  (vb "wait" :future :subject (pro :1st :singular)
                       :obliques (list (obl :at (n "home"))))))
   ;; #194 If you come early, wait in the hall.
   (cons "[194] If you come, wait at the door"
         (cond-if (vb "come" :present :subject (pro :2nd :singular))
                  (imp (vb "wait" nil :obliques (list (obl :at (n "door")))))))
   ;; #138 Are you warm enough now?
   (cons "[138] If you are warm, I am happy"
         (cond-if (copula (pro :2nd :singular) (a "warm"))
                  (copula (pro :1st :singular) (a "happy"))))

   ;; ═══════════════════════════════════════════
   ;; Relative clauses — subject and object role
   ;; ═══════════════════════════════════════════

   ;; #184 The boy who brought the book has gone.
   (cons "[184] The man who brought the sword has gone"
         (vb "go" :past :subject (rel (n "man")
                                      (vb "bring" :past :object (n "sword")))))
   ;; #186 I have lost the book that you gave me.
   (cons "[186] I lost the sword that you gave me"
         (vb "lose" :past :subject (pro :1st :singular)
              :object (rel (n "sword")
                           (vb "give" :past :subject (pro :2nd :singular)
                                :object (pro :1st :singular))
                           :role :object)))
   ;; #196 They are small men who live under the mountain.
   (cons "[196] They are small men who live under the mountain"
         (copula (pro :3rd :plural)
                 (rel (adj "small" (n "man" :indefinite :plural))
                      (vb "live" :present
                           :obliques (list (obl :at (n "mountain")))))))
   ;; #199 I found the ring I lost.
   (cons "[199] I found the gold I lost"
         (vb "find" :past :subject (pro :1st :singular)
              :object (rel (n "gold")
                           (vb "lose" :past)
                           :role :object)))

   ;; ═══════════════════════════════════════════
   ;; Imperative
   ;; ═══════════════════════════════════════════

   ;; #26 Go away!
   (cons "[26] Go!"
         (imp (vb "go" nil)))
   ;; #103 Listen.
   (cons "[103] Hear!"
         (imp (vb "hear" nil)))
   ;; #106 Come with us.
   (cons "[106] Come with us"
         (imp (vb "come" nil :obliques (list (obl :with (pro :1st :plural))))))
   ;; #125 Stop your game and be silent.
   (cons "[125] Stop and be silent"
         (imp (conj (vb "stop" nil) (copula (pro :2nd :singular) (a "silent")))))

   ;; ═══════════════════════════════════════════
   ;; Obliques — with, from, to, on, at
   ;; ═══════════════════════════════════════════

   ;; #48 The people sat around the fire.
   (cons "[48] The people sat at the fire"
         (vb "sit" :past :subject (n "people")
              :obliques (list (obl :at (n "fire")))))
   ;; #46 We arrived at the river.
   (cons "[46] We came to the river"
         (vb "come" :past :subject (pro :1st :plural)
              :obliques (list (obl :to (n "river")))))
   ;; #107 Bring your friends with you.
   (cons "[107] Bring your friend with you"
         (imp (vb "bring" nil :object (poss (pro :2nd :singular) (n "friend"))
                   :obliques (list (obl :with (pro :2nd :singular))))))
   ;; #77 The roots of the tree were torn from the earth.
   (cons "[77] The root came from the earth"
         (vb "come" :past :subject (n "root")
              :obliques (list (obl :from (n "earth")))))
   ;; #67 The seeds waited under the snow for the warm sun.
   (cons "[67] The seed waited in the snow for the warm sun"
         (vb "wait" :past :subject (n "seed")
              :obliques (list (obl :at (n "snow"))
                              (obl :to (adj "warm" (n "sun"))))))

   ;; ═══════════════════════════════════════════
   ;; Reported speech
   ;; ═══════════════════════════════════════════

   ;; #211 "This tree is old," said the man.
   (cons "[211] The man said: this tree is old"
         (quote-speech (n "man")
                       (copula (n "tree") (a "old"))))
   ;; #218 I met a girl; she said she was young.
   (cons "[218] The woman said: I am young"
         (quote-speech (n "woman")
                       (copula (pro :1st :singular) (a "young"))
                       :tense :present))

   ;; ═══════════════════════════════════════════
   ;; Causative, exception, degree/result
   ;; ═══════════════════════════════════════════

   ;; causative — fear makes the wolf run
   (cons "[cause] Fear makes the wolf run"
         (cause (n "fear") (vb "run" :present :subject (n "wolf"))))
   ;; exception — all except the king
   (cons "[except] All people came except the king"
         (vb "come" :past :subject (except-for (n "people") (n "king"))))
   ;; degree/result — so hot that the stone breaks
   (cons "[degree] The fire is so hot that the stone breaks"
         (so-that (copula (n "fire") (a "hot"))
                  (vb "break" :present :subject (n "stone"))))

   ;; ═══════════════════════════════════════════
   ;; Adverbs
   ;; ═══════════════════════════════════════════

   ;; #42 I always sleep soundly.
   (cons "[42] I always sleep"
         (adv "always" (vb "sleep" :present :subject (pro :1st :singular))))
   ;; never
   (cons "[never] The wolf never sleeps"
         (adv "never" (vb "sleep" :present :subject (n "wolf"))))

   ;; ═══════════════════════════════════════════
   ;; Pronouns — all persons and numbers
   ;; ═══════════════════════════════════════════

   ;; 1sg
   (cons "[pro-1sg] I walk"
         (vb "walk" :present :subject (pro :1st :singular)))
   ;; 2sg
   (cons "[pro-2sg] You see the star"
         (vb "see" :present :subject (pro :2nd :singular) :object (n "star")))
   ;; 3sg
   (cons "[pro-3sg] She holds the sword"
         (vb "hold" :present :subject (pro :3rd :singular) :object (n "sword")))
   ;; 1pl
   (cons "[pro-1pl] We fight"
         (vb "fight" :present :subject (pro :1st :plural)))
   ;; 2pl
   (cons "[pro-2pl] You hide the gold"
         (vb "hide" :present :subject (pro :2nd :plural) :object (n "gold")))
   ;; 3pl
   (cons "[pro-3pl] They call the people"
         (vb "call" :present :subject (pro :3rd :plural) :object (n "people")))

   ;; ═══════════════════════════════════════════
   ;; Additional combos — flowers, snow, dress, happy
   ;; ═══════════════════════════════════════════

   ;; #34 Lovely flowers are growing.
   (cons "[34] The flowers grow"
         (vb "grow" :present :subject (n "flower" :plural)))
   ;; #67 seeds under snow
   (cons "[67b] The flower hides in the snow"
         (vb "hide" :present :subject (n "flower")
              :obliques (list (obl :at (n "snow")))))
   ;; #14 Happy people shout.
   (cons "[14] Happy people shout"
         (vb "shout" :present :subject (adj "happy" (n "people"))))
   ;; #132 I dressed hastily.
   (cons "[132] I dressed and went"
         (conj (vb "dress" :past :subject (pro :1st :singular))
               (vb "go" :past :subject (pro :1st :singular))))
   ;; #200 Play and I will sing.
   (cons "[200] Play and I will sing"
         (conj (imp (vb "play" nil))
               (vb "sing" :future :subject (pro :1st :singular))))

   ;; ═══════════════════════════════════════════
   ;; Questions (yes/no interrogative)
   ;; ═══════════════════════════════════════════

   ;; #54 Did the man leave?
   (cons "[54] Did the man go?"
         (ask (vb "go" :past :subject (n "man"))))
   ;; #58 Does the bird sing?
   (cons "[58] Does the bird sing?"
         (ask (vb "sing" :present :subject (n "bird"))))
   ;; #95q Is the day good?
   (cons "[95q] Is the day good?"
         (ask (copula (n "day") (a "good"))))
   ;; #97q Will you come?
   (cons "[97q] Will you come?"
         (ask (vb "come" :future :subject (pro :2nd :singular))))
   ;; #98 Will you send the word?
   (cons "[98] Will you send the word?"
         (ask (vb "send" :future :subject (pro :2nd :singular) :object (n "word"))))
   ;; #99 Are you waiting for me?
   (cons "[99] Are you waiting for me?"
         (ask (vb "wait" :present :subject (pro :2nd :singular)
                   :obliques (list (obl :to (pro :1st :singular))))))

   ;; ═══════════════════════════════════════════
   ;; Modality — ability and obligation
   ;; ═══════════════════════════════════════════

   ;; #28 You should go.
   (cons "[28] You should go"
         (mdl :obligation (vb "go" :present :subject (pro :2nd :singular))))
   ;; #35 We should eat.
   (cons "[35] We should eat"
         (mdl :obligation (vb "eat" :present :subject (pro :1st :plural))))
   ;; #44 I can play.
   (cons "[44] I can play"
         (mdl :ability (vb "play" :present :subject (pro :1st :singular))))
   ;; #209 We can go.
   (cons "[209] We can go"
         (mdl :ability (vb "go" :present :subject (pro :1st :plural))))
   ;; #155 You must obey the king.
   (cons "[155] You must obey the king"
         (mdl :obligation (vb "obey" :present :subject (pro :2nd :singular) :object (n "king"))))

   ;; ═══════════════════════════════════════════
   ;; Nested: question + modal
   ;; ═══════════════════════════════════════════

   ;; #53 Can the man sing?
   (cons "[53] Can the man sing?"
         (ask (mdl :ability (vb "sing" :present :subject (n "man")))))
   ;; #56 Can you come?
   (cons "[56] Can you come?"
         (ask (mdl :ability (vb "come" :present :subject (pro :2nd :singular)))))

   ;; ═══════════════════════════════════════════
   ;; Disjunction — "X or Y"
   ;; ═══════════════════════════════════════════

   ;; #115 Is this tree an oak or a maple?
   (cons "[115] Is the tree old or young?"
         (ask (copula (n "tree") (disj (a "old") (a "young")))))
   ;; #116 Does the sky look blue or gray?
   (cons "[116] Is the sky dark or bright?"
         (ask (copula (n "sky") (disj (a "dark") (a "bright")))))
   ;; #117 Come with your father or mother.
   (cons "[117] Come with your father or mother"
         (imp (vb "come" nil :obliques (list (obl :with (disj (poss (pro :2nd :singular) (n "father"))
                                                              (poss (pro :2nd :singular) (n "mother"))))))))
   ;; #127 Do you like summer or winter better?
   (cons "[127] Do you love the sun or the moon?"
         (ask (vb "love" :present :subject (pro :2nd :singular)
                   :object (disj (n "sun") (n "moon")))))
   ;; #159 Be quick or you will be too late.
   (cons "[159] Be fast or you will fall"
         (disj (imp (copula (pro :2nd :singular) (a "fast")))
               (vb "fall" :future :subject (pro :2nd :singular))))
   ;; #160 Will you go with us or wait here?
   (cons "[160] Will you go with us or wait?"
         (ask (disj (vb "go" :future :subject (pro :2nd :singular)
                        :obliques (list (obl :with (pro :1st :plural))))
                    (vb "wait" :future :subject (pro :2nd :singular)))))

   ;; ═══════════════════════════════════════════
   ;; Reflexive
   ;; ═══════════════════════════════════════════

   ;; #170 I hurt myself.
   (cons "[170] I see myself"
         (vb "see" :present :subject (pro :1st :singular) :object (refl :1st :singular)))
   ;; #171 She was talking to herself.
   (cons "[171] She speaks to herself"
         (vb "speak" :present :subject (pro :3rd :singular)
              :obliques (list (obl :to (refl :3rd :singular)))))
   ;; #172 He proved himself trustworthy.
   (cons "[172] He made himself strong"
         (cause (pro :3rd :singular) (copula (refl :3rd :singular) (a "strong"))))
   ;; #173 We could see ourselves in the water.
   (cons "[173] We see ourselves in the water"
         (vb "see" :present :subject (pro :1st :plural)
              :object (refl :1st :plural)
              :obliques (list (obl :at (n "water")))))
   ;; #174 Do it yourself.
   (cons "[174] Do it yourself"
         (imp (vb "make" nil :subject (refl :2nd :singular))))
   ;; #175 I feel ashamed of myself.
   (cons "[175] I fear myself"
         (vb "fear" :present :subject (pro :1st :singular) :object (refl :1st :singular)))
   ;; #176 Sit here by yourself.
   (cons "[176] Sit alone"
         (imp (adv "alone" (vb "sit" nil :subject (refl :2nd :singular)))))

   ;; ═══════════════════════════════════════════
   ;; Causal clause — "because X, Y"
   ;; ═══════════════════════════════════════════

   ;; #191 Do not laugh at me because I seem so absent minded.
   (cons "[191] Do not laugh because I am silent"
         (because-of (copula (pro :1st :singular) (a "silent"))
                     (imp (neg (vb "laugh" nil)))))
   ;; #197 He is loved by everybody, because he has a gentle disposition.
   (cons "[197] The people love him because he is good"
         (because-of (copula (pro :3rd :singular) (a "good"))
                     (vb "love" :present :subject (n "people")
                          :object (pro :3rd :singular))))
   ;; #217 The gate is never opened, for the grass grows close against it.
   (cons "[217] The door is never opened because the tree grows at the wall"
         (because-of (vb "grow" :present :subject (n "tree")
                          :obliques (list (obl :at (n "wall"))))
                     (adv "never" (vb "open" :present :subject (n "door")))))

   ;; ═══════════════════════════════════════════
   ;; Exclamation
   ;; ═══════════════════════════════════════════

   ;; #133 Aha! I have caught you!
   (cons "[133] Ha! I find you!"
         (excl (vb "find" :present :subject (pro :1st :singular) :object (pro :2nd :singular))))
   ;; #134 This string is too short!
   (cons "[134] The road is too short!"
         (excl (copula (n "road") (a "short"))))
   ;; #135 The wind has blown my hat away!
   (cons "[135] The wind took my shield!"
         (excl (vb "take" :past :subject (n "wind")
                    :object (poss (pro :1st :singular) (n "shield")))))
   ;; #136 Alas! That news is sad indeed!
   (cons "[136] The word is bad!"
         (excl (copula (n "word") (a "bad"))))
   ;; #137 That cold wind freezes my nose!
   (cons "[137] The cold wind burns my hand!"
         (excl (vb "burn" :present :subject (adj "cold" (n "wind"))
                    :object (poss (pro :1st :singular) (n "hand")))))

   ;; ═══════════════════════════════════════════
   ;; Simile — "X like Y"
   ;; ═══════════════════════════════════════════

   ;; #79 The wind blew across my face like a friendly caress.
   (cons "[79] The wind came like a friend"
         (like-as (vb "come" :past :subject (n "wind"))
                  (n "friend" :indefinite)))
   ;; #147 The boat sails away, like a bird on the wing.
   (cons "[147] The ship sails like a bird"
         (like-as (vb "sail" :present :subject (n "ship"))
                  (n "bird" :indefinite)))
   ;; #181 Light he thought her, like a feather.
   (cons "[181] She is light like a leaf"
         (like-as (copula (pro :3rd :singular) (a "small"))
                  (n "leaf" :indefinite)))

   ;; ═══════════════════════════════════════════
   ;; Wh-question
   ;; ═══════════════════════════════════════════

   ;; #73 When will your guests arrive?
   (cons "[73] When will your friend come?"
         (ask-wh :when (vb "come" :future :subject (poss (pro :2nd :singular) (n "friend")))))
   ;; #102 How wide is the River?
   (cons "[102] How deep is the river?"
         (ask-wh :how (copula (n "river") (a "deep"))))
   ;; #210 Where did we see the flowers?
   (cons "[210] Where did we see the flower?"
         (ask-wh :where (vb "see" :past :subject (pro :1st :plural) :object (n "flower"))))
   ;; #214 Why he has left the city is a mystery.
   (cons "[214] Why did he go from the city?"
         (ask-wh :why (vb "go" :past :subject (pro :3rd :singular)
                            :obliques (list (obl :from (n "city"))))))
   ;; #215 The house stands where three roads meet.
   (cons "[215] Where does the path end?"
         (ask-wh :where (vb "end" :present :subject (n "path"))))

   ;; ═══════════════════════════════════════════
   ;; Complement clause — "I think that X"
   ;; ═══════════════════════════════════════════

   ;; #23 I hope the rain stops soon.
   (cons "[23] I hope the rain stops"
         (think (pro :1st :singular) "hope"
                (vb "stop" :present :subject (n "rain"))))
   ;; #212 I think that this train leaves earlier today.
   (cons "[212] I think the ship sails"
         (think (pro :1st :singular) "think"
                (vb "sail" :present :subject (n "ship"))))
   ;; #213 My opinion is that the governor will grant him a pardon.
   (cons "[213] I believe the king will give mercy"
         (think (pro :1st :singular) "believe"
                (vb "give" :future :subject (n "king") :object (n "mercy"))))
   ;; #214b complement version
   (cons "[214b] She knows the man will come"
         (think (pro :3rd :singular) "know"
                (vb "come" :future :subject (n "man"))))

   ;; ═══════════════════════════════════════════
   ;; Optative — wish/blessing
   ;; ═══════════════════════════════════════════

   (cons "[opt-1] May the sun shine on your path"
         (wish (vb "shine" :present :subject (n "sun")
                    :obliques (list (obl :on (poss (pro :2nd :singular) (n "path")))))))
   (cons "[opt-2] May your sword stay sharp"
         (wish (copula (poss (pro :2nd :singular) (n "sword")) (a "sharp"))))

   ;; ═══════════════════════════════════════════
   ;; Concessive — "though X, Y"
   ;; ═══════════════════════════════════════════

   (cons "[conc-1] Though the king is strong, the people are free"
         (although (copula (n "king") (a "strong"))
                   (copula (n "people") (a "free"))))
   (cons "[conc-2] Though he fell, he rose again"
         (although (vb "fall" :past :subject (pro :3rd :singular))
                   (vb "rise" :past :subject (pro :3rd :singular))))

   ;; ═══════════════════════════════════════════
   ;; Reciprocal — "each other"
   ;; ═══════════════════════════════════════════

   (cons "[recip-1] The brothers love each other"
         (vb "love" :present :subject (n "brother" :plural) :object (recip :3rd :plural)))
   (cons "[recip-2] We see each other"
         (vb "see" :present :subject (pro :1st :plural) :object (recip :1st :plural)))

   ;; ═══════════════════════════════════════════
   ;; Nominalization — verb-to-noun
   ;; ═══════════════════════════════════════════

   (cons "[nmlz-1] Killing is good, ruling is bad"
         (but-conj (copula (nmlz (vb "kill" nil)) (a "good"))
                   (copula (nmlz (vb "rule" nil)) (a "bad"))))
   (cons "[nmlz-2] The knowing of truth brings fear"
         (vb "bring" :present :subject (nmlz (vb "know" nil :object (n "truth")))
                              :object (n "fear")))

   ;; ═══════════════════════════════════════════
   ;; Distributive — "every X"
   ;; ═══════════════════════════════════════════

   (cons "[dist-1] Every wolf dies alone"
         (adv "alone" (vb "die" :present :subject (every-np (n "wolf")))))
   (cons "[dist-2] Every man holds a sword"
         (vb "hold" :present :subject (every-np (n "man")) :object (n "sword")))
   (cons "[dist-3] Every path leads to death"
         (vb "lead" :present :subject (every-np (n "path"))
                              :obliques (list (obl :to (n "death")))))

   ;; ═══════════════════════════════════════════
   ;; Counterfactual — "had X, would Y"
   ;; ═══════════════════════════════════════════

   (cons "[cf-1] Had the wolf been wise, it would have lived"
         (counter-if (copula (n "wolf") (a "wise"))
                     (vb "live" :past :subject (n "wolf"))))
   (cons "[cf-2] Had I known, I would not have gone"
         (counter-if (vb "know" :past :subject (pro :1st :singular))
                     (adv "not" (vb "go" :past :subject (pro :1st :singular)))))

   ;; ═══════════════════════════════════════════
   ;; Info-structure: promote / demote
   ;; ═══════════════════════════════════════════

   ;; Promote subject (topicalization)
   (cons "[info-1] As for the wolf, it hunts"
         (vb "hunt" :present :subject (n "wolf" :promote)))
   ;; Promote object
   (cons "[info-2] The food, I cook it"
         (vb "cook" :present :subject (pro :1st :singular) :object (n "food" :promote)))
   ;; Demote subject (agent backgrounding)
   (cons "[info-3] The food is cooked"
         (vb "cook" :present :subject (pro :3rd :singular :demote) :object (n "food")))
   ;; Promote object + demote subject (full passive-like)
   (cons "[info-4] The sword was taken"
         (vb "take" :past :subject (pro :3rd :singular :demote) :object (n "sword" :promote)))
   ;; Demote subject impersonal
   (cons "[info-5] One speaks the truth"
         (vb "speak" :present :subject (pro :3rd :plural :demote) :object (n "truth")))
   ;; Promote in copula
   (cons "[info-6] As for death, it is silent"
         (copula (n "death" :promote) (a "silent")))
   ;; Both promote and demote with oblique
   (cons "[info-7] The child was given food"
         (vb "give" :past :subject (pro :3rd :singular :demote) :object (n "food" :promote)
              :obliques (list (obl :to (n "child")))))
   ;; Demote only, no promote
   (cons "[info-8] Someone broke the wall"
         (vb "break" :past :subject (pro :3rd :singular :demote) :object (n "wall")))))

(defun run-stress (&key (seed 42))
  "Generate all languages and translate stress-test sentences."
  (setf *random-state* (sb-ext:seed-random-state seed))
  (initialize)
  (let ((langs (make-world))
        (phrases (stress-sentences)))
    (translate-phrases langs phrases "STRESS-TEST SENTENCES")
    langs))

(defun translate-phrases (langs phrases title)
  "Display grammar summaries and translate phrases across all languages."
  (format t "~%========================================~%")
  (format t "  LANGUAGES OF THE WORLD~%")
  (format t "========================================~%")
  ;; Print grammar summaries
  (dolist (lang (remove nil langs))
    (format t "~%--- ~a ---~%" (lang-name lang))
    (let ((g (grammar lang)))
      (format t "  Order: ~a  NP: ~a  Gen: ~a~%"
              (getf g :clause-order) (getf g :np-order) (getf g :gen-order))
      (format t "  Copula: ~a  Conj: ~a  Adpos: ~a~%"
              (getf g :copula-strategy)
              (getf g :conjunction-strategy)
              (getf g :adposition-strategy))
      (format t "  Conditional: ~a (~a)  Comp: ~a~%"
              (getf g :conditional-strategy)
              (getf g :conditional-order)
              (getf g :comp-order))
      (format t "  Relative: ~a (~a)  Purpose: ~a  Causative: ~a (~a)~%"
              (getf g :relative-strategy)
              (getf g :relative-order)
              (getf g :purpose-order)
              (getf g :causative-strategy)
              (getf g :causative-order))
      (format t "  Quotation: ~a  Adversative: ~a (~a)  Degree: ~a~%"
              (getf g :quotation-order)
              (getf g :adversative-strategy)
              (getf g :adversative-order)
              (getf g :degree-order))
      (format t "  Question: ~a (~a)  Modal: ~a (~a)~%"
              (getf g :question-strategy) (getf g :question-particle-position)
              (getf g :modal-strategy) (getf g :modal-order))
      (format t "  Disjunction: ~a  Reflexive: ~a  Causal: ~a (~a)~%"
              (getf g :disjunction-strategy)
              (getf g :reflexive-strategy)
              (getf g :causal-strategy)
              (getf g :causal-order))
      (format t "  Exclamation: ~a (~a)  Simile: ~a (~a)  Wh: ~a (~a)  Complement: ~a~%"
              (getf g :exclamation-strategy) (getf g :exclamation-position)
              (getf g :simile-strategy) (getf g :simile-order)
              (getf g :wh-strategy) (getf g :wh-position)
              (getf g :complement-order))
      (format t "  Concessive: ~a (~a)  Reciprocal: ~a~%"
              (getf g :concessive-strategy)
              (getf g :concessive-order)
              (getf g :reciprocal-strategy))
      (format t "  Nominalization: ~a  Distributive: ~a~%"
              (getf g :nominalization-strategy)
              (getf g :distributive-strategy))
      (format t "  Noun classes: ~a~@[ (~{~a~^, ~})~]~%"
              (getf g :noun-class-count)
              (getf g :noun-classes))
      (format t "  Compound: ~a ~a~@[ linker [~{~a~}]~]~%"
              (getf g :compound-strategy)
              (getf g :compound-order)
              (getf g :compound-linker))
      (format t "  Promote: ~a  Demote: ~a~%"
              (getf g :promote-strategy)
              (getf g :demote-strategy))
      (format t "  Agreement: ~a  Pro-drop: ~a  Pronoun collapse: ~a~@[  Pl-disambig: ~a~]~%"
              (getf g :agreement-type)
              (getf g :pro-drop)
              (getf g :pronoun-collapse)
              (getf g :plural-pronoun-strategy))
      (format t "  Morphology:~%")
      (dolist (rule (getf g :morphology))
        (format t "    ~a ~a: ~a~@[ ~a~]~%"
                (mrule-feature rule) (mrule-applies-to rule)
                (mrule-strategy rule)
                (when (mrule-marker rule)
                  (format nil "[~{~a~}]" (mrule-marker rule)))))))
  ;; Translate phrases
  (format t "~%~%========================================~%")
  (format t "  ~a~%" title)
  (format t "========================================~%")
  (dolist (phrase phrases)
    (format t "~%~%>> ~a~%" (car phrase))
    (format t "~40,,,'-a~%" "")
    (dolist (lang (remove nil langs))
      (handler-case
          (let ((msg (cdr phrase)))
            (format t "  ~30a ~a~%" (lang-name lang)
                    (render-string lang msg :orthography #'anglicize))
            (format t "  ~30a ~a~%" ""
                    (render-string lang msg :orthography #'roman))
            (format t "  ~30a ~a~%" ""
                    (render-string lang msg :orthography #'ipa))
            (format t "  ~30a ~a~%~%" ""
                    (render-gloss-string lang msg)))
        (error (e)
          (format t "  ~30a [error: ~a]~%" (lang-name lang) e))))))

(defun run-demo (&key (seed 42))
  "Generate all languages and translate demo phrases."
  (setf *random-state* (sb-ext:seed-random-state seed))
  (initialize)
  (let ((langs (make-world))
        (phrases (demo-phrases)))
    (translate-phrases langs phrases "TRANSLATIONS")
    langs))

(defun run-sample (&key (seed 42))
  "Generate all languages and translate sample text phrases."
  (setf *random-state* (sb-ext:seed-random-state seed))
  (initialize)
  (let ((langs (make-world))
        (phrases (sample-phrases)))
    (translate-phrases langs phrases "SAMPLE TEXT TRANSLATIONS")
    langs))

(defun run-text (&key (seed 42))
  "Generate all languages and translate the full sample text."
  (setf *random-state* (sb-ext:seed-random-state seed))
  (initialize)
  (let ((langs (make-world))
        (phrases (sample-text)))
    (translate-phrases langs phrases "SAMPLE TEXT — FULL TRANSLATION")
    langs))
