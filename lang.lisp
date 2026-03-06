;;;; lang.lisp

(in-package #:lang)

(defmacro swap (a b)
  (let ((s (gensym)))
    `(let ((,s ,a))
       (setf ,a ,b)
       (setf ,b ,s))))

(defun scramble-sequence (seq)
  (let ((l (length seq)))
    (loop 
       for i below (- l 1)
       do (let ((j (+ i (random (- l i)))))
            (swap (elt seq i) (elt seq j)))))
  seq)

(defun zipf (l)
  (if (listp l)
      (iter (for i upfrom 1) (for s in l)
        (collect (cons s (/ 1.0 i))))
      (list (cons l 1.0))))

(defun uniform (l)
  (iter (for s in l)
    (collect (cons s 1))))

(defclass phone ()
  ((ipa :reader ipa :initarg :ipa)
   (num :reader num :initarg :num)
   (roman :reader roman :initarg :roman :initform "")))

(defclass vowel (phone)
  ((height :reader height :initarg :height)
   (backness :reader backness :initarg :backness)
   (rounding :reader rounding :initarg :rounding)))

(defclass consonant (phone)
  ((voicing :reader voicing :initarg :voicing)
   (place :reader place :initarg :place)
   (manner :reader manner :initarg :manner)))

(defclass sequence-point () ())

(defclass inter-phone-marker (sequence-point)
  ((boundary-type :initarg :boundary-type :accessor boundary-type)))

(defclass phone-point (sequence-point)
  ((phone :accessor phone :initarg :phone)
   (stress :accessor stress :initarg :stress)))

(defgeneric consonant-p (p))

(defmethod consonant-p (p)
  nil)

(defmethod consonant-p ((p consonant))
  t)

(defgeneric vowel-p (p))

(defmethod vowel-p (p)
  nil)

(defmethod vowel-p ((p vowel))
  t)

(defmethod consonant-p ((p phone-point))
  (consonant-p (phone p)))

(defmethod vowel-p ((p phone-point))
  (vowel-p (phone p)))

(defgeneric phone-p (p))

(defmethod phone-p (p)
  nil)

(defmethod phone-p ((p phone))
  t)

(defmethod phone-p ((p phone-point))
  t)

(defun consonant-properties (l)
  (list (nth 4 l)
        (nth 5 l)
        (nth 6 l)))

(defun vowel-properties (l)
  (list (nth 7 l)
        (nth 8 l)
        (nth 9 l)))

(defun describes-vowel-p (l)
  (and
   (iter
     (for p in (vowel-properties l))
     (thereis (not (equal p ""))))
   (not
    (iter
      (for p in (consonant-properties l))
      (thereis (not (equal p "")))))))

(defun describes-consonant-p (l)
  (and
   (iter
     (for p in (consonant-properties l))
     (thereis (not (equal p ""))))
   (not
    (iter
      (for p in (vowel-properties l))
      (thereis (not (equal p "")))))))

(defparameter *vowels* nil)
(defparameter *consonants* nil)
(defparameter *phones* nil)
(defparameter *safe-consonants* nil)
(defparameter *no-replacement-seen* (make-hash-table :test #'equal))

(defun fix-place (p)
  (if (equal p "dental;alveolar")
      "alveolar"
      p))

(defun read-place (l)
  (let ((p (nth 1 (consonant-properties l))))
    (fix-place p)))

(defun remove-weird-consonants (x)
  (iter (for manner in '(click lateral-click fricative-release 
                        lateral-release nasal-release implosive 
                        ejective percussive))
        (setf x (filter-consonant x :remove t :manner manner))
        (finally (return x))))

(defun symbolize (str)
  (intern (string-upcase (substitute #\- #\  str))))

(defun alt-load-consonants ()
  (setf *consonants* nil)
  (iter
    (for l in (fare-csv:read-csv-file "consonants.csv"))
    (pushnew (make-instance 'consonant
                            :num (parse-integer (nth 0 l))
                            :ipa (nth 1 l)
                            :voicing (symbolize (nth 2 l))
                            :place (symbolize (nth 3 l))
                            :manner (symbolize (nth 4 l))
                            :roman (nth 5 l))
             *consonants* :key #'num :test #'equal)))

(defun alt-load-vowels ()
  (setf *vowels* nil)
  (iter
    (for l in (fare-csv:read-csv-file "vowels.csv"))
    (pushnew (make-instance 'vowel
                            :num (parse-integer (nth 0 l))
                            :ipa (nth 1 l)
                            :height (symbolize (nth 2 l))
                            :backness (symbolize (nth 3 l))
                            :rounding (symbolize (nth 4 l))
                            :roman (nth 5 l))
                *vowels* :key #'num :test #'equal)))

(defun initialize ()
  (let ((*package* (find-package :lang)))
    (alt-load-consonants)
    (alt-load-vowels)
    (setf *safe-consonants* (remove-weird-consonants *consonants*))
    (setf *vowels* (filter-vowel *vowels* :remove t :height '||))
    (setf *phones* (append *vowels* *consonants*))
    (initialize-frequencies)))

(defun flatten (lol) 
  (cond ((null lol) lol)
        ((not (listp (car lol))) (cons (car lol) (flatten (cdr lol)))) 
        (t (append (flatten (car lol)) (flatten (cdr lol))))))

(defun filter-phone (list &rest rest &key base ipa num &allow-other-keys)
  (cond
    ((consonant-p base) (apply #'filter-consonant list rest))
    ((vowel-p base) (apply #'filter-vowel list rest))
    (t (if (or ipa num)
           (iter (for p in list)
             (when (and
                    (or (not ipa)
                        (equal ipa (ipa p))
                        (and (listp ipa) (member (ipa p) ipa :test #'equal)))
                    (or (not num)
                        (equal num (num p))
                        (and (listp num) (member (num p) num :test #'equal))))
               (collect p)))
           nil))))

(defun lookup-phone (&rest rest &key ipa num base &allow-other-keys)
  (declare (ignore ipa num))
  (apply #'filter-phone *phones* rest))

(defun filter-consonant (list &rest r &key base voicing place manner ipa num remove &allow-other-keys)
  (declare (ignore r))
  (let ((v (or voicing (and base (voicing base))))
        (p (or place (and base (place base))))
        (m (or manner (and base (manner base)))))
    (iter (for c in list)
      (if (and (consonant-p c)
               (or (not v) (equal v :any)
                   (equal v (voicing c))
                   (and (listp v) (member (voicing c) v :test #'equal)))
               (or (not p) (equal p :any)
                   (equal p (place c))
                   (and (listp p) (member (place c) p :test #'equal)))
               (or (not m) (equal m :any)
                   (equal m (manner c))
                   (and (listp m) (member (manner c) m :test #'equal)))
               (or (not ipa) (equal ipa (ipa c)))
               (or (not num) (equal num (num c))))
          (unless remove (collect c))
          (when remove (collect c))))))

(defun lookup-consonant (&rest rest &key base voicing place manner ipa num &allow-other-keys)
  (declare (ignore base voicing place manner ipa num))
  (apply #'filter-consonant *consonants* rest))

(defun filter-vowel (list &key base height backness rounding remove ipa num)
  (when (and base (not (vowel-p base))) (return-from filter-vowel nil))
  (let ((v (or height (and base (height base))))
        (p (or backness (and base (backness base))))
        (m (or rounding (and base (rounding base)))))
    (iter (for c in list)
      (if (and (vowel-p c)
               (or (not v) (equal v :any)
                   (equal v (height c))
                   (and (listp v) (member (height c) v :test #'equal)))
                 (or (not p) (equal p :any)
                     (equal p (backness c))
                     (and (listp p) (member (backness c) p :test #'equal)))
                 (or (not m) (equal m :any)
                     (equal m (rounding c))
                     (and (listp m) (member (rounding c) m :test #'equal)))
                 (or (not ipa) (equal ipa (ipa c)))
                 (or (not num) (equal num (num c))))
          (unless remove (collect c))
          (when remove (collect c))))))

(defun lookup-vowel (&rest rest &key base height backness rounding ipa num)
  (declare (ignore base height backness rounding ipa num))
  (apply #'filter-vowel *vowels* rest))


(defmethod describe-phoneme ((v vowel) s)
  (format s "~a - ~a ~a ~a"
            (ipa v) (height v) (backness v) (rounding v)))

(defmethod print-object ((v vowel) s)
  (print-unreadable-object (v s :type t)
    (describe-phoneme v s)))

(defmethod describe-phoneme ((c consonant) s)
  (format s "~a - ~a ~a ~a"
          (ipa c) (voicing c) (place c) (manner c)))

(defmethod print-object ((c consonant) s)
  (print-unreadable-object (c s :type t)
    (describe-phoneme c s)))

(defmethod print-object ((p phone-point) s)
  (print-unreadable-object (p s :type t)
    (format s "~a ~a" (describe-phoneme (phone p) nil) (stress p))))


(defparameter *vowel-sonority*
  '((open . 20)
    (near-open . 19)
    (open-mid . 18)
    (mid . 17)
    (close-mid . 16)
    (near-close . 15)
    (close . 14)))

(defparameter *consonant-sonority*
  '((approximant . 13)
    (flap . 12)
    (tap . 11)
    (lateral-approximant . 10)
    (nasal . 9)
    (trill . 8)))

(defgeneric sonority (p))

(defmethod sonority ((p vowel))
  (cdr (assoc (height p) *vowel-sonority*)))

(defmethod sonority ((c consonant))
  (let ((method (manner c))
        (voiced (equal (voicing c) 'voiced)))
    (or
     (cdr (assoc method *consonant-sonority*))
     (case method
       ((lateral-fricative fricative) (if voiced 7 6))
       (plosive (if voiced 5 4))
       (stop 3)
       ((implosive) 2)
       ((fricative-approximant lateral-flap) 1)
       ((lateral-click click fricative-release) 0)))))


(defun peak (in)
  (labels ((inner (l last max direction)
             (cond
               ((null l) max)
               ((and (> direction 0) (>= (car l) last))
                (inner (cdr l) (car l) (max (car l) max) direction))
               ((and (< direction 0) (<= (car l) last))
                (inner (cdr l) (car l) (max (car l) max) direction))
               ((and (> direction 0) (< (car l) last))
                (inner (cdr l) (car l) (max (car l) max) -1))
               (t nil))))
    (inner in -1 -1 1)))

(defun freq-lookup (item flist)
  (let ((v (cdr (assoc item flist))))
    (or v 1)))

(defun random-letter (letters freqs)
  (when (null letters) (error "random-letter called with empty list"))
  (let ((total (float (iter (for letter in letters)
                        (summing (freq-lookup letter freqs))))))
    (when (<= total 0.0) (return-from random-letter (first letters)))
    (let ((v (random total)))
      (iter
        (for letter in letters)
        (let ((weight (freq-lookup letter freqs)))
          (if (< v weight)
              (return letter)
              (decf v weight)))))))

(defun add-one-using-flist (lang selection flist)
  (remove-duplicates (cons (random-letter selection flist) lang)))

(defun symmetrize (lang full-list)
  (let* ((lang-consonants (iter (for p in lang)
                            (when (consonant-p p) (collect p))))
         (places (iter (for c in lang-consonants)
                   (adjoining (place c) test #'equal)))
         (manners (iter (for c in lang-consonants)
                    (adjoining (manner c) test #'equal))))
    (union
     lang 
     (iter (for manner in manners)
       (unioning
        (let ((voicings
                (iter (for c in (filter-consonant lang-consonants :manner manner))
                  (adjoining (voicing c) test #'equal))))
          (iter (for place in places)
            (unioning
             (iter (for voicing in voicings)
               (unioning (filter-consonant full-list :voicing voicing :manner manner :place place)))))))))))

(defun select-vowels (&optional (n 5))
  (setf n (min n 8))
  (iter (with x = (list (random-letter *vowels* *vowel-frequencies*)))
    (repeat (1- n))
    (setf x
          (cons
           (let* ((rounded (length (filter-vowel x :rounding 'rounded)))
                  (unrounded (- (length x) rounded))
                  (test-letter
                    (cond
                      ((and (< rounded 4) (< unrounded 4))
                       (random-letter *vowels* *vowel-frequencies*))
                      ((>= rounded 4)
                       (random-letter (filter-vowel *vowels* :rounding 'unrounded) *vowel-frequencies*))
                      (t (random-letter (filter-vowel *vowels* :rounding 'rounded) *vowel-frequencies*)))))
             (if (filter-vowel x :base test-letter :backness :any)
                 (random-letter
                  (filter-vowel *vowels* :remove t
                                         :backness :any
                                         :base test-letter
                                         :height (mapcar #'height
                                                         (filter-vowel x :base test-letter
                                                                         :backness :any
                                                                         :height :any)))
                  *vowel-frequencies*)
                 test-letter))
           x))
    (finally (return x))))

(defun filter-vowels-by-height-and-rounding (height-list rounding previous-vowels)
  (let* ((prev (remove nil previous-vowels))
         (base-vowels (if prev
                         (filter-vowel *vowels*
                                     :remove t
                                     :height (mapcar #'height prev))
                         *vowels*)))
    (filter-vowel base-vowels 
                  :rounding rounding
                  :height height-list)))

(defun select-random-vowels (vowel-list n)
  (let ((source (copy-list vowel-list)))
    (iter
     (with result = nil)
     (while (and source (< (length result) n)))
     (let ((selected (random-letter source *vowel-frequencies*)))
       (setf result (adjoin selected result))
       (setf source (set-difference source result)))
     (finally (return result)))))

(defun alt-vowels (&optional (n 5))
  (iter
   (with rounded-vowels = nil)
   (with unrounded-vowels = nil)
   (for height-groups in '((close near-close)
                          (near-close close-mid mid)
                          (mid open-mid near-open)
                          (near-open open)))
   (let ((rounded (random-letter 
                   (filter-vowels-by-height-and-rounding 
                    height-groups 'rounded rounded-vowels)
                   *vowel-frequencies*))
         (unrounded (random-letter 
                     (filter-vowels-by-height-and-rounding 
                      height-groups '(unrounded) unrounded-vowels)
                     *vowel-frequencies*)))
     (setf rounded-vowels (cons rounded rounded-vowels))
     (setf unrounded-vowels (cons unrounded unrounded-vowels)))
   (finally
    (return (remove-duplicates
             (select-random-vowels
              (remove nil (append rounded-vowels unrounded-vowels))
              n))))))


(defun merge-into-zipflist (items freqs)
  (iter (for item in items)
    (when (not (assoc item freqs))
      (setf freqs (cons (cons item (/ 1 (1+ (length freqs)))) freqs)))
    (finally (return freqs))))

(defun initialize-zipflist (items)
  (merge-into-zipflist (scramble-sequence (copy-seq items)) nil))

(defun expand (lang full-list freqs n)
  (if (>= (length lang) n)
      (merge-into-zipflist (symmetrize (mapcar #'car lang) full-list)
                           lang)
      (expand (merge-into-zipflist
               (symmetrize (add-one-using-flist (mapcar #'car lang) full-list freqs)
                           full-list)
               lang)
              full-list freqs n)))

(defclass language ()
  ((name    :accessor lang-name :initarg :name    :initform nil)
   (lexicon :accessor lexicon   :initarg :lexicon :initform nil)
   (grammar :accessor grammar   :initarg :grammar :initform nil)))

(defclass proto-language (language)
  ((consonant-frequencies :accessor consonant-frequencies :initarg :consonant-frequencies)
   (vowel-frequencies :accessor vowel-frequencies :initarg :vowel-frequencies)
   (onset-min :accessor onset-min :initarg :onset-min :initform 1)
   (nucleus-min :accessor nucleus-min :initarg :nucleus-min :initform 1)
   (coda-min :accessor coda-min :initarg :coda-min :initform 1)
   (onset-max :accessor onset-max :initarg :onset-max :initform 1)
   (nucleus-max :accessor nucleus-max :initarg :nucleus-max :initform 1)
   (coda-max :accessor coda-max :initarg :coda-max :initform 1)
   (stressed-syllable :accessor stressed-syllable :initarg :stressed-syllable :initform -2)))

(defclass derived-language (language)
  ((source            :accessor source            :initarg :source)
   (transformers      :accessor transformers      :initarg :transformers)
   (transformer-specs :accessor transformer-specs :initarg :transformer-specs :initform nil)))

(defclass lexical-entry ()
  ((gloss      :accessor gloss      :initarg :gloss)
   (form       :accessor form       :initarg :form)
   (category   :accessor category   :initarg :category)
   (origin     :accessor origin     :initarg :origin :initform :native)
   (domain     :accessor domain     :initarg :domain :initform nil)
   (noun-class :accessor noun-class :initarg :noun-class :initform nil)
   (inflected-forms :accessor inflected-forms :initarg :inflected-forms :initform nil)))

(defclass morpheme-rule ()
  ((feature           :accessor mrule-feature     :initarg :feature)
   (applies-to        :accessor mrule-applies-to  :initarg :applies-to)
   (strategy          :accessor mrule-strategy    :initarg :strategy    :initform :none)
   (marker            :accessor mrule-marker      :initarg :marker      :initform nil)
   (disambig-particle :accessor mrule-disambig    :initarg :disambig-particle :initform nil)))

(defun make-morpheme-rule (&key feature applies-to (strategy :none) marker disambig-particle)
  (make-instance 'morpheme-rule :feature feature :applies-to applies-to
                                :strategy strategy :marker marker
                                :disambig-particle disambig-particle))

(defun copy-morpheme-rule (rule)
  (make-morpheme-rule :feature (mrule-feature rule)
                      :applies-to (mrule-applies-to rule)
                      :strategy (mrule-strategy rule)
                      :marker (copy-list (mrule-marker rule))
                      :disambig-particle (copy-list (mrule-disambig rule))))

(defclass grammar-feature ()
  ((strategy :accessor gfeat-strategy :initarg :strategy :initform nil)
   (marker   :accessor gfeat-marker   :initarg :marker   :initform nil)
   (order    :accessor gfeat-order    :initarg :order    :initform nil)
   (position :accessor gfeat-position :initarg :position :initform nil)))

(defun make-grammar-feature (&key strategy marker order position)
  (make-instance 'grammar-feature :strategy strategy :marker marker
                                   :order order :position position))

(defun copy-grammar-feature (f)
  (make-grammar-feature :strategy (gfeat-strategy f)
                         :marker (copy-list (gfeat-marker f))
                         :order (copy-list (gfeat-order f))
                         :position (gfeat-position f)))

(defmethod ensure-phone ((p phone))
  p)

(defmethod ensure-phone ((n number))
  (find n *phones* :key #'num :test #'eql))

(defmethod ensure-phone ((s string))
  (or (find s *phones* :key #'ipa :test #'equal)
      (find s *phones* :key #'roman :test #'equal)))

(defmethod initialize-instance :after ((lang proto-language) &rest initargs)
  (declare (ignore initargs))
  (setf (consonant-frequencies lang)
        (mapcar #'(lambda (pair) (cons (ensure-phone (car pair))
                                       (cdr pair)))
                (consonant-frequencies lang)))
  (setf (vowel-frequencies lang)
        (mapcar #'(lambda (pair) (cons (ensure-phone (car pair))
                                       (cdr pair)))
                (vowel-frequencies lang))))

(defmethod serialize ((p phone))
  (ipa p))

(defmethod serialize ((n number))
  n)

(defmethod serialize ((s string))
  s)

(defmethod serialize ((s symbol))
  s)

(defmethod serialize ((l list))
  (cons (serialize (first l)) (serialize (rest l))))


(defmethod serialize ((o standard-object))
  (let ((class (class-of o)))
    (cons (class-name class)
          (iter (for slot in (closer-mop:class-slots class))
            (appending (list (first (closer-mop:slot-definition-initargs slot))
                             (serialize (slot-value o (closer-mop:slot-definition-name slot)))))))))

(defmethod lispify-lang ((lang proto-language))
  (list
   'proto-language
   :consonant-frequencies
   (mapcar #'(lambda (pair) (cons (ipa (car pair)) (cdr pair))) (consonant-frequencies lang))
   :vowel-frequencies
   (mapcar #'(lambda (pair) (cons (ipa (car pair)) (cdr pair))) (vowel-frequencies lang))
   :onset-min
   (onset-min lang)
   :onset-max
   (onset-max lang)
   :nucleus-min
   (nucleus-min lang)
   :nucleus-max
   (nucleus-max lang)
   :coda-min
   (coda-min lang)
   :coda-max
   (coda-max lang)))


(defmethod consonants ((lang proto-language))
  (mapcar #'car (consonant-frequencies lang)))

(defmethod vowels ((lang proto-language))
  (mapcar #'car (vowel-frequencies lang)))

(defmethod incompatible-manners (manners)
  (remove-duplicates
   (append
    (list 'flap)
    (when (member 'flap manners :test #'equal) (list 'lateral-approximant 'plosive 'approximant))
    (when (member 'fricative manners :test #'equal) (list 'lateral-fricative))
    (when (member 'lateral-fricative manners :test #'equal) (list 'fricative))
    manners)))

(defparameter *obstruent-manners*
  '(plosive fricative lateral-fricative))

(defun obstruent-p (c)
  (member (manner c) *obstruent-manners* :test #'equal))

(defmethod consonant-sequence ((lang proto-language) min-len max-len)
  (if (= 0 max-len)
      nil
      (let ((len (+ min-len (random (1+ (- max-len min-len))))))
        (iter (with s = nil)
          (repeat len)
          (let* ((manner-filtered
                   (if s (filter-consonant (consonants lang)
                                           :remove t
                                           :manner (incompatible-manners
                                                    (mapcar #'manner s)))
                       (consonants lang)))
                 ;; Adjacent obstruents must agree in voicing
                 (prev (first s))
                 (candidates
                   (if (and prev (obstruent-p prev))
                       (let ((voiced (filter-consonant manner-filtered
                                                       :voicing (voicing prev)))
                             (sonorants (remove-if #'obstruent-p manner-filtered)))
                         (or (append voiced sonorants) manner-filtered))
                       manner-filtered)))
            (setf s (cons (random-letter candidates
                                         (consonant-frequencies lang))
                          s)))
          (finally (return s))))))

(defmethod coda ((lang proto-language))
  (sort (consonant-sequence lang (coda-min lang) (coda-max lang)) #'> :key #'sonority))

(defmethod onset ((lang proto-language))
  (sort (consonant-sequence lang (onset-min lang) (onset-max lang)) #'< :key #'sonority))

(defmethod nucleus  ((lang proto-language))
  (remove-duplicates
   (iter (repeat (+ (nucleus-min lang) (random (1+ (- (nucleus-max lang) (nucleus-min lang))))))
     (collect (random-letter (vowels lang) (vowel-frequencies lang))))))

(defun deconstructed-syllable (language)
  (let ((s (list (onset language) (nucleus language) (coda language))))
    (if (not (and (null (first s)) (null (third s))))
        s
        (deconstructed-syllable language))))

(defmethod syllable ((lang proto-language))
  (mapcar #'ensure-phone-point (flatten (deconstructed-syllable lang))))

(defmethod word ((lang proto-language) n)
  (let ((w (iter (repeat n) (collecting (syllable lang)))))
    (add-special-markers (set-stress w (stressed-syllable lang)))))


(defun language-helper (consonants &key name (num-vowels 5)
                                     (coda-min 1) (onset-min 1) (nucleus-min 1)
                                     (coda-max 1) (onset-max 1) (nucleus-max 1))
  (make-instance 'proto-language :name name
                                 :consonant-frequencies consonants
                                 :vowel-frequencies (initialize-zipflist (alt-vowels num-vowels))
                                 :coda-min coda-min
                                 :onset-min onset-min
                                 :nucleus-min nucleus-min
                                 :coda-max coda-max
                                 :onset-max onset-max
                                 :nucleus-max nucleus-max))

(defun elvish (&optional (n 12) &key name)
  (language-helper
   (expand (initialize-zipflist
            (filter-consonant *safe-consonants* :place '(labiodental alveolar)
                                                :manner '(lateral-approximant fricative)))
           (filter-consonant *safe-consonants* :remove t :place '(uvular velar))
           *consonant-frequencies* n)
   :name name))

(defun gnomish (&optional (n 12) &key name)
  (language-helper
   (expand (initialize-zipflist
            (filter-consonant *safe-consonants* :manner '(approximant nasal)
                                                :place '(palatal alveolar bilabial)))
           *safe-consonants*
           *consonant-frequencies*
           n)
   :name name))

(defun halfling (&optional (n 12) &key name)
  (language-helper
   (expand
    (initialize-zipflist
     (union (filter-consonant *safe-consonants* :manner '(plosive)
                                                :place '(bilabial alveolar))
            (union
             (filter-consonant *safe-consonants* :manner 'lateral-approximant
                                                 :place 'alveolar)
             (union
              (filter-consonant *safe-consonants* :manner 'nasal
                                                  :place 'velar)
              (filter-consonant *safe-consonants* :manner 'fricative
                                                  :place 'epiglottal
                                                  :voicing 'voiceless)))))
    *safe-consonants*
    *consonant-frequencies*
    n)
   :name name))

(defun orcish (&optional (n 12) &key name)
  (language-helper
   (expand (initialize-zipflist
            (union (filter-consonant *safe-consonants* :manner '(plosive)
                                                       :place '(velar)
                                                       :voicing 'voiceless)
                   (union
                    (filter-consonant *safe-consonants* :manner 'approximant
                                                        :place 'alveolar)
                    (filter-consonant *safe-consonants* :manner 'fricative
                                                        :place 'epiglottal
                                                        :voicing 'voiceless))))
           (set-difference
            *safe-consonants*
            (union (filter-consonant *safe-consonants* :place '(bilabial labiodental labial-velar))
                   (filter-consonant *safe-consonants* :manner 'fricative :voicing 'voiced)))
           *consonant-frequencies* n)
   :name name
   :onset-max 0
   :coda-max 2))

(defun dwarvish (&optional (n 12) &key name)
  (language-helper
   (expand
    (initialize-zipflist
     (filter-consonant *safe-consonants* :place '(uvular palatal velar)
                                         :manner '(plosive fricative)))
    *safe-consonants*
    *consonant-frequencies*
    n)
   :name name
   :coda-max 2
   :onset-max 2))

(defun goblin (&optional (n 12) &key name)
  (language-helper
   (expand
    (initialize-zipflist
     (filter-consonant *safe-consonants* :place '(bilabial velar alveolar)
                                         :manner '(plosive nasal lateral-approximant approximant)
                                         :voicing 'voiced))
    (filter-consonant (filter-consonant *safe-consonants* :remove t :manner '(fricative lateral-fricative))
                      :remove t :voicing 'voiceless)
    *consonant-frequencies*
    n)
   :name name))

(defun ccv-test (&optional (n 12) &key name)
  (language-helper
   (expand nil
           *consonants*
           *consonant-frequencies*
           n)
   :name name
   :onset-max 2
   :coda-max 0))

(defun max-variety-test (&optional (n 12) &key name)
  (language-helper
   (expand nil
           *consonants*
           *consonant-frequencies*
           n)
   :name name
   :onset-min 0
   :onset-max 2
   :coda-min 0
   :coda-max 2))

(define-condition no-match (condition) ())

(define-condition no-replacement (condition)
  ((transfomer :accessor transformer :initarg :transformer)
   (phone :accessor phone :initarg :phone)))

(define-condition skip-phone (condition)
  ())

(define-condition skip-clause (condition)
  ())

(defmethod ensure-phone-point ((p phone-point) &key stress)
  (if stress
      (make-instance 'phone-point :phone (phone p) :stress stress)
      p))

(defmethod ensure-phone-point ((p phone) &key (stress 'unstressed))
  (make-instance 'phone-point :phone p :stress stress))

(defmethod ensure-phone-point ((s string) &key (stress 'unstressed))
  (make-instance 'phone-point :phone (ensure-phone s) :stress stress))

(defmethod ensure-phone-point ((l (eql nil)) &key (stress 'unstressed))
  nil)

(defmethod C ((v vowel) tests transforms)
  (signal 'no-match))

(defmethod C ((i inter-phone-marker) tests transforms)
  (declare (ignore i tests transforms))
  (signal 'skip-phone))

(labels ((lift (f p tests transforms)
           (ensure-phone-point (funcall f (phone p) tests transforms))))
  (defmethod C ((p phone-point) tests transforms)
    (lift #'C p tests transforms)))


(defun char-transform (p filter-func lookup-func tests transforms)
  (let ((match (apply filter-func (list p) tests)))
    (cond
      ((not match) (signal 'no-match))
      ((equal transforms '(())) nil)
      ((stringp (first transforms))
       (let ((replacement (lookup-phone :ipa (first transforms))))
         (if replacement (random-letter replacement (zipf replacement))
             (signal 'no-replacement :phone p :transformer (list 'V tests transforms)))))
      (t (let ((replacement (apply lookup-func :base p transforms)))
           (if replacement (random-letter replacement (uniform replacement))
               (signal 'no-replacement :phone p :transformer (list 'V tests transforms))))))))

(defmethod C ((c consonant) tests transforms)
  (char-transform c #'filter-consonant #'lookup-consonant tests transforms))

(defmethod V ((v vowel) tests transforms)
  (char-transform v #'filter-vowel #'lookup-vowel tests transforms))

(defmethod V ((c consonant) tests transforms)
  (signal 'no-match))

(defmethod V ((i inter-phone-marker) tests transforms)
  (declare (ignore i tests transforms))
  (signal 'skip-phone))

(defmethod pho ((p phone) tests transforms)
  (char-transform p #'filter-phone #'lookup-phone tests transforms))

(defmethod pho ((i inter-phone-marker) tests transforms)
  (declare (ignore i tests transforms))
  (signal 'skip-phone))

(defmethod phovec ((p phone) tests transforms)
  (iter
    (for pp in-vector (first tests))
    (for r in-vector (first transforms))
    (when (equal (ipa p) pp)
      (return (random-letter (lookup-phone :ipa r) (zipf r))))
    (finally (signal 'no-match))))

(defmethod phovec ((i inter-phone-marker) tests transforms)
  (declare (ignore i tests transforms))
  (signal 'skip-phone))

(defmethod stress-match ((p phone-point) &rest r &key stress &allow-other-keys )
  (declare (ignore r))
  (or (not stress) (and (equal stress 'stressed)
                        (member (stress p) (list 'stressed 'primary-stress 'secondary-stress)))
      (equal stress (stress p))))

(macrolet ((fix (fname)
             `(defmethod ,fname ((p phone-point) tests transforms)
                (if (apply #'stress-match p tests)
                    (ensure-phone-point (funcall (function ,fname) (phone p) tests transforms)
                                        :stress (stress p))
                    (signal 'no-match)))))
  (fix C)
  (fix V)
  (fix pho)
  (fix phovec))

(macrolet ((fix (fname)
             `(defmethod ,fname ((p phone-point))
                (,fname (phone p)))))
  (fix sonority)
  (fix roman)
  (fix ipa)
  (fix anglicize))

(defmethod roman ((m inter-phone-marker))
  "")

(defmethod ipa ((m inter-phone-marker))
  "")

(defmethod anglicize ((m inter-phone-marker))
  "")

(defmethod anglicize ((c consonant))
  "Map a consonant to an English-friendly spelling. Collapses places English
doesn't distinguish (uvular→velar, retroflex→alveolar, palatal→postalveolar)
and uses familiar digraphs."
  (let ((manner (manner c))
        (place (place c))
        (voiced (equal (voicing c) 'voiced)))
    ;; Normalize place: collapse exotic places to nearest English equivalent
    (let ((p (case place
               ((uvular velar) 'velar)
               ((palatal postalveolar) 'postalveolar)
               ((retroflex alveolar) 'alveolar)
               (dental 'dental)
               (bilabial 'bilabial)
               (labiodental 'labiodental)
               ((glottal epiglottal pharyngeal) 'glottal)
               (labial-velar 'labial-velar)
               (t 'alveolar))))
      (case manner
        (plosive
         (case p
           (bilabial    (if voiced "b" "p"))
           (alveolar    (if voiced "d" "t"))
           (velar       (if voiced "g" "k"))
           (glottal     "'")
           (t           (if voiced "d" "t"))))
        (nasal
         (case p
           (bilabial    "m")
           (alveolar    "n")
           (velar       "ng")
           (t           "n")))
        (fricative
         (case p
           (bilabial    (if voiced "v" "f"))
           (labiodental (if voiced "v" "f"))
           (dental      (if voiced "dh" "th"))
           (alveolar    (if voiced "z" "s"))
           (postalveolar (if voiced "zh" "sh"))
           (velar       "h")
           (glottal     "h")
           (labial-velar "wh")
           (t           (if voiced "z" "s"))))
        (lateral-fricative
         (if voiced "lz" "lh"))
        (lateral-approximant "l")
        (approximant
         (case p
           (bilabial     "w")
           (labiodental  "w")
           (alveolar     "r")
           (postalveolar "y")
           (velar        "y")
           (labial-velar "w")
           (t            "r")))
        ((trill tap flap)
         "r")
        (t "?")))))

(defmethod anglicize ((v vowel))
  "Map a vowel to an English-friendly spelling."
  (let ((height (height v))
        (backness (backness v))
        (rounded (equal (rounding v) 'rounded)))
    (case height
      ((close near-close)
       (cond
         ((and (member backness '(back near-back)) rounded) "u")
         ((member backness '(front near-front)) (if rounded "u" "i"))
         (t (if rounded "u" "i"))))
      ((close-mid mid)
       (cond
         ((member backness '(back near-back)) (if rounded "o" "e"))
         ((member backness '(front near-front)) (if rounded "o" "e"))
         (t "e")))
      ((open-mid near-open)
       (cond
         ((member backness '(back near-back)) (if rounded "o" "a"))
         ((member backness '(front near-front)) "e")
         (t "a")))
      ((open)
       (if (member backness '(back near-back)) "a" "a"))
      (t "a"))))

(defun parse-arg (arg)
  (let ((f (first arg))
        (arrow-pos (position '-> arg)))
    (cond
      ((and (vectorp f) (vectorp (elt arg (1+ arrow-pos))))
       (list #'phovec (subseq arg 0 arrow-pos) (subseq arg (1+ arrow-pos))))
      ((stringp f)
       (if arrow-pos
           (list #'pho (list :ipa f) (subseq arg (1+ arrow-pos)))
           (list #'pho (list :ipa f) nil)))
      ((and (null f) (eql arrow-pos 1))
       (list nil nil (subseq arg 2)))
      ((symbolp f)
       (if arrow-pos
           (list (symbol-function f) (subseq arg 1 arrow-pos) (subseq arg (1+ arrow-pos)))
           (list (symbol-function f) (rest arg) nil))))))

(defun parse-transformer (args)
  (iter (for arg in args)
    (collecting
     (cond
       ((stringp arg)
        (list #'pho (list :ipa arg) nil))
       ((symbolp arg) (list (symbol-function arg) nil nil))
       ((listp arg) (parse-arg arg))))))


(defmethod pho-insert ((i inter-phone-marker) tests transforms)
  (mapcar #'ensure-phone transforms))

(defmethod pho-insert ((p phone) tests transforms)
  (signal 'no-match))


(defun build-word (p word)
  (if (phone-p p)
      (cons p word)
      word))

(defun apply-transformer-recursive-inner (transformer word)
  (cond
    ((null transformer) word)
    ((null word) (signal 'no-match))
    (t (let ((clause (first transformer))
             (phone (first word)))
         (handler-case
             (if (null (first clause))
                 (append (apply #'pho-insert phone (rest clause))
                         (apply-transformer-recursive-inner (rest transformer) (rest word)))
                 (cons (apply (first clause) phone (rest clause))
                       (apply-transformer-recursive-inner (rest transformer) (rest word))))
           (skip-phone (sig)
             (declare (ignore sig))
             (build-word (first word) (apply-transformer-recursive-inner transformer (rest word))))
           (skip-clause (sig)
             (declare (ignore sig))
             (apply-transformer-recursive-inner (rest transformer) word)))))))

(defun apply-transformer-recursive (transformer word)
  (cond
    ((null transformer) word)
    ((null word) word)
    (t (handler-case
           (let ((result (apply-transformer-recursive-inner transformer word)))
             (build-word (first result)
                         (apply-transformer-recursive transformer (remove nil (rest result)))))
         (no-match (sig)
           (declare (ignore sig))
           (build-word (first word)
                       (apply-transformer-recursive transformer (rest word))))
         (no-replacement (sig)
           (let ((key (format nil "~a ~a" (transformer sig) (phone sig))))
             (unless (gethash key *no-replacement-seen*)
               (format t "no replacement: ~a~%" key)
               (setf (gethash key *no-replacement-seen*) t)))
           (build-word (first word)
                       (apply-transformer-recursive transformer (rest word))))))))

(defun add-special-markers (word)
  (let ((insertion-point (make-instance 'inter-phone-marker :boundary-type 'phone))
        (syllable-boundary (make-instance 'inter-phone-marker :boundary-type 'syllable))
        (word-boundary (make-instance 'inter-phone-marker :boundary-type 'word)))
   (labels ((recur-syllable (syl)
              (if (or (null syl) (null (rest syl)))
                  syl
                  (append (list (first syl) insertion-point)
                          (recur-syllable (rest syl)))))
            (recur-word (word)
              (cond
                ((null word) (list word-boundary))
                ((null (rest word)) (append (recur-syllable (first word)) (list word-boundary)))
                (t (append
                    (recur-syllable (first word))
                    (list syllable-boundary)
                    (recur-word (rest word)))))))
     (cons word-boundary (recur-word word)))))


(defun romanize (word)
  (format nil "~{~a~}" (mapcar #'roman (flatten word))))

(defun ipa-string-to-phones (str)
  (mapcar #'ensure-phone (iter (for c in-string str) (collect (format nil "~a" c)))))

(defun ipa-syllable (syl)
  (format nil "~{~a~}" (mapcar #'ipa (flatten syl))))

(defun print-word (word)
  (format nil "~{~a~} [~{~a~^.~}]"
          (mapcar #'roman word)
          (mapcar #'ipa-syllable (syllabify word))))

(defun alt-print-word (word &optional (s nil) (f #'ipa))
  (let* ((suppress-markers (or (eql f #'anglicize) (eql f #'roman)))
         (capitalize-stress (and suppress-markers
                                 (find-if (lambda (pp)
                                            (and (typep pp 'inter-phone-marker)
                                                 (equal (boundary-type pp) 'syllable)))
                                          word)))
         (str (with-output-to-string (str)
                (iter
                  (with stress = nil)
                  (for pp in word)
                  (cond ((phone-p pp)
                         (setf stress (equal (stress pp) 'primary-stress))
                         (let ((text (funcall f pp)))
                           (format str "~a" (if (and capitalize-stress stress)
                                                (string-upcase text) text))))
                        ((and (typep pp 'inter-phone-marker)
                              (or (equal (boundary-type pp) 'syllable)
                                  (and stress (equal (boundary-type pp) 'word))))
                         (unless suppress-markers
                           (format str "~:[.~;'~]" stress))))))))
    (if s (princ str s) str)))

(defun syllabify (word)
  (labels ((recur (full-syllables partial-syllable rest-of-word)
             (let ((a (first partial-syllable))
                   (b (first rest-of-word))
                   (c (second rest-of-word)) 
                   (d (third rest-of-word)))
              (cond
                ((null rest-of-word)
                 (reverse (cons (reverse partial-syllable) full-syllables)))
                ((null partial-syllable)
                 (recur full-syllables
                        (cons b partial-syllable)
                        (rest rest-of-word)))
                ((and (and a b c d)
                      (> (sonority a) (sonority b))
                      (> (sonority a) (sonority c))
                      (> (sonority d) (sonority b))
                      (> (sonority d) (sonority c)))
                 (recur (cons (reverse (cons b partial-syllable))
                              full-syllables)
                        nil
                        (rest rest-of-word)))
                ((and (<= (sonority b)
                          (sonority a))
                      (not (null c))
                      (> (sonority c)
                         (sonority b)))
                 (recur (cons (reverse partial-syllable)
                              full-syllables)
                        (list b)
                        (rest rest-of-word)))
                (t
                 (recur full-syllables
                        (cons b partial-syllable)
                        (rest rest-of-word)))))))
    (recur nil nil word)))

(defun fix-stress (word)
  (let ((peak (iter (for syl in word)
                (finding (iter (for phone in syl)
                           (when (equal (stress phone) 'primary-stress)
                             (finding phone maximizing (sonority phone))))
                         maximizing #'(lambda (x) (or (and x (sonority x)) -1))))))
    (iter (for syl in word)
      (collect
          (if (member peak syl)
              (mapcar #'(lambda (p) (ensure-phone-point p :stress 'primary-stress)) syl)
              (mapcar #'(lambda (p) (ensure-phone-point p :stress 'unstressed)) syl))))))

(defun set-stress (word n)
  (let ((num (cond
               ((> n (length word)) (length word))
               ((> (- n) (length word)) 1)
               ((> n 0) n)
               ((< n 0) (+ 1 (length word) n))
               (t (floor (/ (length word) 2))))))
   (iter (for syl in word)
     (for i upfrom 1)
     (collect (if (= i num)
                  (mapcar #'(lambda (p) (ensure-phone-point p :stress 'primary-stress)) syl)
                  (mapcar #'(lambda (p) (ensure-phone-point p :stress 'unstressed)) syl))))))

(defun reanalyze (word)
  (add-special-markers (fix-stress (syllabify (mapcar #'ensure-phone-point (flatten word))))))

(defun evolve (transformers word)
  (if (null transformers)
      word
      (evolve (rest transformers)
              (reanalyze
               (apply-transformer-recursive (first transformers) word)))))

;;; Lexicon

(defparameter *derived-suffixes*
  '(("-ly" . :adverbialize) ("-er" . :agentive)
    ("-y" . :adj-from-noun) ("-ness" . :noun-from-adj))
  "Map gloss suffixes to derivation types for fallback lookup.")

(defun parse-derived-gloss (gloss)
  "If GLOSS looks like a derived form (e.g. \"hunt-er\"), return (base . derivation-type).
   Returns NIL if not a recognized derived pattern."
  (iter (for (suffix . type) in *derived-suffixes*)
    (when (and (> (length gloss) (length suffix))
               (string= gloss suffix :start1 (- (length gloss) (length suffix))))
      (return (cons (subseq gloss 0 (- (length gloss) (length suffix)))
                    type)))))

(defun lookup-word (language gloss)
  (or (find gloss (lexicon language) :key #'gloss :test #'equal)
      ;; Fallback: if this is a derived gloss and the derivation type is :none
      ;; (unproductive in this language), return the base word (zero derivation).
      (let ((parsed (parse-derived-gloss gloss)))
        (when parsed
          (find (car parsed) (lexicon language) :key #'gloss :test #'equal)))))

(defun min-lexicon-distance (word lexicon)
  (if (null lexicon)
      most-positive-fixnum
      (iter (for entry in lexicon)
        (minimizing (loanword-similarity-max-consonants
                     (strip-markers word) (strip-markers (form entry)))))))

(defun collect-all-markers (language)
  "Collect all marker phone-lists from morphology rules and grammar features."
  (let ((markers nil))
    (iter (for rule in (gfeature language :morphology))
      (when (mrule-marker rule)
        (push (deserialize-form (mrule-marker rule)) markers))
      (when (mrule-disambig rule)
        (push (deserialize-form (mrule-disambig rule)) markers)))
    (iter (for key in '(:copula-marker :conjunction-marker :topic-particle
                        :question-particle :disjunction-marker :reflexive-marker
                        :reciprocal-marker :nominalization-marker :distributive-marker
                        :exclamation-particle :simile-marker :compound-linker))
      (let ((m (gfeature language key)))
        (when m (push (deserialize-form m) markers))))
    markers))

(defun min-phones-distance (word phone-lists)
  "Minimum loanword-similarity distance from WORD to a list of phone-lists."
  (if (null phone-lists)
      most-positive-fixnum
      (iter (for phones in phone-lists)
        (minimizing (loanword-similarity-max-consonants
                     (strip-markers word) phones)))))

(defmethod define-word ((lang proto-language) gloss category &key (syllables 1) (min-distance nil) (max-attempts 100) domain semantic-tags)
  (let* ((existing (lexicon lang))
         (all-markers (collect-all-markers lang))
         (w (block found
              (iter (for syls from syllables to (+ syllables 4))
                (let ((effective-distance (or min-distance (if (<= syls 1) 3 10))))
                  (iter (repeat max-attempts)
                    (for candidate = (word lang syls))
                    (when (and (>= (min-lexicon-distance candidate existing) effective-distance)
                               (>= (min-phones-distance candidate all-markers) effective-distance))
                      (return-from found candidate)))))
              (word lang (+ syllables 5))))
         (scheme (gfeature lang :noun-class-scheme))
         (classes (gfeature lang :noun-classes))
         (nc (when (and (eql category 'noun) scheme classes)
               (assign-noun-class semantic-tags scheme (car (last classes)))))
         (entry (make-instance 'lexical-entry
                               :gloss gloss
                               :form w
                               :category category
                               :origin :native
                               :domain domain
                               :noun-class nc)))
    (push entry (lexicon lang))
    entry))

(defun generate-lexicon (proto-language word-specs)
  (let ((sorted (sort (copy-list word-specs) #'< :key #'third)))
    (iter (for spec in sorted)
      (define-word proto-language (first spec) (second spec)
        :domain (fourth spec)
        :semantic-tags (nthcdr 4 spec)))))

(defun collect-derivation-chain (lang)
  (labels ((recur (l acc)
             (if (typep l 'derived-language)
                 (recur (source l) (append (transformers l) acc))
                 (values l acc))))
    (recur lang nil)))

(defun derive-language (parent transformer-specs &key name)
  (let* ((transformers (mapcar #'list (parse-transformer transformer-specs)))
         (derived (make-instance 'derived-language
                                 :name name
                                 :source parent
                                 :transformers transformers
                                 :transformer-specs transformer-specs)))
    (setf (lexicon derived)
          (iter (for entry in (lexicon parent))
            (collect (make-instance 'lexical-entry
                                    :gloss (gloss entry)
                                    :form (evolve transformers (form entry))
                                    :category (category entry)
                                    :origin (cons :inherited (lang-name parent))
                                    :domain (domain entry)
                                    :noun-class (noun-class entry)
                                    :inflected-forms
                                    (mapcar (lambda (pair)
                                              (cons (car pair)
                                                    (evolve transformers (cdr pair))))
                                            (inflected-forms entry))))))
    (when (grammar parent)
      (setf (grammar derived)
            (evolve-grammar (grammar parent) transformers))
      ;; Regenerate paradigms — strategy drift may have created new bound morphemes
      (generate-paradigms derived)
      ;; Disambiguate marker collisions, regenerate if needed
      (when (disambiguate-markers derived)
        (generate-paradigms derived)))
    derived))

;;; Save/Restore

(defun serialize-form (form)
  (mapcar #'ipa (remove-if-not #'phone-p form)))

(defun deserialize-form (ipa-list)
  (reanalyze (mapcar #'ensure-phone ipa-list)))

(defun serialize-inflected-forms (inflected-forms)
  (mapcar (lambda (pair)
            (cons (car pair) (serialize-form (cdr pair))))
          inflected-forms))

(defun deserialize-inflected-forms (data)
  (mapcar (lambda (pair)
            (cons (car pair) (deserialize-form (cdr pair))))
          data))

(defun serialize-lexicon (lexicon)
  (iter (for entry in lexicon)
    (collect (list :gloss (gloss entry)
                   :category (category entry)
                   :origin (origin entry)
                   :form (serialize-form (form entry))
                   :noun-class (noun-class entry)
                   :inflected-forms (serialize-inflected-forms
                                     (inflected-forms entry))))))

(defun deserialize-lexicon (data)
  (iter (for plist in data)
    (collect (make-instance 'lexical-entry
                            :gloss (getf plist :gloss)
                            :category (getf plist :category)
                            :origin (getf plist :origin)
                            :form (deserialize-form (getf plist :form))
                            :noun-class (getf plist :noun-class)
                            :inflected-forms (deserialize-inflected-forms
                                              (getf plist :inflected-forms))))))

(defun serialize-grammar (grammar)
  (when grammar
    (let ((result (copy-list grammar)))
      (setf (getf result :morphology)
            (mapcar (lambda (rule)
                      (list :feature (mrule-feature rule)
                            :applies-to (mrule-applies-to rule)
                            :strategy (mrule-strategy rule)
                            :marker (mrule-marker rule)
                            :disambig-particle (mrule-disambig rule)))
                    (getf grammar :morphology)))
      result)))

(defmethod serialize-language ((lang proto-language))
  (list :proto-language
        :name (lang-name lang)
        :consonant-frequencies
        (mapcar #'(lambda (pair) (cons (ipa (car pair)) (cdr pair)))
                (consonant-frequencies lang))
        :vowel-frequencies
        (mapcar #'(lambda (pair) (cons (ipa (car pair)) (cdr pair)))
                (vowel-frequencies lang))
        :onset-min (onset-min lang)
        :onset-max (onset-max lang)
        :nucleus-min (nucleus-min lang)
        :nucleus-max (nucleus-max lang)
        :coda-min (coda-min lang)
        :coda-max (coda-max lang)
        :stressed-syllable (stressed-syllable lang)
        :lexicon (serialize-lexicon (lexicon lang))
        :grammar (serialize-grammar (grammar lang))))

(defmethod serialize-language ((lang derived-language))
  (list :derived-language
        :name (lang-name lang)
        :source (lang-name (source lang))
        :transformer-specs (transformer-specs lang)
        :lexicon (serialize-lexicon (lexicon lang))
        :grammar (serialize-grammar (grammar lang))))

(defun save-world (languages filename)
  (with-open-file (s filename :direction :output :if-exists :supersede)
    (write (mapcar #'serialize-language languages) :stream s :readably t :pretty t)))

(defun deserialize-proto-language (plist)
  (let ((lang (make-instance 'proto-language
                              :name (getf plist :name)
                              :consonant-frequencies (getf plist :consonant-frequencies)
                              :vowel-frequencies (getf plist :vowel-frequencies)
                              :onset-min (getf plist :onset-min)
                              :onset-max (getf plist :onset-max)
                              :nucleus-min (getf plist :nucleus-min)
                              :nucleus-max (getf plist :nucleus-max)
                              :coda-min (getf plist :coda-min)
                              :coda-max (getf plist :coda-max)
                              :stressed-syllable (getf plist :stressed-syllable))))
    (setf (lexicon lang) (deserialize-lexicon (getf plist :lexicon)))
    (setf (grammar lang) (deserialize-grammar (getf plist :grammar)))
    lang))

(defun deserialize-grammar (grammar)
  "Convert deserialized grammar plist: morphology rule plists → morpheme-rule objects."
  (when grammar
    (let ((result (copy-list grammar)))
      (setf (getf result :morphology)
            (mapcar (lambda (rule-plist)
                      (make-morpheme-rule :feature (getf rule-plist :feature)
                                          :applies-to (getf rule-plist :applies-to)
                                          :strategy (getf rule-plist :strategy)
                                          :marker (getf rule-plist :marker)
                                          :disambig-particle (getf rule-plist :disambig-particle)))
                    (getf grammar :morphology)))
      result)))

(defun deserialize-derived-language (plist loaded-langs)
  (let* ((source-name (getf plist :source))
         (source (find source-name loaded-langs :key #'lang-name :test #'equal))
         (transformer-specs (getf plist :transformer-specs))
         (lang (make-instance 'derived-language
                               :name (getf plist :name)
                               :source source
                               :transformers (parse-transformer transformer-specs)
                               :transformer-specs transformer-specs)))
    (setf (lexicon lang) (deserialize-lexicon (getf plist :lexicon)))
    (setf (grammar lang) (deserialize-grammar (getf plist :grammar)))
    lang))

(defun load-world (filename)
  (let* ((data (with-open-file (s filename :direction :input)
                 (read s)))
         (protos (remove-if-not #'(lambda (d) (eql (first d) :proto-language)) data))
         (derived (remove-if-not #'(lambda (d) (eql (first d) :derived-language)) data))
         (loaded (mapcar #'(lambda (d) (deserialize-proto-language (rest d))) protos)))
    (iter (for d in derived)
      (let ((lang (deserialize-derived-language (rest d) loaded)))
        (setf loaded (append loaded (list lang)))))
    loaded))

;;; Word derivation

(defparameter *agentive-verbs*
  '("hunt" "fight" "rule" "lead" "kill" "sing" "speak" "make"
    "trade" "sail" "write" "serve"))

(defparameter *adjectivizable-nouns*
  '("blood" "fire" "stone" "iron" "gold" "dust" "ash"))

(defparameter *nominalizable-adjs*
  '("wise" "strong" "brave" "dark" "good" "true" "weak" "blind"
    "free" "proud" "rich" "safe"))

(defun derived-gloss (base-gloss derivation-type)
  (case derivation-type
    (:adverbialize  (format nil "~a-ly" base-gloss))
    (:agentive      (format nil "~a-er" base-gloss))
    (:adj-from-noun (format nil "~a-y" base-gloss))
    (:noun-from-adj (format nil "~a-ness" base-gloss))))

(defun category-keyword (category)
  "Convert a category symbol to a keyword for morphology rule lookup."
  (intern (symbol-name category) :keyword))

(defun derive-word (language base-gloss derived-gloss derived-category derivation-feature)
  "Create a derived lexical entry by applying a derivational rule to an existing word."
  (let* ((base-entry (lookup-word language base-gloss))
         (rule (when base-entry
                 (find-morpheme-rule language derivation-feature
                                    (category-keyword (category base-entry))))))
    (when (and base-entry rule (not (eql (mrule-strategy rule) :none)))
      (let* ((strategy (mrule-strategy rule))
             ;; For particle strategies, store just the base form in the lexicon.
             ;; The particle lives in the grammar and is inserted at render time.
             ;; For affixes, fuse into a single derived form.
             (derived-form (if (member strategy '(:particle-before :particle-after))
                               (form base-entry)
                               (first (inflect (form base-entry) rule))))
             ;; Assign noun class to derived nouns
             (scheme (gfeature language :noun-class-scheme))
             (classes (gfeature language :noun-classes))
             (nc (when (and (eql derived-category 'noun) scheme classes)
                   (case derivation-feature
                     (:agentive (assign-noun-class '(:animate :human)
                                                   scheme (car (last classes))))
                     (:noun-from-adj (assign-noun-class '(:abstract)
                                                        scheme (car (last classes))))
                     (t (car (last classes))))))
             (entry (make-instance 'lexical-entry
                                   :gloss derived-gloss
                                   :form derived-form
                                   :category derived-category
                                   :origin (list :derived-from base-gloss
                                                 derivation-feature)
                                   :noun-class nc)))
        (push entry (lexicon language))
        entry))))

(defun entry-words (language entry)
  "Return the list of word-forms for a lexical entry.
   For particle-derived entries, expands the derivation particle from the grammar."
  (if (and (listp (origin entry))
           (eql (first (origin entry)) :derived-from))
      (let* ((feature (third (origin entry)))
             (rule (find-if (lambda (r) (eql (mrule-feature r) feature))
                            (gfeature language :morphology))))
        (if (and rule (member (mrule-strategy rule)
                              '(:particle-before :particle-after)))
            (inflect (form entry) rule)
            (list (form entry))))
      (list (form entry))))

(defun generate-derived-lexicon (language)
  "Generate derived words from existing lexicon entries using derivational morphology."
  ;; All adjectives → adverbs (productive)
  (iter (for entry in (copy-list (lexicon language)))
    (when (eql (category entry) 'adjective)
      (derive-word language (gloss entry)
                   (derived-gloss (gloss entry) :adverbialize)
                   'adverb :adverbialize)))
  ;; Selected verbs → agent nouns
  (iter (for base-gloss in *agentive-verbs*)
    (when (lookup-word language base-gloss)
      (derive-word language base-gloss
                   (derived-gloss base-gloss :agentive)
                   'noun :agentive)))
  ;; Selected nouns → adjectives
  (iter (for base-gloss in *adjectivizable-nouns*)
    (when (lookup-word language base-gloss)
      (derive-word language base-gloss
                   (derived-gloss base-gloss :adj-from-noun)
                   'adjective :adj-from-noun)))
  ;; Selected adjectives → abstract nouns
  (iter (for base-gloss in *nominalizable-adjs*)
    (when (lookup-word language base-gloss)
      (derive-word language base-gloss
                   (derived-gloss base-gloss :noun-from-adj)
                   'noun :noun-from-adj))))

;;; Compound words

(defun generate-compound-entry (language modifier-gloss head-gloss compound-gloss)
  "Create a compound lexical entry by fusing two existing words."
  (let* ((strategy (gfeature language :compound-strategy))
         (order (gfeature language :compound-order))
         (head-entry (lookup-word language head-gloss))
         (mod-entry (lookup-word language modifier-gloss))
         (head-form (when head-entry (strip-markers (form head-entry))))
         (mod-form (when mod-entry (strip-markers (form mod-entry)))))
    (when (and head-form mod-form)
      (let* ((ordered (if (eql order :head-final)
                          (list mod-form head-form)
                          (list head-form mod-form)))
             (form (case strategy
                     (:juxtapose
                      (reanalyze (append (first ordered) (second ordered))))
                     (:linking
                      (let ((linker (mapcar #'ensure-phone
                                            (gfeature language :compound-linker))))
                        (reanalyze (append (first ordered) linker
                                           (second ordered)))))
                     (:genitive
                      (let* ((gen-rule (find-morpheme-rule language :genitive :noun))
                             (mod-inflected (if gen-rule
                                               (strip-markers
                                                (first (inflect
                                                        (reanalyze mod-form)
                                                        gen-rule)))
                                               mod-form)))
                        (reanalyze (if (eql order :head-final)
                                       (append mod-inflected head-form)
                                       (append head-form mod-inflected)))))))
             (entry (make-instance 'lexical-entry
                                   :gloss compound-gloss
                                   :form form
                                   :category 'noun
                                   :origin (list :compound modifier-gloss
                                                 head-gloss)
                                   :noun-class (when head-entry
                                                 (noun-class head-entry)))))
        (push entry (lexicon language))
        entry))))

(defun generate-compound-lexicon (language compound-specs)
  "Generate compound entries from specs of the form (compound-gloss modifier head)."
  (iter (for spec in compound-specs)
    (generate-compound-entry language (first spec) (second spec) (third spec))))

;;; Compound derivations — replace a word's root with a compound of simpler words

(defun apply-compound-derivations (language derivation-specs &key (probability 0.35))
  "For each derivation spec, with PROBABILITY chance, replace the target word's
   root form with a compound of two simpler words.  Specs are:
     (target-gloss (modifier head) (modifier head) ...)
   Skips gracefully if the target or either component word is missing from the lexicon."
  (iter (for spec in derivation-specs)
    (let* ((target-gloss (first spec))
           (alternatives (rest spec))
           (target-entry (lookup-word language target-gloss)))
      ;; Only derive if the target exists AND the dice roll succeeds
      (when (and target-entry (< (random 1.0) probability))
        ;; Filter alternatives to those whose components both exist in the lexicon
        (let ((viable (remove-if-not
                       (lambda (alt)
                         (and (lookup-word language (first alt))
                              (lookup-word language (second alt))))
                       alternatives)))
          (when viable
            ;; Pick one alternative at random
            (let* ((chosen (nth (random (length viable)) viable))
                   (mod-gloss (first chosen))
                   (head-gloss (second chosen))
                   (strategy (gfeature language :compound-strategy))
                   (order (gfeature language :compound-order))
                   (head-entry (lookup-word language head-gloss))
                   (mod-entry (lookup-word language mod-gloss))
                   (head-form (strip-markers (form head-entry)))
                   (mod-form (strip-markers (form mod-entry)))
                   (ordered (if (eql order :head-final)
                                (list mod-form head-form)
                                (list head-form mod-form)))
                   (form (case strategy
                           (:juxtapose
                            (reanalyze (append (first ordered) (second ordered))))
                           (:linking
                            (let ((linker (mapcar #'ensure-phone
                                                  (gfeature language :compound-linker))))
                              (reanalyze (append (first ordered) linker
                                                 (second ordered)))))
                           (:genitive
                            (let* ((gen-rule (find-morpheme-rule language :genitive :noun))
                                   (mod-inflected (if gen-rule
                                                      (strip-markers
                                                       (first (inflect
                                                               (reanalyze mod-form)
                                                               gen-rule)))
                                                      mod-form)))
                              (reanalyze (if (eql order :head-final)
                                             (append mod-inflected head-form)
                                             (append head-form mod-inflected))))))))
              ;; Replace the target entry's form and mark its origin
              (setf (form target-entry) form)
              (setf (origin target-entry)
                    (list :compound-derivation mod-gloss head-gloss)))))))))

;;; Grammar

(defun inflect (base-form rule &key gloss lang feature applies-to)
  "Apply a morpheme rule to a base form. Returns a list of word-forms
   (one element for affixes, two for particles).
   When GLOSS is true, returns gloss tokens via morpheme-gloss instead."
  (if gloss
      (morpheme-gloss lang feature applies-to base-form)
      (let ((marker (mrule-marker rule)))
        (case (mrule-strategy rule)
          (:suffix          (list (reanalyze (append (strip-markers base-form)
                                                      (mapcar #'ensure-phone marker)))))
          (:prefix          (list (reanalyze (append (mapcar #'ensure-phone marker)
                                                      (strip-markers base-form)))))
          (:particle-before (list (reanalyze (mapcar #'ensure-phone marker)) base-form))
          (:particle-after  (list base-form (reanalyze (mapcar #'ensure-phone marker))))
          ;; Compound: merged suffix + disambiguating particle
          (:disambig-suffix-before
           (list (reanalyze (mapcar #'ensure-phone (mrule-disambig rule)))
                 (reanalyze (append (strip-markers base-form)
                                    (mapcar #'ensure-phone marker)))))
          (:disambig-suffix-after
           (list (reanalyze (append (strip-markers base-form)
                                    (mapcar #'ensure-phone marker)))
                 (reanalyze (mapcar #'ensure-phone (mrule-disambig rule)))))
          (:none            (list base-form))))))

;;; Paradigm pre-computation

(defun bound-rule-p (rule)
  "True if a morpheme rule uses a bound strategy (suffix or prefix)."
  (member (mrule-strategy rule) '(:suffix :prefix)))

(defun bound-rules-for-class (lang word-class)
  "Return all morpheme rules for word-class that use suffix or prefix strategy."
  (remove-if-not (lambda (r)
                   (and (eql (mrule-applies-to r) word-class)
                        (bound-rule-p r)))
                 (gfeature lang :morphology)))

(defun paradigm-dimensions (lang word-class &key noun-class)
  "Return a list of feature dimensions for paradigm enumeration.
   Each dimension is a list of feature values (excluding nil).
   Only features with suffix/prefix rules are included."
  (let ((bound-rules (bound-rules-for-class lang word-class)))
    (flet ((has-bound (feat)
             (find-if (lambda (r) (eql (mrule-feature r) feat)) bound-rules)))
      (case word-class
        (:noun
         (let ((def-dim nil)
               (case-dim nil)
               (number-dim nil)
               (other-dim nil))
           ;; Definiteness — prefer class-specific if entry has a noun class
           (when noun-class
             (let ((class-def (intern (format nil "DEFINITE-~a" (symbol-name noun-class))
                                      :keyword))
                   (class-indef (intern (format nil "INDEFINITE-~a" (symbol-name noun-class))
                                        :keyword)))
               (when (has-bound class-def) (push class-def def-dim))
               (when (has-bound class-indef) (push class-indef def-dim))))
           ;; Fall back to base definite/indefinite if no class-specific
           (unless (find-if (lambda (f) (search "DEFINITE" (symbol-name f))) def-dim)
             (when (has-bound :definite) (push :definite def-dim))
             (when (has-bound :indefinite) (push :indefinite def-dim)))
           ;; Case/oblique — mutually exclusive, merged into one dimension
           (iter (for feat in '(:accusative :genitive
                                :oblique-with :oblique-from :oblique-to
                                :oblique-on :oblique-at :oblique-over))
             (when (has-bound feat) (push feat case-dim)))
           ;; Number
           (when (has-bound :plural) (push :plural number-dim))
           ;; Other noun features
           (iter (for feat in '(:comp-standard :exception :simile :negation))
             (when (has-bound feat) (push feat other-dim)))
           (remove nil (list (nreverse def-dim)
                             (nreverse case-dim)
                             (nreverse number-dim)
                             (nreverse other-dim)))))
        (:verb
         (let ((tense-dim nil)
               (agr-dim nil)
               (voice-dim nil)
               (other-dim nil))
           ;; Tense
           (iter (for feat in '(:past :present :future))
             (when (has-bound feat) (push feat tense-dim)))
           ;; Agreement
           (iter (for feat in '(:agr-sg :agr-pl :agr-1 :agr-2 :agr-3
                                :agr-1sg :agr-2sg :agr-3sg
                                :agr-1pl :agr-2pl :agr-3pl))
             (when (has-bound feat) (push feat agr-dim)))
           ;; Passive
           (when (has-bound :passive) (push :passive voice-dim))
           ;; Other verb features
           (iter (for feat in '(:negation :imperative :causative :interrogative
                                :modal-ability :modal-obligation :exclamation
                                :optative :nominalization :counterfactual))
             (when (has-bound feat) (push feat other-dim)))
           (remove nil (list (nreverse tense-dim)
                             (nreverse agr-dim)
                             (nreverse voice-dim)
                             (nreverse other-dim)))))
        (:adjective
         (let ((class-dim nil)
               (comp-dim nil))
           ;; Class agreement
           (iter (for nc in (or (gfeature lang :noun-classes) nil))
             (when (has-bound nc) (push nc class-dim)))
           ;; Comparative
           (when (has-bound :comparative) (push :comparative comp-dim))
           (remove nil (list (nreverse class-dim)
                             (nreverse comp-dim)))))
        (t nil)))))

(defun cross-product (dimensions)
  "Enumerate cross product of dimensions. Each dimension is a list of values.
   Each combo includes nil for unselected dimensions.
   Returns list of feature-key lists (non-nil values in order)."
  (if (null dimensions)
      (list nil)
      (let ((rest-combos (cross-product (rest dimensions))))
        (iter outer
          ;; nil = skip this dimension
          (for val in (cons nil (first dimensions)))
          (iter (for rest-combo in rest-combos)
            (in outer
                (collect (if val
                             (cons val rest-combo)
                             rest-combo))))))))

(defun generate-paradigm (entry lang)
  "Pre-compute all inflected forms for a lexical entry.
   Stores alist of (feature-key . fused-word-form) on the entry."
  (let* ((cat (category entry))
         (word-class (intern (symbol-name cat) :keyword))
         ;; Pronouns use :noun morpheme rules
         (morph-class (if (eql word-class :pronoun) :noun word-class))
         (nc (noun-class entry))
         (base (form entry))
         (dims (paradigm-dimensions lang morph-class :noun-class nc)))
    (when dims
      (let ((paradigm nil))
        (iter (for combo in (cross-product dims))
          ;; Skip the all-nil case (empty feature list)
          (when combo
            ;; Apply suffix/prefix rules in order, accumulating onto base
            (let ((current base)
                  (valid t))
              (iter (for feat in combo)
                (let ((rule (find-morpheme-rule lang feat morph-class)))
                  (if (and rule (bound-rule-p rule))
                      (setf current (first (inflect current rule)))
                      ;; Feature has no bound rule — shouldn't happen since
                      ;; paradigm-dimensions only includes bound features
                      (setf valid nil))))
              (when (and valid (not (eq current base)))
                (push (cons combo current) paradigm)))))
        (setf (inflected-forms entry) (nreverse paradigm))))))

(defun forms-identical-p (form-a form-b)
  "Compare two word-forms by IPA representation."
  (equal (serialize-form form-a) (serialize-form form-b)))

(defun measure-paradigm-collision (feature-a feature-b word-class lang)
  "Return the fraction of entries (0.0–1.0) where two bound features produce identical forms."
  (let ((total 0) (collisions 0))
    (iter (for entry in (lexicon lang))
      (let ((cat (category entry)))
        (when (or (eql (intern (symbol-name cat) :keyword) word-class)
                  (and (eql word-class :noun) (eql cat 'pronoun)))
          (let* ((base (form entry))
                 (rule-a (find-morpheme-rule lang feature-a word-class))
                 (rule-b (find-morpheme-rule lang feature-b word-class))
                 (form-a (first (inflect base rule-a)))
                 (form-b (first (inflect base rule-b))))
            (incf total)
            (when (forms-identical-p form-a form-b)
              (incf collisions))))))
    (if (zerop total) 0.0 (/ collisions total))))

(defun disambiguate-markers (language &key (collision-threshold 0.5))
  "Detect and resolve marker collisions in mutually exclusive paradigm dimensions.
   Returns T if any rules were changed."
  (let* ((proto (collect-derivation-chain language))
         (clause-order (gfeature language :clause-order))
         (verb-final-p (eql (third clause-order) :verb))
         (disambig-strategy (if verb-final-p :disambig-suffix-after :disambig-suffix-before))
         (fixed nil)
         (dimensions
          '((:noun (:accusative :genitive
                    :oblique-with :oblique-from :oblique-to
                    :oblique-on :oblique-at :oblique-over))
            (:verb (:past :present :future)))))
    (iter (for (word-class features) in dimensions)
      (let ((active-rules
             (remove-if-not
              (lambda (rule)
                (and (member (mrule-feature rule) features)
                     (eql (mrule-applies-to rule) word-class)
                     (not (eql (mrule-strategy rule) :none))
                     (mrule-marker rule)))
              (gfeature language :morphology))))
        ;; Check pairs: particle-vs-particle (compare markers) and bound-vs-bound (compare forms)
        (iter (for (rule-a . rest) on active-rules)
          (iter (for rule-b in rest)
            (let ((both-bound (and (bound-rule-p rule-a) (bound-rule-p rule-b)))
                  (both-particle (and (not (bound-rule-p rule-a)) (not (bound-rule-p rule-b)))))
              (when (or both-bound both-particle)
                (let ((collision-rate
                       (if both-bound
                           (measure-paradigm-collision
                            (mrule-feature rule-a) (mrule-feature rule-b)
                            word-class language)
                           ;; Both particles: compare markers directly
                           (if (equal (mrule-marker rule-a) (mrule-marker rule-b))
                               1.0
                               0.0))))
                  (when (>= collision-rate collision-threshold)
                    (if both-bound
                        ;; Bound collision: keep merged suffix, add disambig particle
                        (progn
                          (setf (mrule-strategy rule-b) disambig-strategy)
                          (setf (mrule-disambig rule-b) (generate-marker proto)))
                        ;; Pure particle collision: switch to fresh particle
                        (setf (mrule-marker rule-b) (generate-marker proto)))
                    (setf fixed t)))))))))
    fixed))

(defun lookup-inflected (entry features)
  "Look up pre-computed inflected form for a feature list.
   Returns the fused word-form, or nil if not found."
  (when (and features (inflected-forms entry))
    (cdr (assoc features (inflected-forms entry) :test #'equal))))

(defun generate-paradigms (language)
  "Generate paradigms for all entries in a language's lexicon."
  (when (grammar language)
    (iter (for entry in (lexicon language))
      (generate-paradigm entry language))))

(defun random-clause-order ()
  (let ((r (random 1.0)))
    (cond
      ((< r 0.35) '(:subject :verb :object))        ; SVO
      ((< r 0.70) '(:subject :object :verb))        ; SOV
      ((< r 0.80) '(:verb :subject :object))        ; VSO
      ((< r 0.90) '(:verb :object :subject))        ; VOS
      ((< r 0.95) '(:object :subject :verb))        ; OSV
      (t           '(:object :verb :subject)))))     ; OVS

(defun feature-supplied-p (plist key)
  "Return T if KEY appears in PLIST (distinguishes absent from NIL-valued)."
  (loop for (k) on plist by #'cddr thereis (eql k key)))

(defmacro override-or (overrides key &body default-form)
  "Use override value if KEY is in OVERRIDES plist, otherwise evaluate DEFAULT-FORM."
  (let ((ov (gensym "OV")))
    `(let ((,ov ,overrides))
       (if (feature-supplied-p ,ov ,key)
           (getf ,ov ,key)
           (progn ,@default-form)))))

(defun random-word-order-grammar (&optional overrides)
  "Generate word-order grammar: clause-order, np-order, vp-order, gen-order.
Derives head-final-p from clause order (OV → head-final, VO → head-initial)
and biases NP/VP/gen orders accordingly."
  (let* ((clause-order (override-or overrides :clause-order
                         (random-clause-order)))
         (head-final-p (< (position :object clause-order)
                          (position :verb clause-order))))
    (list :clause-order clause-order
          :np-order (override-or overrides :np-order
                      (head-biased head-final-p
                                   '(:adjective :noun) '(:noun :adjective) 0.75))
          :vp-order (override-or overrides :vp-order
                      (head-biased head-final-p
                                   '(:adverb :verb) '(:verb :adverb) 0.75))
          :gen-order (override-or overrides :gen-order
                       (head-biased head-final-p
                                    :possessor-first :possessed-first 0.75)))))

(defun random-element (list)
  (nth (random (length list)) list))

(defun random-biased (threshold option-a option-b)
  "Return OPTION-A with probability THRESHOLD, else OPTION-B."
  (if (< (random 1.0) threshold) option-a option-b))

(defun head-biased (head-final-p head-final-option head-initial-option
                    &optional (bias 0.75))
  "Return HEAD-FINAL-OPTION or HEAD-INITIAL-OPTION biased by head directionality."
  (if head-final-p
      (random-biased bias head-final-option head-initial-option)
      (random-biased bias head-initial-option head-final-option)))

(defun random-strategy (typology &optional head-final-p)
  (let ((strategies '(:suffix :prefix :particle-before :particle-after :none)))
    (case typology
      (:synthetic (random-element '(:suffix :prefix :suffix :suffix :prefix)))
      (:analytic
       (let ((base (random-element '(:particle-before :particle-after :none
                                     :particle-before :particle-after))))
         (if (and head-final-p (member base '(:particle-before :particle-after)))
             (head-biased head-final-p :particle-before :particle-after 0.65)
             base)))
      (t (let ((base (random-element strategies)))
           (if (and head-final-p (member base '(:particle-before :particle-after)))
               (head-biased head-final-p :particle-before :particle-after 0.65)
               base))))))

(defun random-derivation-strategy (typology &optional head-final-p)
  "Derivation biases ~70% suffix (cross-linguistically typical)."
  (let ((r (random 1.0)))
    (case typology
      (:synthetic (if (< r 0.7) :suffix :prefix))
      (:analytic  (if (< r 0.5)
                      (if head-final-p
                          (head-biased head-final-p :particle-before :particle-after 0.65)
                          (random-element '(:particle-before :particle-after)))
                      (if (< r 0.85) :suffix :prefix)))
      (t (cond ((< r 0.7) :suffix)
               ((< r 0.85) :prefix)
               (t (if head-final-p
                      (head-biased head-final-p :particle-before :particle-after 0.65)
                      (random-element '(:particle-before :particle-after)))))))))

(defvar *marker-phones* nil
  "Accumulated phone lists of markers generated during the current generate-grammar call.")

(defun min-marker-distance (phones)
  "Minimum loanword-similarity distance from PHONES to all entries in *marker-phones*."
  (if (null *marker-phones*)
      most-positive-fixnum
      (iter (for existing in *marker-phones*)
        (minimizing (loanword-similarity-max-consonants phones existing)))))

(defun generate-marker (language &key (min-distance 3) (max-attempts 20))
  "Generate a 1-syllable marker distinct from previously generated markers.
Falls back to 2 syllables if no distinct 1-syllable marker is found."
  (block found
    (iter (repeat max-attempts)
      (for syl = (syllable language))
      (for phones = (strip-markers syl))
      (when (>= (min-marker-distance phones) min-distance)
        (push phones *marker-phones*)
        (return-from found (serialize-form syl))))
    ;; Fallback: try 2-syllable markers
    (let ((form (word language 2)))
      (push (strip-markers form) *marker-phones*)
      (serialize-form form))))

(defun generate-short-marker (language &key (min-distance 3) (max-attempts 20))
  "Generate a short class marker: CV if onsets exist, V otherwise. Distinct from previous markers.
Falls back to a full syllable if no distinct short marker is found."
  (block found
    (iter (repeat max-attempts)
      (for syl = (let ((v (ensure-phone-point (random-letter (vowels language) (vowel-frequencies language)))))
                   (if (> (onset-max language) 0)
                       (let ((c (ensure-phone-point (random-letter (consonants language) (consonant-frequencies language)))))
                         (list c v))
                       (list v))))
      (for phones = (strip-markers syl))
      (when (>= (min-marker-distance phones) min-distance)
        (push phones *marker-phones*)
        (return-from found (serialize-form syl))))
    ;; Fallback: use a full syllable
    (let ((syl (syllable language)))
      (push (strip-markers syl) *marker-phones*)
      (serialize-form syl))))

(defun generate-morphology (language typology &key question-strategy modal-strategy exclamation-strategy noun-classes head-final-p morphology-overrides)
  (let* ((base-features '((:accusative :noun)
                           (:genitive   :noun)
                           (:plural     :noun)
                           (:past       :verb)
                           (:present    :verb)
                           (:future     :verb)
                           (:negation   :verb)
                           (:negation   :noun)
                           (:imperative :verb)
                           (:comparative    :adjective)
                           (:comp-standard  :noun)
                           (:conditional :clause)
                           (:oblique-with :noun)
                           (:oblique-from :noun)
                           (:oblique-to   :noun)
                           (:oblique-on   :noun)
                           (:oblique-at   :noun)
                           (:oblique-over :noun)
                           (:temporal-before :clause)
                           (:temporal-after  :clause)
                           (:temporal-when   :clause)
                           (:relative :clause)
                           (:purpose :clause)
                           (:causative :verb)
                           (:quotative :clause)
                           (:adversative :clause)
                           (:exception :noun)
                           (:degree :clause)
                           (:interrogative :verb)
                           (:modal-ability    :verb)
                           (:modal-obligation :verb)
                           (:causal          :clause)
                           (:exclamation     :verb)
                           (:simile          :noun)
                           (:complementizer  :clause)
                           (:passive :verb)
                           (:optative :verb)
                           (:concessive :clause)
                           (:nominalization :verb)
                           (:counterfactual :clause)))
         ;; Agreement type
         (agr-roll (random 1.0))
         (agreement-type (cond ((< agr-roll 0.20) :none)
                                ((< agr-roll 0.45) :number-only)
                                ((< agr-roll 0.55) :person-only)
                                ((< agr-roll 0.90) :full)
                                (t :partial)))
         (agreement-features
           (case agreement-type
             (:none nil)
             (:number-only '((:agr-sg :verb)
                             (:agr-pl :verb)))
             (:person-only '((:agr-1 :verb)
                             (:agr-2 :verb)
                             (:agr-3 :verb)))
             (:full '((:agr-1sg :verb)
                      (:agr-2sg :verb)
                      (:agr-3sg :verb)
                      (:agr-1pl :verb)
                      (:agr-2pl :verb)
                      (:agr-3pl :verb)))
             (:partial '((:agr-1sg :verb)
                         (:agr-2sg :verb)
                         (:agr-3 :verb)
                         (:agr-1pl :verb)
                         (:agr-2pl :verb)))))
         ;; Article systems: ~40% no articles, ~35% definite only, ~25% both
         (article-roll (random 1.0))
         (article-features (cond
                             ((< article-roll 0.40) nil)
                             ((< article-roll 0.75) '((:definite :noun)))
                             (t '((:definite   :noun)
                                  (:indefinite :noun)))))
         ;; Derivational features — each has ~20% chance of :none (unproductive)
         (derivation-features
           (iter (for (feat-name applies-to) in '((:adverbialize :adjective)
                                                   (:agentive     :verb)
                                                   (:adj-from-noun :noun)
                                                   (:noun-from-adj :adjective)))
             (let ((feat-override (getf morphology-overrides feat-name)))
               (collect (let ((strat (if (and feat-override
                                              (feature-supplied-p feat-override :strategy))
                                         (getf feat-override :strategy)
                                         (if (< (random 1.0) 0.2)
                                             :none
                                             (random-derivation-strategy typology head-final-p)))))
                          (make-morpheme-rule :feature feat-name :applies-to applies-to
                                              :strategy strat
                                              :marker (if (and feat-override
                                                               (feature-supplied-p feat-override :marker))
                                                          (getf feat-override :marker)
                                                          (if (eql strat :none)
                                                              nil
                                                              (generate-marker language)))))))))
         (features (append base-features article-features agreement-features)))
    (let ((main-rules
            (iter (for (feat-name applies-to) in features)
              (let* ((feat-override (getf morphology-overrides feat-name))
                     (strategy (if (and feat-override
                                        (feature-supplied-p feat-override :strategy))
                                   (getf feat-override :strategy)
                                   (cond
                                     ;; Present tense: unmarked (bare stem)
                                     ((eql feat-name :present) :none)
                                     ;; Imperative: 60% bare stem, rest normal
                                     ((eql feat-name :imperative)
                                      (if (< (random 1.0) 0.6) :none (random-strategy typology head-final-p)))
                                     ;; Interrogative: only mark on verb when question-strategy is :verb-morphology
                                     ((eql feat-name :interrogative)
                                      (if (eql question-strategy :verb-morphology)
                                          (random-strategy typology head-final-p)
                                          :none))
                                     ;; Exclamation: only mark when exclamation-strategy is :morpheme
                                     ((eql feat-name :exclamation)
                                      (if (eql exclamation-strategy :morpheme)
                                          (random-strategy typology head-final-p)
                                          :none))
                                     ;; Modal: morpheme strategy → normal; particle → particle; auxiliary → :none
                                     ((member feat-name '(:modal-ability :modal-obligation))
                                      (case modal-strategy
                                        (:morpheme (random-strategy typology head-final-p))
                                        (:particle (random-strategy typology head-final-p))
                                        (t :none)))
                                     ;; Passive: default :none, activated post-hoc by demote :verb-morphology
                                     ((eql feat-name :passive) :none)
                                     ;; Otherwise normal
                                     (t (random-strategy typology head-final-p))))))
                (collect (make-morpheme-rule :feature feat-name :applies-to applies-to
                                             :strategy strategy
                                             :marker (if (and feat-override
                                                              (feature-supplied-p feat-override :marker))
                                                         (getf feat-override :marker)
                                                         (if (eql strategy :none)
                                                             nil
                                                             (generate-marker language)))))))))
      (values
       (append
        main-rules
        derivation-features
        ;; Class-specific morphology rules when noun classes are present
        (when noun-classes
          (let ((class-rules nil))
            ;; Class-specific articles: same strategy as base rule, different markers per class
            (dolist (def-feat '(:definite :indefinite))
              (let ((base-rule (find-if (lambda (r)
                                          (and (eql (mrule-feature r) def-feat)
                                               (eql (mrule-applies-to r) :noun)))
                                        main-rules)))
                (when base-rule
                  (let ((base-strat (mrule-strategy base-rule)))
                    (dolist (nc noun-classes)
                      (let ((class-feat (intern (format nil "~a-~a"
                                                       (symbol-name def-feat)
                                                       (symbol-name nc))
                                               :keyword)))
                        (push (make-morpheme-rule :feature class-feat
                                                  :applies-to :noun
                                                  :strategy base-strat
                                                  :marker (if (eql base-strat :none)
                                                              nil
                                                              (generate-short-marker language)))
                              class-rules)))))))
            ;; Adjective class agreement rules — one strategy, different markers per class
            (let ((adj-strat (random-strategy typology head-final-p)))
              (dolist (nc noun-classes)
                (push (make-morpheme-rule :feature nc
                                          :applies-to :adjective
                                          :strategy adj-strat
                                          :marker (if (eql adj-strat :none)
                                                      nil
                                                      (generate-short-marker language)))
                      class-rules)))
            (nreverse class-rules))))
       agreement-type))))

;;; Grammar feature generators — each returns a plist fragment

(defun random-copula-grammar (language head-final-p &optional overrides)
  "Generate copula grammar: strategy, marker, order."
  (let* ((strategy (override-or overrides :copula-strategy
                     (let ((roll (random 1.0)))
                       (cond ((< roll 0.35) :zero)
                             ((< roll 0.70) :particle)
                             (t :verb)))))
         (marker (override-or overrides :copula-marker
                   (when (eql strategy :particle)
                     (generate-marker language))))
         (order (override-or overrides :copula-order
                  (head-biased head-final-p
                      '(:subject :predicate :copula)
                      '(:subject :copula :predicate) 0.70))))
    (list :copula-strategy strategy
          :copula-marker marker
          :copula-order order)))

(defun random-conjunction-grammar (language &optional overrides)
  "Generate conjunction grammar: strategy, marker."
  (let* ((strategy (override-or overrides :conjunction-strategy
                     (let ((roll (random 1.0)))
                       (cond ((< roll 0.60) :medial)
                             ((< roll 0.85) :each)
                             (t :juxtaposition)))))
         (marker (override-or overrides :conjunction-marker
                   (unless (eql strategy :juxtaposition)
                     (generate-marker language)))))
    (list :conjunction-strategy strategy
          :conjunction-marker marker)))

(defun random-adposition-grammar (typology head-final-p &optional overrides)
  "Generate adposition grammar: strategy."
  (list :adposition-strategy
        (override-or overrides :adposition-strategy
          (if (and (eql typology :synthetic) (< (random 1.0) 0.4))
              :case-marking
              (if head-final-p
                  (random-biased 0.7 :postposition :preposition)
                  (random-biased 0.7 :preposition :postposition))))))

(defun random-conditional-grammar (&optional overrides)
  "Generate conditional grammar: strategy, order."
  (let* ((strategy (override-or overrides :conditional-strategy
                     (let ((roll (random 1.0)))
                       (cond ((< roll 0.50) :particle)
                             ((< roll 0.80) :verb-morphology)
                             (t :juxtaposition)))))
         (order (override-or overrides :conditional-order
                  (random-biased 0.8
                      '(:protasis :apodosis)
                      '(:apodosis :protasis)))))
    (list :conditional-strategy strategy
          :conditional-order order)))

(defun random-comparative-grammar (&optional overrides)
  "Generate comparative grammar: comp-order."
  (list :comp-order (override-or overrides :comp-order
                      (random-element '((:target :quality :standard)
                                        (:quality :target :standard)
                                        (:target :standard :quality))))))

(defun random-relative-grammar (head-final-p &optional overrides)
  "Generate relative clause grammar: strategy, order."
  (let* ((strategy (override-or overrides :relative-strategy
                     (let ((roll (random 1.0)))
                       (cond ((< roll 0.40) :particle)
                             ((< roll 0.75) :gap)
                             (t :participial)))))
         (order (override-or overrides :relative-order
                  (if head-final-p
                      (random-biased 0.6 '(:clause :head) '(:head :clause))
                      (random-biased 0.6 '(:head :clause) '(:clause :head))))))
    (list :relative-strategy strategy
          :relative-order order)))

(defun random-purpose-grammar (head-final-p &optional overrides)
  "Generate purpose clause grammar: order."
  (list :purpose-order (override-or overrides :purpose-order
                         (head-biased head-final-p
                             '(:purpose :main)
                             '(:main :purpose) 0.70))))

(defun random-causative-grammar (head-final-p &optional overrides)
  "Generate causative grammar: strategy, order."
  (let* ((strategy (override-or overrides :causative-strategy
                     (let ((roll (random 1.0)))
                       (cond ((< roll 0.50) :analytic)
                             ((< roll 0.80) :morpheme)
                             (t :juxtapose)))))
         (order (override-or overrides :causative-order
                  (head-biased head-final-p
                      '(:caused :causer)
                      '(:causer :caused) 0.65))))
    (list :causative-strategy strategy
          :causative-order order)))

(defun random-quotation-grammar (head-final-p &optional overrides)
  "Generate quotation grammar: order."
  (list :quotation-order (override-or overrides :quotation-order
                           (head-biased head-final-p
                               '(:content :verb :speaker)
                               '(:speaker :verb :content) 0.65))))

(defun random-adversative-grammar (&optional overrides)
  "Generate adversative grammar: strategy, order."
  (let* ((strategy (override-or overrides :adversative-strategy
                     (let ((roll (random 1.0)))
                       (cond ((< roll 0.50) :particle)
                             ((< roll 0.80) :each)
                             (t :juxtapose)))))
         (order (override-or overrides :adversative-order
                  (random-biased 0.7
                      '(:conceded :asserted)
                      '(:asserted :conceded)))))
    (list :adversative-strategy strategy
          :adversative-order order)))

(defun random-degree-grammar (head-final-p &optional overrides)
  "Generate degree/result grammar: order."
  (list :degree-order (override-or overrides :degree-order
                        (head-biased head-final-p
                            '(:result :quality)
                            '(:quality :result) 0.65))))

(defun random-disjunction-grammar (language &optional overrides)
  "Generate disjunction grammar: strategy, marker."
  (let* ((strategy (override-or overrides :disjunction-strategy
                     (let ((roll (random 1.0)))
                       (cond ((< roll 0.60) :medial)
                             ((< roll 0.85) :each)
                             (t :juxtaposition)))))
         (marker (override-or overrides :disjunction-marker
                   (unless (eql strategy :juxtaposition)
                     (generate-marker language)))))
    (list :disjunction-strategy strategy
          :disjunction-marker marker)))

(defun random-reflexive-grammar (language &optional overrides)
  "Generate reflexive grammar: strategy, marker."
  (let* ((strategy (override-or overrides :reflexive-strategy
                     (let ((roll (random 1.0)))
                       (cond ((< roll 0.40) :suffix)
                             ((< roll 0.75) :separate-word)
                             (t :reduplication)))))
         (marker (override-or overrides :reflexive-marker
                   (unless (eql strategy :reduplication)
                     (generate-marker language)))))
    (list :reflexive-strategy strategy
          :reflexive-marker marker)))

(defun random-causal-grammar (head-final-p &optional overrides)
  "Generate causal grammar: strategy, order."
  (let* ((strategy (override-or overrides :causal-strategy
                     (let ((roll (random 1.0)))
                       (cond ((< roll 0.50) :particle)
                             ((< roll 0.80) :verb-morphology)
                             (t :juxtaposition)))))
         (order (override-or overrides :causal-order
                  (head-biased head-final-p
                      '(:reason :result)
                      '(:result :reason) 0.65))))
    (list :causal-strategy strategy
          :causal-order order)))

(defun random-concessive-grammar (&optional overrides)
  "Generate concessive grammar: strategy, order."
  (let* ((strategy (override-or overrides :concessive-strategy
                     (let ((roll (random 1.0)))
                       (cond ((< roll 0.50) :particle)
                             ((< roll 0.80) :verb-morphology)
                             (t :juxtaposition)))))
         (order (override-or overrides :concessive-order
                  (random-biased 0.7
                      '(:conceded :asserted)
                      '(:asserted :conceded)))))
    (list :concessive-strategy strategy
          :concessive-order order)))

(defun random-reciprocal-grammar (language &optional overrides)
  "Generate reciprocal grammar: strategy, marker."
  (let* ((strategy (override-or overrides :reciprocal-strategy
                     (let ((roll (random 1.0)))
                       (cond ((< roll 0.40) :suffix)
                             ((< roll 0.75) :separate-word)
                             (t :particle)))))
         (marker (override-or overrides :reciprocal-marker
                   (generate-marker language))))
    (list :reciprocal-strategy strategy
          :reciprocal-marker marker)))

(defun random-nominalization-grammar (language &optional overrides)
  "Generate nominalization grammar: strategy, marker."
  (let* ((strategy (override-or overrides :nominalization-strategy
                     (let ((roll (random 1.0)))
                       (cond ((< roll 0.50) :suffix)
                             ((< roll 0.80) :prefix)
                             (t :particle)))))
         (marker (override-or overrides :nominalization-marker
                   (generate-marker language))))
    (list :nominalization-strategy strategy
          :nominalization-marker marker)))

(defun random-distributive-grammar (language head-final-p &optional overrides)
  "Generate distributive grammar: strategy, marker."
  (let* ((strategy (override-or overrides :distributive-strategy
                     (let ((roll (random 1.0)))
                       (cond ((< roll 0.80)
                              (head-biased head-final-p
                                           :particle-before :particle-after 0.65))
                             (t :reduplication)))))
         (marker (override-or overrides :distributive-marker
                   (unless (eql strategy :reduplication)
                     (generate-marker language)))))
    (list :distributive-strategy strategy
          :distributive-marker marker)))

(defun random-exclamation-grammar (language head-final-p &optional overrides)
  "Generate exclamation grammar: strategy, particle, position."
  (let* ((strategy (override-or overrides :exclamation-strategy
                     (random-biased 0.60 :particle :morpheme)))
         (particle (override-or overrides :exclamation-particle
                     (when (eql strategy :particle)
                       (generate-marker language))))
         (position (override-or overrides :exclamation-position
                     (head-biased head-final-p :final :initial 0.65))))
    (list :exclamation-strategy strategy
          :exclamation-particle particle
          :exclamation-position position)))

(defun random-simile-grammar (language head-final-p &optional overrides)
  "Generate simile grammar: strategy, marker, order."
  (let* ((strategy (override-or overrides :simile-strategy
                     (random-biased 0.60 :particle :morpheme)))
         (marker (override-or overrides :simile-marker
                   (when (eql strategy :particle)
                     (generate-marker language))))
         (order (override-or overrides :simile-order
                  (head-biased head-final-p
                      '(:standard :compared)
                      '(:compared :standard) 0.65))))
    (list :simile-strategy strategy
          :simile-marker marker
          :simile-order order)))

(defun random-wh-grammar (head-final-p &optional overrides)
  "Generate wh-question grammar: strategy, position."
  (list :wh-strategy (override-or overrides :wh-strategy
                       (random-biased 0.60 :dedicated :q-plus-content))
        :wh-position (override-or overrides :wh-position
                       (head-biased head-final-p :final :initial 0.70))))

(defun random-complement-grammar (head-final-p &optional overrides)
  "Generate complement clause grammar: order."
  (list :complement-order (override-or overrides :complement-order
                            (head-biased head-final-p
                                '(:content :verb :subject)
                                '(:subject :verb :content) 0.70))))

(defun random-question-grammar (language head-final-p &optional overrides)
  "Generate question grammar: strategy, particle, position."
  (let* ((strategy (override-or overrides :question-strategy
                     (let ((roll (random 1.0)))
                       (cond ((< roll 0.50) :particle)
                             ((< roll 0.80) :verb-morphology)
                             (t :intonation)))))
         (position (override-or overrides :question-particle-position
                     (head-biased head-final-p :final :initial 0.65)))
         (particle (override-or overrides :question-particle
                     (when (eql strategy :particle)
                       (generate-marker language)))))
    (list :question-strategy strategy
          :question-particle-position position
          :question-particle particle)))

(defun random-modal-grammar (head-final-p &optional overrides)
  "Generate modal grammar: strategy, order."
  (let* ((strategy (override-or overrides :modal-strategy
                     (let ((roll (random 1.0)))
                       (cond ((< roll 0.40) :auxiliary)
                             ((< roll 0.80) :morpheme)
                             (t :particle)))))
         (order (override-or overrides :modal-order
                  (head-biased head-final-p :modal-last :modal-first 0.70))))
    (list :modal-strategy strategy
          :modal-order order)))

(defun random-compound-grammar (language head-final-p &optional overrides)
  "Generate compound grammar: strategy, order, linker."
  (let* ((strategy (override-or overrides :compound-strategy
                     (let ((roll (random 1.0)))
                       (cond ((< roll 0.45) :juxtapose)
                             ((< roll 0.75) :linking)
                             (t :genitive)))))
         (order (override-or overrides :compound-order
                  (if head-final-p
                      (random-biased 0.7 :head-final :head-initial)
                      (random-biased 0.6 :head-initial :head-final))))
         (linker (override-or overrides :compound-linker
                   (when (eql strategy :linking)
                     (generate-marker language)))))
    (list :compound-strategy strategy
          :compound-order order
          :compound-linker linker)))

(defun random-pronoun-grammar (head-final-p &optional overrides)
  "Generate pronoun grammar: collapse, plural-pronoun-strategy."
  (let* ((collapse (override-or overrides :pronoun-collapse
                     (let ((roll (random 1.0)))
                       (cond ((< roll 0.40) :none)
                             ((< roll 0.65) :collapse-2)
                             ((< roll 0.80) :collapse-3)
                             ((< roll 0.90) :collapse-2-3)
                             (t :collapse-number)))))
         (plural-strategy
           (override-or overrides :plural-pronoun-strategy
             (when (member collapse '(:collapse-2 :collapse-3
                                      :collapse-2-3 :collapse-number))
               (let ((r (random 1.0)))
                 (if (< r 0.20)
                     :reduplication
                     (head-biased head-final-p
                                  :quantifier-before :quantifier-after 0.60)))))))
    (list :pronoun-collapse collapse
          :plural-pronoun-strategy plural-strategy)))

(defun random-noun-class-grammar (&optional overrides)
  "Generate noun class grammar: count, classes, scheme."
  (let* ((count (override-or overrides :noun-class-count
                  (let ((roll (random 1.0)))
                    (cond ((< roll 0.35) 0)
                          ((< roll 0.60) 2)
                          ((< roll 0.80) 3)
                          ((< roll 0.90) 4)
                          ((< roll 0.95) 5)
                          (t (+ 6 (random 3)))))))
         (classes (override-or overrides :noun-classes
                    (when (>= count 2)
                      (iter (for i from 1 to count)
                        (collect (intern (format nil "CLASS-~d" i) :keyword))))))
         (scheme (override-or overrides :noun-class-scheme
                   (when classes
                     (let* ((all-features '(:human :animate :natural :abstract
                                             :metal :liquid :plant :weapon :food-item
                                             :celestial :bodypart :building))
                            (shuffled (scramble-sequence (copy-list all-features)))
                            (picked (subseq shuffled 0 (min (1- count)
                                                            (length all-features))))
                            (specificity '(:human :animate :bodypart :weapon :food-item
                                           :metal :liquid :plant :celestial :building
                                           :natural :abstract))
                            (sorted (sort (copy-list picked)
                                          (lambda (a b)
                                            (< (position a specificity)
                                               (position b specificity))))))
                       (iter (for feat in sorted)
                         (for i from 1)
                         (collect (cons feat (intern (format nil "CLASS-~d" i) :keyword)))))))))
    (list :noun-class-count count
          :noun-classes classes
          :noun-class-scheme scheme)))

(defun generate-info-structure (language typology adposition-strategy)
  "Generate info-structure: promote/demote strategies, activate passive if needed."
  (let* ((has-case (eql adposition-strategy :case-marking))
         (has-pro-drop (gfeature language :pro-drop))
         (promote-strategy (random-biased (if has-case 0.65 0.35)
                                          :fronting :particle))
         (topic-particle (when (eql promote-strategy :particle)
                           (generate-marker language)))
         (demote-roll (random 1.0))
         (demote-strategy
           (cond
             (has-pro-drop
              (cond ((< demote-roll 0.60) :pro-drop)
                    ((< demote-roll 0.85) :impersonal)
                    (t :verb-morphology)))
             ((eql typology :synthetic)
              (cond ((< demote-roll 0.40) :verb-morphology)
                    ((< demote-roll 0.75) :impersonal)
                    (t :pro-drop)))
             (t
              (cond ((< demote-roll 0.35) :impersonal)
                    ((< demote-roll 0.70) :pro-drop)
                    (t :verb-morphology))))))
    (when (eql demote-strategy :verb-morphology)
      (let ((passive-rule (find-morpheme-rule language :passive :verb)))
        (when passive-rule
          (setf (mrule-strategy passive-rule) (random-strategy typology))
          (setf (mrule-marker passive-rule) (generate-marker language)))))
    (setf (gfeature language :promote-strategy) promote-strategy)
    (setf (gfeature language :topic-particle) topic-particle)
    (setf (gfeature language :demote-strategy) demote-strategy)))

(defun random-topic-drop-profile (language typology)
  "Generate a topic-drop profile plist based on typology.
   Only particle-based features are eligible for drop."
  (when (eql typology :synthetic)
    (return-from random-topic-drop-profile nil))
  (let ((biases (if (eql typology :analytic)
                    '((:subject-pronoun . 0.7)
                      (:possessor-pronoun . 0.7)
                      (:shared-tense . 0.6)
                      (:shared-case . 0.5)
                      (:conjunction-marker . 0.3))
                    ;; :mixed
                    '((:subject-pronoun . 0.25)
                      (:possessor-pronoun . 0.25)
                      (:shared-tense . 0.15)
                      (:shared-case . 0.10)
                      (:conjunction-marker . 0.10))))
        ;; Map drop types to the morpheme features + word-classes to check strategy
        (feature-checks '((:shared-tense . (:past :verb))
                          (:shared-tense . (:future :verb))
                          (:shared-case . (:accusative :noun))
                          (:shared-case . (:genitive :noun))))
        (result nil))
    (dolist (entry biases)
      (let ((drop-type (car entry))
            (threshold (cdr entry)))
        (if (< (random 1.0) threshold)
            ;; Check if the relevant morpheme strategies are particle-based
            (let ((forced-nil nil))
              ;; For tense/case drops, verify at least one relevant rule uses particles
              (let ((checks (remove-if-not (lambda (c) (eql (car c) drop-type))
                                           feature-checks)))
                (when checks
                  (let ((any-particle nil))
                    (dolist (check checks)
                      (let* ((feat (first (cdr check)))
                             (wc (second (cdr check)))
                             (rule (find-morpheme-rule language feat wc)))
                        (when (and rule (member (mrule-strategy rule)
                                                '(:particle-before :particle-after)))
                          (setf any-particle t))))
                    (unless any-particle
                      (setf forced-nil t)))))
              (setf (getf result drop-type) (not forced-nil)))
            (setf (getf result drop-type) nil))))
    result))

(defun generate-grammar (language &key (typology :mixed) features)
  "Randomly generate a grammar for a proto-language.
FEATURES is an optional plist of grammar key overrides. Any key present in
FEATURES will be used as-is instead of being randomized. The special key
:MORPHOLOGY holds a plist of per-feature morphology overrides (keyed by
feature name, value is a plist with :STRATEGY and/or :MARKER).
:HEAD-FINAL-P can be explicitly set to decouple it from clause order."
  (let ((*marker-phones* nil))
    (let* ((word-order-feat (random-word-order-grammar features))
           (clause-order (getf word-order-feat :clause-order))
           (head-final-p (if (feature-supplied-p features :head-final-p)
                             (getf features :head-final-p)
                             (< (position :object clause-order)
                                (position :verb clause-order))))
           ;; Features needed as args to generate-morphology
           (question-feat (random-question-grammar language head-final-p features))
           (modal-feat (random-modal-grammar head-final-p features))
           (exclamation-feat (random-exclamation-grammar language head-final-p features))
           ;; Noun classes needed as arg to generate-morphology
           (noun-class-feat (random-noun-class-grammar features))
           (noun-classes (getf noun-class-feat :noun-classes))
           ;; Adposition needed in post-morphology block
           (adposition-feat (random-adposition-grammar typology head-final-p features)))
      (setf (grammar language)
            (append
             word-order-feat
             (random-copula-grammar language head-final-p features)
             (random-conjunction-grammar language features)
             adposition-feat
             (random-conditional-grammar features)
             (random-comparative-grammar features)
             (random-relative-grammar head-final-p features)
             (random-purpose-grammar head-final-p features)
             (random-causative-grammar head-final-p features)
             (random-quotation-grammar head-final-p features)
             (random-adversative-grammar features)
             (random-degree-grammar head-final-p features)
             (random-disjunction-grammar language features)
             (random-reflexive-grammar language features)
             (random-causal-grammar head-final-p features)
             (random-concessive-grammar features)
             (random-reciprocal-grammar language features)
             (random-nominalization-grammar language features)
             (random-distributive-grammar language head-final-p features)
             (random-simile-grammar language head-final-p features)
             (random-wh-grammar head-final-p features)
             (random-complement-grammar head-final-p features)
             (random-compound-grammar language head-final-p features)
             (random-pronoun-grammar head-final-p features)
             noun-class-feat
             question-feat
             modal-feat
             exclamation-feat))
      ;; Generate morphology (depends on question/modal/exclamation strategies and noun classes)
      (multiple-value-bind (morphology agr-type)
          (generate-morphology language typology
                               :question-strategy (getf question-feat :question-strategy)
                               :modal-strategy (getf modal-feat :modal-strategy)
                               :exclamation-strategy (getf exclamation-feat :exclamation-strategy)
                               :noun-classes noun-classes
                               :head-final-p head-final-p
                               :morphology-overrides (getf features :morphology))
        (setf (gfeature language :morphology) morphology)
        (setf (gfeature language :agreement-type) agr-type)
        (setf (gfeature language :pro-drop)
              (case agr-type
                (:full (< (random 1.0) 0.7))
                (:partial (< (random 1.0) 0.5))
                (:person-only (< (random 1.0) 0.25))
                (t nil)))
        (generate-info-structure language typology
                                 (getf adposition-feat :adposition-strategy))
        ;; Generate topic-drop profile (depends on morphology being set)
        (let ((topic-drop (random-topic-drop-profile language typology)))
          (when topic-drop
            (setf (gfeature language :topic-drop) topic-drop)))))))

(defun find-morpheme-rule (lang feature applies-to)
  "Find the morpheme rule for a given feature and word class."
  (find-if (lambda (rule)
             (and (eql (mrule-feature rule) feature)
                  (eql (mrule-applies-to rule) applies-to)))
           (gfeature lang :morphology)))

(defmethod gfeature ((lang language) key)
  "Look up a grammar feature by keyword. Returns the grammar-feature object,
   or a plain value for non-object grammar keys."
  (getf (grammar lang) key))

(defmethod (setf gfeature) (value (lang language) key)
  "Set a grammar feature by keyword."
  (setf (getf (grammar lang) key) value))

(defun linearize (order parts)
  "Given an order like (:subject :verb :object) and a plist of parts,
   return the parts in the specified order, skipping nils."
  (iter (for slot in order)
    (let ((val (getf parts slot)))
      (when val (appending val)))))

;;; Pronouns

(defun canonical-pronoun-key (person number collapse)
  "Map person+number to a canonical key based on collapse pattern.
   Collapsed cells share the same key so they get the same word form."
  (case collapse
    (:none (format nil "~a~a"
                   (string-downcase (symbol-name person))
                   (string-downcase (symbol-name number))))
    (:collapse-2
     (if (eql person :2nd)
         "2nd"
         (format nil "~a~a"
                 (string-downcase (symbol-name person))
                 (string-downcase (symbol-name number)))))
    (:collapse-3
     (if (eql person :3rd)
         "3rd"
         (format nil "~a~a"
                 (string-downcase (symbol-name person))
                 (string-downcase (symbol-name number)))))
    (:collapse-2-3
     (if (member person '(:2nd :3rd))
         (format nil "~a" (string-downcase (symbol-name person)))
         (format nil "~a~a"
                 (string-downcase (symbol-name person))
                 (string-downcase (symbol-name number)))))
    (:collapse-number
     (format nil "~a" (string-downcase (symbol-name person))))
    (t (format nil "~a~a"
               (string-downcase (symbol-name person))
               (string-downcase (symbol-name number))))))

(defun needs-plural-disambiguation (person collapse)
  "Return T if a pronoun of the given person needs plural disambiguation
   because the collapse pattern merges its singular and plural forms."
  (case collapse
    (:collapse-2 (eql person :2nd))
    (:collapse-3 (eql person :3rd))
    (:collapse-2-3 (member person '(:2nd :3rd)))
    (:collapse-number t)
    (t nil)))

(defun generate-pronoun-lexicon (language)
  "Generate pronoun lexicon entries, reusing forms for collapsed cells."
  (let ((collapse (gfeature language :pronoun-collapse))
        (noun-classes (gfeature language :noun-classes))
        (forms (make-hash-table :test 'equal)))
    (dolist (person '(:1st :2nd :3rd))
      (dolist (number '(:singular :plural))
        (let* ((gloss (format nil "~a~a"
                              (string-downcase (symbol-name person))
                              (string-downcase (symbol-name number))))
               (canonical (canonical-pronoun-key person number collapse))
               (existing-form (gethash canonical forms)))
          (if existing-form
              ;; Collapsed cell — reuse form, just add the lexical entry
              (push (make-instance 'lexical-entry
                                   :gloss gloss :form existing-form
                                   :category 'pronoun :origin :native)
                    (lexicon language))
              ;; New canonical form — use define-word for distance checks
              (let ((entry (define-word language gloss 'pronoun :syllables 1)))
                (setf (gethash canonical forms) (form entry)))))))
    ;; Generate class-specific 3rd person pronouns
    (when noun-classes
      (dolist (number '(:singular :plural))
        (let ((base-gloss (format nil "3rd~a"
                                  (string-downcase (symbol-name number)))))
          (dolist (nc noun-classes)
            (let ((class-gloss (format nil "~a-~a" base-gloss (symbol-name nc))))
              (define-word language class-gloss 'pronoun :syllables 1))))))
    ;; Generate plural quantifier if needed
    (when (gfeature language :plural-pronoun-strategy)
      (define-word language "plural-quantifier" 'particle :syllables 1))))

;;; Modal lexicon — auxiliary verbs for "can" and "must"

(defun generate-modal-lexicon (language)
  "Generate auxiliary verb entries for modal constructions when modal-strategy is :auxiliary."
  (when (eql (gfeature language :modal-strategy) :auxiliary)
    (dolist (gloss '("can" "must"))
      (define-word language gloss 'particle :syllables 1))))

;;; Wh-word lexicon — dedicated wh-question words

(defun generate-wh-lexicon (language)
  "Generate lexicon entries for wh-question words."
  (dolist (gloss '("wh-when" "wh-where" "wh-what" "wh-why" "wh-how"))
    (define-word language gloss 'particle :syllables 1)))

;;; Noun class helpers

(defun assign-noun-class (semantic-tags scheme default-class)
  "Assign a noun class based on semantic tags and the language's classification scheme.
   First matching feature wins; unmatched nouns get default-class."
  (or (iter (for (feature . nc) in scheme)
        (when (member feature semantic-tags) (return nc)))
      default-class))

(defun extract-noun-class (language np)
  "Extract noun class from a noun-like expression, or nil."
  (typecase np
    (noun (let ((entry (lookup-word language (name np))))
            (when entry (noun-class entry))))
    (noun-phrase
     (let ((noun-part (find-if (lambda (c) (typep c 'noun-like)) (components np))))
       (when noun-part (extract-noun-class language noun-part))))
    (possessive (extract-noun-class language (possessed np)))
    (compound-word
     (let ((head-entry (lookup-word language (compound-head np))))
       (when head-entry (noun-class head-entry))))
    (relative-clause (extract-noun-class language (head-noun np)))
    (negation (extract-noun-class language (target np)))
    (distributive (extract-noun-class language (dist-target np)))
    (exception-clause (extract-noun-class language (excluded np)))
    (nominalization :class-1)
    (t nil)))

;;; Agreement extraction and lookup

(defun extract-agreement (subject)
  "Extract person and number from a subject expression.
   Returns (values person number)."
  (typecase subject
    (pronoun (values (pronoun-person subject) (pro-number subject)))
    (noun (values :3rd (if (member :plural (traits subject)) :plural :singular)))
    (noun-phrase
     (let ((noun-part (find-if (lambda (c) (typep c 'noun-like))
                               (components subject))))
       (if noun-part
           (extract-agreement noun-part)
           (values :3rd :singular))))
    (possessive (extract-agreement (possessed subject)))
    (conj-np (values :3rd :plural))
    (negation (extract-agreement (target subject)))
    (distributive (extract-agreement (dist-target subject)))
    (nominalization (values :3rd :singular))
    (reciprocal (values (recip-person subject) (recip-number subject)))
    (t (values nil nil))))

(defun extract-info-structure (np)
  "Return (promote-p . demote-p) for any NP.
   When both are set, promote wins and demote is suppressed."
  (let ((result
          (typecase np
            (noun     (cons (member :promote (traits np)) (member :demote (traits np))))
            (pronoun  (cons (member :promote (traits np)) (member :demote (traits np))))
            (noun-phrase
             (let ((noun-part (find-if (lambda (c) (typep c 'noun-like))
                                       (components np))))
               (if noun-part
                   (extract-info-structure noun-part)
                   '(nil))))
            (possessive (extract-info-structure (possessed np)))
            (relative-clause (extract-info-structure (head-noun np)))
            (negation (extract-info-structure (target np)))
            (distributive (extract-info-structure (dist-target np)))
            (t '(nil)))))
    (if (car result)
        (cons (car result) nil)
        result)))

(defun find-agreement-rule (lang agr-person agr-number)
  "Find the agreement morpheme rule given person, number, and agreement type."
  (let ((agr-type (gfeature lang :agreement-type)))
    (case agr-type
      (:none nil)
      (:number-only
       (find-morpheme-rule lang
                           (if (eql agr-number :plural) :agr-pl :agr-sg)
                           :verb))
      (:person-only
       (find-morpheme-rule lang
                           (case agr-person
                             (:1st :agr-1) (:2nd :agr-2) (:3rd :agr-3))
                           :verb))
      ((:full :partial)
       (let* ((combined (intern (format nil "AGR-~a~a"
                                        (case agr-person (:1st "1") (:2nd "2") (:3rd "3"))
                                        (if (eql agr-number :plural) "PL" "SG"))
                                :keyword))
              (rule (find-morpheme-rule lang combined :verb)))
         (or rule
             ;; For :partial, 3sg/3pl collapse to :agr-3
             (when (and (eql agr-type :partial) (eql agr-person :3rd))
               (find-morpheme-rule lang :agr-3 :verb))))))))

(defun agreement-gloss-tag (agr-person agr-number)
  "Return a gloss tag like \"1SG\" for agreement annotation."
  (format nil "~a~a"
          (case agr-person (:1st "1") (:2nd "2") (:3rd "3") (t "3"))
          (if (eql agr-number :plural) "PL" "SG")))

;;; Grammar evolution

(defun evolve-marker (marker transformers)
  "Evolve a single IPA string list marker through sound changes."
  (when marker
    (serialize-form
     (evolve transformers
            (reanalyze (mapcar #'ensure-phone marker))))))

(defun evolve-grammar (grammar transformers)
  "Evolve all marker forms in a grammar through sound changes."
  (let ((result (copy-list grammar)))
    (setf (getf result :morphology)
          (mapcar (lambda (rule)
                    (let ((r (copy-morpheme-rule rule)))
                      (when (mrule-marker r)
                        (setf (mrule-marker r)
                              (evolve-marker (mrule-marker r) transformers)))
                      (when (mrule-disambig r)
                        (setf (mrule-disambig r)
                              (evolve-marker (mrule-disambig r) transformers)))
                      r))
                  (getf grammar :morphology)))
    ;; Evolve top-level markers
    (when (getf result :copula-marker)
      (setf (getf result :copula-marker)
            (evolve-marker (getf result :copula-marker) transformers)))
    (when (getf result :conjunction-marker)
      (setf (getf result :conjunction-marker)
            (evolve-marker (getf result :conjunction-marker) transformers)))
    (when (getf result :compound-linker)
      (setf (getf result :compound-linker)
            (evolve-marker (getf result :compound-linker) transformers)))
    (when (getf result :question-particle)
      (setf (getf result :question-particle)
            (evolve-marker (getf result :question-particle) transformers)))
    (when (getf result :disjunction-marker)
      (setf (getf result :disjunction-marker)
            (evolve-marker (getf result :disjunction-marker) transformers)))
    (when (getf result :reflexive-marker)
      (setf (getf result :reflexive-marker)
            (evolve-marker (getf result :reflexive-marker) transformers)))
    (when (getf result :exclamation-particle)
      (setf (getf result :exclamation-particle)
            (evolve-marker (getf result :exclamation-particle) transformers)))
    (when (getf result :simile-marker)
      (setf (getf result :simile-marker)
            (evolve-marker (getf result :simile-marker) transformers)))
    (when (getf result :topic-particle)
      (setf (getf result :topic-particle)
            (evolve-marker (getf result :topic-particle) transformers)))
    (when (getf result :reciprocal-marker)
      (setf (getf result :reciprocal-marker)
            (evolve-marker (getf result :reciprocal-marker) transformers)))
    (when (getf result :nominalization-marker)
      (setf (getf result :nominalization-marker)
            (evolve-marker (getf result :nominalization-marker) transformers)))
    (when (getf result :distributive-marker)
      (setf (getf result :distributive-marker)
            (evolve-marker (getf result :distributive-marker) transformers)))
    ;; Grammaticalization: particles may cliticize into affixes
    (drift-grammar-strategies result)
    result))

(defun topic-drop-protected-p (rule grammar)
  "Return T if RULE's particle is relied upon by the topic-drop profile.
Topic-dropped particles are elided in discourse context, so cliticizing
them into affixes would be incoherent."
  (let ((td (getf grammar :topic-drop)))
    (when td
      (let ((feat (mrule-feature rule)))
        (or (and (getf td :shared-tense)
                 (member feat '(:past :future)))
            (and (getf td :shared-case)
                 (member feat '(:accusative :genitive))))))))

(defun drift-grammar-strategies (grammar &key (cliticization-rate 0.15))
  "Probabilistically shift particle strategies to affix strategies (grammaticalization).
   Skips rules whose particles are topic-droppable.
   Mutates rules in-place — caller should pass a copied grammar."
  (iter (for rule in (getf grammar :morphology))
    (let ((strategy (mrule-strategy rule)))
      (when (and (member strategy '(:particle-before :particle-after))
                 (not (topic-drop-protected-p rule grammar))
                 (< (random 1.0) cliticization-rate))
        (setf (mrule-strategy rule)
              (if (eql strategy :particle-before) :prefix :suffix))))))

;;; Frequency analysis — extract actual phone frequencies from a language

(defun count-phones-in-words (word-forms counts)
  "Increment phone counts for each phone in a list of word-forms."
  (dolist (form word-forms)
    (dolist (pp form)
      (when (phone-p pp)
        (let ((raw (if (typep pp 'phone-point) (phone pp) pp)))
          (incf (gethash raw counts 0)))))))

(defun analyze-lexicon-frequencies (language)
  "Count phone occurrences across all lexicon entries."
  (let ((counts (make-hash-table)))
    (dolist (entry (lexicon language))
      (count-phones-in-words (list (form entry)) counts))
    counts))

(defun analyze-corpus-frequencies (language corpus)
  "Count phone occurrences by rendering a corpus of semantic messages.
   CORPUS is a list of (description . semantic-form) pairs."
  (let ((counts (make-hash-table)))
    (dolist (phrase corpus)
      (handler-case
          (let ((rendered (render language (cdr phrase))))
            (count-phones-in-words rendered counts))
        (error () nil)))
    counts))

(defun merge-frequency-counts (corpus-counts lexicon-counts
                                &key (corpus-weight 0.75) (lexicon-weight 0.25))
  "Merge two phone count hash tables with weights.
   Returns separate consonant and vowel frequency alists."
  (let ((merged (make-hash-table))
        (all-phones nil))
    ;; Collect all phones from both tables
    (maphash (lambda (k v) (declare (ignore v)) (push k all-phones)) corpus-counts)
    (maphash (lambda (k v) (declare (ignore v)) (pushnew k all-phones)) lexicon-counts)
    ;; Weighted merge
    (dolist (phone all-phones)
      (let ((corpus-n (gethash phone corpus-counts 0))
            (lexicon-n (gethash phone lexicon-counts 0)))
        (setf (gethash phone merged)
              (+ (* corpus-weight corpus-n)
                 (* lexicon-weight lexicon-n)))))
    ;; Split into consonant and vowel alists
    (let ((consonants nil)
          (vowels nil))
      (maphash (lambda (phone freq)
                 (cond ((consonant-p phone) (push (cons phone freq) consonants))
                       ((vowel-p phone) (push (cons phone freq) vowels))))
               merged)
      (values (sort consonants #'> :key #'cdr)
              (sort vowels #'> :key #'cdr)))))

;;; Phone coalescence — merging similar phones for contact languages

(defun consonants-mergeable-p (c1 c2)
  "Can two consonants merge? Yes if they differ only in voicing (same place+manner)
   or only one step in place (same manner+voicing)."
  (and (consonant-p c1) (consonant-p c2)
       (not (equal (ipa c1) (ipa c2)))
       (or
        ;; Same place and manner, differ in voicing
        (and (equal (place c1) (place c2))
             (equal (manner c1) (manner c2))
             (not (equal (voicing c1) (voicing c2))))
        ;; Same manner and voicing, one step in place
        (and (equal (manner c1) (manner c2))
             (equal (voicing c1) (voicing c2))
             (<= (abs (- (place-number c1) (place-number c2))) 1)))))

(defun vowels-mergeable-p (v1 v2)
  "Can two vowels merge? Yes if they differ by at most one step in height
   or one step in backness (not both, and same rounding)."
  (and (vowel-p v1) (vowel-p v2)
       (not (equal (ipa v1) (ipa v2)))
       (equal (rounding v1) (rounding v2))
       (let ((dh (abs (- (height-number v1) (height-number v2))))
             (db (abs (- (backness-number v1) (backness-number v2)))))
         (or (and (<= dh 1) (= db 0))
             (and (= dh 0) (<= db 1))))))

(defun build-merge-map (freq-alist-1 freq-alist-2 mergeable-fn
                        &key (max-inventory nil))
  "Build a hash table mapping phones to their merged representative.
   Phase 1: Only merge across languages — a phone unique to lang-1 can
   merge into a phone from lang-2 (shared or unique-to-2), and vice versa.
   Same-language contrasts are preserved.
   Phase 2: If MAX-INVENTORY is set and the result is still too large,
   collapse the lowest-frequency mergeable pairs until under the limit."
  (let ((merge-map (make-hash-table))
        (phones-1 (mapcar #'car freq-alist-1))
        (phones-2 (mapcar #'car freq-alist-2))
        (combined (make-hash-table)))
    ;; Combined frequencies for picking winners
    (dolist (pair freq-alist-1)
      (incf (gethash (car pair) combined 0) (cdr pair)))
    (dolist (pair freq-alist-2)
      (incf (gethash (car pair) combined 0) (cdr pair)))
    ;; Phase 1: cross-language merges only
    (let ((shared (intersection phones-1 phones-2 :key #'ipa :test #'equal))
          (unique-1 (set-difference phones-1 phones-2 :key #'ipa :test #'equal))
          (unique-2 (set-difference phones-2 phones-1 :key #'ipa :test #'equal)))
      ;; Unique-to-1 can merge into shared or unique-to-2
      (let ((targets-for-1 (append shared unique-2)))
        (dolist (phone unique-1)
          (unless (gethash phone merge-map)
            (let ((best nil) (best-freq 0))
              (dolist (candidate targets-for-1)
                (when (and (not (equal (ipa phone) (ipa candidate)))
                           (not (gethash candidate merge-map))
                           (funcall mergeable-fn phone candidate)
                           (> (gethash candidate combined 0) best-freq))
                  (setf best candidate)
                  (setf best-freq (gethash candidate combined 0))))
              (when best
                (setf (gethash phone merge-map) best))))))
      ;; Unique-to-2 can merge into shared or unique-to-1
      (let ((targets-for-2 (append shared unique-1)))
        (dolist (phone unique-2)
          (unless (gethash phone merge-map)
            (let ((best nil) (best-freq 0))
              (dolist (candidate targets-for-2)
                (when (and (not (equal (ipa phone) (ipa candidate)))
                           (not (gethash candidate merge-map))
                           (funcall mergeable-fn phone candidate)
                           (> (gethash candidate combined 0) best-freq))
                  (setf best candidate)
                  (setf best-freq (gethash candidate combined 0))))
              (when best
                (setf (gethash phone merge-map) best)))))))
    ;; Phase 2: if still over max-inventory, collapse lowest-frequency pairs
    (when max-inventory
      (let ((surviving (remove-if (lambda (p) (gethash p merge-map))
                                  (remove-duplicates
                                   (append phones-1 phones-2)
                                   :key #'ipa :test #'equal))))
        (iter (while (> (length surviving) max-inventory))
          (let ((worst nil) (worst-freq most-positive-fixnum)
                (worst-target nil))
            ;; Find the lowest-frequency phone that has a mergeable partner
            (dolist (phone surviving)
              (when (< (gethash phone combined 0) worst-freq)
                (dolist (candidate surviving)
                  (when (and (not (equal (ipa phone) (ipa candidate)))
                             (funcall mergeable-fn phone candidate)
                             (> (gethash candidate combined 0)
                                (gethash phone combined 0)))
                    (setf worst phone)
                    (setf worst-freq (gethash phone combined 0))
                    (setf worst-target candidate)))))
            (if worst
                (progn
                  (setf (gethash worst merge-map) worst-target)
                  (setf surviving (remove worst surviving)))
                (return))))))
    merge-map))

(defun resolve-merge (phone merge-map)
  "Follow merge chains to find the final representative phone."
  (let ((target (gethash phone merge-map)))
    (if target
        (resolve-merge target merge-map)
        phone)))

(defun coalesce-frequencies (freq-alist-1 freq-alist-2 mergeable-fn
                              &key max-inventory)
  "Merge two frequency alists by coalescing similar phones.
   Returns a single frequency alist with merged phones."
  (let ((merge-map (build-merge-map freq-alist-1 freq-alist-2 mergeable-fn
                                    :max-inventory max-inventory))
        (result (make-hash-table)))
    ;; Accumulate all frequencies, redirecting merged phones
    (dolist (pair freq-alist-1)
      (let ((target (resolve-merge (car pair) merge-map)))
        (incf (gethash target result 0) (cdr pair))))
    (dolist (pair freq-alist-2)
      (let ((target (resolve-merge (car pair) merge-map)))
        (incf (gethash target result 0) (cdr pair))))
    ;; Convert to alist
    (let ((alist nil))
      (maphash (lambda (k v) (push (cons k v) alist)) result)
      (values (sort alist #'> :key #'cdr)
              merge-map))))

(defun analyze-frequencies (language corpus)
  "Analyze a language's actual phone frequencies from corpus (75%) and lexicon (25%).
   Returns (values consonant-freqs vowel-freqs) as alists of (phone . count)."
  (let ((corpus-counts (analyze-corpus-frequencies language corpus))
        (lexicon-counts (analyze-lexicon-frequencies language)))
    (merge-frequency-counts corpus-counts lexicon-counts)))

;;; Contact languages — pidginization

(defclass contact-language (proto-language)
  ((superstrate :accessor superstrate :initarg :superstrate)
   (substrate   :accessor substrate   :initarg :substrate)
   (merge-map   :accessor merge-map   :initarg :merge-map :initform nil)))

(defun find-nearest-phone (phone inventory)
  "Find the closest phone in INVENTORY to PHONE using distance metrics."
  (let ((best nil)
        (best-dist most-positive-fixnum))
    (dolist (candidate inventory)
      (let ((dist (if (consonant-p phone)
                      (if (consonant-p candidate)
                          (consonant-distance phone candidate)
                          most-positive-fixnum)
                      (if (vowel-p candidate)
                          (vowel-distance phone candidate)
                          most-positive-fixnum))))
        (when (< dist best-dist)
          (setf best candidate)
          (setf best-dist dist))))
    best))

(defun remap-phone (phone merge-map inventory)
  "Remap a phone through the merge-map, then nearest-match if not in inventory."
  (let ((resolved (resolve-merge phone merge-map)))
    (if (member resolved inventory :key #'ipa :test #'equal)
        resolved
        (or (find-nearest-phone resolved inventory) resolved))))

(defun remap-form (form merge-map inventory)
  "Walk a word form's phone-points, remapping each phone, then reanalyze."
  (let ((phones-only (remove-if-not #'phone-p form)))
    (reanalyze
     (mapcar (lambda (pp)
               (let ((raw (if (typep pp 'phone-point) (phone pp) pp)))
                 (let ((remapped (remap-phone raw merge-map inventory)))
                   (if (typep pp 'phone-point)
                       (make-instance 'phone-point
                                      :phone remapped
                                      :stress (stress pp))
                       remapped))))
             phones-only))))

(defun remap-marker (marker-ipa-list merge-map inventory)
  "Remap a grammar marker (list of IPA strings) through the merge-map."
  (when marker-ipa-list
    (serialize-form
     (remap-form (reanalyze (mapcar #'ensure-phone marker-ipa-list))
                 merge-map inventory))))

;;; Contact lexicon construction

(defparameter *substrate-domains* '(:body :kinship :nature :animal :agriculture :food))
(defparameter *superstrate-domains* '(:combat :commerce :society :religion :architecture :magic))

(defun build-contact-lexicon (superstrate substrate merge-map inventory
                              &key (superstrate-ratio 0.75))
  "Build a mixed lexicon from superstrate and substrate, biased by domain."
  (let ((sup-lexicon (lexicon superstrate))
        (sub-lexicon (lexicon substrate))
        (all-glosses (remove-duplicates
                      (append (mapcar #'gloss (lexicon superstrate))
                              (mapcar #'gloss (lexicon substrate)))
                      :test #'equal))
        (result nil))
    (dolist (gl all-glosses)
      (let* ((sup-entry (find gl sup-lexicon :key #'gloss :test #'equal))
             (sub-entry (find gl sub-lexicon :key #'gloss :test #'equal))
             (entry-domain (or (and sup-entry (domain sup-entry))
                               (and sub-entry (domain sub-entry))))
             (use-sup-p (cond
                          ((and sup-entry (not sub-entry)) t)
                          ((and sub-entry (not sup-entry)) nil)
                          ((member entry-domain *substrate-domains*)
                           (< (random 1.0) 0.15))
                          ((member entry-domain *superstrate-domains*)
                           (< (random 1.0) 0.85))
                          (t (< (random 1.0) superstrate-ratio))))
             (source-entry (if use-sup-p sup-entry sub-entry))
             (source-name (if use-sup-p
                              (lang-name superstrate)
                              (lang-name substrate))))
        (when source-entry
          (push (make-instance 'lexical-entry
                               :gloss gl
                               :form (remap-form (form source-entry) merge-map inventory)
                               :category (category source-entry)
                               :origin (cons :contact source-name)
                               :domain entry-domain
                               :noun-class (noun-class source-entry)
                               :inflected-forms
                               (mapcar (lambda (pair)
                                         (cons (car pair)
                                               (remap-form (cdr pair) merge-map inventory)))
                                       (inflected-forms source-entry)))
                result))))
    result))

;;; Grammar creolization

(defun creolize-grammar (substrate-grammar merge-map inventory)
  "Simplify and creolize a substrate grammar for a contact language."
  (let ((result (copy-list substrate-grammar)))
    ;; Strip agreement
    (setf (getf result :agreement-type) :none)
    (setf (getf result :pro-drop) nil)
    ;; Analytic drift: affixes → particles
    (setf (getf result :morphology)
          (mapcar (lambda (rule)
                    (let ((r (copy-morpheme-rule rule)))
                      (let ((feat (mrule-feature r)))
                        (if (and (symbolp feat)
                                 (let ((name (symbol-name feat)))
                                   (and (>= (length name) 4)
                                        (string= name "AGR-" :end1 4))))
                            ;; Strip agreement rules
                            (progn
                              (setf (mrule-strategy r) :none)
                              (setf (mrule-marker r) nil))
                            ;; Non-agreement: analytic drift + remap
                            (progn
                              (when (eql (mrule-strategy r) :suffix)
                                (setf (mrule-strategy r) :particle-after))
                              (when (eql (mrule-strategy r) :prefix)
                                (setf (mrule-strategy r) :particle-before))
                              (when (mrule-marker r)
                                (setf (mrule-marker r)
                                      (remap-marker (mrule-marker r) merge-map inventory))))))
                      r))
                  (getf substrate-grammar :morphology)))
    ;; Remap top-level markers
    (when (getf result :copula-marker)
      (setf (getf result :copula-marker)
            (remap-marker (getf result :copula-marker) merge-map inventory)))
    (when (getf result :conjunction-marker)
      (setf (getf result :conjunction-marker)
            (remap-marker (getf result :conjunction-marker) merge-map inventory)))
    (when (getf result :compound-linker)
      (setf (getf result :compound-linker)
            (remap-marker (getf result :compound-linker) merge-map inventory)))
    (when (getf result :question-particle)
      (setf (getf result :question-particle)
            (remap-marker (getf result :question-particle) merge-map inventory)))
    (when (getf result :disjunction-marker)
      (setf (getf result :disjunction-marker)
            (remap-marker (getf result :disjunction-marker) merge-map inventory)))
    (when (getf result :reflexive-marker)
      (setf (getf result :reflexive-marker)
            (remap-marker (getf result :reflexive-marker) merge-map inventory)))
    (when (getf result :exclamation-particle)
      (setf (getf result :exclamation-particle)
            (remap-marker (getf result :exclamation-particle) merge-map inventory)))
    (when (getf result :simile-marker)
      (setf (getf result :simile-marker)
            (remap-marker (getf result :simile-marker) merge-map inventory)))
    (when (getf result :topic-particle)
      (setf (getf result :topic-particle)
            (remap-marker (getf result :topic-particle) merge-map inventory)))
    (when (getf result :reciprocal-marker)
      (setf (getf result :reciprocal-marker)
            (remap-marker (getf result :reciprocal-marker) merge-map inventory)))
    (when (getf result :nominalization-marker)
      (setf (getf result :nominalization-marker)
            (remap-marker (getf result :nominalization-marker) merge-map inventory)))
    (when (getf result :distributive-marker)
      (setf (getf result :distributive-marker)
            (remap-marker (getf result :distributive-marker) merge-map inventory)))
    ;; Creolization strips verb morphology — drift demote :verb-morphology → :impersonal
    (when (eql (getf result :demote-strategy) :verb-morphology)
      (setf (getf result :demote-strategy) :impersonal))
    ;; Simplify pronouns
    (setf (getf result :pronoun-collapse) :collapse-number)
    (setf (getf result :plural-pronoun-strategy) :quantifier-after)
    ;; Creolization strips noun class distinctions
    (setf (getf result :noun-class-count) 0)
    (setf (getf result :noun-classes) nil)
    (setf (getf result :noun-class-scheme) nil)
    ;; Aggressive topic-drop for contact languages (maximally analytic)
    ;; High probability but not guaranteed — each feature rolls independently
    (setf (getf result :topic-drop)
          (list :subject-pronoun (< (random 1.0) 0.9)
                :possessor-pronoun (< (random 1.0) 0.9)
                :shared-tense (< (random 1.0) 0.85)
                :shared-case (< (random 1.0) 0.8)
                :conjunction-marker (< (random 1.0) 0.5)))
    result))

;;; Main pidginize function

(defun pidginize (superstrate substrate corpus
                  &key name (superstrate-ratio 0.75)
                       (max-consonants 12) (max-vowels 5))
  "Create a contact language from a superstrate (lexicon donor) and
   substrate (grammar donor), with merged phonology and creolized grammar."
  (multiple-value-bind (sup-cons sup-vows)
      (analyze-frequencies superstrate corpus)
    (multiple-value-bind (sub-cons sub-vows)
        (analyze-frequencies substrate corpus)
      ;; Coalesce consonant and vowel inventories
      (multiple-value-bind (merged-cons c-merge-map)
          (coalesce-frequencies sup-cons sub-cons #'consonants-mergeable-p
                                :max-inventory max-consonants)
        (multiple-value-bind (merged-vows v-merge-map)
            (coalesce-frequencies sup-vows sub-vows #'vowels-mergeable-p
                                  :max-inventory max-vowels)
          ;; Combine merge maps
          (let ((merge-map (make-hash-table)))
            (maphash (lambda (k v) (setf (gethash k merge-map) v)) c-merge-map)
            (maphash (lambda (k v) (setf (gethash k merge-map) v)) v-merge-map)
            (let* ((inventory (append (mapcar #'car merged-cons)
                                      (mapcar #'car merged-vows)))
                   (contact (make-instance 'contact-language
                                           :name (or name "pidgin")
                                           :consonant-frequencies merged-cons
                                           :vowel-frequencies merged-vows
                                           :onset-min 0
                                           :onset-max 1
                                           :coda-min 0
                                           :coda-max 1
                                           :superstrate superstrate
                                           :substrate substrate
                                           :merge-map merge-map)))
              ;; Build mixed lexicon
              (setf (lexicon contact)
                    (build-contact-lexicon superstrate substrate
                                          merge-map inventory
                                          :superstrate-ratio superstrate-ratio))
              ;; Creolize grammar from substrate
              (setf (grammar contact)
                    (creolize-grammar (grammar substrate) merge-map inventory))
              ;; Generate pronouns, modal auxiliaries, and wh-words with new phonology
              (generate-pronoun-lexicon contact)
              (generate-modal-lexicon contact)
              (generate-wh-lexicon contact)
              contact)))))))

