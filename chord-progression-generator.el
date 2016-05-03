(defun sub-chord(chords)
  (print chords)
  (mapcar #'get-sub chords))

(defun flatten(l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (apply #'append (mapcar #'flatten l)))))

;;create chord sub hash table
(setq chord-subs (make-hash-table :test 'equal))

;;currently implemented substitutions:
;;ii V I substitution/addition
;;tonic substitution
;;tritone substitution

(puthash "I" '(("ii" "V" "I") ("iii") ("vi")) chord-subs)
(puthash "V" '(("bii0")) chord-subs)




(defun get-sub(chord)
  (if (gethash chord chord-subs)
      (choose-random (gethash chord chord-subs))
    chord))

(get-sub "V")
  


;;create hash table
(setq chord-progressions (make-hash-table :test 'equal))
(puthash "I" '("I" "ii" "iii" "IV" "V" "vi" "vii0") chord-progressions)
(puthash "i" '("i" "VII" "III" "VI" "ii0" "iv" "V" "vii0") chord-progressions)


(puthash "iii" '( "vi" ) chord-progressions)
(puthash "VII" '( "III" ) chord-progressions)

(puthash "vi" '( "ii" "IV" ) chord-progressions)
(puthash "III" '( "VI" ) chord-progressions)
(puthash "VI" '( "ii0 iv" ) chord-progressions)

;;predominants
(puthash "ii" '("V") chord-progressions)
(puthash "IV" '("vii0") chord-progressions)
(puthash "ii0" '("V") chord-progressions)
(puthash "iv" '("vii0" "VII") chord-progressions)

;;dominants
(puthash "V" '("I") chord-progressions)
(puthash "vii0" '("I" "iii") chord-progressions)

(defun get-progression(start-chord min-len)

  ;;TODO - instead of just making sure we end on a 1, track the tonic over multiple keys and end on that
  
  ;;if the minimum progression length has been met, and we are on a 1, stop
  (if (and (equal min-len 0) (or (equal start-chord "I") (equal start-chord "i")) )
      (list start-chord)
    (progn
      ;;if the minimum progression length has been met, but we are not on a 1, continue
      ;;by incrementing the minimum length by 1. This will keep the process going until
      ;;we hit a 1.
      (if (equal min-len 0)
          (append (list start-chord)
                (get-progression (choose-chord start-chord) (+ min-len 1)))
        
        (append (list start-chord)
                (get-progression (choose-chord start-chord) (- min-len 1)))))))

(defun choose-random(input-list)
  (nth 
   (random (length input-list))
   input-list))

(defun choose-chord (start-chord)
  (choose-random (gethash start-chord chord-progressions)))


(gethash "I" chord-progressions)
(choose-chord "I")
(print (get-progression "I" 7))

(flatten (sub-chord (flatten (sub-chord '("I" "V" "I")))))

(flatten (sub-chord (flatten (sub-chord (get-progression "I" 3)))))
