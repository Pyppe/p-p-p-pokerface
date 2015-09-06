(ns p-p-p-pokerface)

; http://iloveponies.github.io/120-hour-epic-sax-marathon/p-p-p-pokerface.html

(defn rank [card]
  (let [[r _] card
        replacements {\T 10
                      \J 11
                      \Q 12
                      \K 13
                      \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get replacements r)
      )))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn rank-frequencies [hand]
  (let [ranks (map rank hand)]
    (frequencies ranks)))

(defn suit-frequencies [hand]
  (let [suits (map suit hand)]
    (frequencies suits)))

(defn contains-of-n-kind [hand n]
  (contains? (set (vals (rank-frequencies hand))) n))

(defn pair? [hand]
  (contains-of-n-kind hand 2))

(defn three-of-a-kind? [hand]
  (contains-of-n-kind hand 3))

(defn four-of-a-kind? [hand]
  (contains-of-n-kind hand 4))

(defn flush? [hand]
  (== (count (set (vals (suit-frequencies hand)))) 1))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= 2 (get (frequencies (vals (rank-frequencies hand))) 2)))

; hhngh.. ugleeeh!
(defn straight? [hand]
  (let [ranks (map rank hand)
        is-numeric-straight?
          (fn [sorted-ranks]
            (and
              (apply < sorted-ranks)
              (== (- (apply max sorted-ranks) (apply min sorted-ranks)) 4)))]
    (or
      (is-numeric-straight? (sort ranks))
      (is-numeric-straight? (sort (replace {14 1} ranks))))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn value [hand]
  (cond
    (straight-flush?  hand) 8
    (four-of-a-kind? hand)  7
    (full-house? hand)      6
    (flush? hand)           5
    (straight? hand)        4
    (three-of-a-kind? hand) 3
    (two-pairs? hand)       2
    (pair? hand)            1
    :else                   0))
