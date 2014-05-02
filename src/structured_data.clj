(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ y] v]
    (+ x y)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[ [x1 _] [x2 _]] rectangle]
    (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[ [_ x1] [_ x2]] rectangle]
    (Math/abs (- x1 x2))))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle)
     (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[rx1 ry1] [rx2 ry2]] rectangle
        [x y] point]
    (and (<= rx1 x rx2)
         (<= ry1 y ry2))))

(defn contains-rectangle? [outer inner]
  (let [[bl tr] inner]
    (and (contains-point? outer bl)
         (contains-point? outer tr))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (not (== (author-count book) 1)))

(defn add-author [book new-author]
  (let [x (:authors book)
        new (conj x new-author)]
    (assoc book :authors (conj (:authors book) new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map #(second %) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (reduce str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq)
           (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map #(:authors %) books)))

(defn all-author-names [books]
  (set (map :name  (authors books))))

(defn author->string [author]
  (let [n (:name author)
        b (:birth-year author)
        d (:death-year author)]
    (cond d (str n " (" b " - " d ")")
          b (str n " (" b " - "  ")")
          :else (str n))))


(defn authors->string [authors]
  (apply  str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [bookname (:title book)
        authors  (authors->string (:authors book))]
    (str bookname ", written by " authors)))

(defn books->string [books]
  (let [n (count books)
        lst (apply str (interpose ". " (map book->string books)))]
    (cond (== n 0) "No books."
          (== n 1) (str "1 book. " lst ".")
          :else    (str n " books. " lst "."))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter #(alive? %) authors))

(defn has-a-living-author? [book]
  (not (empty? (filter #(alive? %) (:authors book)))))

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books))

; %________%
