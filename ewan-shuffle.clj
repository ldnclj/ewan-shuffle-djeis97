(ns ewan-shuffle)

(def ranked-products
  [{:rank 4 :provider "BT"     :product "High Speed Fiber"}
   {:rank 2 :provider "Virgin" :product "Ultra High Speed Fiber"}
   {:rank 5 :provider "Sky"    :product "Bog Standard Fiber"}
   {:rank 1 :provider "Virgin" :product "SuperX"}
   {:rank 3 :provider "Virgin" :product "High Speed Fiber"}])

(defn drop-first-of-group [groups]
  (if (seq (rest (first groups)))
    (cons (rest (first groups)) (rest groups))
    (rest groups)))

(defn extract-first-first [groups]
  [(first (first groups))
   (drop-first-of-group groups)])
(defn extract-first-second [groups]
  [(first (second groups))
   (cons (first groups)
         (drop-first-of-group (rest groups)))])

(defn run-of-2? [sorted]
  (and (seq sorted)
       (= (:provider (first sorted))
          (:provider (second sorted)))))

(defn extract-next [sofar groups]
  (if (and (run-of-2? sofar)
           (seq (rest groups)))
    (extract-first-second groups)
    (extract-first-first groups)))

(defn n-merge-sort [groups]
  (loop [groups groups
         sorted ()]
    (if (seq groups)
      (let [[selected new] (->> groups
                                (sort-by (comp first first))
                                (extract-next sorted))]
        ;; (pprint {:groups groups :sorted sorted-groups
        ;;          :selected selected :new new})
        (recur new (cons selected sorted)))
      (reverse sorted))))

(defn shuffle [products]
  (->> products
       (group-by :provider)
       (map second)
       (map #(sort-by :rank %))
       n-merge-sort))
