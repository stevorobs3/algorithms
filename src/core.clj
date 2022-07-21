(ns core
  (:gen-class))

(defn -main []
  (println "(-main) invoked"))


(defn diagonal-difference [data]
  (let [n         (count (or (first data) []))
        left-sum  (->> (range n)
                       (map #(get-in data [% %]))
                       (reduce + 0))
        right-sum (->> (range n)
                       (map #(get-in data [% (- (dec n) %)]))
                       (reduce + 0))]
    (Math/abs (- left-sum right-sum))))

(defn plus-minus [arr]
  (let [n         (count arr)
        {:keys [pos zero neg]} (group-by (fn [num]
                                           (cond
                                             (neg? num) :neg
                                             (pos? num) :pos
                                             :else :zero))
                                         arr)
        print-num #(println (format "%.6f" (double (/ % n))))]
    (print-num (count pos))
    (print-num (count neg))
    (print-num (count zero))))

(defn staircase [n]
  (doseq [m (range n)]
    (println (format (str "%" n "s") (apply str (repeat (inc m) "#"))))))

(defn miniMaxSum [arr]
  (let [n    (count arr)
        nums (sort arr)]
    (println (->> nums
                  (take (dec n))
                  (apply +))
             (->> nums
                  (take-last (dec n))
                  (apply +)))))

;; square
;; a-z
;; sort the rows
;; are columns in ascending order?
;; early termination / greedy?
;; parallelised at the end?
;; sort will be n Lg n for each row = m*n log n
;; in this case up to 100 characters
(defn gridChallenge [grid]
  ;; naive n^2 solution
  (if (true? (->> grid
                  (map sort)
                  (apply map (fn [& args]
                               (apply <= (map int args))))
                  (apply = true)))
    "YES"
    "NO"))

(comment
  (let [n    10
        data (->> (range n)
                  (map (fn [i]
                         (->> (range n)
                              (map (fn [_]
                                     (char (rand-nth (range (int \a) (inc (int \z)))))))
                              (apply str)))))
        data ["abi"
              "def"
              "ghi"]]
    (time (gridChallenge data)))

  (miniMaxSum [1 3 5 7 9])

  (staircase 5)
  (plus-minus [1 1 1 1 0 0 -1])
  (diagonal-difference [[1 2 4]
                        [4 5 6]
                        [7 8 9]])
  )