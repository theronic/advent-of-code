(ns advent-of-code.year2021.day21
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [com.rpl.specter :as S]))

;(defn det-die [n]
;  ())

;(def roll! (iterate (fn [!die] (swap! !die inc)) !die))
;(take 3 (repeatedly #(swap! !die inc)))

(comment
  (first roll!)
  (take 3 (roll!)))

;(defn roll! [!die]
;  (iterate)
;  (swap))

;(def !die (atom 0))

(defn win? [{:keys [players]}]
  (some #(>= % 1000) (map :score players)))

(comment

  (win? {:players [{:score 1} {:score 1000}]})

  (inc (mod 99 100))

  (drop-while
    (complement win?)))

(inc (mod (dec 11) 10))

(def *x (first (drop-while (complement win?)
                 (iterate (fn [{:as state :keys [!die round-num players]}]
                            (let [pre-die @!die]
                              ;(prn "iterate" state !die)
                              ;(prn (repeatedly 3 #(swap! !die inc)))
                              {:!die      !die
                               :pre-die   pre-die
                               :round-num (inc round-num)
                               :players   (mapv (fn [{:as player :keys [pos score]}]
                                                  (let [dies (mapv #(mod % 100) (repeatedly 3 #(swap! !die inc)))
                                                        _    (prn dies)
                                                        pos' (inc (mod (dec (+ pos (apply + dies))) 10))]
                                                    (assoc player
                                                      :pre-score score
                                                      :score (+ score pos')
                                                      :dies dies
                                                      :pos pos')))
                                            players)}))
                   {:!die      (atom 0)
                    :round-num 0
                    :players   [{:number 1 :score 0 :pos 4}
                                {:number 2 :score 0 :pos 8}]}))))

(mapv (fn [{:as player :keys [pos score]}]
        (let [dies (mapv #(mod % 100) (repeatedly 3 #(swap! !die inc)))
              _    (prn dies)
              pos' (inc (mod (dec (+ pos (apply + dies))) 10))]
          (assoc player
            :pre-score score
            :score (+ score pos')
            :dies dies
            :pos pos')))
  players)

;; messing around:

(defn with-die
  ;[score die]
  ;(+ score die)
  ;(prn "iterate" state !die)
  ;(prn (repeatedly 3 #(swap! !die inc)))
  [{:as player :keys [pos score]} die]
  (let [dies (mapv #(mod % 100) (repeatedly 3 #(swap! !die inc)))
        _    (prn dies)
        pos' (inc (mod (dec (+ pos (apply + dies))) 10))]
    (assoc player
      :pre-score score
      :score (+ score pos')
      :dies dies
      :pos pos')))

;[{:number 1 :score 0 :pos 10}
;               {:number 2 :score 0 :pos 8}]

(defn winner? [score]
  (>= score 5))

;(winner? (with-die 4 5))

(defn play
  [p1 p2 die]
  ;; if p1 wins, return true. if p2 wins, return false, otherwise recurse into the 3 possible worlds.
  ; returns [p1-wins p2-wins]
  (let [p1' (with-die p1 die)]
    (if (winner? p1')
      [true]
      (let [p2' (with-die p2 die)]
        (if (winner? p2')
          [false]
          (mapcat #(play p1' p2' %) [1 2 3]))))))

(comment
  (frequencies (play 0 3 1)))


(def *x (first (drop-while (complement win?)
                 (iterate (fn [{:as state :keys [!die round-num players]}]
                            (let [pre-die @!die]
                              ;(prn "iterate" state !die)
                              ;(prn (repeatedly 3 #(swap! !die inc)))
                              {:!die      !die
                               :pre-die   pre-die
                               :round-num (inc round-num)
                               :players   (mapv (fn [{:as player :keys [pos score]}]
                                                  (let [dies (mapv #(mod % 100) (repeatedly 3 #(swap! !die inc)))
                                                        _    (prn dies)
                                                        pos' (inc (mod (dec (+ pos (apply + dies))) 10))]
                                                    (assoc player
                                                      :pre-score score
                                                      :score (+ score pos')
                                                      :dies dies
                                                      :pos pos')))
                                            players)}))
                   {:!die      (atom 0)
                    :round-num 0
                    :players   [{:number 1 :score 0 :pos 10}
                                {:number 2 :score 0 :pos 8}]}))))

*x

(* 753 (+ 3 996))