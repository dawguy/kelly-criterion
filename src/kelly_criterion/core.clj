(ns kelly-criterion.core)

; Assumings odds work like this since I forget how they actually work.
; There are certainly multiple different type of odds, and I think what I'm describing
; here does count as one of them.
;
; Bet 100
; Winning total = 200
; Losing total = 0
; Odds 1:1 or 1.0
;
; Bet 100
; Winning total = 300
; Losing total = 0
; Odds 2:1 or 2.0
;
; To make all my math simple right now though I'm going to do all calculations based
; on a payout value assuming a 100 bet.

(defn bet-to-odds [payout] (/ payout 100))

(defn normalize-probs [chances]
  (let [prob-sum (reduce #(+ %1 (:prob %2)) 0 chances)] (map #(assoc %1 :prob (/ (:prob %1) prob-sum)) chances)))

(defn expected-value [chances]
  (reduce + 0 (map #(* (:prob %) (/ (:payout %) 100)) chances))
)

(defn translate-to-bet-payoff [chances]
  (map #(array-map :prob (:prob %) :payout (- (/ (:payout %1) 100) 1)) chances))

; I don't have the formula for this memorized :'(
; Going to simulate it based on averaging a couple thousand runs of a couple thousand bets
(defn growth-rate [chances bet-size]
  ; https://math.stackexchange.com/questions/662104/kelly-criterion-with-more-than-two-outcomes
  ; sum(p * (log(1 + odds * bet-size) + log(M))
  (reduce + 0 (map #(* (:prob %) (Math/log (+ 1 (* (:payout %) bet-size)))) (translate-to-bet-payoff (normalize-probs chances))))
  )

(defn find-growth-rates [chances] (map #(vector %1 (growth-rate chances (/ %1 100))) (range 101)))

(defn optimal-growth-rate [chances]
  "Calculates the optimal whole percentage growth rate based on a given set of probabilities."
  (let [v (find-growth-rates chances)]
    (loop [a (first v)
           b (second v)
           rem (rest (rest v))]
      (if (< (second a) (second b))
        (recur b (first rem) (rest rem))
        a))
    ))

(defn simulate-bet [chances bet-amount]
  (loop [dart (rand)
         [c & rem-chances] chances]
    (if (< 0 (- dart (:prob c)))
      (recur (- dart (:prob c)) rem-chances)
      (* bet-amount (bet-to-odds (:payout c)))
    )
  ))

(defn chances-seq [chances times] (take times (for [x (range times) y [(simulate-bet chances times)] :while (< x times)] y)))
(defn chances-counts [s] (reduce #(assoc %1 %2 (inc (get %1 %2 0))) {} s))

(defn simulate-bets [chances bet-percent initial-pool num-bets]
  (loop [n num-bets
         money initial-pool]
      (if (> n 0)
        (if (< 0.1 money)
          (recur (dec n) (+ (- money (* bet-percent money)) (simulate-bet chances (* bet-percent money))))
        0)
      money)
  ))

(defn calculate-best-growth-rate [chances]
  (take 101 (for [x (range 101) :while (< x 101)]
              (let [l (take 2500 (repeatedly #(simulate-bets chances (double (/ x 100)) 1 500)))]
                [x (/ (reduce + 0 l) (count l))])
              )))

(comment
"A list of runnable helpful commands"
[]
  (def bet-size 0.1)
  (def chances [{:prob 0.6 :payout 200} {:prob 0.4 :payout 0}])
  (def chances [{:prob 0.6 :payout 200} {:prob 0.4 :payout 0.25}])
  (def chances [{:prob 0.15 :payout 300} {:prob 0.35 :payout 200} {:prob 0.5 :payout 0}])
  (def chances [{:prob 0.3 :payout 300} {:prob 0.7 :payout 200} {:prob 1.0 :payout 0}])
  (def chances [{:prob 0.1 :payout 1250} {:prob 0.9 :payout 0}])
  (def chances [{:prob 0.025 :payout 1500} {:prob 0.1 :payout 500} {:prob 0.2 :payout 300} {:prob 0.15 :payout 150} {:prob 0.35 :payout 50} {:prob 0.175 :payout 20}])
  (def chances [{:prob 0.02 :payout 2000} {:prob 0.03 :payout 1500} {:prob 0.05 :payout 1000} {:prob 0.05 :payout 500} {:prob 0.25 :payout 250} {:prob 0.15 :payout 150} {:prob 0.45 :payout 25}])
  (def chances-run (chances-counts (chances-seq chances 10000)))
  (calculate-best-growth-rate chances)
)