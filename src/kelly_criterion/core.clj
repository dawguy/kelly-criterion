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

(defn expected-value [chances]
  (reduce + 0 (map #(* (:prob %) (:payout %)) chances))
)

; I don't have the formula for this memorized :'(
; Going to simulate it based on averaging a couple thousand runs of a couple thousand bets
(defn growth-rate [chances bet-size]
  )

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
  (prn "yo dog")
  (def chances [{:prob 0.15 :payout 300} {:prob 0.35 :payout 200} {:prob 0.5 :payout 0}])
  (def chances [{:prob 0.1 :payout 1250} {:prob 0.9 :payout 0}])
  (def chances [{:prob 0.025 :payout 1500} {:prob 0.1 :payout 500} {:prob 0.2 :payout 300} {:prob 0.15 :payout 150} {:prob 0.35 :payout 50} {:prob 0.175 :payout 20}])
  (def chances [{:prob 0.02 :payout 2000} {:prob 0.03 :payout 1500} {:prob 0.05 :payout 1000} {:prob 0.05 :payout 500} {:prob 0.25 :payout 250} {:prob 0.15 :payout 150} {:prob 0.45 :payout 25}])
  (def chances-run (chances-counts (chances-seq chances 10000)))
  (calculate-best-growth-rate chances)
)