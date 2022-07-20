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
  "Generates series of randomly picked outcomes and groups them."
  (reduce + 0 (map #(* (:prob %) (/ (:payout %) 100)) chances))
)

(defn translate-to-bet-payoff [chances]
  "Updates all :prob values from being 100 based to being 1 based. This allows the growth-rate formula to work without added complexity of in-function normalization."
  (map #(array-map :prob (:prob %) :payout (- (/ (:payout %1) 100) 1)) chances))

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
      (if (nil? b) a
         (if (< (second a) (second b))
        (recur b (first rem) (rest rem))
        a)))
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

(defn calculate-best-growth-rate [unnormalized-chances]
  (let [chances (normalize-probs unnormalized-chances)] (take 101 (for [x (range 101) :while (< x 101)]
              (let [l (take 2500 (repeatedly #(simulate-bets chances (double (/ x 100)) 1 500)))]
                [x (/ (reduce + 0 l) (count l))])
              ))))

(comment
  "A list of runnable helpful commands"
  []
  (def bet-size 0.1)
  (def chances [{:prob 0.53 :payout 200} {:prob 0.47 :payout 0}])
  (def chances [{:prob 0.6 :payout 200} {:prob 0.4 :payout 0}])
  (def chances [{:prob 0.6 :payout 200} {:prob 0.4 :payout 0.25}])
  (def chances [{:prob 0.15 :payout 300} {:prob 0.35 :payout 200} {:prob 0.5 :payout 0}])
  (def chances [{:prob 0.3 :payout 300} {:prob 0.7 :payout 200} {:prob 1.0 :payout 0}])
  (def chances [{:prob 0.1 :payout 1250} {:prob 0.9 :payout 0}])
  (def chances [{:prob 0.1 :payout 200} {:prob 0.7 :payout 170} {:prob 0.2 :payout 66}])
  (def chances [{:prob 0.025 :payout 1500} {:prob 0.1 :payout 500} {:prob 0.2 :payout 300} {:prob 0.15 :payout 150} {:prob 0.35 :payout 50} {:prob 0.175 :payout 20}])
  (def chances [{:prob 0.05 :payout 2000} {:prob 0.05 :payout 1500} {:prob 0.1 :payout 1000} {:prob 0.1 :payout 500} {:prob 0.25 :payout 250} {:prob 0.15 :payout 150} {:prob 0.2 :payout 66}])

  (expected-value chances)
  (chances-counts (chances-seq chances 10000))
  (normalize-probs chances)
  (translate-to-bet-payoff chances)

  ; Formula based growth rates
  (growth-rate chances 0.05)
  (find-growth-rates chances)                               ; Returns a complete list of growth rates at every whole percentage value
  (optimal-growth-rate chances)

  ; Following functions build upon each other.
  ; simulate-bet plays a single bet.
  ; simulate-bets plays multiple bets one after another with a given percentage bet-size.
  ; calculate-best-growth-rate simulates-bets for all bet-sizes from 0-100
  (simulate-bet chances 100)
  (simulate-bets chances 0.05 100 1000)
  (def chances [{:prob 0.02 :payout 2000} {:prob 0.03 :payout 1500} {:prob 0.05 :payout 1000} {:prob 0.05 :payout 500} {:prob 0.25 :payout 250} {:prob 0.15 :payout 150} {:prob 0.45 :payout 25}])
  (def chances-run (chances-counts (chances-seq chances 10000)))
  (def bet-array [[0 1.0] [1 13.475269801657726] [2 180.9801791405882] [3 2360.2081134892555] [4 30824.136103187953] [5 398069.0353677065] [6 5078327.839510582] [7 6.2832202304948196E7] [8 7.664069801506547E8] [9 9.268237232711592E9] [10 1.0812221925376405E11] [11 1.3746803342727988E12] [12 1.5896377126257244E13] [13 1.8344334164206278E14] [14 2.1006616291516792E15] [15 2.387129675757052E16] [16 2.28857726235517984E17] [17 2.6483960063067366E18] [18 3.051225169883811E19] [19 3.0317427269928734E20] [20 3.740581888495892E21] [21 3.5021325766897166E22] [22 4.0027229954240826E23] [23 3.336087632231492E24] [24 4.23774613241411E25] [25 3.863320128512053E26] [26 3.586978855122996E27] [27 3.3577143160269397E28] [28 2.0844201417207088E30] [29 4.350845572355788E30] [30 4.0012424102873313E31] [31 3.084134944780243E32] [32 3.777984942765465E33] [33 1.8491404079690313E34] [34 3.3163561112374385E35] [35 2.6894735783903486E36] [36 2.168463593725525E37] [37 1.036409378004699E38] [38 1.5413378908870618E39] [39 1.998660094177145E40] [40 8.145400816922738E40] [41 7.741147548075008E41] [42 5.147493580051033E42] [43 5.195435475338659E43] [44 8.951820040584623E44] [45 1.0705302342041236E46] [46 1.7673795232361186E46] [47 3.120741710958852E48] [48 2.4684581353816718E48] [49 4.871885905419702E48] [50 1.6605412296130462E50] [51 4.1427277429077244E50] [52 5.017190139473112E51] [53 2.868681298859896E52] [54 1.702802529258536E53] [55 5.0000595279145976E54] [56 2.0011947409840762E55] [57 2.536761052432268E55] [58 8.633420897710606E56] [59 4.7100242270620836E57] [60 2.042471571565663E58] [61 2.2279955124387888E58] [62 3.645346337422533E59] [63 4.2387102742269114E61] [64 6.144900310615245E61] [65 1.420807536873408E63] [66 4.707678666229275E62] [67 6.58843008293313E63] [68 2.0013300640605898E64] [69 4.319023975071545E65] [70 7.332160749539507E65] [71 1.0133126873205961E67] [72 2.4904999333928747E67] [73 1.351049852893211E68] [74 5.256874840999914E68] [75 8.655283738996329E69] [76 4.361422240890312E70] [77 9.74719762198013E71] [78 2.220090163519371E72] [79 2.8108041872664192E73] [80 1.3743116206191135E74] [81 1.4400862724105265E73] [82 6.669731686087233E75] [83 1.462911291991748E76] [84 1.0818138834273091E80] [85 1.9970704389449132E77] [86 4.695411221453028E77] [87 2.196679460938658E78] [88 6.5934577743094685E78] [89 2.825151571026681E80] [90 8.862565756030544E80] [91 8.944443183710103E81] [92 2.8446428500727496E81] [93 3.138111128990692E83] [94 2.975677257692891E84] [95 4.501786445083767E84] [96 3.2183792663984357E84] [97 1.3742820343933883E85] [98 1.5139062839664251E88] [99 2.987674354123818E86] [100 1.0732613007147156E88]])
  (calculate-best-growth-rate chances)
)

(defn format-as-csv [bet-array] (str "Bet Size %,Average Ending Pool Of Money\n" (apply str (map #(apply str %) (map #(conj %1 "\n") (map #(vec (interpose "," %1)) bet-array))))))

