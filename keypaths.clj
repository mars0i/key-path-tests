;; Marshall Abrams
;; Ways to list key paths in embedded maps

;; From 
;; http://stackoverflow.com/questions/21768802/how-can-i-get-the-nested-keys-of-a-map-in-clojure:
;; Responses to David Rz Ayala's question.
;; Not included:
;; Arthur Ulfeldt's main answer responds to the question behind Ayala's
;; question about generating keypaths.

;; NOTE: miner49r noted in an Issue here in 5/2018 that he has revised his SO answer,
;; and that it's now faster than his old answer, and in fact is faster than all other
;; solutions.  This recent answer isn't included in this file, but can be found at
;; https://stackoverflow.com/a/38443772/1455243

;; miner49r's:
(defn miner49r-keypaths
  ([m] (miner49r-keypaths [] m))
  ([prev m]
   (reduce-kv (fn [res k v] 
                (if (map? v)
                  (into res (miner49r-keypaths (conj prev k) v))
                  (conj res (conj prev k))))
              []
              m)))

;; A. Webb's:
(require '[clojure.zip :as z])

(defn AWebb-keypaths [m] 
  (letfn [(branch? [[path m]] (map? m)) 
          (children [[path m]] (for [[k v] m] [(conj path k) v]))] 
    (if (empty? m) 
      []
      (loop [t (z/zipper branch? children nil [[] m]), paths []] 
        (cond (z/end? t) paths 
              (z/branch? t) (recur (z/next t), paths) 
              :leaf (recur (z/next t), (conj paths (first (z/node t)))))))))

;; Alex Miller's:
;; (note you have to wrap result in doall or vec for time trials, because I removed his vec call)
(defn AlexMiller-keypaths [m]
  (if (map? m)
     (mapcat (fn [[k v]]
               (let [sub (AlexMiller-keypaths v)
                     nested (map #(into [k] %)
                                 (filter (comp not empty?) sub))]
                 (if (seq nested)
                   nested
                   [[k]])))
             m)
    []))

;; amalloy's:
;; (note you have to wrap result in doall or vec for time trials)
(defn amalloy-keypaths [m]
  (if (or (not (map? m))
          (empty? m))
    '(())
    (for [[k v] m
          subkey (amalloy-keypaths v)]
      (cons k subkey))))

;; noisesmith's at duplicate question:
;; http://stackoverflow.com/questions/25268818/get-key-chains-of-a-tree-in-clojure
;; (note you have to wrap result in doall or vec for time trials)
(defn noisesmith-keypaths
  ([trie] (noisesmith-keypaths trie []))
  ([trie prefix]
     (if (map? trie)
       (mapcat (fn [[k v]]
                 (noisesmith-keypaths v (conj prefix k)))
               trie)
       [prefix])))

;; Nathan Marz's versions, from 
;; https://clojurians.slack.com/archives/C0FVDQLQ5/p1489779215484550
;; (I subsequently added them as answers to the question above.)

(use 'com.rpl.specter)

;; Simple version:
;; (note you have to wrap result in doall or vec for time trials)
(defn simple-specter-keypaths [m]
  (let [p (recursive-path [] p
            (if-path map?
              [ALL (collect-one FIRST) LAST p]
              STAY))]
    (map butlast (select p m))))
;; Explanation:
;; I'm given a map of maps of ... and
;; (A) since it's a map
;; it's passed to the first collection of navigators, which begins with ALL, so
;; for each MapEntry in the map
;; add its first element (key) to:
;; passing its last element (val) to ... now recurse (which is what p does),
;; i.e. for each of those map vals continue at (A)
;;
;; On each of these branches, we eventually get to a non-map;
;; return it and stop processing on that branch (STAY).
;;
;; However, this last thing returned is not one of the keys, it's
;; the terminal val.  So we end up with the leaf vals in each
;; sequence.  To strip them out, map butlast over the entire result.

;; More efficient version:
(defn fast-specter-keypaths [m]
  (let [p (recursive-path [] p
            (if-path map?
              [ALL
               (if-path [LAST map?]
                [(collect-one FIRST) LAST p]
                FIRST)]))]
    (select p m)))
;; Explanation:
;; I'm given a map of maps of ... and
;; (A) since it's a map
;; it's passed to ALL, i.e.
;; for each MapEntry e in the map,
;;   if e's val is also a map
;;     add its first element (key) to:
;;     passing its last element (val) to ... now recurse (which is what p does),
;;     i.e. for each of those map vals continue at (A)
;;   or if e's val is not a map, then
;;     get its first element, i.e. its key
;; On each of these branches, we eventually get to a non-map,
;; in which case stop processing on that branch and return nothing.


;; If you actually want the leaf node data, you can use the simple
;; Specter version without mapping butlast over the result:
(defn simple-specter-keypaths-with-leaves [m]
  (let [p (recursive-path [] p
            (if-path map?
              [ALL (collect-one FIRST) LAST p]
              STAY))]
    (select p m)))



;; For easy testing:
(def keypath-fns [["simple-specter-keypaths" simple-specter-keypaths]
                  ["fast-specter-keypaths"   fast-specter-keypaths]
                  ["miner49r-keypaths"	     miner49r-keypaths]
                  ["amalloy-keypaths"	     amalloy-keypaths]
                  ["noisesmith-keypaths"     noisesmith-keypaths]
                  ["AWebb-keypaths"	     AWebb-keypaths]
                  ["AlexMiller-keypaths"     AlexMiller-keypaths]
                  ["simple-specter-keypaths-with-leaves" simple-specter-keypaths-with-leaves]])

(use 'criterium.core)

(defn make-big-map
  "Make an embedded map structure with width entries at each level, and
  depth levels.  There will be width^depth paths through the structure."
  [width depth]
  (if (pos? depth)
    (zipmap (map #(keyword (str "k" %)) (range width))
            (repeat (make-big-map width (dec depth))))
    nil))

(defn test-keypath-fns
  [m]
  (doseq [[nam fun] keypath-fns]
    (println "-------------------------------------------------------\n" nam)
    (bench (def _ (doall (fun m))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Versions that return intermediate keypaths as well:

;; Laurent's:
(require '[clojure.set :as set])

(defn Laurent-keypaths-all
  ([m current]
   ;; base case: map empty or not a map
   (if (or (not (map? m)) (empty? m))
     #{current}
   ;; else: recursive call for every (key, value) in the map
     (apply set/union #{current}
            (map (fn [[k v]]
                   (Laurent-keypaths-all v (conj current k)))
                 m))))
  ([m]
   (-> m (Laurent-keypaths-all []) (disj []))))

;; Aaron Cummings':
(defn AaronCummings-keypaths-all
  ([m] (if (map? m) (AaronCummings-keypaths-all (seq m) [])))
  ([es c]
   (lazy-seq
    (when-let [e (first es)]
      (let [c* (conj c (key e))]
        (cons c* (concat (if (map? (val e))
                           (AaronCummings-keypaths-all (seq (val e)) c*))
                         (AaronCummings-keypaths-all (rest es) c))))))))

;; miner49r's, allowing also vectors:
(defn miner49r-keypaths-all
  ([m] (miner49r-keypaths-all [] m))
  ([prev m]
   (reduce-kv (fn [res k v] (if (associative? v)
                              (let [kp (conj prev k)]
                                (conj (into res (miner49r-keypaths-all kp v)) kp))
                              (conj res (conj prev k))))
              []
              m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; David Rz Ayala's doesn't produce the right answer:
;(defn genkeys [parent data]
;  (let [mylist (transient [])]
;    (doseq [k (keys data)]
;      (do
;        (if ( = (class (k data)) clojure.lang.PersistentHashMap )
;          (#(reduce conj! %1 %2) mylist (genkeys (conj parent  k ) (k data) ))
;          (conj! mylist  (conj parent  k ) )
;          )))
;    (persistent! mylist)))
;
;(defn DavidRzAyala-keypaths [data] (genkeys [] data))
