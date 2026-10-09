(ns me.tonsky.persistent-sorted-set.test.storage
  (:require
    [cljs.reader :as edn]
    [clojure.test :as t :refer [is are deftest testing]]
    [me.tonsky.persistent-sorted-set :as set]))

(defn node? [x]
  (or (instance? set/Node x) (instance? set/Leaf x)))

;; Storage that always deserializes a fresh copy, so set can't rely on node identity
(defrecord Storage [*disk *stats]
  set/IStorage
  (-store [_ node]
    (swap! *stats update :writes inc)
    (let [address (inc (count @*disk))]
      (swap! *disk assoc address
        (pr-str
          (if (instance? set/Node node)
            {:keys      (vec (.-keys node))
             :addresses (vec (.-pointers node))}
            {:keys (vec (.-keys node))})))
      address))
  (-restore [_ address]
    (swap! *stats update :reads inc)
    (let [{:keys [keys addresses]} (edn/read-string (@*disk address))]
      (if addresses
        (set/->Node (into-array keys) (into-array addresses))
        (set/->Leaf (into-array keys))))))

(defn storage
  ([] (storage (atom {})))
  ([*disk] (->Storage *disk (atom {:reads 0 :writes 0}))))

(defn reads [storage]
  (:reads @(:*stats storage)))

(defn writes [storage]
  (:writes @(:*stats storage)))

(defn in-memory-nodes
  "Count node objects reachable from set root (excluding root)"
  [set]
  (let [walk (fn walk [node]
               (if (instance? set/Node node)
                 (reduce
                   (fn [acc ptr]
                     (if (node? ptr)
                       (+ acc 1 (walk ptr))
                       acc))
                   0 (.-pointers node))
                 0))]
    (walk (.-root set))))

(defn in-memory-nodes*
  "Like in-memory-nodes but counts root too"
  [set]
  (inc (in-memory-nodes set)))

(deftest test-roundtrip
  (doseq [size [0 1 10 31 33 100 1000 10000 50000]] ;; rseq on exactly 32 hangs, pre-existing
    (testing size
      (let [xs       (shuffle (range size))
            original (into (set/sorted-set) xs)
            storage  (storage)
            address  (set/store original storage)
            loaded   (set/restore address storage)]
        (is (= (seq original) (seq loaded)))
        (when (pos? size)
          (is (= (rseq original) (rseq loaded))))
        (is (= (count original) (count loaded)))
        (is (= original loaded))
        (is (= (vec (set/slice original 100 200)) (vec (set/slice loaded 100 200))))
        (is (= (vec (set/rslice original 200 100)) (vec (set/rslice loaded 200 100))))
        (is (= (some-> (seq original) (set/seek (quot size 2)) vec)
               (some-> (seq loaded) (set/seek (quot size 2)) vec)))
        (is (every? #(contains? loaded %) (take 100 xs)))
        (is (not (contains? loaded -1)))
        (is (= 0 (in-memory-nodes loaded)))))))

(deftest test-from-sorted-array
  (let [arr     (into-array (range 10000))
        storage (storage)
        set     (set/from-sorted-array compare arr (alength arr) {:storage storage})
        address (set/store set)
        loaded  (set/restore address storage)]
    (is (= (range 10000) (seq loaded)))))

(deftest test-lazyness
  (let [size     100000
        original (into (set/sorted-set) (shuffle (range size)))
        storage  (storage)
        address  (set/store original storage)
        _        (is (= 0 (reads storage)))
        stored   (writes storage)
        loaded   (set/restore address storage)
        depth    (inc (.-shift loaded))
        _        (is (= depth (reads storage)) "restore reads leftmost path only")]

    (testing "lookup reads one path"
      (let [before (reads storage)]
        (is (contains? loaded (quot size 2)))
        (is (<= (- (reads storage) before) depth))))

    (testing "first 100 reads a few leaves"
      (let [before (reads storage)]
        (is (= (range 100) (take 100 loaded)))
        (is (< (- (reads storage) before) (* 10 depth)))))

    (testing "set holds no restored nodes"
      (is (= (range size) (seq loaded)))
      (is (= 0 (in-memory-nodes loaded))))

    (testing "count walks the tree"
      (let [loaded (set/restore address storage)]
        (is (= size (count loaded)))))

    (testing "incremental store writes one path"
      (let [before  (writes storage)
            loaded' (conj loaded size)
            new     (in-memory-nodes* loaded')
            _       (is (<= depth new (+ depth 2)))
            _       (set/store loaded')]
        (is (= new (- (writes storage) before)))
        (is (= 0 (in-memory-nodes loaded')))))

    (testing "disj keeps unchanged siblings stored"
      (let [loaded' (disj loaded (quot size 2))]
        (is (<= (in-memory-nodes* loaded') depth))))
    
    (testing "store is idempotent"
      (let [before (writes storage)]
        (set/store loaded)
        (is (= before (writes storage)))))))

(deftest test-walk-addresses
  (let [size     10000
        original (into (set/sorted-set) (shuffle (range size)))
        *addrs   (atom [])
        _        (set/walk-addresses original #(do (swap! *addrs conj %) true))
        _        (is (= [] @*addrs))
        storage  (storage)
        address  (set/store original storage)
        loaded   (set/restore address storage)
        *addrs   (atom #{})]
    (set/walk-addresses loaded #(do (swap! *addrs conj %) true))
    (is (= (writes storage) (count @*addrs)))
    (is (= (set (keys @(:*disk storage))) @*addrs))

    (testing "skip subtree"
      (let [*addrs (atom 0)]
        (set/walk-addresses loaded #(do (swap! *addrs inc) false))
        (is (= 1 @*addrs))))))

(deftest stresstest-stored
  (dotimes [_ 5]
    (let [size     5000
          storage  (storage)
          *set     (atom (set/sorted-set* {:storage storage}))
          *ref     (atom (sorted-set))
          ops      (repeatedly 20000 #(vector (rand-nth [:conj :conj :disj :store :restore]) (rand-int size)))]
      (doseq [[op x] ops]
        (case op
          :conj    (do (swap! *set conj x) (swap! *ref conj x))
          :disj    (do (swap! *set disj x) (swap! *ref disj x))
          :store   (set/store @*set)
          :restore (when (< (rand) 0.05)
                     (reset! *set (set/restore (set/store @*set) storage)))))
      (is (= (seq @*ref) (seq @*set)))
      (is (= (rseq @*ref) (rseq @*set)))
      (is (= (count @*ref) (count @*set)))
      (let [from (rand-int size)
            to   (+ from (rand-int 500))]
        (is (= (subseq @*ref >= from <= to) (seq (set/slice @*set from to))))
        (is (= (rsubseq @*ref >= from <= to) (seq (set/rslice @*set to from)))))
      (let [loaded (set/restore (set/store @*set) storage)]
        (is (= (seq @*ref) (seq loaded)))))))
