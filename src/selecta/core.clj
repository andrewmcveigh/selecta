(ns selecta.core
  (:refer-clojure :exclude [apply assoc-in eval find update-in])
  (:require [selecta.reader :as reader]))

(defn get-in*
  "Like clojure.core/get-in but with wild-cards. Always returns a
  sequence of results E.G.,
    (get-in* {:a [{:b [{:d 4}]} {:c 2}]} [:a _ :b _ :d]) ;=> (4)"
  [x ks]
  (let [[k & more] ks]
    (cond (nil? k)
          [x]
          (= k ::_)
          (cond (map? x)
                (mapcat #(get-in* % more) (vals x))
                (sequential? x)
                (mapcat #(get-in* % more) x))
          (associative? x)
          (some-> x (get k) (get-in* more)))))

(defn find
  "Performs a depth-first search in `x` for key `k`, returns `val` for
  the first found `k`."
  [x k]
  (if (and (associative? x) (contains? x k))
    (x k)
    (cond (map? x)
          (some #(find % k) (vals x))
          (sequential? x)
          (some #(find % k) x))))

(defn find-in
  "Performs a depth-first search in `x` for key-path `ks`, returns `val` for
  the first found `ks`. The path `ks` must match without gaps. E.G.,
    (find-in {:items [{:a {:b {:c 1}}}]} [:a :b :c]) ;=> 1
    (find-in {:items [{:a {:b {:c {:d 1}}}}]} [:a :b :d]) ;=> nil"
  [x ks]
  (let [[k] ks]
    (when (associative? x)
      (or (get-in x ks)
          (cond (map? x)
                (some #(find-in % ks) (vals x))
                (sequential? x)
                (some #(find-in % ks) x))))))

(defmacro lambda-let1 [bindings body]
  `((~'fn [~(first bindings)]
      ~body)
    ~(second bindings)))

(defmacro lambda-let [bindings body]
  `(lambda-let1 [~(first bindings) ~(second bindings)]
     ~(if-let [bs (next (next bindings))]
        `(~'let ~bs ~body)
        body)))

(def env
  {'_           ::_
   '->          #'->
   'apply       #'clojure.core/apply
   'assoc       #'assoc
   'assoc-in    #'clojure.core/assoc-in
   'comp        #'comp
   'dissoc      #'dissoc
   'filter      #'filter
   'find        #'find
   'find-in     #'find-in
   'get         #'get
   'get-in      #'get-in
   'get-in*     #'get-in*
   'juxt        #'juxt
   'let         #'lambda-let
   'map         #'map
   'partial     #'partial
   'partition   #'partition
   're-matches  #'re-matches
   'reduce      #'reduce
   'select-keys #'select-keys
   'update      #'update
   'update-in   #'clojure.core/update-in})

(defn self-evaluating? [x]
  (or (number? x)
      (keyword? x)
      (string? x)
      (instance? java.util.regex.Pattern x)))

(defrecord Fn [env args expr])

(defn create-fn [env [fn-args expr :as args]]
  (letfn [(msg [custom-msg]
            (format "Invalid fn syntax %s, %s"
                    (pr-str (cons 'fn args))
                    custom-msg))]
    (assert (= 2 (count args)) (msg "fn passed wrong number of params"))
    (assert (vector? fn-args) (msg "args must be a vector")))
  (->Fn env fn-args expr))

(declare apply eval)

(defn eval* [env expr]
  (cond (self-evaluating? expr)
        expr

        (symbol? expr)
        (if-let [v (env expr)]
          v
          (throw (ex-info "Symbol not understood"
                          {:type ::eval-error :symbol expr})))

        (map? expr)
        (->> expr
             (map (fn [[k v]] [(eval env k) (eval env v)]))
             (into {}))

        (vector? expr)
        (mapv (partial eval env) expr)

        (seq? expr)
        (let [[op & args] expr]
          (condp = op

            'quote
            (first args)

            'fn
            (create-fn env args)

            (let [f (eval env op)]
              (if (-> f meta :macro true?)
                (eval env (macroexpand `(~f ~@args)))
                (apply f
                       (map (partial eval env) args))))))

        :else (throw (ex-info "Expression not understood"
                              {:type ::eval-error :expr expr}))))

(defn apply [f args]
  (cond (or (var? f) (fn? f))
        (clojure.core/apply f args)

        (instance? Fn f)
        (let [env (merge (:env f) (zipmap (:args f) args))]
          (eval env (:expr f)))

        :else
        (throw (ex-info "Don't know how to apply" {:type ::apply-error :f f}))))

(defn eval
  ([expr]
   (eval env expr))
  ([env expr]
   (eval* env expr)))

(defn select [query response]
  (-> query
      (reader/read-string)
      (eval)
      (apply [response])))

;;; need some sort of server timeout/processing limit, could max out
;;; server with infinite loop or something
;;; how can we get an infinite loop? recursion, or traversing an
;;; infinite seq?
;;; Remove both? - done

;;; API

(comment
  
;;; Write a fn that takes 1 argument, the response, and returns a
;;; modified response
;;; E.G.,

    (fn [response]
      (-> response
          (update :z find :this-key)
          (update :y find-in [0 :this-key])
          (assoc :stuff (get-in response [_ 0 :test "thing"]))
          (update :x get-in [:y _ ])))

    (select "(fn [response] (get-in* response [:a _ :b _ :d]))"
            {:a [{:b [{:d 4}]} {:c 2}]})

    (select "(fn [response] (filter (partial re-matches #\"^t.*\") response))"
            ["asas" "test" "thsth" "bbbb"])

    )
