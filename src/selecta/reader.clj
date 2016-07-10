(ns selecta.reader
  (:refer-clojure :exclude [read read-string])
  (:require [selecta.monad :as m])
  (:import
   [clojure.lang BigInt Numbers]
   [java.util LinkedList]))

(defprotocol Reader
  (read-char [reader]
    "Returns the next char from the Reader, nil if the end of stream
    has been reached")
  (peek-char [reader]
    "Returns the next char from the Reader without removing it from
    the reader stream")
  (unread [reader ch]
    "Pushes back a single character on to the stream"))

(deftype StringReader [s len ^:volatile-mutable pos]
  Reader
  (read-char [_]
    (let [p pos]
      (when (> len pos)
        (set! pos (inc pos))
        (nth s p))))
  (peek-char [_]
    (when (> len pos)
      (nth s pos)))
  (unread [_ ch]
    (when ch
      (set! pos (dec pos))
      nil)))

(defn string-reader
  "Creates a StringReader from a given string"
  ([s]
   (->StringReader s (count s) 0)))

(defn reader-error
  "Throws an ExceptionInfo with the given message.
   If rdr is an IndexingReader, additional information about column
   and line number is provided"
  [rdr & msg]
  (throw (ex-info (apply str msg) (merge {:type ::reader-error}))))

(defn number-literal?
  "Checks whether the reader is at the start of a number literal"
  [reader initch]
  (or (numeric? initch)
      (and (or (identical? \+ initch) (identical?  \- initch))
           (numeric? (peek-char reader)))))

(defn read-past
  "Read until first character that doesn't match pred, returning
   char."
  [pred rdr]
  (loop [ch (read-char rdr)]
    (if (pred ch)
      (recur (read-char rdr))
      ch)))

(defn skip-line
  "Advances the reader to the end of a line. Returns the reader"
  [reader]
  (loop []
    (when-not (newline? (read-char reader))
      (recur)))
  reader)

(def int-pattern   #"([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?")
(def ratio-pattern #"([-+]?[0-9]+)/([0-9]+)")
(def float-pattern #"([-+]?[0-9]+(\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?")

(defn match-int
  [m]
  (if (.group m 2)
    (if (.group m 8) 0N 0)
    (let [negate? (= "-" (.group m 1))
          a (cond
             (.group m 3) [(.group m 3) 10]
             (.group m 4) [(.group m 4) 16]
             (.group m 5) [(.group m 5) 8]
             (.group m 7) [(.group m 7) (Integer/parseInt (.group m 6))]
             :else        [nil nil])
          ^String n (a 0)]
      (when n
        (let [bn (BigInteger. n (int (a 1)))
              bn (if negate? (.negate bn) bn)]
          (if (.group m 8)
            (BigInt/fromBigInteger bn)
            (if (< (.bitLength bn) 64)
              (.longValue bn)
              (BigInt/fromBigInteger bn))))))))

(defn match-ratio
  [m]
  (let [^String numerator (.group m 1)
        ^String denominator (.group m 2)
        numerator (if (.startsWith numerator "+")
                    (subs numerator 1)
                    numerator)]
    (/ (-> numerator   BigInteger. BigInt/fromBigInteger Numbers/reduceBigInt)
       (-> denominator BigInteger. BigInt/fromBigInteger Numbers/reduceBigInt))))

(defn match-float
  [s m]
  (if (.group m 4)
    (BigDecimal. (.group m 1))
    (Double/parseDouble s)))

(defn match-number [^String s]
  (let [int-matcher (.matcher int-pattern s)]
    (if (.matches int-matcher)
      (match-int int-matcher)
      (let [float-matcher (.matcher float-pattern s)]
        (if (.matches float-matcher)
          (match-float s float-matcher)
          (let [ratio-matcher (.matcher ratio-pattern s)]
            (when (.matches ratio-matcher)
              (match-ratio ratio-matcher))))))))

(defn parse-symbol
  "Parses a string into a vector of the namespace and symbol"
  [^String token]
  (when-not (or (= "" token)
                (.endsWith token ":")
                (.startsWith token "::"))
    (let [ns-idx (.indexOf token "/")]
      (if-let [^String ns (and (pos? ns-idx)
                               (subs token 0 ns-idx))]
        (let [ns-idx (inc ns-idx)]
          (when-not (== ns-idx (count token))
            (let [sym (subs token ns-idx)]
              (when (and (not (numeric? (nth sym 0)))
                         (not (= "" sym))
                         (not (.endsWith ns ":"))
                         (or (= sym "/")
                             (== -1 (.indexOf sym "/"))))
                [ns sym]))))
        (when (or (= token "/")
                  (== -1 (.indexOf token "/")))
          [nil token])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-comment
  [rdr & _]
  (skip-line rdr))

(defn throwing-reader
  [msg]
  (fn [rdr & _]
    (reader-error rdr msg)))

(declare read*
         macros
         dispatch-macros)

(defn macro-terminating? [ch]
  (case ch
    (\" \; \@ \^ \` \~ \( \) \[ \] \{ \} \\) true
    false))

(defn whitespace?
  "Checks whether a given character is whitespace"
  [ch]
  (when ch
    (or (Character/isWhitespace ^Character ch)
        (identical? \,  ch))))

(defn numeric?
  "Checks whether a given character is numeric"
  [^Character ch]
  (when ch
    (Character/isDigit ch)))

(defn newline?
  "Checks whether the character is a newline"
  [c]
  (or (identical? \newline c)
      (nil? c)))

(defn read-token
  "Read in a single logical token from the reader"
  [rdr initch]
  (if-not initch
    (reader-error rdr "EOF while reading")
    (loop [sb (StringBuilder.) ch initch]
      (if (or (whitespace? ch)
              (macro-terminating? ch)
              (nil? ch))
        (do (when ch
              (unread rdr ch))
            (str sb))
        (recur (.append sb ch) (read-char rdr))))))


(defn read-dispatch
  [rdr _ opts pending-forms]
  (if-let [ch (read-char rdr)]
    (if-let [dm (dispatch-macros ch)]
      (dm rdr ch opts pending-forms)
      (reader-error rdr "Unknown dispatch character"))
    (reader-error rdr "EOF while reading character")))

(defn read-unmatched-delimiter
  [rdr ch opts pending-forms]
  (reader-error rdr "Unmatched delimiter " ch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-regex
  [rdr ch opts pending-forms]
  (let [sb (StringBuilder.)]
    (loop [ch (read-char rdr)]
      (if (identical? \" ch)
        (re-pattern (str sb))
        (if (nil? ch)
          (reader-error rdr "EOF while reading regex")
          (do
            (.append sb ch )
            (when (identical? \\ ch)
              (let [ch (read-char rdr)]
                (if (nil? ch)
                  (reader-error rdr "EOF while reading regex"))
                (.append sb ch)))
            (recur (read-char rdr))))))))

(defn read-unicode-char
  ([^String token ^long offset ^long length ^long base]
   (let [l (+ offset length)]
     (when-not (== (count token) l)
       (throw (IllegalArgumentException. (str "Invalid unicode character: \\" token))))
     (loop [i offset uc 0]
       (if (== i l)
         (char uc)
         (let [d (Character/digit (int (nth token i)) (int base))]
           (if (== d -1)
             (throw (IllegalArgumentException. (str "Invalid digit: " (nth token i))))
             (recur (inc i) (long (+ d (* uc base))))))))))

  ([rdr initch base length exact?]
   (let [base (long base)
         length (long length)]
     (loop [i 1 uc (long (Character/digit (int initch) (int base)))]
       (if (== uc -1)
         (throw (IllegalArgumentException. (str "Invalid digit: " initch)))
         (if-not (== i length)
           (let [ch (peek-char rdr)]
             (if (or (whitespace? ch)
                     (macros ch)
                     (nil? ch))
               (if exact?
                 (throw (IllegalArgumentException.
                         (str "Invalid character length: " i ", should be: " length)))
                 (char uc))
               (let [d (Character/digit (int ch) (int base))]
                 (read-char rdr)
                 (if (== d -1)
                   (throw (IllegalArgumentException. (str "Invalid digit: " ch)))
                   (recur (inc i) (long (+ d (* uc base))))))))
           (char uc)))))))

(def ^:private ^:const upper-limit (int \uD7ff))
(def ^:private ^:const lower-limit (int \uE000))

(defn read-char*
  "Read in a character literal"
  [rdr backslash opts pending-forms]
  (let [ch (read-char rdr)]
    (if-not (nil? ch)
      (let [token (if (or (macro-terminating? ch)
                          (whitespace? ch))
                    (str ch)
                    (read-token rdr ch))
            token-len (count token)]
        (cond

         (== 1 token-len)  (Character/valueOf (nth token 0))

         (= token "newline") \newline
         (= token "space") \space
         (= token "tab") \tab
         (= token "backspace") \backspace
         (= token "formfeed") \formfeed
         (= token "return") \return

         (.startsWith token "u")
         (let [c (read-unicode-char token 1 4 16)
               ic (int c)]
           (if (and (> ic upper-limit)
                    (< ic lower-limit))
             (reader-error rdr "Invalid character constant: \\u" (Integer/toString ic 16))
             c))

         (.startsWith token "o")
         (let [len (dec token-len)]
           (if (> len 3)
             (reader-error rdr "Invalid octal escape sequence length: " len)
             (let [uc (read-unicode-char token 1 len 8)]
               (if (> (int uc) 0377)
                 (reader-error rdr "Octal escape sequence must be in range [0, 377]")
                 uc))))

         :else (reader-error rdr "Unsupported character: \\" token)))
      (reader-error rdr "EOF while reading character"))))

(defonce ^:private READ_EOF (Object.))
(defonce ^:private READ_FINISHED (Object.))

(def ^:dynamic *read-delim* false)
(defn read-delimited
  "Reads and returns a collection ended with delim"
  [delim rdr opts pending-forms]
  (let [delim (char delim)]
    (binding [*read-delim* true]
      (loop [a (transient [])]
        (let [form (read* rdr false READ_EOF delim opts pending-forms)]
          (if (identical? form READ_FINISHED)
            (persistent! a)
            (if (identical? form READ_EOF)
              (reader-error rdr "EOF while reading")
              (recur (conj! a form)))))))))

(defn read-list
  "Read in a list"
  [rdr _ opts pending-forms]
  (let [the-list (read-delimited \) rdr opts pending-forms)]
    (if (empty? the-list)
      '()
      (clojure.lang.PersistentList/create the-list))))

(defn read-vector
  "Read in a vector"
  [rdr _ opts pending-forms]
  (read-delimited \] rdr opts pending-forms))

(defn read-map
  "Read in a map"
  [rdr _ opts pending-forms]
  (let [the-map (read-delimited \} rdr opts pending-forms)
        map-count (count the-map)]
    (when (odd? map-count)
      (reader-error rdr "Map literal must contain an even number of forms"))
    (if (zero? map-count)
      {}
      (clojure.lang.RT/map (to-array the-map)))))

(defn read-number
  [rdr initch]
  (loop [sb (doto (StringBuilder.) (.append initch))
         ch (read-char rdr)]
    (if (or (whitespace? ch) (macros ch) (nil? ch))
      (let [s (str sb)]
        (unread rdr ch)
        (or (match-number s)
            (reader-error rdr "Invalid number format [" s "]")))
      (recur (doto sb (.append ch)) (read-char rdr)))))

(defn escape-char [sb rdr]
  (let [ch (read-char rdr)]
    (case ch
      \t "\t"
      \r "\r"
      \n "\n"
      \\ "\\"
      \" "\""
      \b "\b"
      \f "\f"
      \u (let [ch (read-char rdr)]
           (if (== -1 (Character/digit (int ch) 16))
             (reader-error rdr "Invalid unicode escape: \\u" ch)
             (read-unicode-char rdr ch 16 4 true)))
      (if (numeric? ch)
        (let [ch (read-unicode-char rdr ch 8 3 false)]
          (if (> (int ch) 0337)
            (reader-error rdr "Octal escape sequence must be in range [0, 377]")
            ch))
        (reader-error rdr "Unsupported escape character: \\" ch)))))

(defn read-string*
  [reader _ opts pending-forms]
  (loop [sb (StringBuilder.)
         ch (read-char reader)]
    (case ch
      nil (reader-error reader "EOF while reading string")
      \\ (recur (doto sb (.append (escape-char sb reader)))
                (read-char reader))
      \" (str sb)
      (recur (doto sb (.append ch)) (read-char reader)))))

(defn read-symbol
  [rdr initch]
  (when-let [token (read-token rdr initch)]
    (case token

      ;; special symbols
      "nil" nil
      "true" true
      "false" false
      "/" '/
      "NaN" Double/NaN
      "-Infinity" Double/NEGATIVE_INFINITY
      ("Infinity" "+Infinity") Double/POSITIVE_INFINITY

      (or (when-let [p (parse-symbol token)]
            (symbol (p 0) (p 1)))
          (reader-error rdr "Invalid token: " token)))))

(def ^:dynamic *alias-map*
  "Map from ns alias to ns, if non-nil, it will be used to resolve read-time
   ns aliases instead of (ns-aliases *ns*).

   Defaults to nil"
  nil)

(defn resolve-ns [sym]
  (or ((or *alias-map*
           (ns-aliases *ns*)) sym)
      (find-ns sym)))

(defn read-keyword
  [reader initch opts pending-forms]
  (let [ch (read-char reader)]
    (if-not (whitespace? ch)
      (let [token (read-token reader ch)
            s (parse-symbol token)]
        (if s
          (let [^String ns (s 0)
                ^String name (s 1)]
            (if (identical? \: (nth token 0))
              (if ns
                (let [ns (resolve-ns (symbol (subs ns 1)))]
                  (if ns
                    (keyword (str ns) name)
                    (reader-error reader "Invalid token: :" token)))
                (keyword (str *ns*) (subs name 1)))
              (keyword ns name)))
          (reader-error reader "Invalid token: :" token)))
      (reader-error reader "Invalid token: :"))))

(defn wrapping-reader
  "Returns a function which wraps a reader in a call to sym"
  [sym]
  (fn [rdr _ opts pending-forms]
    (list sym (read* rdr true nil opts pending-forms))))

(defn read-set
  [rdr _ opts pending-forms]
  (set (read-delimited \} rdr opts pending-forms)))

(defn read-discard
  "Read and discard the first object from rdr"
  [rdr _ opts pending-forms]
  (doto rdr
    (read* true nil opts pending-forms)))

(def ^:private RESERVED_FEATURES #{:else :none})

(defn has-feature?
  [rdr feature opts]
  (if (keyword? feature)
    (or (= :default feature) (contains? (get opts :features) feature))
    (reader-error rdr (str "Feature should be a keyword: " feature))))

(defn check-eof-error
  [form rdr ^long first-line]
  (when (identical? form READ_EOF)
    (if (< first-line 0)
      (reader-error rdr "EOF while reading")
      (reader-error rdr "EOF while reading, starting at line " first-line))))

(defn check-reserved-features
  [rdr form]
  (when (get RESERVED_FEATURES form)
    (reader-error rdr (str "Feature name " form " is reserved"))))

(defn check-invalid-read-cond
  [form rdr ^long first-line]
  (when (identical? form READ_FINISHED)
    (if (< first-line 0)
      (reader-error rdr "read-cond requires an even number of forms")
      (reader-error rdr (str "read-cond starting on line " first-line " requires an even number of forms")))))

(def ^:private NO_MATCH (Object.))

(def ^:private ^:dynamic arg-env)

(declare read-symbol)

(defn macros [ch]
  (case ch
    \" read-string*
    \: read-keyword
    \; read-comment
    \' (wrapping-reader 'quote)
    \( read-list
    \) read-unmatched-delimiter
    \[ read-vector
    \] read-unmatched-delimiter
    \{ read-map
    \} read-unmatched-delimiter
    \\ read-char*
    \# read-dispatch
    nil))

(defn dispatch-macros [ch]
  (case ch
    \{ read-set
    \< (throwing-reader "Unreadable form")
    \" read-regex
    \! read-comment
    \_ read-discard
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn ^:private read*
  ([reader eof-error? sentinel opts pending-forms]
     (read* reader eof-error? sentinel nil opts pending-forms))
  ([reader eof-error? sentinel return-on opts pending-forms]
     (when (= :unknown *read-eval*)
       (reader-error "Reading disallowed - *read-eval* bound to :unknown"))
     (try
       (loop []
         (if (seq pending-forms)
           (.remove pending-forms 0)
           (let [ch (read-char reader)]
             (cond
               (whitespace? ch) (recur)
               (nil? ch) (if eof-error? (reader-error reader "EOF") sentinel)
               (= ch return-on) READ_FINISHED
               (number-literal? reader ch) (read-number reader ch)
               :else (let [f (macros ch)]
                       (if f
                         (let [res (f reader ch opts pending-forms)]
                           (if (identical? res reader)
                             (recur)
                             res))
                         (read-symbol reader ch)))))))
       (catch Exception e
         (if (instance? clojure.lang.ExceptionInfo e)
           (let [d (ex-data e)]
             (if (= :reader-exception (:type d))
               (throw e)
               (throw (ex-info (.getMessage e)
                               (merge {:type :reader-exception} d)
                               e))))
           (throw (ex-info (.getMessage e) {:type :reader-exception} e)))))))

(defn read
  "Reads the first object from a Reader Returns the object read. If
   EOF, throws if eof-error? is true. Otherwise returns sentinel. If no
   stream is providen, *in* will be used.

   Opts is a persistent map with valid keys:
    :read-cond - :allow to process reader conditionals, or
                 :preserve to keep all branches
    :features - persistent set of feature keywords for reader conditionals
    :eof - on eof, return value unless :eofthrow, then throw.
           if not specified, will throw"
  {:arglists '([] [reader] [opts reader] [reader eof-error? eof-value])}
  ([]
   (read *in* true nil))
  ([reader]
   (read reader true nil))
  ([{eof :eof :as opts :or {eof :eofthrow}} reader]
   (read* reader (= eof :eofthrow) eof nil opts (LinkedList.)))
  ([reader eof-error? sentinel]
   (read* reader eof-error? sentinel nil {} (LinkedList.))))

(defn read-string
  "Reads one object from the string s.
   Returns nil when s is nil or empty."
  ([s]
   (read-string {} s))
  ([opts s]
   (when (and s (not (identical? s "")))
     (read opts (string-reader s)))))
