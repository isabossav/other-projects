; Meta-circular interpreter: clojure interpreter in clojure
; Write an evaluator implementing a small subset of clojure
; running a basic clojure REPL without built-in eval function


(defn rl [] (load-file "metacirc.clj"))

(declare my-eval)
(declare bind-symexpr2)

(def func-map
  {
   '+ +
   '- -
   '* *
   '/ /
   'cons cons
   'list list
   '= =
   'first first
   'rest rest
   'println println
   'empty? empty?
   })
  

(defn apply-built-in [f-symbol state expr]
  (println (str "entering apply-built-in with symbol=" f-symbol ", state=" state ", expr=" expr))
  (let [func (get func-map f-symbol) ;; look up the actual function
        actuals (map (fn [form] (:ret (my-eval form state)))
                     (rest expr))]
    (assoc state :ret (apply func actuals))))


(defn environment-bindings [state]
  (assoc state :ret (:env state)))


; Takes a symbol, a value and a state
; and returns a new state that contains 
; the symbol and value associated in :env of the old state
; and the symbol associated in :ret of the old state
(defn add-binding [sym value state]
  (let [newenv (assoc (:env state) sym value)]
    (assoc state :ret sym :env newenv)))


; Takes a vector and a state 
; takes the first two elements of the vector
; the first one being a symbol, and the second an expr
; evaluates the expr using the state, and takes its return value
; binds the symbol and the return value to the env of the state,
; and calls itselt recursively until there are <2 elements in vect
(defn add-vectbinding [vect state]
  (println (str "entering add-vectbinding with vect=" vect " and state=" state))
  (if (> (count vect) 1) 
    (let [sym (first vect)
          expr (:ret (my-eval (second vect) state))
          newvect (subvec vect 2 (count vect))
          newstate (add-binding sym expr state)]
      (add-vectbinding newvect newstate))
    state))


; Evaluates the body in a state that contains
; the old state plus the bindings given by let
(defn eval-let [vect body state]
  (let [letstate (add-vectbinding vect state)
        retvalue (:ret (my-eval body letstate))]
    (assoc state :ret retvalue)))


; Evaluate a declaration of defn
; Associate in the env the name of the function 
; with a map containing the parameters, body, and
; env in which the fn was declared
(defn evald-defn [fname p b s]
  (let [fnmap {:param p :body b :denv (:env s)}]
    (add-binding fname fnmap s)))


; Takes a vector (param), a list (args) and another vector (newv)
; "Adds" the interweaved elements of the first vector and the list
; at the end of the the second vector and calls itself recursively 
; until it  has interweaved every (vector, list) couple 
; (as indivd elements), in which case it returns newv
(defn make-intwvect [param args newv]
  ;(println (str "entering make-intwvect with param=" param ", args=" args ", newv=" newv))
  (if (> (count param) 0)
    (let [newv (assoc newv (count newv) (first param))
          newv (assoc newv (count newv) (first args))
          newparam (subvec param 1 (count param))
          newargs (rest args)] 
      (make-intwvect newparam newargs newv))
    newv))


; Helper function of evale-defn
; Evaluates the arguments given in vect in state
; And associates the :ret of the resulting state
; in both the state and the env
(defn add-vectbinding2 [vect state env]
  (println (str "entering add-vectbinding2 with vect=" vect ", state=" state "and env=" env \newline))
  (if (> (count vect) 1) 
    (let [sym (first vect)
          ; Use state to evaluate the arguments to the fn
          expr (:ret (my-eval (second vect) state))
          ; Add binding in envt of state and in env
          newstate (add-binding sym expr state)
          newenv (assoc env sym expr)
          newvect (subvec vect 2 (count vect))]
      (add-vectbinding2 newvect newstate newenv))
    env))


; Evaluate an evaluation of a user defined function
; Use defining state to evaluate symbols within the fn body
; This state contains the state in which fn was defined plus the
; necessary bindings to run the function (this was run
; using the current state)
(defn evale-defn [fname fnmap args cstate]
  (println (str "entering evale-defn with fname=" fname ", fnmap=" fnmap "args=" args " and state=" cstate \newline))
  (let [intw-vect (make-intwvect (:param fnmap) args [])
        ;; envt in which the fn was declared plus the necessary
        ;; bindings of the args of the function
        fn-envt (add-vectbinding2 intw-vect cstate (:denv fnmap))
        ;; fn-prestate plus the binding of the fn with its map, so that
        ;; it works for recursive calls
        fn-state (assoc cstate :env (assoc fn-envt fname fnmap))
        ;; evaluate the body of the fn in the state in which the fn was 
        ;; declared plus the bindings added from prestate and state
        ;; and take the return value of this evaluation
        fn-ret (:ret (my-eval (:body fnmap) fn-state))]
    ;; associate the prev return value to the current state and return it
    (assoc cstate :ret fn-ret)))


(defn my-eval [expr state]
  ;;; return a state: a map with :env and :ret fields
  (println (str "enter my-eval with expr=" expr " and state=" state \newline))
  (if (not (seq? expr))
    (if (contains? (:env state) expr)
      (assoc state :ret (expr (:env state)))
      (assoc state :ret expr))
    (let [env (:env state)
          f (first expr)
          s (second expr)
          r (rest expr)]
      (cond
        ;; if it's a built-in function, apply it!
        (contains? func-map f) (apply-built-in f state expr)
        (= 'quote f) (assoc state :ret s)
        ;; if the if cond is true, eval the 3rd element of expr,
        ;; if not, eval the 4rd element of expr
        (= 'if f) (if (:ret (my-eval s state)) 
          (my-eval (nth expr 2) state) (my-eval (nth expr 3) state))
        (= 'environment-bindings f) (environment-bindings state)
        (= 'def f) (add-binding (second expr) (:ret (my-eval (nth expr 2) state)) state)
        (= 'let f) (eval-let (second expr) (nth expr 2) state)
        (= 'defn f) (evald-defn (second expr) (nth expr 2) (nth expr 3) state)
        (and (contains? (:env state) f) (map? (f (:env state))))
          (evale-defn f (f env) r state)
        (and (contains? (:env state) f) (not (map? (f (:env state)))))
          (my-eval f state)
        (list? expr) (assoc state :ret expr)
        :else ((println "nope") {:env env :ret :complex})))))

  
(defn repl 
  ([] (repl {:env {}}))
  ([state]
   (print "H]=> ")
   (flush)
   (let [line (read-line)]
     (when (not (empty? line))
       (let [new-state (my-eval (read-string line) state)]
         (println (:ret new-state))
         (recur new-state))))))