(ns pulumi-cljs.core
  "Macros for working with Pulumi from CLJS")

(defmacro all
  "Give an binding vector (similar to let) of symbols and Pulumi Outputs,
  execute the body when the concrete values are available, yielding a
  new Output.

  Under the hood, this macro wraps the pulumi.all and Output.apply
  functions."
  [bindings & body]
  (let [binding-pairs (partition 2 bindings)
       binding-forms (map first binding-pairs)
       output-exprs (map second binding-pairs)]
    `(.apply
       (all* (cljs.core/clj->js [~@output-exprs]))
       (fn [[~@binding-forms]]
         ~@body))))

(defmacro defresource
  ([{:keys [sym type name args opts]}]
   (assert (simple-symbol? sym) (str sym "must be a simple symbol"))
   `(def ~sym
      (pulumi-cljs.core/resource ~type ~name ~args ~opts)))
  ([sym type]
   (let [m {:sym sym :type type :name (name sym) :args {} :opts {}}]
     `(defresource ~m)))
  ([sym type name-or-args]
   (let [name? (string? name-or-args)
         args? (and (not name?) (map? name-or-args))
         m (cond-> {:sym sym :type type}
             name? (assoc :name name-or-args)
             args? (assoc :args name-or-args))]
     `(defresource ~m)))
  ([sym type name-or-args args-or-opts]
   (let [name? (string? name-or-args)
         args? (and (not name?) (map? name-or-args))
         opts? (and args? (map? args-or-opts))
         m (cond-> {:sym sym :type type}
             name? (assoc :name name-or-args)
             args? (assoc :args name-or-args)
             opts? (assoc :opts args-or-opts))]
     `(defresource ~m)))
  ([sym type name args opts]
   (let [m {:sym sym :type type :name name :args args :opts opts}]
     `(defresource ~m))))
