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
  "Convenience macro.
  * avoid having to specify a name which almost always is the same as the var
  * avoid having to call `pulumi-cljs.core/resource`
  `sym` is a symbol, ie the name of the var which will be `def`ed
  `type` is a resource name ie `gcp/storage.Bucket`
  `name` is a string or string returning expr (usually unnecessary)
  `args` is a map or map returning expr
  `opts` is a map or map returning expr"
  ([{:keys [sym type name args opts] :as m}]
   (assert (map? m) "expected single map arg")
   (assert (simple-symbol? sym) (str sym "must be a simple symbol"))
   `(def ~sym
      (pulumi-cljs.core/resource ~type ~name ~args ~opts)))
  ([sym type]
   (let [m {:sym sym :type type :name (name sym) :args {} :opts {}}]
     `(defresource ~m)))
  ([sym type args]
   (assert (map? args) "args must be a map")
   (let [m {:sym sym :type type :name (name sym) :args args :opts {}}]
     `(defresource ~m)))
  ([sym type args opts]
   (assert (map? args) "args must be a map")
   (assert (map? opts) "opts must be a map")
   (let [m {:sym sym :type type :name (name sym) :args args :opts opts}]
     `(defresource ~m)))
  ([sym type name args opts]
   (assert (map? args) "args must be a map")
   (assert (map? opts) "opts must be a map")
   (let [m {:sym sym :type type :name name :args args :opts opts}]
     `(defresource ~m))))
