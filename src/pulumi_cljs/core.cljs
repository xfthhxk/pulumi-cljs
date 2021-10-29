(ns pulumi-cljs.core
  "Tools for working with Pulumi from CLJS.
   Adapted from https://github.com/modern-energy/pulumi-cljs."
  (:require ["@pulumi/pulumi" :as p]
            [clojure.walk :as walk]
            [clojure.string :as str]
            [cljs.pprint]
            [goog.object])
  (:require-macros [pulumi-cljs.core])
  (:refer-clojure :exclude [apply str]))

(def ^:dynamic *default-output-keys* #{:id :urn})

(defn resource
  "Create a Pulumi resource"
  ([type name] (resource type name {} {}))
  ([type name args] (resource type name args {}))
  ([type name args opts]
   (new type name (clj->js args) (clj->js opts))))

(defn ^p/Config load-cfg*
  "Load the Pulumi configuration for a namespace.
  NB. if you have gcp:project-id then construct the config
  with namespace `'gcp'` and retrieve the value with
  (.require (p/Config. 'gcp') 'project-id'). If namespace
  is `nil` uses the project namespace by default."
  ([] (load-cfg* nil))
  ([namespace]
   (if namespace
     (p/Config. namespace)
     (p/Config.))))

(def load-cfg (memoize load-cfg*))

(def ^:private config-type->fns
  {:string {:get (fn [^p/Config cfg key] (.get cfg key))
            :require (fn [^p/Config cfg key] (.require cfg key))}
   :number {:get (fn [^p/Config cfg key] (.getNumber cfg key))
            :require (fn [^p/Config cfg key] (.requireNumber cfg key))}
   :boolean {:get (fn [^p/Config cfg key] (.getBoolean cfg key))
            :require (fn [^p/Config cfg key] (.requireBoolean cfg key))}
   :object {:get (fn [^p/Config cfg key] (.getObject cfg key))
             :require (fn [^p/Config cfg key] (.requireObject cfg key))}
   :secret {:get (fn [^p/Config cfg key] (.getSecret cfg key))
            :require (fn [^p/Config cfg key] (.requireSecret cfg key))}})

(defn cfg
  "Retrieve a single value from the Pulumi configuration. The input
  can either be a string or a keyword ie `:foo:bar` or
  `'foo:bar'` (string), or `:bar` or `'bar'`.
  Pulumi treats the `foo` portion as the namespace
  and `bar` as the key. If no namespace is provided uses the
  default namespace which is the project name.
  `val-type` is a keyword ie `#{:string :number :boolean :object :secret}`
  If the key is missing, throws an error unless a default value is supplied"
  ([key] (cfg key :string ::required))
  ([key val-type] (cfg key val-type ::required))
  ([key val-type default]
   (let [-key (if (keyword? key) (name key) key)
         [-ns -key] (if-let [idx (str/index-of -key ":")]
                      [(subs -key 0 idx) (subs -key (inc idx))]
                      [nil -key])
         config (load-cfg -ns)
         {get-fn :get
          require-fn :require} (config-type->fns val-type :string)
         val (cond
               (instance? p/Output default) val
               (= default ::required) (require-fn config -key)
               :else (get-fn config -key))
         ;; deal with booleans
         val (if (some? val) val default)]
     (if (= :object val-type)
       (some-> val (js->clj :keywordize-keys true))
       val))))

(extend-protocol ILookup
  p/Resource
  (-lookup
    ([o k]
     (if-some [v (goog.object/get o (clj->js k))]
       v
       (throw (ex-info "No such key in Resource" {:key k}))))
    ([o k not-found]
     (if-some [v (goog.object/get o (clj->js k))]
       v
       not-found))))

(defn output->map
  ([^p/Resource output] (output->map output *default-output-keys*))
  ([^p/Resource output & more-keys-or-coll]
   (let [ks (if (coll? (first more-keys-or-coll))
              (first more-keys-or-coll)
              more-keys-or-coll)
         ks (into (set ks) *default-output-keys*)]
     (->> ks
          (mapv (fn [k] [k (k output)]))
          (into {})))))

(defn prepare-output
  "Walk a data structure and replace all Resource objects with a map
  containing their Pulumi URN and provider ID, then convert to a JS
  object."
  [output]
  (clj->js
   (walk/prewalk (fn [form]
                   (if (instance? p/Resource form)
                     (output->map form)
                     form))
                 output)
   :keyword-fn (fn [k]
                 (if-let [-ns (namespace k)]
                   (clojure.core/str -ns "/" (name k))
                   (name k)))))

(defn json
  "Convert a Clojure data structure to a JSON string, handling nested
  Pulumi Output values"
  [data]
  (.apply (p/output (clj->js data)) #(js/JSON.stringify %)))


(defn- keyword->name
  "keeps the namespace"
  [^clojure.lang.Keyword k]
  (.-fqn k))


(defn quasi-edn
  "Preserves keyword keys but values are lossy because of
  having to go through clj->js conversion."
  [data]
  (-> data
      (clj->js :keyword-fn keyword->name)
      p/output
      (.apply (fn [js-object]
                (with-out-str
                  (cljs.pprint/pprint
                   (js->clj js-object :keywordize-keys true)))))))

(defn group
  "Create a Component Resource to group together other sets of resources"
  [name opts]
  (new p/ComponentResource (clojure.core/str "group:" name) name (js-obj) (clj->js opts) false))

(defn invoke
  "Invoke a Pulumi function. Converts args map and opts from
  ClojureScript to JavaScript. Returns a Pulumi Output, not a Promise,
  to avoid special handling. Does not convert the result to a CLJS
  object (CLJS objects behave oddly when wrapped in an Output)"
  [f & args]
  (p/output (clojure.core/apply f (map clj->js args))))

(defn apply
  [output f]
  (.apply output f))

(defn all*
  "Alias for pulumi.all since JS modules can't be imported into the Clojure macro namespace"
  [& args]
  (clojure.core/apply p/all args))

(defn str
  "Generates a string from args that maybe a string or an output."
  [& more]
  (clojure.core/apply p/concat more))
