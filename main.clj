;; ------------------------------------------
;; 🔧 Funciones básicas reemplazo de string
;; ------------------------------------------

(defn split [texto separador]
  (vec (.split texto separador)))

(defn lower-case [texto]
  (.toLowerCase texto))

(defn split-lines [texto]
  (split texto "\n"))

;; ------------------------------------------
;; ⚙️ Diccionarios de calorías y conversiones
;; ------------------------------------------

(def calorias
  {"granulated sugar" 773
   "all-purpose flour" 455
   "cocoa powder" 196
   "powdered sugar" 467
   "chocolate chips" 805
   "egg" 72
   "canola oil" 1927
   "extra-virgin olive oil" 1910
   "almond flour" 640
   "baking powder" 53
   "kosher salt" 0
   "vanilla extract" 288
   "lemon zest" 6
   "lemon juice" 54
   "fettuccine pasta" 220
   "butter" 1627
   "heavy cream" 821
   "romano cheese" 431
   "parmesan cheese" 431
   "salt" 0
   "garlic salt" 5
   "vegetable oil" 1927
   "garlic" 203
   "fresh rosemary" 11
   "olive oil" 1910
   "white wine vinegar" 5
   "red pepper flakes" 49
   "smoked paprika" 19
   "parsley" 22})

(def conversiones
  {"granulated sugar" 200
   "all-purpose flour" 120
   "cocoa powder" 85
   "powdered sugar" 120
   "chocolate chips" 175
   "egg" 50
   "canola oil" 218
   "extra-virgin olive oil" 216
   "almond flour" 96
   "baking powder" 192
   "kosher salt" 288
   "vanilla extract" 208
   "lemon zest" 96
   "lemon juice" 240
   "fettuccine pasta" 100
   "butter" 227
   "heavy cream" 240
   "romano cheese" 100
   "parmesan cheese" 100
   "salt" 273
   "garlic salt" 288
   "vegetable oil" 218
   "garlic" 136
   "fresh rosemary" 28
   "olive oil" 216
   "white wine vinegar" 239
   "red pepper flakes" 45
   "smoked paprika" 92
   "parsley" 60})

;; ------------------------------------------
;; 📄 Lista manual de archivos de recetas
;; ------------------------------------------

(def archivos-recetas
  ["recetas/Best Homemade Brownies-1.txt"
   "recetas/Lemon Cake-1.txt"
   "recetas/Fettuccine Alfredo.txt"
   "recetas/Pan-Seared Steak with Garlic Butter.txt"
   "recetas/Chimichurri Sauce.txt"])

;; ------------------------------------------
;; 🧾 Leer y parsear options.txt sin librerías
;; ------------------------------------------

(defn leer-options [ruta]
  (let [lineas (split-lines (slurp ruta))]
    (into {}
          (map (fn [linea]
                 (let [partes (split linea ": ")
                       clave (keyword (nth partes 0))
                       valor (lower-case (nth partes 1))]
                   [clave valor]))
               lineas))))

;; ------------------------------------------
;; 🔍 Buscar si una palabra está en un texto
;; ------------------------------------------

(defn contiene-palabra? [texto palabra]
  (let [contenido (.toLowerCase texto)
        palabras (split (.toLowerCase palabra) " ")]
    (some (fn [p] (.contains contenido p)) palabras)))


;; ------------------------------------------
;; 📁 Leer archivos y aplicar filtro
;; ------------------------------------------

(defn filtrar-recetas [archivos palabra-clave]
  (filter
   (fn [ruta]
     (let [contenido (slurp ruta)]
       (if (= palabra-clave "all")
         true
         (contiene-palabra? contenido palabra-clave))))
   archivos))

;; ------------------------------------------
;; 🏁 Función principal
;; ------------------------------------------
;; ------------------------------------------
;; 🍽️ Extraer porciones, temperatura e ingredientes
;; ------------------------------------------

(defn extraer-porciones [texto]
  (let [lineas (split-lines texto)
        linea (first (filter #(not (nil? (re-find #"(?i)serves\s*-\s*\d+" %))) lineas))]
    (if linea
      (let [partes (split linea "-")
            porciones (nth partes 1)]
        (clojure.string/trim porciones))  ;; puedes reemplazar esto con .trim si no usas clojure.string
      "null")))

(defn extraer-temperatura [texto]
  (let [lineas (split-lines texto)
        linea (first (filter #(re-find #"\d+\s*°[FCfc]" %) lineas))]
    (if linea
      (let [temp (re-find #"\d+\s*°[FCfc]" linea)]
        temp)
      "null")))

(defn extraer-ingredientes [texto]
  (let [lineas (split-lines texto)
        start (->> lineas (map-indexed vector)
                   (filter (fn [[_ l]] (re-find #"(?i)ingredients" l)))
                   first
                   first)]
    (if start
      (let [seccion (subvec lineas (inc start))
            ingredientes (take-while #(not (re-find #"(?i)instructions" %)) seccion)]
        (if (empty? ingredientes) ["null"] ingredientes))
      ["null"])))

;; ------------------------------------------
;; 🧾 Procesar receta completa
;; ------------------------------------------

(defn procesar-receta [ruta]
  (let [contenido (slurp ruta)
        porciones (extraer-porciones contenido)
        temperatura (extraer-temperatura contenido)
        ingredientes (extraer-ingredientes contenido)]
    (println "\n📄 Receta:" ruta)
    (println "Porciones originales:" porciones)
    (println "Temperatura:" temperatura)
    (println "Ingredientes:")
    (doseq [i ingredientes]
      (println "•" i))))

(defn -main []
  (let [opciones (leer-options "options.txt")
        palabra (:filtra opciones)
        recetas-filtradas (filtrar-recetas archivos-recetas palabra)]
    (println "\nRecetas encontradas con filtro:" palabra)
    (if (empty? recetas-filtradas)
      (println "→ Ninguna receta coincide con el filtro.")
      (doseq [ruta recetas-filtradas]
        (println "✔" ruta)))))

(-main)
