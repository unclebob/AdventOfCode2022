(ns day7-no-space-left-on-device.core
  (:require [clojure.string :as string]))

(defn add-directory [path directory dir-name]
  (let [new-path (conj path dir-name)]
    (assoc-in directory new-path {})))

(defn add-file [path directory size name]
  (let [new-path (conj path name)]
    (assoc-in directory new-path size)))

(defn exec-command [path directories command]
  (cond
    (= command "$ cd /")
    [["/"] directories]

    (= command "$ cd ..")
    [(drop-last path) directories]

    (= command "$ ls")
    [path directories]

    (re-matches #"\$ cd (.+)$" command)
    (let [directory (second (re-matches #"\$ cd (.+)$" command))]
      [(conj (vec path) directory) directories])

    (re-matches #"dir (.+)$" command)
    [path (add-directory path directories (second (re-matches #"dir (.+)$" command)))]

    (re-matches #"(\d+) (.+)$" command)
    (let [[_ size name] (re-matches #"(\d+) (.+)$" command)
          size (Integer/parseInt size)]
      [path (add-file path directories size name)])

    :else
    "TILT"
    ))

(defn exec-commands [path directories commands]
  (if (empty? commands)
    directories
    (let [command (first commands)
          [path directories] (exec-command path directories command)]
      (recur path directories (rest commands)))))

(defn exec-command-file [file-name]
  (let [input (slurp file-name)
        commands (string/split-lines input)]
    (exec-commands ["/"] {"/" {}} commands)))

(defn dir-size [directory]
  (loop [names (keys directory)
         size 0]
    (if (empty? names)
      size
      (let [name (first names)
            file (directory name)]
        (if (integer? file)
          (recur (rest names) (+ size file))
          (recur (rest names) (+ size (dir-size file))))))))

(defn map-directories [path directories]
  (loop [names (keys directories)
         sizes {}]
    (if (empty? names)
      sizes
      (let [name (first names)
            file (directories name)]
        (if (integer? file)
          (recur (rest names) sizes)
          (recur (rest names) (merge (assoc sizes (conj path name) (dir-size file))
                                     (map-directories (conj path name) file))))))))

(defn total-small-directories [file-name]
  (let [directories (exec-command-file file-name)
        directory-map (map-directories [] directories)
        sizes (vals directory-map)
        small-sizes (filter #(<= % 100000) sizes)]
    (reduce + small-sizes)))

(defn find-best-to-delete [file-name]
  (let [directories (exec-command-file file-name)
        directory-map (map-directories [] directories)
        sizes (sort (vals directory-map))
        total-used (directory-map ["/"])
        free-space (- 70000000 total-used)
        needed-space (- 30000000 free-space)
        sufficient-sizes (filter #(>= % needed-space) sizes)]
    (first sufficient-sizes)))




