(import sys re os hy collections urllib.request subprocess socket threading queue importlib)  

(setv mailboxes (collections.defaultdict queue.Queue))

(defn get-tape-str [t start-d]
  (setv chars [] curr start-d)
  (while (!= (get t curr) 0)
    (.append chars (chr (get t curr)))
    (setv curr (+ curr 1)))
  (.join "" chars))

(defn write-tape-str [t start-d s]
  (for [[i c] (enumerate s)]
    (setv (get t (+ start-d i)) (ord c)))
  (setv (get t (+ start-d (len s))) 0))

(defn bmap [c] 
  (setv bm {} st [])
  (for [[i x] (enumerate c)]
    (cond
      (= x "[") (.append st i) 
      (= x "]") (setv p (.pop st) (get bm p) i (get bm i) p)))
  bm)

(defn get-lisp-block [t d]
  (setv chars [] curr d)
  (while (!= (get t curr) 0)
    (setv val (get t curr))
    (.append chars (if (isinstance val int) (chr val) (str val)))
    (setv curr (+ curr 1)))
  (.join "" chars))

(defn eval-homoiconic [code-str t d]
  (try
    (if code-str
      (do
        (setv res (hy.eval (hy.read code-str) :locals {"t" t "d" d "sys" sys}))
        (not (= res False)))
      False)
    (except [e Exception] (do (print e) False))))

(defn run [c [hl False] [t None] [d 0]] 
  (setv t (or t (collections.defaultdict int)) i 0 m 0 ob [] bm (bmap c))
  
  (while (< i (len c))
    (setv x (get c i))
    (cond
       (= x "^") (setv m (- 1 m))

      (and (= m 0) (= x "+")) (setv (get t d) (+ (get t d) 1)) 
      (and (= m 0) (= x "-")) (setv (get t d) (- (get t d) 1)) 
      
      (and (= m 0) (= x "[")) 
              (cond
                (= d -1000) (do
                              (setv host (get-tape-str t -1100))       
                              (setv port (int (get-tape-str t -1200))) 
                              (setv payload (get-tape-str t -1300))
                              (try
                                (with [s (socket.socket socket.AF_INET socket.SOCK_STREAM)] 
                                  (.settimeout s 5.0)
                                  (.connect s (, host port))
                                  (.sendall s (.encode payload "utf-8"))
                                  (setv resp (.decode (.recv s 4096) "utf-8"))
                                  (write-tape-str t -1400 resp)) 
                                (except [e Exception] (write-tape-str t -1400 "ERR"))))

                (= d 69099) (do
                              (setv mode (chr (get t 69000))) 
                              (setv path (get-tape-str t 69001))   
                              (try
                                (if (= mode "r") 
                                  (with [f (open path "r")]
                                    (write-tape-str t 69100 (.read f)))
                                  (do 
                                    (setv content (get-tape-str t 69100))
                                    (with [f (open path "w")] 
                                      (.write f content))
                                    (write-tape-str t 69100 "OK"))) 
                                (except [e Exception] (write-tape-str t 69100 "ERR"))))

                (= d -7777) (do
                              (setv mod-name (get-tape-str t d))
                              (try
                                (setv (get t d) (importlib.import_module mod-name))
                                (except [e Exception] (write-tape-str t d "ERR"))))
                
                (= d -734) (do
                             (setv url (get-tape-str t d)) 
                             (try
                               (with [req (urllib.request.urlopen url)]
                                 (write-tape-str t d (.decode (.read req) "utf-8"))) 
                               (except [e Exception] (write-tape-str t d "ERR"))))

                (= d 42069) (do
                              (setv cmd (get-tape-str t d))
                              (try 
                                (setv out (.decode (subprocess.check_output cmd :shell True) "utf-8"))
                                (write-tape-str t d out) 
                                (except [e Exception] (write-tape-str t d "ERR"))))                               

                (= d -8192) (do
                              (setv fp (get-tape-str t d)) 
                              (try
                                (with [f (open fp "r")]
                                  (write-tape-str t d (.read f))) 
                                (except [e Exception] (write-tape-str t d "ERR"))))

                (= d 1337) (do
                             (setv chunk (get-tape-str t d)) 
                             (write-tape-str t d (get chunk (slice None None -1))))

                (= d 88888) (do
                              (setv thread-code (get-tape-str t d))
                              (try
                                (.start (threading.Thread :target (fn [] (run thread-code False None 0))))
                                (write-tape-str t d "OK")
                                (except [e Exception] (write-tape-str t d "ERR"))))

                (= d 88889) (do
                              (setv raw-msg (get-tape-str t d))
                              (try
                                 (setv parts (.split raw-msg "|" 1))
                                 (if (= (len parts) 2)
                                   (do
                                     (.put (get mailboxes (get parts 0)) (get parts 1))
                                     (write-tape-str t d "OK"))
                                   (write-tape-str t d "ERR:BAD_FORMAT"))
                                 (except [e Exception] (write-tape-str t d "ERR"))))

                (= d 88890) (do
                              (setv channel-id (get-tape-str t d)) 
                              (try
                                (setv msg (.get (get mailboxes channel-id)))
                                (write-tape-str t d msg)
                                (except [e Exception] (write-tape-str t d "ERR"))))
 

                True (do 
                       (setv val (get t d)) 
                       (setv ch (if (isinstance val int) (chr val) (str val)))
                       (if hl 
                         (.append ob ch) 
                         (do (.write sys.stdout ch) (.flush sys.stdout))))) 
      
      (and (= m 0) (= x "]")) (when (not hl) (setv (get t d) (ord (.read sys.stdin 1))))

      (and (= m 1) (= x "+")) (setv d (+ d 1))
      (and (= m 1) (= x "-")) (setv d (- d 1))
      (and (= m 1) (= x "[")) (do 
                                (setv code (get-lisp-block t d)) 
                                (when (not (eval-homoiconic code t d)) 
                                  (setv i (get bm i)))) 
    )
    (setv i (+ i 1)))
  (if hl (.join "" ob) [t d]))

(defn repl []
  (print "bi-his 1.0.0 (look ma, no head!)")
  (setv t (collections.defaultdict int) d 0)
  (while True
    (setv l (input "<< "))
    (when l
      (when (= (.strip (run l True)) "exit") (break))
      (setv [t d] (run l False t d))
      (print))))

(defn imp [c [bd "."] [v None]]
  (setv v (or v (set))) 
  (defn rep [m]
    (setv f (.strip (run (.group m 1) True)))
    (setv fp (os.path.join bd f))
    (if (in fp v) ""
      (do
        (.add v fp)
        (with [o (open fp "r")] (setv ic (.strip (.read o))))
        (setv p (.split ic "|" 1) bo (run (get p 0) True) exp f"{f}.impdecl")
        (when (!= bo exp) (raise (ValueError "Err"))) 
        (imp (get p 1) (os.path.dirname fp) v))))
  (re.sub r":impres:\s+\"([^\"]+)\"" rep c))

(when (= __name__ "__main__")
  (if (>= (len sys.argv) 2)
    (do
      (setv f (get sys.argv 1) bd (os.path.dirname (os.path.abspath f)))
      (with [o (open f "r")] (setv rc (.read o)))
      (run (imp rc bd)))
    (repl)))
