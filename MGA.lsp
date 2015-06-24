(defun cutset (InputGraph)  ; returns the cutset of the inputgraph
(let ((lstresult '()) (lstmax '()) (lstcutset '()) (lstweight '()) (lstnodes '()) (lstc '()) (lstcw '()) (var 0) (len 0))     ;initializing variables
(setf lstresult (rzeroandone InputGraph))    ;remove all the vertex of degree zero and one from the inputgraph and set it to lstresult
(setf lstcw   (mapcar #'(lambda(y) (/ 1 (length (second y)))) lstresult))
(setq lstnodes (mapcar #'(lambda(y) (first y)) lstresult))
(setq lst1  (make-list (length lstnodes) :initial-element 1 ))

(setq lstc (mapcar #'list lstnodes lstcw))
(setf lstweight (mapcar #'list lstnodes lst1))


(loop
(setf lstmin (first lstc))     ;initialize lstmax as the first of lstresult
(dolist (a (rest lstc))   ;traverese lstresult
(if (< (second a)  (second lstmin)) (setq lstmin a)) ;if degree of current node is greater than the lstmax, set lstmax to the current node
)

(setf lstcutset (cons (first lstmin) lstcutset)) 
(setf lstresult (remove (first lstmin) lstresult :key #'car))
(setf lstc (remove (first lstmin) lstc :key #'car))
(setf lstweight (remove (first lstmin) lstweight :key #'car))

(dolist (b lstresult)  ;traverse the graph
(if (find (first lstmin) (second b) :test #'equal) (progn   ;if the vertex is found  in  the neighbor list
(setf  (second b)  (remove  (first lstmin) (second b) :test #'equal))
(setf var  (- (second (find (first b) lstweight :key #'car :test #'equal)) (second lstmin)))
(setf (second (find (first b) lstweight :key #'car :test #'equal)) var)
(setf (second (find (first b) lstc :key #'car :test #'equal) ) (/ var (length (second b))))
)))
(setf copylst (copy-tree lstresult))
(print lstcutset)
(dolist (a copylst)


(setf len (length (second a)))   
(if (or (equal len 0) (equal len 1))
(progn 

(setf lstresult (remove (first a) lstresult :key #'car))
(setf lstc (remove (first a) lstc :key #'car))
(setf lstweight (remove (first a) lstweight :key #'car))
(if (equal len 1) (progn
(if (find (first (second a)) lstweight :key #'car :test #'equal) (progn
(setf var  (- (second (find (first (second a)) lstweight :key #'car :test #'equal)) (second lstmin)))
(setf (second (find (first (second a)) lstweight :key #'car :test #'equal)) var)
(setf (second (find (first (second a)) lstc :key #'car :test #'equal)) (/ var (length (second a))))

)))


))))
(if (null lstresult) (return lstcutset)))
))




(defun rzeroandone(lstmain)   ;removes vertices with degree 0 and 1 from the map
(let ((copylist '())  (lstreturn '()) (len 0) ) 
(setf copylist (copy-tree lstmain))  ;create a copy of lstmain
(setf lstreturn lstmain)     
(loop  ;loop until all  the vertices with degree 0 and 1 are removed
(let ((lstfinal '()))      ;initialize lstfinal to nil
(dolist (a lstmain)   ;traverse lst main
(setf len (length (second a)))    ;get the length of a
(if (or (equal len 0) (equal len 1))     ;check if length is 0 or 1
(progn
(setf lstfinal (cons (first a) lstfinal))     ;if it is add a to lstfinal

(setf copylist (remove (first a) copylist :key #'car ))  ;remove a from the copylist,which is copy of lstmain
)))
(if (null lstfinal) (return lstreturn)      ;if lstfinal is null,it means all 0 and 1 degree vertex are removed return the lstreturn
(progn
(setf lstreturn (removevertex lstfinal copylist))   ; else remove the  lstfinal vertex from  the neighbors list of copylist
(setf lstmain lstreturn)   ;setf lstmain to lstreturn
))
))))


(defun removevertex (lstvertex lstgraph)   ;removes a vertex from the neighbor lists of the graph
(dolist (a lstvertex)   ;traverse through the vertices to be removed
(dolist (b lstgraph)  ;traverse the graph
(if (find a (second b) :test #'equal) (progn   ;if the vertex is found  in  the neighbor list
(setf  (second b)  (remove  a (second b) :test #'equal))  ;remove the vertex from the neighbor list
))
))
lstgraph  ;return the changed graph
)