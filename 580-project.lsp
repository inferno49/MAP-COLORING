;ASHITA NAGESH G00919260
;BHAWNA MONGA G00921287
;RAKESH REDDY JAMMULA G00913614
;CS580 FALL 2014
;DR.DURIC
;PROJECT


(defun colormap (InputGraphMap)

(let ((lstcolors '((RED GREEN BLUE YELLOW))) (lstforward '()) (lstNodes '())  (lstcutset '()) ;initialize variables
(lstfinal '()) (temp '()) (lstnei '()) (color nil) (copyinput '()) (lstdomain '()) (lsttopo '()) (InputGraph '()))  


(setf InputGraph (copy-tree InputGraphMap)) 
(setq lstNodes (mapcar #'(lambda(y) (first y)) InputGraph)) ; get the nodes of the inputgraph graph

(dolist (a lstNodes)                         ; traverse the list of nodes
(setq lstforward (cons (cons a lstcolors) lstforward)))           ;associate domain (R G B Y) with each country

(setq lstforward (reverse lstforward))     ;reverse lstforward
(setq lstfinal (copy-tree lstforward))     ;make a copy of lstforward
(setq lstcutset (cutset InputGraph))      ;get the cutset  nodes of inputgraph in lstcutset
(setq copyinput (copy-tree InputGraph))   ; make a copy of inputgraph



(dolist (b lstcutset)  ; traverse through lstcutset to get the linear tree


(setf copyinput (remove b copyinput :key #'car ))    ;remove current cutset node from the inputgraph as it is already colored.
(setf copyinput (removevertex (list b) copyinput))  ;remove current cutset node from the neighboring list of the inputgraph.

)



(setf lsttopo (toposort copyinput)) ; toposort will return a list conataining the edges and the sorting order

(dolist (cut lstcutset) ;traverse the cutset to color it

(loop  ; loop until a suitable color for the cutset is found

(setf (second (find cut lstfinal :key #'first :test #'equal)) (first (second (find cut lstfinal :key #'first :test #'equal)))) ;set the color of cutset to first of the available colors
(setf color (second (find cut lstfinal :key #'first :test #'equal))) ;save the assigned color
(setf lstnei (second (find cut InputGraph :key #'first :test #'equal))) ; get the neighbors of cutset


(dolist (a lstnei)  ;traverse the neighbors of the current cutset to remove the color assigned to cutset from their domains.

(setf (second (find a lstfinal :key #'first :test #'equal)) (remove color (second (find a lstfinal :key #'first :test #'equal)))) ;remove the color assigned to cutset from its neighbor.

)

(setf lstdomain (arc (first lsttopo) lstfinal))  ;check the arc consistency of the tree with the assigned color

(if (not(null lstdomain)) (progn  ;if the tree is arc consistent
(setq temp (cons (find cut lstfinal :key #'first :test #'equal) temp)) ;add the assigned cutsetset node into the list temp.
(setf lstfinal (remove cut lstfinal :key #'car )) (setf Inputgraph (remove cut Inputgraph :key #'car ))    ;remove current cutset node from the inputgraph as it is already colored.
(setf Inputgraph (removevertex (list cut) Inputgraph)) ;remove current cutset node from the inputgraph as it is already colored
(setf lstforward (copy-tree lstfinal)) ;save the lstfinal state in lstforward
(return temp) ) 
(progn   ;if tree is not arc consistent
(setf lstfinal (copy-tree lstforward) )  ;revert back lstfinal to last saved state 
(setf (second (find cut lstfinal :key #'first :test #'equal)) (rest (second (find cut lstfinal :key #'first :test #'equal))))  ;remove the assigned color and move to next available color
)))

)
(setf temp (append temp (treesolver (first lsttopo) lstdomain (second lsttopo)))) ;color the tree at the end by and append colored cutset to it


))


(defun cutset (InputGraph)  ; returns the cutset of the inputgraph
(let ((lstresult '()) (lstmax '()) (lstcutset '()))     ;initializing variables
(setf lstresult (rzeroandone InputGraph))    ;remove all the vertex of degree zero and one from the inputgraph and set it to lstresult
(loop   ;loop until graph is empty
(setf lstmax (first lstresult))     ;initialize lstmax as the first of lstresult
(dolist (a (rest lstresult))   ;traverese lstresult
(if (> (length (second a)) (length (second lstmax))) (setq lstmax a) ;if degree of current node is greater than the lstmax, set lstmax to the current node
))

(setf lstcutset (cons lstmax lstcutset))   ;add lstmax to the cutset
(setf lstresult (remove (first lstmax) lstresult :key #'car))  ;remove lstmax from the lstresult
(setf lstresult (removevertex lstmax lstresult)) ;remove the lstmax vertex from the neighbor lists of lstresult
(setf lstresult (rzeroandone lstresult))   ;remove the vertex which have degree zero and one after removing lstmax from the lstresult

(setf lstcutset (reverse lstcutset)) 

(if (null lstresult) (return (setq lstcutset (mapcar #'(lambda(y) (first y)) lstcutset)) )) ;if lstresult is null, return cutset nodes.

)))



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

(defun toposort (inputgraph) ;sorts the linear tree and returns the list of edges and the order 
(let ((edglst '())(orderlst '()) (lstneigh '()) (flag 0)  (queue '()) (root nil) (node nil) (copyinputgraph '()) )
(setf copyinputgraph (copy-tree inputgraph))
(loop    ;loop until all the trees are ordered
(if (equal flag 0) (progn   ;if running for the first time
(setf node (first  inputgraph)) (setf flag 1))  ;setf node as the first of the input
(setf node (find (first queue) inputgraph :key #'first :test #'equal)))  ;else setf node as the first node in the queue 
(setf copyinputgraph (remove (first node) copyinputgraph :key #'car :test #'equal ))  ;remove the node from the copyinputgraph
(setf root (first node))   ;set the root as first of the node
(setf orderlst (cons  root orderlst))  ;add root to the orderlist
(setf queue (remove root queue  :test #'equal))   ;remove root from the queue

(setf lstneigh (second node))  ;get the neighbors of the root

(dolist (b lstneigh)   ;traverese neighbors of the root

(if (not (find b orderlst :test #'equal))    ;if neighbor is  not there in the orderlist, it means it is not traverses yet
(progn
(setf edglst (cons (list root b) edglst )) ;add the edge (root neighbor) to the edgelist
(setf queue (append (list b) queue))(setf queue (reverse queue)) )  ;add the neighbor to the queue
))
 

(cond ((and (null queue) (null copyinputgraph)) (progn (return (list edglst (reverse orderlst))))) ;if both queue and copyinputgraph are null return the list of edgelist and orderlist
( (and (null queue) (not (null copyinputgraph))) (progn  (setf inputgraph (copy-tree copyinputgraph)) (setf flag 0)  ;if only queue is null, setf flag to 0 and traverse it for next tree
))
)
)
))

(defun revise (lstdomp lstdomc) ;removes in-consistent domains from the parent domain and return t if any changes are made in domain
(let (( revised nil ) (ldc '()) ) ;
(dolist (a (second lstdomp))     ;loop for each domain in the parent
(setf ldc (remove a (second lstdomc))) ;if a domain in the child does not allow the contstraint between the parent
(if  (null ldc)                        ;and child to be satisfied then remove that domain
(progn (setf (second lstdomp) (remove a (second lstdomp)))
(setf revised t)))
)
revised ;return the revised value to chech arc consistancy
))


(defun arc (lstarc lstdomain) ;makes the linear tree arc-consistent and returns false if it is not possible 
(let ((q '())  (lstdomp '()) (lstdomc '())(temp '()) (revised nil)  ) 
(setf q (copy-tree lstarc)) ;copies the contents of lstarc to the queue q
(loop 
(setf temp ( pop q)) ;pop the first parent-child from the topologically sorted tree
(setf lstdomp (find (first temp) lstdomain :key #'first :test #'equal)) ; check the domain of the parent
(setf lstdomc (find (second temp) lstdomain :key #'first :test #'equal)) ; check the domain of the child
(setf revised (revise lstdomp lstdomc)) ; call the function revise on the domains of current parent-child
(if (equal revised t) (if (null (second lstdomp)) (return nil))) ; if the domain is 0 return nil
(if (null q) (return lstdomain)) ;when the queue is exhausted return the list of domains
)
))

(defun treesolver (lstedges lstdomains lstorder) ;colors the linear tree, takes the edges, order and lstdomain as parameter
(let ((colorfirst nil) (parent nil) (colorp nil) (lstdome '()) (lstcfinal '()))
(loop  ;loop until all the nodes in orderlist are colored
(setf colorfirst (pop lstorder))  ;pop the element from the lstorder
(setf parent (first (find colorfirst lstedges :key #'second :test #'equal))) ;get the parent of the current element from the edge list
(setf colorp (second (find parent lstcfinal :key #'first :test #'equal))) ;get the color of the parent
(setf lstdome (second (find colorfirst lstdomains  :key #'first :test #'equal )))  ;get the available colors of element from the domain list
(setf lstdome (remove colorp lstdome)) ;remove the parent color from the domain
(setf lstcfinal (cons (list colorfirst (first lstdome)) lstcfinal))  ;color the elemnt with the left available color and add the pair to final list

(if (null lstorder) (return lstcfinal)) ;if all the elements are colored return lstcfinal
)
))


