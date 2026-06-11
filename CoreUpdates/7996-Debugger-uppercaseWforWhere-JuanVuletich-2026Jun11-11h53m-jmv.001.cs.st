'From Cuis7.9 [latest update: #7995] on 11 June 2026 at 11:56:28 am'!

!DebuggerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/Jun/2026 11:55:49'!
contextStackMenuSpec
	"Spec for the menu and shortcut keys for call stack list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(10	'Browse (b)' 				$b 	window 	browseMethodFull 		nil 	'open a Browser on method')
		#(20	'Full Stack (f)' 			$f 	window 	fullStack 				nil 	'show the complete call stack')
		#(30	'Proceed (p)' 			$p 	window 	proceed 				nil 	'close the debugger and proceed')
		#(40	'Restart (r)' 				$r 	window 	restart 					nil 	'reset this context to its start')
		#(50	'Step Into Send (e)' 		$e 	window 	send 					nil 	'step into message sends')
		#(60	'Step Over Send (t)' 		$t 	window 	doStep 					nil 	'step over message sends')
		#(70	'Step into Block (T)' 		$T 	window 	stepIntoBlock 			nil 	'step into a code block')
		#(80	'Run to Cursor' 			nil 	window 	runToCursor 			nil 	'run to cursor')
		#(90	'Where (W)' 				$W 	window 	where 					nil 	'select current pc range')
		#(100	'Peel to First like this' 	nil 	window 	peelToFirst 			nil 	'peel stack back to the second occurance of selected message')
		#(110 	'-----')
		#(120	'Return entered Value' 	nil 	window 	returnValue 			nil 	'lets you enter an expression and return method answering result')
		#(130	'Toggle break on Entry' 	nil 	model 		toggleBreakOnEntry 	nil 	'install or uninstall a breakpoint on method entry')
		
		"Keyboard shortcut only. No menu entry."
		#(991	nil 						$m 	window 	browseImplementors)
		#(992	nil 						$n 	window 	browseSenders)
		#(993	nil 						$i 	window 	methodInheritance)
		#(994	nil 						$v 	window 	browseVersions)
		#(995	nil 						$N 	window 	browseClassRefs)
	)! !

