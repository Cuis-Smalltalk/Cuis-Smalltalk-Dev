'From Cuis7.1 [latest update: #6888] on 4 December 2024 at 4:42:50 pm'!

!CodeProvider methodsFor: 'evaluation' stamp: 'jmv 12/4/2024 16:30:01'!
doItReceiver
	^ nil! !


!DebuggerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 12/4/2024 16:42:26'!
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
		#(90	'Where (w)' 				$w 	window 	where 					nil 	'select current pc range')
		#(100	'Peel to First like this' 	nil 	window 	peelToFirst 			nil 	'peel stack back to the second occurance of selected message')
		#(110 	'-----')
		#(120	'Return entered Value' 	nil 	window 	returnValue 			nil 	'lets you enter an expression and return method answering result')
		#(130	'Toggle break on Entry' 	nil 	model 		toggleBreakOnEntry 	nil 	'install or uninstall a breakpoint on method entry')
		
		#(991	nil 						$m 	window 	browseImplementors)
		#(992	nil 						$n 	window 	browseSenders)
		#(993	nil 						$i 	window 	methodInheritance)
		#(994	nil 						$v 	window 	browseVersions)
		#(995	nil 						$N 	window 	browseClassRefs)
	)! !

