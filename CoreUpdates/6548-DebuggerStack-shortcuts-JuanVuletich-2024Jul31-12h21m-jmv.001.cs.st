'From Cuis7.1 [latest update: #6544] on 31 July 2024 at 12:22:11 pm'!

!DebuggerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 7/31/2024 12:18:55'!
contextStackMenuSpec
	"Spec for the menu and shortcut keys for call stack list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(10		'fullStack (f)' 						$f 	window 			fullStack 						nil 	'show the complete call stack')
		#(20		'proceed (p)' 						$p 	window 			proceed 						nil 	'close the debugger and proceed')
		#(30		'restart (r)' 						$r 	window 			restart 						nil 	'reset this context to its start')
		#(40		'step into send (e)' 						$e 	window 			send 						nil 	'step into message sends')
		#(50		'step over send (t)' 						$t 	window 			doStep 						nil 	'step over message sends')
		#(60		'step into block (T)' 						$T 	window 			stepIntoBlock 						nil 	'step into a code block')
		#(70		'run to cursor' 						nil 	window 			runToCursor 						nil 	'run to cursor')
		#(80		'where (w)' 						$w 	window 			where 						nil 	'select current pc range')
		#(90		'peel to first like this' 						nil 	window 			peelToFirst 						nil 	'peel stack back to the second occurance of selected message')
		#(100 		'-----')
		#(110		'return entered value' 						nil 	window 			returnValue 						nil 	'lets you enter an expression and return method answering result')
		#(120		'toggle break on entry' 						nil 	model 			toggleBreakOnEntry 						nil 	'install or uninstall a breakpoint on method entry')
		
		#(991		nil 						$m 	window 			browseImplementors)
		#(992		nil 						$n 	window 			browseSenders)
		#(993		nil 						$i 	window 			methodInheritance)
		#(994		nil 						$v 	window 			browseVersions)
		#(995		nil 						$N 	window 			browseClassRefs)
	)! !

