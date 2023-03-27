'From Cuis 6.0 [latest update: #5727] on 27 March 2023 at 12:00:10 pm'!

!SmalltalkEditor methodsFor: 'editing keys' stamp: 'jmv 3/27/2023 11:57:17'!
cancel: aKeyboardEvent
	"Cancel all edits done. Revert to saved version."

	model refetch.
	^ true! !


!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 3/27/2023 11:51:08'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	^#(
		#(		$r	#acceptAndDebugTest:							'Saves method and debugs it')
		#(		$t	#acceptAndTest:							'Saves method, runs it as test and then all tests in class. Opens debugger if error')
		#(		$y	#acceptAndTestAll:							'Saves method, runs it as test and then all tests in class category. Opens debugger if error')
		#(		$i	#inspectIt:							'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
		#(		$p	#printIt:							'Print it (selection is a valid expression)')

		#(		$s	#save:							'Save (i.e. accept)')
		#(		$d	#doIt:							'Do it (selection is a valid expression)')
		#(		$l	#cancel:							'Cancel all edits')

		#(		$b	#browseIt:							'Browse it (selection is a class name or cursor is over a class-list or message-list)')
		#(		$n	#sendersOfIt:							'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
		#(		$m	#implementorsOfIt:							'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')

		#(		$E	#methodStringsContainingit:							'Method strings containing it')
		#(		$I	#exploreIt:							'Inspect via Object Explorer')

		#(		$Q	#argPrev:							'Previous argument')
		#(		$W	#argNext:							'Next argument')
		#(		$D	#debugIt:							'Debug it')

		#(		$N	#referencesToIt:							'References to it (selection is a class name, or cursor is over a class-list or message-list)')
	)! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

	Editor initialize!

