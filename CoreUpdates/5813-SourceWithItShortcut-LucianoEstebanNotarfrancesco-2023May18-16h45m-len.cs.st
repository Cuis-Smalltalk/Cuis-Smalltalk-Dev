'From Cuis 6.0 [latest update: #5801] on 26 May 2023 at 5:27:00 pm'!

!SmalltalkEditor methodsFor: 'editing keys' stamp: 'len 5/26/2023 14:56:04'!
methodSourceContainingIt: aKeyboardEvent
	"Triggered by Cmd-e"

	self methodSourceContainingIt.
	^ true! !

!SmalltalkEditor methodsFor: 'editing keys' stamp: 'len 5/26/2023 14:56:27'!
methodStringsContainingIt: aKeyboardEvent
	"Triggered by Cmd-E"

	self methodStringsContainingIt.
	^ true! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'len 5/26/2023 14:55:52'!
methodStringsContainingIt
	"Open a browser on methods which contain the current selection as part of a string constant."

	self lineSelectAndEmptyCheck: [^ self].
	Smalltalk browseMethodsWithString: self selectedString! !


!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'len 5/26/2023 14:55:03'!
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

		#(		$d	#doIt:							'Do it (selection is a valid expression)')
		#(		$l	#cancel:							'Cancel all edits')

		#(		$b	#browseIt:							'Browse it (selection is a class name or cursor is over a class-list or message-list)')
		#(		$n	#sendersOfIt:							'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
		#(		$m	#implementorsOfIt:							'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')

		#(		$E	#methodStringsContainingIt:							'Method strings containing it')
		#(		$e	#methodSourceContainingIt:							'Method source containing it')
		#(		$I	#exploreIt:							'Inspect via Object Explorer')

		#(		$Q	#argPrev:							'Previous argument')
		#(		$W	#argNext:							'Next argument')
		#(		$D	#debugIt:							'Debug it')

		#(		$N	#referencesToIt:							'References to it (selection is a class name, or cursor is over a class-list or message-list)')
	)! !

!SmalltalkEditor class methodsFor: 'menu' stamp: 'len 5/26/2023 14:56:41'!
smalltalkEditorMenuOptions
	
	^`{
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'Help...'.
				#selector 		-> 		#openHelp.
				#icon 			-> 		#helpIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'Do it (d)'.
				#selector 		-> 		#doIt.
				#icon 			-> 		#doItIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'Print it (p)'.
				#selector 		-> 		#printIt.
				#icon 			-> 		#printIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'Inspect it (i)'.
				#selector 		-> 		#inspectIt.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		40.
				#label 			-> 		'Explore it (I)'.
				#selector 		-> 		#exploreIt.
				#icon 			-> 		#exploreIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		50.
				#label 			-> 		'Debug it (D)'.
				#selector 		-> 		#debugIt.
				#icon 			-> 		#debugIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		60.
				#label 			-> 		'Profile it'.
				#selector 		-> 		#profileIt.
				#icon 			-> 		#clockIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		5.
				#label 			-> 	SmalltalkEditor editMenuLabel.
				#selector 		-> 		#yourself.
				#icon 			-> 		#textEditorIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'Explain'.
				#selector 		-> 		#explain.
				#icon 			-> 		#helpIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'Browse it (b)'.
				#selector 		-> 		#browseIt.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		30.
				#label 			-> 		'Senders of it (n)'.
				#selector 		-> 		#sendersOfIt.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		40.
				#label 			-> 		'Implementors of it (m)'.
				#selector 		-> 		#implementorsOfIt.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		50.
				#label 			-> 		'References to it (N)'.
				#selector 		-> 		#referencesToIt.
				#icon 			-> 		#addressBookIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'Method Strings with it (E)'.
				#selector 		-> 		#methodStringsContainingIt.
				#icon 			-> 		#genericTextIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'Method Source with it (e)'.
				#selector 		-> 		#methodSourceContainingIt.
				#icon 			-> 		#scriptIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		30.
				#label 			-> 		'Class Comments with it'.
				#selector 		-> 		#classCommentsContainingIt.
				#icon 			-> 		#chatIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		10.
				#label 			-> 		'Accept (s)'.
				#selector 		-> 		#acceptContents.
				#icon 			-> 		#acceptIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		30.
				#label 			-> 		'Accept & Run Test in Class (t)'.
				#selector 		-> 		#acceptAndTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		40.
				#label 			-> 		'Accept & Run Test in Category (y)'.
				#selector 		-> 		#acceptAndTestAll.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		50.
				#label 			-> 		'Accept & Debug Test (r)'.
				#selector 		-> 		#acceptAndDebugTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
		}, SmalltalkEditor smalltalkEditorEditingMenuOptions `! !

!methodRemoval: SmalltalkEditor #methodStringsContainingit: stamp: 'len 5/26/2023 14:55:10'!
SmalltalkEditor removeSelector: #methodStringsContainingit:!
!methodRemoval: SmalltalkEditor #methodStringsContainingit stamp: 'len 5/26/2023 14:56:53'!
SmalltalkEditor removeSelector: #methodStringsContainingit!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

Editor initialize!

