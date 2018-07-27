'From Cuis 5.0 of 7 November 2016 [latest update: #3375] on 26 July 2018 at 3:11:29 pm'!

!Editor class methodsFor: 'help' stamp: 'jmv 7/26/2018 15:10:49'!
help
	"
	Editor help
	SimpleEditor help
	CellStyleEditor help
	TextEditor help
	SmalltalkEditor help
	"
	| allSpecs |
	allSpecs _ self cmdShortcutsSpec, self basicCmdShortcutsSpec.
	^String streamContents: [ :strm |
		allSpecs do: [ :triplet | | c |
			c _ triplet first = Character space
				ifFalse: [ triplet first asString, '   	' ]
				ifTrue: [ 'Space'].
			strm nextPutAll: ('Cmd-', c, String tab, String tab, triplet third).
			strm newLine ]]! !


!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 7/26/2018 15:05:27'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	^#(
		#(		$r	#acceptAndDebugTest:		'Saves method and debugs it')
		#(		$t	#acceptAndTest:				'Saves method, runs it as test and then all tests in class. Opens debugger if error')
		#(		$y	#acceptAndTestAll:				'Saves method, runs it as test and then all tests in class category. Opens debugger if error')
		#(		$i	#inspectIt:							'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
		#(		$p	#printIt:								'Print it (selection is a valid expression)')

		#(		$s	#save:								'Save (i.e. accept)')
		#(		$d	#doIt:								'Do it (selection is a valid expression)')

		#(		$b	#browseIt:							'Browse it (selection is a class name or cursor is over a class-list or message-list)')
		#(		$n	#sendersOfIt:						'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
		#(		$m	#implementorsOfIt:				'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')

		#(		$E	#methodStringsContainingit:'Method strings containing it')
		#(		$T	#displayIfTrue:					'Insert #ifTrue:')
		#(		$I	#exploreIt:							'Inspect via Object Explorer')

		#(		$A	#argAdvance:						'Advance argument')
		#(		$D	#debugIt:							'Debug it')
		#(		$F	#displayIfFalse:					'Insert #ifFalse:')
		#(		$G	#fileItIn:							'File in selection')

		#(		$V	#pasteInitials:					'Paste author initials')
		#(		$N	#referencesToIt:					'References to it (selection is a class name, or cursor is over a class-list or message-list)')
	)! !

!SmalltalkEditor class methodsFor: 'menu' stamp: 'jmv 7/26/2018 15:04:57'!
smalltalkEditorMenuOptions
	
	^`{
			{
				#itemGroup 	-> 		10.
				#itemOrder 	-> 		10.
				#label 			-> 		'Help...'.
				#selector 		-> 		#openHelp.
				#icon 			-> 		#helpIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 	-> 		10.
				#label 			-> 		'Do it (d)'.
				#selector 		-> 		#doIt.
				#icon 			-> 		#doItIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 	-> 		20.
				#label 			-> 		'Print it (p)'.
				#selector 		-> 		#printIt.
				#icon 			-> 		#printIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 	-> 		30.
				#label 			-> 		'Inspect it (i)'.
				#selector 		-> 		#inspectIt.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 	-> 		40.
				#label 			-> 		'Explore it (I)'.
				#selector 		-> 		#exploreIt.
				#icon 			-> 		#exploreIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 	-> 		50.
				#label 			-> 		'Debug it (D)'.	
				#selector 		-> 		#debugIt.
				#icon 			-> 		#debugIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 	-> 		60.
				#label 			-> 		'Profile it'.	
				#selector 		-> 		#profileIt.
				#icon 			-> 		#clockIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 	-> 		10.
				#label 			-> 		'Explain'.
				#selector 		-> 		#explain.
				#icon 			-> 		#helpIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 	-> 		20.
				#label 			-> 		'Browse it (b)'.
				#selector 		-> 		#browseIt.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 	-> 		30.
				#label 			-> 		'Senders of it (n)'.
				#selector 		-> 		#sendersOfIt.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 	-> 		40.
				#label 			-> 		'Implementors of it (m)'.
				#selector 		-> 		#implementorsOfIt.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 	-> 		50.
				#label 			-> 		'References to it (N)'.
				#selector 		-> 		#referencesToIt.
				#icon 			-> 		#addressBookIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 	-> 		10.
				#label 			-> 		'Method Strings with it (E)'.
				#selector 		-> 		#methodStringsContainingit.
				#icon 			-> 		#genericTextIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 	-> 		20.
				#label 			-> 		'Method Source with it'.
				#selector 		-> 		#methodSourceContainingIt.
				#icon 			-> 		#scriptIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 	-> 		30.
				#label 			-> 		'Class Comments with it'.
				#selector 		-> 		#classCommentsContainingIt.
				#icon 			-> 		#chatIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 	-> 		10.
				#label 			-> 		'Accept (s)'.
				#selector 		-> 		#acceptContents.
				#icon 			-> 		#acceptIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 	-> 		30.
				#label 			-> 		'Accept & Run Test in Class (t)'.
				#selector 		-> 		#acceptAndTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 	-> 		40.
				#label 			-> 		'Accept & Run Test in Category (y)'.
				#selector 		-> 		#acceptAndTestAll.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 	-> 		50.
				#label 			-> 		'Accept & Debug Test (r)'.
				#selector 		-> 		#acceptAndDebugTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		60.
				#itemOrder 	-> 		10.
				#label 			-> 		'More...'.
				#selector 		-> 		#openMenu2.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`! !

!methodRemoval: InnerTextMorph #cancelEdits!
InnerTextMorph removeSelector: #cancelEdits!
!methodRemoval: TextEditor #cancelEdits!
TextEditor removeSelector: #cancelEdits!
!methodRemoval: TextEditor #cancelEdits:!
TextEditor removeSelector: #cancelEdits:!
