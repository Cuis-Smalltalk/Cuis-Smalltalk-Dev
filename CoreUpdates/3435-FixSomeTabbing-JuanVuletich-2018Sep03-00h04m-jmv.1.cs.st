'From Cuis 5.0 of 7 November 2016 [latest update: #3430] on 3 September 2018 at 12:07:09 am'!

!Float methodsFor: 'mathematical functions' stamp: 'jmv 9/3/2018 00:05:26'!
smoothStep
	"A Function that is
		x <= -1 		-> 		1
		-1 < x < 1 	->			smooth differentiable transition from 1 to 0
		1 <= x 		-> 		0
	
	It is a 'smooth' version of an #negative
	Has countless applications. For example in image and signal processing, but also in other fields."
	"
	| g |
	Feature require: 'Morphic-Widgets-Extras'.
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x abs < 1 ifTrue: [1] ifFalse:[0] ] color: Color brown.
	g addFunction: [ :x | x smoothStep ] color: Color red.
	(g embeddedInMorphicWindowLabeled: 'graph') openInWorld
	"
	self > -1 ifFalse: [ ^ 1 ].
	1 > self ifFalse: [ ^ 0 ].
	^ (self +2 * Float halfPi) sin * 0.5 + 0.5! !


!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 9/3/2018 00:05:46'!
mouseScroll: aMouseEvent localPosition: localEventPosition
	aMouseEvent direction
		caseOf: {
			[ #up ] 		-> 		[  scrollBar scrollUp: 1 ].
			[ #down ] 	-> 		[ scrollBar scrollDown: 1 ].
			[ #left ] 	-> 		[  hScrollBar scrollUp: 1 ].
			[ #right ] 	-> 		[  hScrollBar scrollDown: 1 ] }! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 9/3/2018 00:06:36'!
helpMenu
	"Build the help menu for the world."

	^ (self menu: 'Help...')
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'About this System...'.
				#object 			-> 		Smalltalk.
				#selector 		-> 		#aboutThisSystem.
				#icon 			->			#helpIcon.
				#balloonText 	-> 		'current version information.'
			} asDictionary.
			{
				#label 			-> 		'Terse Guide to Cuis'.
				#selector 		-> 		#openTerseGuide.
				#icon 			->			#helpIcon.
				#balloonText 	-> 		'explore Cuis Smalltalk'
			} asDictionary.
			{
				#label 			-> 		'Class Comment Browser'.
				#selector 		-> 		#openCommentGuide.
				#icon 			->			#helpIcon.
				#balloonText 	-> 		'search & explore Cuis Class Comments'
			} asDictionary.
			{
				#label 			-> 		'Code management in Cuis'.
				#object 			-> 		Utilities.
				#selector 		-> 		#openCodeManagementInCuis.
				#icon 			->			#helpIcon.
				#balloonText 	-> 		'Features are kept in Packages.'
			} asDictionary.
			{
				#label 			-> 		'Using GitHub to host Cuis packages'.
				#object 			-> 		Utilities.
				#selector 		-> 		#openCuisAndGitHub.
				#icon 			->			#helpIcon.
				#balloonText 	-> 		'GitHub usage pattern.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Editor keyboard shortcuts'.
				#object 			-> 		SmalltalkEditor.
				#selector 		-> 		#openHelp.
				#icon 			-> 		#keyboardShortcutsIcon.
				#balloonText 	-> 		'summary of keyboard shortcuts in editors for Smalltalk code.'
			} asDictionary.
			{
				#label 			-> 		'Useful Expressions'.
				#object 			-> 		Utilities.
				#selector 		-> 		#openUsefulExpressions.
				#icon 			-> 		#chatIcon.
				#balloonText 	-> 		'a window full of useful expressions.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'VM Statistics'.
				#selector 		-> 		#vmStatistics.
				#icon 			-> 		#systemMonitorIcon.
				#balloonText 	-> 		'obtain some intriguing data about the vm.'
			} asDictionary.
			{
				#label 			-> 		'Space Left'.
				#selector 		-> 		#garbageCollect.
				#icon 			-> 		#removableMediaIcon.
				#balloonText 	-> 		'perform a full garbage-collection and report how many bytes of space remain in the image.'
			} asDictionary.
		}`! !

