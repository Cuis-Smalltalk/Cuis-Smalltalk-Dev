'From Cuis 4.2 of 25 July 2013 [latest update: #2299] on 3 May 2015 at 7:13:33.146583 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/3/2015 19:13'!
printStuffToCleanOnImageSave
	"So far, to serve experiments.
	Some day, when this is empty, we'll be closer to bootstrap from sources."
	| n nonNilVars m classInstVars v|
	n _ 0.
	""
	'--------' print.
	'Proceeses: ' print.
	((Process allSubInstances reject: [:each | each isTerminated ])
		sort: [ :a :b | a priority >= b priority ]) do: [ :p | p print ].
	'======' print.
	'Globals: ' print.
	Smalltalk globals associationsDo: [ :g | " {g key. g value class }" g  print ].
	'======' print.
	'ClassVars (notNil):  ' print.
	n _ 0.
	Smalltalk hierachySortedAllClassesDo: [ :cls |
		"Cleared, but come again for lazy init during this report.
		therefore, not required for bootstrap from sources"
		({ Scanner } includes: cls) ifFalse: [
			nonNilVars _ cls classPool select: [ :var | var notNil ].
			nonNilVars notEmpty ifTrue: [
				n _ n + 1.
				{ cls. nonNilVars keys} print ]]].
	'======' print.
	'InstClassVars (notNil): ' print.
	m _ 0.
	Smalltalk hierachySortedAllClassesDo: [ :cls |
		classInstVars _ Dictionary new.
		cls class allRegularInstVarNames do: [ :nam |
			v _ cls instVarNamed: nam.
			v ifNotNil: [
				classInstVars at: nam put: v ]].
		classInstVars notEmpty ifTrue: [
			m _ m + 1.
			{ cls class. classInstVars keys }  print ]].
	'======' print.
	
	{ n. m}  print! !


!Scanner class methodsFor: 'cached class state' stamp: 'jmv 5/3/2015 19:11'!
releaseClassCachedState
	TypeTable _ nil! !

