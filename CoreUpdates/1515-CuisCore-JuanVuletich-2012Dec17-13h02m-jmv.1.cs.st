'From Cuis 4.1 of 12 December 2012 [latest update: #1514] on 17 December 2012 at 1:11:33 pm'!

!AutoCompleterMorph class methodsFor: 'instance creation' stamp: 'jmv 12/17/2012 13:11'!
initializedInstance
	| completer |
	completer _ SmalltalkCompleter withModel: (TextModel withText: 'Small').
	completer
		instVarNamed: 'position'
		put: 5.
	completer computeEntries.
	^ AutoCompleterMorph
		completer: completer
		position: 200 @ 200! !

