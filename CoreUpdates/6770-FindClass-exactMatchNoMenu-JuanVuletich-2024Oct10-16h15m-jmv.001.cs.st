'From Cuis7.1 [latest update: #6769] on 10 October 2024 at 4:16:01 pm'!

!BrowserWindow class methodsFor: 'GUI building' stamp: 'jmv 10/10/2024 16:00:06'!
findClassFrom: potentialClassNames ifFound: aBlock with: pattern

	| exactMatch foundClass index  classNames toMatch |
	pattern isEmpty ifTrue: [^self flash].

	"If the user entered an exact class name (including case), just use it."
	(potentialClassNames includes: pattern) ifTrue: [
		foundClass := Smalltalk at: pattern asSymbol.
		aBlock value: foundClass.
		^self ].

	toMatch := (pattern copyWithout: $.) asLowercase withBlanksTrimmed.
	classNames := self classNamesFrom: potentialClassNames with: pattern matching: toMatch.
	classNames isEmpty ifTrue: [^self flash].

	exactMatch := classNames detect: [ :each | each asLowercase = toMatch] ifNone: [ nil ].
	index := self indexOfClassFrom: classNames exactMatch: exactMatch.
	index = 0 ifTrue: [^self flash].

	foundClass := Smalltalk at: (classNames at: index) asSymbol.
	aBlock value: foundClass.! !

