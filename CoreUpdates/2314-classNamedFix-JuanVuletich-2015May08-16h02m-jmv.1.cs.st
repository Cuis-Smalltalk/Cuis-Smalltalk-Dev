'From Cuis 4.2 of 25 July 2013 [latest update: #2312] on 8 May 2015 at 4:12:53.282299 pm'!

!SystemDictionary methodsFor: 'class names' stamp: 'jmv 5/8/2015 16:12'!
classNamed: className 
	"className is either a class name or a class name followed by ' class'.
	Answer the class or metaclass it names.
	Answer nil if no class by that name."
	"
	Smalltalk classNamed: #Point
	Smalltalk classNamed: 'Point'
	Smalltalk classNamed: 'Point class'
	Smalltalk classNamed: 'BogusClassName'
	Smalltalk classNamed: 'BogusClassName class'

	Smalltalk classNamed: #Display
	Smalltalk classNamed: 'Display'
	Smalltalk classNamed: 'Display class'
	"

	Smalltalk at: className asSymbol ifPresent: [ :found |
		^ found isBehavior ifTrue: [ found ]].

	(className withoutSuffix: ' class') ifNotNil: [ :baseName |
		Smalltalk at: baseName asSymbol ifPresent: [ :found |
			^ found isBehavior ifTrue: [ found class ]]].

	^ nil! !

