'From Cuis 4.2 of 25 July 2013 [latest update: #1883] on 16 December 2013 at 11:04:08.966832 pm'!

!String methodsFor: 'converting' stamp: 'jmv 12/16/2013 22:55'!
withoutPrefix: aString
	"
	'Elvis' withoutPrefix: 'El'
	'Elvis' withoutPrefix: 'Paul'
	"
	^(self beginsWith: aString)
		ifTrue: [ self copyFrom: 1+aString size to: self size ]! !

!String methodsFor: 'converting' stamp: 'jmv 12/16/2013 22:51'!
withoutSuffix: aString
	"
	'Elvis' withoutSuffix: 'vis'
	'Elvis' withoutSuffix: 'Paul'
	"
	^(self endsWith: aString)
		ifTrue: [ self copyFrom: 1 to: self size - aString size ]! !


!SystemDictionary methodsFor: 'class names' stamp: 'jmv 12/16/2013 22:59'!
classNamed: className 
	"className is either a class name or a class name followed by ' class'.
	Answer the class or metaclass it names"
	"
	Smalltalk classNamed: 'Point'
	Smalltalk classNamed: 'Point class'
	Smalltalk classNamed: 'BogusClassName'
	Smalltalk classNamed: 'BogusClassName class'
	"
.
	Smalltalk at: className asSymbol ifPresent: [ :found | ^ found ].
	
	^(className withoutSuffix: ' class') ifNotNil: [ :baseName |
		(Smalltalk at: baseName asSymbol ifAbsent: [ ^nil ]) class ]! !

