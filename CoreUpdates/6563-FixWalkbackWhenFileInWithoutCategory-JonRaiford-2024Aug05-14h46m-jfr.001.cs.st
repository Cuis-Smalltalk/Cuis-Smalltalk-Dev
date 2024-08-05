'From Cuis7.1 [latest update: #6561] on 5 August 2024 at 2:49:33 pm'!

!CodePackage methodsFor: 'testing' stamp: 'jfr 8/3/2024 10:43:36'!
isForeignClassExtension: categoryName
	"Answer true if <categoryName> is a foreign class extension reference"
	
	categoryName ifNil: [ ^false ].
	(categoryName size = 0) ifTrue:[^false].
	^ categoryName first = $* and: [(self isYourClassExtension: categoryName) not]! !

