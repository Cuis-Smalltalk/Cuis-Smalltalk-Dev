'From Cuis 4.2 of 25 July 2013 [latest update: #2918] on 23 August 2016 at 10:43:51 am'!

!CodePackage methodsFor: 'testing' stamp: 'jmv 8/23/2016 10:42:15'!
isForeignClassExtension: categoryName
	categoryName ifNil: [ ^false ].
	^ categoryName first = $* and: [(self isYourClassExtension: categoryName) not]! !

