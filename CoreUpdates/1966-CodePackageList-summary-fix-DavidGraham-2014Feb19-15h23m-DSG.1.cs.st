'From Cuis 4.2 of 25 July 2013 [latest update: #1962] on 19 February 2014 at 3:25:21.882943 pm'!

!CodePackageList methodsFor: 'accessing' stamp: 'DSG 2/19/2014 15:24'!
summary

	| count |
	selection ifNil: [ ^'' ].
	^ String streamContents: [ :strm |
		| methods  linesOfCode linesPerMethod |
		strm
			nextPutAll: 'Package: ';
			nextPutAll: selection packageName;
			nextPutAll: ' -- ';
			nextPutAll: selection sourceSystem.
		strm nextPutAll: ' -- Number of system categories '.
		selection systemCategories size printOn: strm.
		strm nextPutAll: '.'.
		strm nextPutAll: ' -- Number of classes: '.
		count _ 0.
		selection classesDo: [ :cls | count _ count + 1 ].
		count printOn: strm.
		strm nextPutAll: '. Number of extension methods: '.
		selection extensionMethods size printOn: strm.
		strm nextPutAll: '. Total number of methods: '.
		methods := selection methods size.
		methods printOn: strm.
		strm nextPutAll: '. Total lines of code: '.
		linesOfCode := selection linesOfCode.
		linesOfCode printOn: strm.
		strm nextPutAll: ' ('.
		(methods > 0)
			ifTrue: [ linesPerMethod := linesOfCode / methods ]
			ifFalse: [ linesPerMethod := 0 ].
		linesPerMethod asFloat printOn: strm fractionDigits: 2.
		strm nextPutAll: ' per method).' ]! !