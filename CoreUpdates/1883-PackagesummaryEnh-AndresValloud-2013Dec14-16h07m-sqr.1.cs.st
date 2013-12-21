'From Cuis 4.2 of 25 July 2013 [latest update: #1867] on 14 December 2013 at 5:16:55.118875 pm'!

!CodePackageList methodsFor: 'accessing' stamp: 'sqr 12/14/2013 17:16'!
summary

	| count |
	selection ifNil: [ ^'' ].
	^ String streamContents: [ :strm |
		| methods  linesOfCode |
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
		linesOfCode / methods asFloat
			printOn: strm
			showingDecimalPlaces: 2.
		strm nextPutAll: ' per method).' ]! !

