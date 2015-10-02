'From Cuis 4.2 of 25 July 2013 [latest update: #2506] on 27 September 2015 at 7:51:41.698036 pm'!

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 9/27/2015 19:43'!
allMethodsSourceStringMatching: aString
	"Answer a sorted Collection of all the methods that contain, in source code, aString as a substring. 
	Search the class comments also.
	Argument might include $*, that matches any subsequence.
	For example, try:
		ensure:*[*close*]
	"
	| list classCount adder |
	list _ Set new.
	adder _ [ :mrClass :mrSel |
	list add:
		(MethodReference new
			setStandardClass: mrClass
			methodSymbol: mrSel) ].
	'Searching all source code...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: Smalltalk classNames size
		during: [ :bar |
			classCount _ 0.
			Smalltalk allClassesDo: [ :class |
				bar value: (classCount _ classCount + 1).
				(Array
					with: class
					with: class class) do: [ :cl |
					cl selectorsDo: [ :sel |
						(aString match: (cl sourceCodeAt: sel)) ifTrue: [
							adder
								value: cl
								value: sel ]].
					
					(aString match: cl organization classComment asString) ifTrue: [
						adder
							value: cl
							value: #Comment ]]]].
	^ list asArray sort.! !


!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 9/27/2015 19:50'!
browseMethodsWithSourceString: aString
	"Smalltalk browseMethodsWithSourceString: 'SourceString' "
	"Launch a browser on all methods whose source code contains aString as a substring."

	| caseSensitive suffix selectString |
	Sensor controlKeyPressed ifTrue: [
		selectString _ ((aString findTokens: '*') sorted: [ :a :b | a size > b size ]) first.
		^ self 
			browseMessageList: (self allMethodsSourceStringMatching: '*', aString, '*')
			name: 'Methods matched by ' , aString printString , ' (no ctrl for exact match)'
			autoSelect: selectString
		].
	suffix _ (caseSensitive _ Sensor shiftPressed)
		ifTrue: [' (case-sensitive. Use ctrl for $* pattern match.)']
		ifFalse: [' (shift for case-sensitive. ctrl for $* pattern match.)'].
	^ self 
		browseMessageList: (self allMethodsWithSourceString: aString matchCase: caseSensitive)
		name: 'Methods containing ' , aString printString , suffix
		autoSelect: aString! !

