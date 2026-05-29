'From Cuis7.7 [latest update: #7970] on 29 May 2026 at 1:19:42 pm'!

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 25/Jul/2023 18:44:52'!
transformAnd: encoder
	(Preferences at: #allowNonBooleanReceiversOfBooleanMessages) ifTrue: [
		(receiver == NodeTrue) | (receiver == NodeFalse) ifFalse: [
			^false ]].
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				{
					(arguments at: 1) noteOptimizedIn: self.
					(BlockNode withJust: NodeFalse) noteOptimizedIn: self}.
			^true]
		ifFalse: 
			[^false]! !

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 25/Jul/2023 18:44:56'!
transformIfFalse: encoder
	(Preferences at: #allowNonBooleanReceiversOfBooleanMessages) ifTrue: [
		(receiver == NodeTrue) | (receiver == NodeFalse) ifFalse: [
			^false ]].
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				{
					(BlockNode withJust: NodeNil) noteOptimizedIn: self.
					(arguments at: 1) noteOptimizedIn: self}.
			^true]
		ifFalse:
			[^false]! !

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 3/Sep/2025 10:12:44'!
transformIfNil: encoder

	(Preferences at: #avoidIfNilOptimization) ifTrue: [
		^false ].

	"vb: Removed the original transformBoolean: which amounds to a test we perform in each of the branches below."
	(MacroSelectors at: special) = #ifNotNil: ifTrue:
		[(self checkBlock: arguments first as: 'ifNotNil arg' from: encoder maxArgs: 1) ifFalse:
			[^false].

		"Transform 'ifNotNil: [stuff]' to 'ifNil: [nil] ifNotNil: [stuff]'.
		Slightly better code and more consistent with decompilation."
		self noteSpecialSelector: #ifNil:ifNotNil:.
		selector := SelectorNode new key: (MacroSelectors at: special) code: #macro.
		arguments := {
						(BlockNode withJust: NodeNil) noteOptimizedIn: self.
						arguments first noteOptimizedIn: self}.
		(self transform: encoder) ifFalse:
			[self error: 'compiler logic error'].
		^true].
	(self checkBlock: arguments first as: 'ifNil arg' from: encoder maxArgs: 0) ifFalse:
		[^false].
	arguments first noteOptimizedIn: self.
	^true! !

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 25/Jul/2023 18:45:03'!
transformIfTrue: encoder
	(Preferences at: #allowNonBooleanReceiversOfBooleanMessages) ifTrue: [
		(receiver == NodeTrue) | (receiver == NodeFalse) ifFalse: [
			^false ]].
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				{
					(arguments at: 1) noteOptimizedIn: self.
					(BlockNode withJust: NodeNil) noteOptimizedIn: self}.
			^true]
		ifFalse: 
			[^false]! !

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 25/Jul/2023 18:45:09'!
transformOr: encoder
	(Preferences at: #allowNonBooleanReceiversOfBooleanMessages) ifTrue: [
		(receiver == NodeTrue) | (receiver == NodeFalse) ifFalse: [
			^false ]].
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				{
					(BlockNode withJust: NodeTrue) noteOptimizedIn: self.
					(arguments at: 1) noteOptimizedIn: self}.
			^true]
		ifFalse: 
			[^false]! !


!Behavior methodsFor: 'user interface' stamp: 'jmv 13/Mar/2012 21:53:00'!
crossReference
	"Answer an Array of arrays of size 2 whose first element is a message selector in the receiver's method dictionary and whose second element is a set of all message selectors in the method dictionary whose methods send a message with that selector. Subclasses are not included."

	^self selectors asArray sort collect: [ :x |
		{
			String newLineString, x.
			self whichSelectorsReferTo: x} ]

	"
	Point crossReference.
	"! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'HAW 17/Jan/2024 20:36:20'!
allMethodsSourceStringMatching: aString
	"Answer a sorted Collection of all the methods that contain, in source code, aString as a substring.
	Search the class comments also.
	Argument might include $*, that matches any subsequence.
	For example, try:
		ensure:*[*close*]
	"
	| list classCount adder |
	list := Set new.
	adder := [ :mrClass :mrSel |
		list add: (MethodReference class: mrClass selector: mrSel) ].
	'Searching all source code...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self classNames size
		during: [ :barBlock |
			classCount := 0.
			self allClassesDo: [ :class |
				barBlock value: (classCount := classCount + 1).
				{
					class.
					class class} do: [ :cl |
					cl selectorsDo: [ :sel |
						(aString match: (cl sourceCodeAt: sel)) ifTrue: [
							adder
								value: cl
								value: sel ]].

					(aString match: cl organization classComment) ifTrue: [
						adder
							value: cl
							value: #Comment ]]]].
	^ list asArray sort.! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'HAW 17/Jan/2024 20:38:01'!
allMethodsWithSourceString: aString matchCase: caseSensitive
	"Answer a sorted Collection of all the methods that contain, in source code, aString as a substring.  Search the class comments also"
	| list classCount adder |
	list := Set new.
	adder := [ :mrClass :mrSel |
		list add: (MethodReference class: mrClass selector: mrSel) ].
	'Searching all source code...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self classNames size
		during: [ :barBlock |
			classCount := 0.
			self allClassesDo: [ :class |
				barBlock value: (classCount := classCount + 1).
				{
					class.
					class class} do: [ :cl |
					cl selectorsDo: [ :sel |
						((cl sourceCodeAt: sel)
							findString: aString
							startingAt: 1
							caseSensitive: caseSensitive) > 0 ifTrue: [
							adder
								value: cl
								value: sel ]].
					(cl organization classComment
						findString: aString
						startingAt: 1
						caseSensitive: caseSensitive) > 0 ifTrue: [
						adder
							value: cl
							value: #Comment ]]]].
	^ list asArray sort.! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jfr 19/Aug/2024 09:44:30'!
allMethodsWithSourceString: aString
matchCase: caseSensitive
scopeClass: scopeClass
includeSubclasses: includeSubclasses
includeSuperclasses: includeSuperclasses
	"Answer a sorted Collection of all the methods that contain, in source code, aString as a substring.  Search the class comments also"
	| list classCount adder classesToSearch |
	list := Set new.
	adder := [ :mrClass :mrSel |
		list add: (MethodReference class: mrClass selector: mrSel) ].
	
	"Determine classes to search"
	scopeClass
		ifNil:[classesToSearch := self allClasses]
		ifNotNil:[
			classesToSearch := Set new.
			classesToSearch add: scopeClass.
			(includeSuperclasses)
				ifTrue:[scopeClass allSuperclassesDo: [:aClass | classesToSearch add: aClass]].
			(includeSubclasses)
				ifTrue:[scopeClass allSubclassesDo: [:aClass | classesToSearch add: aClass]].
			classesToSearch := classesToSearch asArray].
	
	'Searching all source code...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: classesToSearch size
		during: [ :aBarBlock |
			classCount := 0.
			classesToSearch do: [ :aClass |
				aBarBlock value: (classCount := classCount + 1).
				{
					aClass.
					aClass class} do: [ :aClassOrMetaClass |
					aClassOrMetaClass selectorsDo: [ :aSelector |
						((aClassOrMetaClass sourceCodeAt: aSelector)
							findString: aString
							startingAt: 1
							caseSensitive: caseSensitive) > 0 ifTrue: [
							adder
								value: aClassOrMetaClass
								value: aSelector ]].
					(aClassOrMetaClass organization classComment
						findString: aString
						startingAt: 1
						caseSensitive: caseSensitive) > 0 ifTrue: [
						adder
							value: aClassOrMetaClass
							value: #Comment ]]]].
	^ list asArray sort.! !


!Time class methodsFor: 'smalltalk-80' stamp: 'pb 4/May/2016 04:01:00'!
dateAndTimeFromSeconds: secondCount

	^ {
		Date fromSeconds: secondCount.
		self fromSeconds: secondCount \\ self secondsInDay}! !

