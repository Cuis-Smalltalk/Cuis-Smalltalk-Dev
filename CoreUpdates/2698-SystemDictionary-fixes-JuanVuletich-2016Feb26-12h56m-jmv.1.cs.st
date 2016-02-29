'From Cuis 4.2 of 25 July 2013 [latest update: #2690] on 26 February 2016 at 12:57:03.63701 pm'!

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 2/24/2016 14:40'!
allClassesImplementing: aSelector  
	"Answer an Array of all classes that implement the message aSelector."

	^ Array streamContents: [ :strm |
		self allBehaviorsDo: [ :class |
			(class includesSelector: aSelector)
				ifTrue: [ strm nextPut: class ]]]! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 2/24/2016 14:43'!
allImplementorsOf: aSelector  localTo: aClass
	"Answer a sorted Collection of all the methods that implement the message 
	aSelector in, above, or below the given class."

	| aSet cls |
	aSet _ Set new.
	cls _ aClass theNonMetaClass.
	cls withAllSuperAndSubclassesDoGently: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [
				aSet add: (MethodReference new
					setStandardClass: class 
					methodSymbol: aSelector)]].
	cls class withAllSuperAndSubclassesDoGently: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [
				aSet add: (MethodReference new
					setStandardClass: class 
					methodSymbol: aSelector)]].
	^aSet asArray sort! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 2/24/2016 14:49'!
allMethodsInCategory: category 
	| aCollection |
	aCollection _ SortedCollection new.
	self allBehaviorsDo: [ :x |
		(x organization listAtCategoryNamed: category) do: [ :sel |
			aCollection add: (MethodReference new
					setStandardClass: x 
					methodSymbol: sel)]].
	^aCollection! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 2/24/2016 14:50'!
allMethodsWithString: aString
	"Answer a sorted Collection of all the methods that contain, in a string literal, aString as a substring.  2/1/96 sw.  The search is case-sensitive, and does not dive into complex literals, confining itself to string constants.
	5/2/96 sw: fixed so that duplicate occurrences of aString in the same method don't result in duplicated entries in the browser"
	| aStringSize list |
	aStringSize _ aString size.
	list _ Set new.
	self allBehaviorsDo: [ :class |
		class selectorsDo: [ :sel |
			(class compiledMethodAt: sel) literalsDo: [ :aLiteral |
				((aLiteral isMemberOf: String) and: [ aLiteral size >= aStringSize ]) ifTrue: [
					(aLiteral
						findString: aString
						startingAt: 1) > 0 ifTrue: [ list add: 
							(MethodReference new
								setStandardClass: class 
								methodSymbol: sel) ]]]]].
	^ list asArray sort! !

!methodRemoval: DataStream #nextPutAll:!
DataStream removeSelector: #nextPutAll:!
