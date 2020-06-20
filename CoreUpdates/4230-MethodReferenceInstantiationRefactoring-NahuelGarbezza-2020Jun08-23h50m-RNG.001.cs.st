'From Cuis 5.0 [latest update: #4213] on 16 June 2020 at 7:48:29 pm'!
"Change Set:		4214-CuisCore-AuthorName-2020Jun08-23h50m
Date:			16 June 2020
Author:			Nahuel Garbezza

Use consistently class methods to create MethodReference instances"!


!ProtocolBrowser methodsFor: 'private' stamp: 'RNG 6/9/2020 00:16:22'!
onSubProtocolOf: aClass
	"Initialize with the entire protocol for the class, aClass,
		but excluding those inherited from Object."
	| selectors |
	selectors _ Set new.
	(aClass withAllSuperclassesPreviousTo: Object)
		do: [ :each | selectors addAll: each selectors ].
	self
		initListFrom: selectors asArray sort
		highlighting: aClass.! !


!ChangeList methodsFor: 'menu actions' stamp: 'RNG 6/8/2020 23:53:46'!
currentVersionsOfSelections
	"Opens a message-list browser on the current in-memory versions of all methods that are currently seleted"
	| aList |

	aList _ OrderedCollection new.
	1 to: changeList size do: [ :i |
		(listSelections at: i) ifTrue: [
			| aClass aChange |
			aChange _ changeList at: i.
			(aChange changeType == #method
				and: [(aClass _ aChange changeClass) notNil
				and: [aClass includesSelector: aChange methodSelector]])
					ifTrue: [
						aList add: (MethodReference
							class: aClass
							selector: aChange methodSelector)]]].
	^ aList! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'RNG 6/9/2020 00:10:27'!
allCallsOn: firstLiteral and: secondLiteral
	"Answer a SortedCollection of all the methods that call on both aLiteral
	and secondLiteral."

	| aCollection secondArray firstSpecial secondSpecial firstByte secondByte |
	aCollection _ SortedCollection new.
	firstSpecial _ self hasSpecialSelector: firstLiteral ifTrueSetByte: [:b | firstByte _ b].
	secondSpecial _ self hasSpecialSelector: secondLiteral ifTrueSetByte: [:b | secondByte _ b].
	self allBehaviorsDo: [ :class |
		secondArray _ class
			whichSelectorsReferTo: secondLiteral
			special: secondSpecial
			byte: secondByte.
		((class whichSelectorsReferTo: firstLiteral special: firstSpecial byte: firstByte) select: [ :aSel |
			(secondArray includes: aSel)]) do: [ :sel |
				aCollection add: (MethodReference class: class selector: sel )]].
	^aCollection! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'RNG 6/9/2020 00:10:45'!
allImplementorsOf: aSelector
	"Answer a SortedCollection of all the methods that implement the message
	aSelector."

	| aCollection |

	aCollection _ SortedCollection new.
	self allBehaviorsDo: [ :class |
		(class includesSelector: aSelector) ifTrue: [
			aCollection add: (MethodReference class: class selector: aSelector )]].
	^aCollection! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'RNG 6/9/2020 00:01:28'!
allImplementorsOf: aSelector  localTo: aClass
	"Answer a sorted Collection of all the methods that implement the message
	aSelector in, above, or below the given class."

	| aSet cls |
	aSet _ Set new.
	cls _ aClass theNonMetaClass.
	cls withAllSuperAndSubclassesDoGently: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [ aSet add: (MethodReference class: class selector: aSelector) ] ].
	cls class withAllSuperAndSubclassesDoGently: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [ aSet add: (MethodReference class: class selector: aSelector) ] ].
	^aSet asArray sort! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'RNG 6/8/2020 23:50:24'!
allMethodsInCategory: category
	| aCollection |
	aCollection := SortedCollection new.
	self allBehaviorsDo: [ :x |
		(x organization listAtCategoryNamed: category) do: [ :sel |
			aCollection add: (MethodReference class: x method: sel)]].
	^aCollection! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'RNG 6/8/2020 23:56:43'!
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
		list add: (MethodReference class: mrClass selector: mrSel) ].
	'Searching all source code...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: Smalltalk classNames size
		during: [ :barBlock |
			classCount _ 0.
			Smalltalk allClassesDo: [ :class |
				barBlock value: (classCount _ classCount + 1).
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

!SystemDictionary methodsFor: 'retrieving' stamp: 'RNG 6/8/2020 23:56:34'!
allMethodsWithSourceString: aString matchCase: caseSensitive
	"Answer a sorted Collection of all the methods that contain, in source code, aString as a substring.  Search the class comments also"
	| list classCount adder |
	list _ Set new.
	adder _ [ :mrClass :mrSel |
		list add: (MethodReference class: mrClass selector: mrSel) ].
	'Searching all source code...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: Smalltalk classNames size
		during: [ :barBlock |
			classCount _ 0.
			Smalltalk allClassesDo: [ :class |
				barBlock value: (classCount _ classCount + 1).
				(Array
					with: class
					with: class class) do: [ :cl |
					cl selectorsDo: [ :sel |
						((cl sourceCodeAt: sel)
							findString: aString
							startingAt: 1
							caseSensitive: caseSensitive) > 0 ifTrue: [
							adder
								value: cl
								value: sel ]].
					(cl organization classComment asString
						findString: aString
						startingAt: 1
						caseSensitive: caseSensitive) > 0 ifTrue: [
						adder
							value: cl
							value: #Comment ]]]].
	^ list asArray sort.! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'RNG 6/8/2020 23:57:01'!
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
					(aLiteral includesSubString: aString) ifTrue: [
						list add: (MethodReference class: class selector: sel) ]]]]].
	^ list asArray sort! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'RNG 6/8/2020 23:57:40'!
allSelect: aBlock
	"Answer a SortedCollection of each method that, when used as the block
	argument to aBlock, gives a true result."
	| aCollection |
	aCollection _ SortedCollection new.
	self allBehaviorsDo: [ :class |
		class selectorsDo: [ :sel |
			(aBlock value: (class compiledMethodAt: sel)) ifTrue: [
				aCollection add: (MethodReference class: class selector: sel)
			]]].
	^ aCollection! !

!SystemDictionary methodsFor: 'browsing' stamp: 'RNG 6/8/2020 23:58:01'!
browseAllStoresInto: instVarName from: aClass
	"Create and schedule a Message Set browser for all the receiver's methods
	or any methods of a subclass/superclass that refer to the instance variable name."

	"self new browseAllStoresInto: 'contents' from: Collection."

	| coll |
	coll _ OrderedCollection new.
	aClass withAllSubAndSuperclassesDo: [:class |
		(class whichSelectorsStoreInto: instVarName) do: [:sel |
			coll add: (MethodReference class: class selector: sel)]].
	^ self
		browseMessageList: coll
		name: 'Stores into ' , instVarName
		autoSelect: instVarName! !

!SystemDictionary methodsFor: 'browsing' stamp: 'RNG 6/8/2020 23:59:45'!
browseClassCommentsWithString: aString
	"Smalltalk browseClassCommentsWithString: 'my instances' "
	"Launch a message list browser on all class comments containing aString as a substring."
	| caseSensitive suffix list |
	suffix _ (caseSensitive _ Sensor shiftPressed)
		ifTrue: [ ' (case-sensitive)' ]
		ifFalse: [ ' (use shift for case-sensitive)' ].
	list _ Set new.
	Smalltalk allClassesDo: [ :class |
		(class organization classComment asString
			findString: aString
			startingAt: 1
			caseSensitive: caseSensitive) > 0 ifTrue: [
			list add: (MethodReference class: class selector: #Comment) ]].
	^ self
		browseMessageList: list asArray sort
		name: 'Class comments containing ', aString printString, suffix
		autoSelect: aString.! !


!CodePackage methodsFor: 'testing' stamp: 'RNG 6/8/2020 23:54:20'!
referenceForMethod: aSymbol ofClass: aClass
	^ MethodReference class: aClass selector: aSymbol! !


!CodeWindow methodsFor: 'menu commands' stamp: 'RNG 6/8/2020 23:54:34'!
openSingleMessageBrowser
	| msgName mr |
	"Create and schedule a message list browser populated only by the currently selected message"

	(msgName _ model selectedMessageName) ifNil: [^ self].

	mr _ MethodReference
		class: model selectedClassOrMetaClass
		selector: msgName.

	Smalltalk
		browseMessageList: (Array with: mr)
		name: mr stringVersion
		autoSelect: nil! !


!MethodReference methodsFor: 'setting' stamp: 'RNG 6/9/2020 00:03:27'!
setClass: aClass methodSymbol: methodSym stringVersion: aString

	self
		setClassSymbol: aClass theNonMetaClass name
		classIsMeta: aClass isMeta
		methodSymbol: methodSym
		stringVersion: aString! !

!MethodReference methodsFor: 'setting' stamp: 'RNG 6/9/2020 00:04:05'!
setStandardClass: aClass methodSymbol: methodSym

	self
		setClass: aClass
		methodSymbol: methodSym
		stringVersion: aClass name , ' ' , methodSym! !


!ChangeSelector methodsFor: 'rename senders - private' stamp: 'RNG 6/9/2020 00:08:19'!
renameSendersIn: aMethod

	| newSource rangesToNewStrings |

	rangesToNewStrings := self rangesToKeywordsOf: aMethod.
	newSource := aMethod sourceCode copyReplacing: rangesToNewStrings.
	aMethod methodClass compile: newSource.

	changes add: (MethodReference method: aMethod)! !
