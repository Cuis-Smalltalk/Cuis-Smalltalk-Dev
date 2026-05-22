"
I represent a special dictionary that supports protocol for asking questions about the structure of the system. Other than class names, I contain (print this)...

	Smalltalk globals
"
Class {
	#name : 'SystemDictionary',
	#superclass : 'IdentityDictionary',
	#instVars : [
		'cachedClassNames',
		'cachedNonClassNames',
		'startUpScriptArguments',
		'startUpArguments'
	],
	#classVars : [
		'ChangesInitialFileSize',
		'EndianCache',
		'LastImageName',
		'LastQuitLogPosition',
		'LowSpaceProcess',
		'LowSpaceSemaphore',
		'PreviousUserChangesName',
		'ShutDownList',
		'SourceFileVersionString',
		'StartUpList',
		'StartupStamp',
		'SystemAttributeCache'
	],
	#category : 'Kernel-Support',
	#package : 'Kernel',
	#tag : 'Support'
}

{ #category : 'copyright' }
SystemDictionary class >> copyright [
	"The Cuis Smalltalk copyright.
	Parts are copyright of many contributors to Squeak and Cuis projects."

	^
'Portions of Cuis are:
Copyright (c) Xerox Corp. 1981, 1982.
Copyright (c) Apple Computer, Inc. 1985-1996.
Copyright (c) Contributors to Squeak project. 1997-2025.
Copyright (c) Contributors to Cuis Smalltalk project. 2009-2025.'.
]

{ #category : 'initialization' }
SystemDictionary class >> initialize [
	"
	SystemDictionary initialize
	"

	StartUpList := OrderedCollection new.
	"These get processed from the top down..."
	#(
		#Delay
		#DisplayScreen
		#Cursor
		#InputSensor
		#ProcessorScheduler 	"Starts low space watcher and bkground."
		#EphemeronFinalizationProcess)
			do: [ :clsName | 	
				Smalltalk at: clsName ifPresent: [:cls | Smalltalk addToStartUpList: cls]].
			
	ShutDownList := OrderedCollection new.
	"These get processed from the bottom up..."
	#(
		#Delay
		#DisplayScreen
		#InputSensor
		#ProcessBrowser
		#CPUWatcher)
			do: [ :clsName | 
				Smalltalk at: clsName ifPresent: [:cls | Smalltalk addToShutDownList: cls]]
]

{ #category : 'shrinking' }
SystemDictionary >> abandonSources [
	"
	Smalltalk abandonSources
	"
	 | m bTotal bCount |
	(self confirm:
'This method will detach the image fom source code.
A fresh changes file will be created to record further changes.
-- CAUTION --
If you have backed up your system and
are prepared to face the consequences of
abandoning source code files, choose Yes.
If you have any doubts, you may choose No
to back out with no harm done.')
		==  true ifFalse: [^ self inform: 'Okay - no harm done'].
	bTotal := 0.  bCount := 0.
	self allBehaviorsDo: [: b | bTotal := bTotal + 1].
'Doing #destroySourcePointer ...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: bTotal
	during: [ :barBlock |
		self allBehaviorsDo: [ :cl |
		"for testing"
		"{ EllipseMorph } do: [ :cl |"
			barBlock value: (bCount := bCount + 1).
			cl selectors do: [:selector |
				m := cl compiledMethodAt: selector.
				m destroySourcePointer ]]].
	self allBehaviorsDo: [:b | b zapOrganization].
	self closeSourceFiles.
	Preferences at: #warnIfNoChangesFile put: false.
	Preferences at: #warnIfNoSourcesFile put: false
]

{ #category : 'sources, change log' }
SystemDictionary >> aboutThisSystem  [
	"Identify software version"

	Utilities defaultTextEditorContents edit.
	^ self inform: self systemInformationString
]

{ #category : 'snapshot and quit' }
SystemDictionary >> add: aClass toList: startUpOrShutDownList after: predecessor [
	"Add the name of aClass to the startUp or shutDown list.
	Add it after the name of predecessor, or at the end if predecessor is nil."

	| name earlierName |
	name := aClass name.
	(self at: name ifAbsent: nil) == aClass ifFalse: [
		self error: name , ' cannot be found in Smalltalk dictionary.'].
	predecessor
		ifNil: ["No-op if alredy in the list."
				(startUpOrShutDownList includes: name) ifFalse:
					[startUpOrShutDownList == StartUpList
						ifTrue: ["Add to end of startUp list"
								startUpOrShutDownList addLast: name]
						ifFalse: ["Add to front of shutDown list"
								startUpOrShutDownList addFirst: name]]]
		ifNotNil: ["Add after predecessor, moving it if already there."
				earlierName := predecessor name.
				(self at: earlierName) == predecessor ifFalse: [
					self error: earlierName , ' cannot be found in Smalltalk dictionary.'].
				(startUpOrShutDownList includes: earlierName) ifFalse: [
					self error: earlierName , ' cannot be found in the list.'].
				startUpOrShutDownList remove: name ifAbsent: nil.
				startUpOrShutDownList add: name after: earlierName]
]

{ #category : 'snapshot and quit' }
SystemDictionary >> addToShutDownList: aClass [
	"This will add a ref to this class at the BEGINNING of the shutDown list."

	self addToShutDownList: aClass after: nil
]

{ #category : 'snapshot and quit' }
SystemDictionary >> addToShutDownList: aClass after: predecessor [

	self add: aClass toList: ShutDownList after: predecessor
]

{ #category : 'snapshot and quit' }
SystemDictionary >> addToStartUpList: aClass [
	"This will add a ref to this class at the END of the startUp list."

	self addToStartUpList: aClass after: nil
]

{ #category : 'snapshot and quit' }
SystemDictionary >> addToStartUpList: aClass after: predecessor [

	self add: aClass toList: StartUpList after: predecessor
]

{ #category : 'retrieving' }
SystemDictionary >> allBehaviorsDo: aBlock  [
	"Evaluate the argument, aBlock, for each kind of Behavior in the system 
	(that is, Object and its subclasses).
	ar 7/15/1999: The code below will not enumerate any obsolete or anonymous
	behaviors for which the following should be executed:

		Smalltalk allObjectsDo:[:obj| obj isBehavior ifTrue:[aBlock value: obj]].

	but what follows is way faster than enumerating all objects."

	self do: [ :root |
		(root isBehavior and: [root superclass isNil]) ifTrue: [	"Grab ProtoObject and any other alike"
			root withAllSubclassesDo: [ :class |
				class isMeta ifFalse: [ "The metaclasses are rooted at Class; don't include them twice."
					aBlock
						value: class;
						value: class class ]]]]
]

{ #category : 'retrieving' }
SystemDictionary >> allCallsOn: aLiteral   "Smalltalk browseAllCallsOn: #open:label:." [
	"Answer a Collection of all the methods that call on aLiteral."
	| aCollection |

	#(23 48 'fred' (new open:label:)) size.
	"Example above should find #open:label:, though it is deeply embedded here."

	aCollection := OrderedCollection new.
	self allBehaviorsDo: [ :class |
		class addMethodsTo: aCollection thatReferExplicitlyTo: aLiteral ].
	
	^ aCollection
]

{ #category : 'retrieving' }
SystemDictionary >> allCallsOn: firstLiteral and: secondLiteral [
	"Answer a SortedCollection of all the methods that call on both aLiteral
	and secondLiteral."

	| aCollection secondArray |
	aCollection := SortedCollection new.
	self allBehaviorsDo: [ :class |
		secondArray := class
			whichSelectorsReferExplicitlyTo: secondLiteral.
				((class whichSelectorsReferExplicitlyTo: firstLiteral ) select: [ :aSel |
			(secondArray includes: aSel)]) do: [ :sel |
				aCollection add: (MethodReference class: class selector: sel )]].
	^aCollection
]

{ #category : 'retrieving' }
SystemDictionary >> allClasses   [
	"Return all the class defines in the Smalltalk SystemDictionary"
	"
	Smalltalk allClasses
	"

	^ self classNames collect: [:name | self at: name]
]

{ #category : 'retrieving' }
SystemDictionary >> allClassesDo: aBlock [
	"Evaluate the argument, aBlock, for each class in the system."

	self classNames do: [ :name |
		aBlock value: (self at: name) ].
]

{ #category : 'retrieving' }
SystemDictionary >> allClassesImplementing: aSelector   [
	"Answer an Array of all classes that implement the message aSelector."

	^ Array streamContents: [ :strm |
		self allBehaviorsDo: [ :class |
			(class includesSelector: aSelector)
				ifTrue: [ strm nextPut: class ]]]
]

{ #category : 'code authors' }
SystemDictionary >> allContributors [
"
	Smalltalk allContributors
"
	| answer author |
	answer := Set new.
	self allBehaviorsDo: [ :behavior |
		behavior methodsDo: [ :compiledMethod |
			author := compiledMethod author.
			author notEmpty ifTrue: [
				answer add: author ]]].
	^answer
]

{ #category : 'retrieving' }
SystemDictionary >> allGlobalRefs [
	"Answer a set of symbols that may be refs to Global names.
	Warning: Will not include references to a class from its own methods"

	^ self allGlobalRefsWithout: #(#() #())
]

{ #category : 'retrieving' }
SystemDictionary >> allGlobalRefsWithout: classesAndMessagesPair [
	"Answer a set of symbols that may be refs to Global names. 
	This method computes its result in the absence of specified classes and messages.
	Does not include references from a class to itself"

	| globalRefs absentClasses absentSelectors |
	globalRefs := IdentitySet new.
	absentClasses := classesAndMessagesPair first.
	absentSelectors := classesAndMessagesPair second.
	self classNames do: [ :cName |
		((absentClasses includes: cName)
					ifTrue: [#()]
					ifFalse: [{(self at: cName). (self at: cName) class}]) do: [ :cl | 
			(absentSelectors isEmpty
					ifTrue: [cl selectors]
					ifFalse: [cl selectors copyWithoutAll: absentSelectors]) do: [ :sel |
				"Include all capitalized symbols for good measure"
				self allSymbolsIn: (cl compiledMethodAt: sel) literals do: [ :m | 
					(m isSymbol and: [ m size > 0] and: [m first isUppercase]) ifTrue: [
						m = cl name ifFalse: [
							globalRefs add: m]].
					(m isVariableBinding) ifTrue:
						[m key ifNotNil: [
							m key = cl name ifFalse: [
								globalRefs add: m key]]]]]]].
	^ globalRefs
]

{ #category : 'retrieving' }
SystemDictionary >> allImplementedMessages [
	"Answer a Set of all the messages that are implemented in the system."

	^ self allImplementedMessagesWithout:  #(#() #())
]

{ #category : 'retrieving' }
SystemDictionary >> allImplementedMessagesWithout: classesAndMessagesPair [
	"Answer a Set of all the messages that are implemented in the system, computed in the absence of the supplied classes and messages.  Note this reports messages that are in the absent selectors set."
	| messages absentClasses |
	messages := IdentitySet new.
	absentClasses := classesAndMessagesPair first.
	self classNames do: [ :cName |
		((absentClasses includes: cName)
			ifTrue: [#()]
			ifFalse: [{(self at: cName). (self at: cName) class}]) do: [ :cl |
				messages addAll: cl selectors]].
	^ messages
]

{ #category : 'retrieving' }
SystemDictionary >> allImplementorsOf: aSelector [
	"Answer a SortedCollection of all the methods that implement the message
	aSelector."

	| aCollection |

	aCollection := SortedCollection new.
	self allBehaviorsDo: [ :class |
		(class includesSelector: aSelector) ifTrue: [
			aCollection add: (MethodReference class: class selector: aSelector )]].
	^aCollection
]

{ #category : 'retrieving' }
SystemDictionary >> allImplementorsOf: aSelector localTo: aClass [
	"Answer a sorted Collection of all the methods that implement the message
	aSelector in, above, or below the given class."

	| aSet cls |
	aSet := Set new.
	cls := aClass theNonMetaClass.
	cls withAllSuperAndSubclassesDo: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [ aSet add: (MethodReference class: class selector: aSelector) ] ].
	cls class withAllSuperAndSubclassesDo: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [ aSet add: (MethodReference class: class selector: aSelector) ] ].
	^aSet asArray
]

{ #category : 'retrieving' }
SystemDictionary >> allImplementorsOf: aSelector subTo: aClass [
	"Answer a sorted Collection of all the methods that implement the message
	aSelector in, or below the given class."

	| aSet cls |
	aSet := Set new.
	cls := aClass theNonMetaClass.
	cls withAllSubclassesDo: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [ aSet add: (MethodReference class: class selector: aSelector) ] ].
	cls class withAllSubclassesDo: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [ aSet add: (MethodReference class: class selector: aSelector) ] ].
	^aSet asArray
]

{ #category : 'retrieving' }
SystemDictionary >> allImplementorsOf: aSelector superTo: aClass [
	"Answer a sorted Collection of all the methods that implement the message
	aSelector in, or above the given class."

	| aSet cls |
	aSet := Set new.
	cls := aClass theNonMetaClass.
	cls withAllSuperclassesDo: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [ aSet add: (MethodReference class: class selector: aSelector) ] ].
	cls class withAllSuperclassesDo: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [ aSet add: (MethodReference class: class selector: aSelector) ] ].
	^aSet asArray
]

{ #category : 'retrieving' }
SystemDictionary >> allMethodsInCategory: category [
	| aCollection |
	aCollection := SortedCollection new.
	self allBehaviorsDo: [ :x |
		(x organization listAtCategoryNamed: category) do: [ :sel |
			aCollection add: (MethodReference class: x selector: sel)]].
	^aCollection
]

{ #category : 'retrieving' }
SystemDictionary >> allMethodsSourceStringMatching: aString [
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
				(Array
					with: class
					with: class class) do: [ :cl |
					cl selectorsDo: [ :sel |
						(aString match: (cl sourceCodeAt: sel)) ifTrue: [
							adder
								value: cl
								value: sel ]].

					(aString match: cl organization classComment) ifTrue: [
						adder
							value: cl
							value: #Comment ]]]].
	^ list asArray sort.
]

{ #category : 'retrieving' }
SystemDictionary >> allMethodsWithSourceString: aString matchCase: caseSensitive [
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
					(cl organization classComment
						findString: aString
						startingAt: 1
						caseSensitive: caseSensitive) > 0 ifTrue: [
						adder
							value: cl
							value: #Comment ]]]].
	^ list asArray sort.
]

{ #category : 'retrieving' }
SystemDictionary >> allMethodsWithSourceString: aString [
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
				(Array
					with: aClass
					with: aClass class) do: [ :aClassOrMetaClass |
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
	^ list asArray sort.
]

{ #category : 'retrieving' }
SystemDictionary >> allMethodsWithString: aString [
	"Answer a sorted Collection of all the methods that contain, in a string literal, aString as a substring.  2/1/96 sw.  The search is case-sensitive, and does not dive into complex literals, confining itself to string constants.
	5/2/96 sw: fixed so that duplicate occurrences of aString in the same method don't result in duplicated entries in the browser"
	| aStringSize list |
	aStringSize := aString size.
	list := Set new.
	self allBehaviorsDo: [ :class |
		class selectorsDo: [ :sel |
			(class compiledMethodAt: sel) literalsDo: [ :aLiteral |
				((aLiteral isMemberOf: String) and: [ aLiteral size >= aStringSize ]) ifTrue: [
					(aLiteral includesSubString: aString) ifTrue: [
						list add: (MethodReference class: class selector: sel) ]]]]].
	^ list asArray sort
]

{ #category : 'retrieving' }
SystemDictionary >> allObjects [
	"Answer an Array of all objects in the system.  Fail if
	 there isn't enough memory to instantiate the result."

	<primitive: 178>
	^self primitiveFailed
]

{ #category : 'retrieving' }
SystemDictionary >> allObjectsDo: aBlock  [
	"Evaluate the argument, aBlock, for each object in the system, excluding immediates
	 such as SmallInteger, Character and SmallFloat64."
	self allObjectsOrNil
		ifNotNil: [:allObjects| allObjects do: aBlock]
		ifNil:
			["Fall back on the old single object primitive code.  With closures, this needs
			  to use an end marker (lastObject) since activation of the block will create
			  new contexts and cause an infinite loop.  The lastObject must be created
			  before calling someObject, so that the VM can settle the enumeration (e.g.
			  by flushing new space) as a side effect of  someObject"
			| object lastObject |
			lastObject := Object new.
			object := self someObject.
			[lastObject == object or: [0 == object]] whileFalse: [
				| nextObject |
				nextObject := object nextObject.
				aBlock value: object.
				object := nextObject ]]
]

{ #category : 'retrieving' }
SystemDictionary >> allObjectsOrNil [
	"Answer an Array of all objects in the system.  Fail if there isn't
	 enough memory to instantiate the result and answer nil."

	<primitive: 178>
	^nil
]

{ #category : 'retrieving' }
SystemDictionary >> allPrimitiveMessages [
	"Answer an OrderedCollection of all the methods that are implemented by 
	primitives."

	| aColl method | 
	aColl := OrderedCollection new.
	self allBehaviorsDo: [ :class |
		class selectorsDo: [ :sel | 
			method := class compiledMethodAt: sel.
			method primitive ~= 0 ifTrue: [
				aColl addLast: class name , ' ' , sel 
					, ' ' , method primitive printString]]].
	^aColl
]

{ #category : 'retrieving' }
SystemDictionary >> allPrimitiveMethodsInCategories: aList [
	"Answer an OrderedCollection of all the methods that are implemented by primitives in the given categories.  1/26/96 sw"

	| aColl method | 
	aColl := OrderedCollection new.
	self allBehaviorsDo: [ :aClass |
		(aList includes: (SystemOrganization categoryOfElement: aClass theNonMetaClass name asPlainString) asPlainString)
			ifTrue: [
				aClass selectorsDo: [ :sel | 
					method := aClass compiledMethodAt: sel.
					method primitive ~= 0
						ifTrue: [
							aColl addLast: aClass name , ' ' , sel 
								, ' ' , method primitive printString]]]].
	^ aColl

"Smalltalk allPrimitiveMethodsInCategories: #('Collections-Streams' 'Files-Streams' 'Files-Abstract' 'Files-Macintosh')"
]

{ #category : 'retrieving' }
SystemDictionary >> allReferencesToLiteral: aLiteral [
	| coll |
	coll := OrderedCollection new.
	self allBehaviorsDo: [ :eaClass |
		eaClass
			addMethodsTo: coll
			thatReferTo: aLiteral ]..
	^ coll.
]

{ #category : 'retrieving' }
SystemDictionary >> allSelect: aUnaryBlock [
	"Answer a SortedCollection of each method in the system
	 for which aUnaryBlock answers true when evaluated with the method."
	| aCollection |
	aCollection := SortedCollection new.
	self allBehaviorsDo: [ :class |
		class selectorsAndMethodsDo: [ :sel :meth |
			(aUnaryBlock value: meth) ifTrue: [
				aCollection add: (MethodReference class: class selector: sel)
			]]].
	^ aCollection
]

{ #category : 'retrieving' }
SystemDictionary >> allSelect: aUnaryBlock localTo: aClass [
	"Answer a SortedCollection of each method in the hierarchy of aClass
	 for which aUnaryBlock answers true when evaluated with the method."
	| aCollection |
	aCollection := SortedCollection new.
	aClass allSuperclasses, aClass withAllSubclasses do:
		[:class |
		class selectorsAndMethodsDo:
			[:sel :meth|
			(aUnaryBlock value: meth) ifTrue:
				[aCollection add: (MethodReference class: class selector: sel)]]].
	^aCollection
]

{ #category : 'retrieving' }
SystemDictionary >> allSentMessages [
	"Answer the set of selectors which are sent somewhere in the system."

	^ self allSentMessagesWithout: #(#() #())
]

{ #category : 'retrieving' }
SystemDictionary >> allSentMessagesWithout: classesAndMessagesPair [
	"Answer the set of selectors which are sent somewhere in the system, 
	computed in the absence of the supplied classes and messages."

	| sent absentClasses absentSelectors |
	sent := IdentitySet new.
	absentClasses := classesAndMessagesPair first.
	absentSelectors := classesAndMessagesPair second.
	self classNames do: [ :cName | 
		((absentClasses includes: cName)
				ifTrue: [#()]
				ifFalse: [{(self at: cName). (self at: cName) class}]) do: [ :cl |
			 (absentSelectors isEmpty
					ifTrue: [cl selectors]
					ifFalse: [cl selectors copyWithoutAll: absentSelectors]) do: [ :sel |
				 "Include all sels, but not if sent by self"
				self allSymbolsIn: (cl compiledMethodAt: sel) literals do: [ :m |
					m == sel ifFalse: [sent add: m] ]]]].
	"The following may be sent without being in any literal frame"
	1 to: self specialSelectorSize do: [ :index | 
		sent add: (self specialSelectorAt: index)].
	self presumedSentMessages do: [ :sel | sent add: sel].
	^ sent
]

{ #category : 'private' }
SystemDictionary >> allSymbolsIn: anArray do: aBlock [
	"Recursively dig into anArray and evaluate aBlock on any Symbols found"
	"allSymbols and variableBindings"
	
	anArray do: [ :elem |
		(elem isSymbol or: [ elem isVariableBinding ])
			ifTrue: [ aBlock value: elem ]
			ifFalse: [
				(elem isMemberOf: Array) ifTrue: [ 
					self allSymbolsIn: elem do: aBlock ]]]
]

{ #category : 'retrieving' }
SystemDictionary >> allUnSentMessages   "Smalltalk allUnSentMessages" [
	"Answer the set of selectors that are implemented by some object in the system but not sent by any."

	^ self allUnSentMessagesWithout: #(#() #())
]

{ #category : 'retrieving' }
SystemDictionary >> allUnSentMessagesIn: selectorSet [
	"Answer the subset of selectorSet which are not sent anywhere in the system."

	^ selectorSet copyWithoutAll: self allSentMessages
]

{ #category : 'retrieving' }
SystemDictionary >> allUnSentMessagesWithout: classesAndMessagesPair [
	"Answer the set of selectors that are implemented but not sent, computed in the absence of the supplied classes and messages."

	^ (self allImplementedMessagesWithout: classesAndMessagesPair)
		copyWithoutAll: (self allSentMessagesWithout: classesAndMessagesPair)
]

{ #category : 'retrieving' }
SystemDictionary >> allUnimplementedCalls [
	"Answer an Array of each message that is sent by an expression in a 
	method but is not implemented by any object in the system."

	| aStream all  selectors |
	all := self allImplementedMessages.
	aStream := WriteStream on: (Array new: 50).
	self allBehaviorsDo: [ :cl |
		 cl selectorsDo: [ :sel |
			selectors := OrderedCollection new.
			(cl compiledMethodAt: sel) messagesDo: [ :m |
				(all includes: m) ifFalse: [selectors add: m ]].
			selectors notEmpty ifTrue: [
				aStream nextPut: (MethodReference class: cl selector: sel).
				(cl name , '>>#' , sel , (String new: (70-sel size * 15//10 max: 0) withAll: $ ), ' calls:          ', selectors asArray printString) print.
				"aStream nextPut: cl name , ' ' , sel , ' calls: ', secondStream contents" ]]].
	^aStream contents
]

{ #category : 'retrieving' }
SystemDictionary >> allUnusedClassesWithout: classesAndMessagesPair [
	"Enumerates all classes in the system and returns a list of those that are apparently unused. A class is considered in use if it (a) has subclasses or (b) is referred to by some method or (c) has its name in use as a literal (but not in the same class) or: (d) some instance is a global."
	"
	Smalltalk unusedClasses
	"

	| unused cl |
	self garbageCollect.
	unused := self classNames asIdentitySet
				copyWithoutAll: (self allGlobalRefsWithout: classesAndMessagesPair).
				
	self do: [ :global |
		unused remove: global class name ifAbsent: nil].
	
	^ unused reject: [ :cName |
		cl := self at: cName.
		cl subclasses notEmpty
			"or: [ cl someInstance notNil ]"
			"or: [cl inheritsFrom: FileDirectory]"]
]

{ #category : 'image, changes name' }
SystemDictionary >> alternativeSourcesName [
	"Answer the alternative full path to the sources file.
	If Sources not found at #defaultSourcesName (this image folder), 
	seek for them here (standard image folder)."
	"
	Smalltalk alternativeSourcesName
	"
	"Answer the default full path to the version-stable source code"
	^ self vmPath , 'CuisImage' , '/' , SourceFileVersionString , '.sources'
]

{ #category : 'snapshot and quit' }
SystemDictionary >> askConfirmationOnQuit [
	"Answer true unless the user cancels quitting because of some warning given.
	Smalltalk askConfirmationOnQuit
	"
	| baseCSdirty dirtyPackages |
	baseCSdirty := ChangeSet allChangeSets anySatisfy: [ :any | any isForBaseSystem and: [ any hasUnsavedChanges ]].
	"dirtyPackages := CodePackage installedPackages anySatisfy: [ :pck | pck hasUnsavedChanges ]."
	dirtyPackages := ChangeSet allChangeSets anySatisfy: [ :any | any codePackage notNil and: [ any hasUnsavedChanges ]].
	baseCSdirty & dirtyPackages ifTrue: [
		^self confirm: 'There are both unsaved Packages', String newLineString,
			'and unsaved Changes to Cuis core.', String newLineString,
			'If you continue, all unsaved changes will be lost.', String newLineString,
			'Do you really want to exit Cuis without saving the image?' ].
	baseCSdirty ifTrue: [
		^self confirm: 'Some ChangeSet for Cuis core have unsaved changes.', String newLineString,
			'If you continue, they would be lost.', String newLineString,
			'Do you really want to exit Cuis without saving the image?' ].
	dirtyPackages ifTrue: [
		^self confirm: 'There are unsaved Packages.', String newLineString,
			'If you continue, their changes will be lost.', String newLineString,
			'Do you really want to exit Cuis without saving the image?' ].
	(Preferences at: #askConfirmationOnQuit) ifTrue: [
		^self confirm: 'Do you really want to exit Cuis without saving the image?' ].
	^true
]

{ #category : 'globals' }
SystemDictionary >> associationOrUndeclaredAt: key  [
	"return an association or install in undeclared. Used for mating up ImageSegments."

	^ self associationAt: key ifAbsent: [
		Undeclared at: key put: nil.
		Undeclared associationAt: key ]
]

{ #category : 'sources, change log' }
SystemDictionary >> assureStartupStampLogged [
	"If there is a startup stamp not yet actually logged to disk, do it now."
	"Note: This renames the existing user changes file, and creates a new one."
	| changesFile directory oldUserChanges |
	(SourceFiles notNil and: [(changesFile := SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			changesFile nextChunkPut: StartupStamp; newLine.
			self forceChangesToDisk ]].
	[
		(Preferences at: #autoNumberUserChanges) ifTrue: [
			oldUserChanges := DirectoryEntry userChangesDirectory // self userChangesFileName.
			oldUserChanges exists ifTrue: [
				directory := oldUserChanges parent.
				PreviousUserChangesName := directory nextNameFor: oldUserChanges nameWithoutExtension extension: 'changes'.
				oldUserChanges rename: PreviousUserChangesName ]]]
		on: Error do: [].
	self userChangesAppendDo: [ :stream |
		stream newLine; newLine.
		stream nextChunkPut: StartupStamp; newLine.
		stream flush ].
]

{ #category : 'globals' }
SystemDictionary >> at: aKey put: anObject  [
	"Override from Dictionary to check Undeclared and fix up
	references to undeclared variables."

	| association |
	(self includesKey: aKey) ifFalse: [
		self flushClassNameCache.

		"Update existing association if there is one."
		(Undeclared includesKey: aKey) ifTrue: [
			association := self declare: aKey from: Undeclared.
			association value: anObject.
			^ anObject ]].

	"Update existing association if there is one."
	^super at: aKey put: anObject
]

{ #category : 'private' }
SystemDictionary >> baseLabel [
	^ 'system'
]

{ #category : 'ui' }
SystemDictionary >> beep [
	"
	Smalltalk beep
	"
	(Preferences at: #soundsEnabled) ifTrue: [
		self
			at: #SampledSound
			ifPresent: [ :cls | cls beep ]
			ifAbsent: [ self primitiveBeep ]]
]

{ #category : 'browsing - main' }
SystemDictionary >> browse [
	"
	Smalltalk browse
	"
	^self browserWindowClass fullOnClass: Object selector: nil
]

{ #category : 'browsing' }
SystemDictionary >> browseAllAccessesTo: instVarName from: aClass [
	"Create and schedule a Message Set browser for all the receiver's methods 
	or any methods of a subclass/superclass that refer to the instance variable name.

	self new browseAllAccessesTo: 'id' from: Morph.
	"
		
	^ self
		browseMessageListUnsorted: (aClass allAccessesTo: instVarName)
		name: 'Accesses to ' , instVarName 
		autoHighlight: instVarName
]

{ #category : 'browsing' }
SystemDictionary >> browseAllCallsOn: aLiteral [
	"Create and schedule a message browser on each method that refers to
	aLiteral. For example, Smalltalk browseAllCallsOn: #open:label:."
	(aLiteral isKindOf: LookupKey)
		ifTrue: [
			self
				browseMessageList: (self allCallsOn: aLiteral)
				name: 'Users of ' , aLiteral key
				autoHighlight: aLiteral key
				allOccurrences: true ]
		ifFalse:
			[self
				browseMessageList: (self allCallsOn: aLiteral)
				name: 'Senders of ' , aLiteral asString
				autoHighlight: aLiteral asString
				allOccurrences: true ].
]

{ #category : 'browsing' }
SystemDictionary >> browseAllCallsOn: literal1 and: literal2  [
	"Create and schedule a message browser on each method that calls on the 
	two Symbols, literal1 and literal2. For example, Smalltalk 
	browseAllCallsOn: #at: and: #at:put:."

	^self 
		browseMessageList: (self allCallsOn: literal1 and: literal2)
		name: literal1 printString , ' -and- ' , literal2 printString
]

{ #category : 'browsing' }
SystemDictionary >> browseAllCallsOn: aLiteral localTo: aClass [
	"Create and schedule a message browser on each method in or below the given class that refers to
	aLiteral. For example, Smalltalk browseAllCallsOn: #open:label:."
	aClass ifNil: [ ^ self inform: 'no selected class' ].
	(aLiteral isKindOf: LookupKey)
		ifTrue: [
			self
				browseMessageList: (aClass allLocalCallsOn: aLiteral)
				name: 'Users of ' , aLiteral key , ' in the full hierarchy of ' , aClass name
				autoHighlight: aLiteral key
				allOccurrences: true ]
		ifFalse: [
			self
				browseMessageList: (aClass allLocalCallsOn: aLiteral)
				name: 'Senders of ' , aLiteral asString , ' in the full hierarchy of ' , aClass name
				autoHighlight: aLiteral asString
				allOccurrences: true ].
]

{ #category : 'browsing' }
SystemDictionary >> browseAllCallsOnClass: aClass [
	"Create and schedule a message browser on each method that refers to 
	aClass. For example, Smalltalk browseAllCallsOnClass: Object."

	self
		browseMessageList: aClass allCallsOn
		name: 'Users of class ' , aClass theNonMetaClass name
		autoHighlight: aClass theNonMetaClass name
		allOccurrences: true.
]

{ #category : 'browsing' }
SystemDictionary >> browseAllClasses [
	"Create and schedule a new browser on all classes alphabetically."
	| newBrowser |
	newBrowser := HierarchyBrowser new initAlphabeticListing.
	HierarchyBrowserWindow openNoSysCat: newBrowser label: 'All Classes Alphabetically'
]

{ #category : 'browsing' }
SystemDictionary >> browseAllImplementorsOf: selector [
	"Create and schedule a message browser on each method that implements 
	the message whose selector is the argument, selector. For example, 
	Smalltalk browseAllImplementorsOf: #at:put:."

	^self browseMessageList: (self allImplementorsOf: selector) name: 'Implementors of ' , selector
]

{ #category : 'browsing' }
SystemDictionary >> browseAllImplementorsOf: selector localTo: aClass [
	"Create and schedule a message browser on each method in, above, or below the given class
	that implements the message whose selector is the argument, selector. For example, 
	Smalltalk browseAllImplementorsOf: #at:put: localTo: Dictionary."

	aClass ifNil: [ ^self inform: 'no class selected' ].
	^self
		browseMessageList: (self allImplementorsOf: selector localTo: aClass)
		name: 'Implementors of ' , selector, ' in the full hierarchy of ', aClass name
]

{ #category : 'browsing' }
SystemDictionary >> browseAllImplementorsOf: selector subTo: aClass [
	"Create and schedule a message browser on each method in or below the given class
	that implements the message whose selector is the argument, selector. For example, 
	Smalltalk browseAllImplementorsOf: #at:put: localTo: Dictionary."

	aClass ifNil: [ ^self inform: 'no class selected' ].
	^self
		browseMessageList: (self allImplementorsOf: selector subTo: aClass)
		name: 'Implementors of ' , selector, ' in ', aClass name, ' and subclasses'
]

{ #category : 'browsing' }
SystemDictionary >> browseAllImplementorsOf: selector superTo: aClass [
	"Create and schedule a message browser on each method in or above the given class
	that implements the message whose selector is the argument, selector. For example, 
	Smalltalk browseAllImplementorsOf: #at:put: localTo: Dictionary."

	aClass ifNil: [ ^self inform: 'no class selected' ].
	^self
		browseMessageList: (self allImplementorsOf: selector superTo: aClass)
		name: 'Implementors of ' , selector, ' in ', aClass name, ' and superclasses'
]

{ #category : 'browsing' }
SystemDictionary >> browseAllImplementorsOfList: selectorList [
	"Create and schedule a message browser on each method that implements 
	the message whose selector is in the argument selectorList. For example, 
	Smalltalk browseAllImplementorsOf: #(at:put: size).
	1/16/96 sw: defer to the titled version"

	self browseAllImplementorsOfList: selectorList title: 'Implementors of all'
]

{ #category : 'browsing' }
SystemDictionary >> browseAllImplementorsOfList: selectorList title: aTitle [
	"Create and schedule a message browser on each method that implements the message whose selector is in the argument selectorList. For example,  Smalltalk browseAllImplementorsOfList: #(at:put: size) title: 'stuff'."

	| flattenedList |
	flattenedList := Array streamContents: [ :stream |
		selectorList do: [ :sel |
			stream nextPutAll: (self allImplementorsOf: sel)]].
	^ self browseMessageList: flattenedList name: aTitle
]

{ #category : 'browsing' }
SystemDictionary >> browseAllMethodsInCategory: aSymbol  [
	^self
		browseMessageList: (self allMethodsInCategory: aSymbol)
		name: aSymbol
]

{ #category : 'browsing' }
SystemDictionary >> browseAllPrimitives [
	self browseAllSelect: [:each| each primitive ~= 0 and: [(each primitive between: 256 and: 291) not]]

]

{ #category : 'browsing' }
SystemDictionary >> browseAllReferencesToLiteral: aLiteral [
	"Create and schedule a message browser on each method that references aLiteral. For example, 
	Smalltalk browseAllReferencesToLiteral: 47.
	Smalltalk browseAllReferencesToLiteral: 0@0.
	"
	^ self
		browseMessageList: (self allReferencesToLiteral: aLiteral)
		name: 'References to literal ' , aLiteral asString
		autoHighlight: aLiteral asString
		allOccurrences: true.
]

{ #category : 'browsing' }
SystemDictionary >> browseAllSelect: aBlock [
	"Create and schedule a message browser on each method that, when used 
	as the block argument to aBlock gives a true result. For example, 
	Smalltalk browseAllSelect: [:method | method numLiterals > 10]."

	^self browseMessageList: (self allSelect: aBlock) name: 'selected messages'
]

{ #category : 'browsing' }
SystemDictionary >> browseAllSelect: aUnaryBlock localTo: aClass [
	"Create and schedule a message browser on each method in the hierarchy of aClass
	 for which aUnaryBlock answers true when evaluated with the method.
	For example, Smalltalk browseAllSelect: [:m| m sendsSelector: #size] localTo: Collection."
	self browseMessageList: (self allSelect: aUnaryBlock localTo: aClass) name: 'selected messages in', aClass name
]

{ #category : 'browsing' }
SystemDictionary >> browseAllSelect: aBlock name: aName autoHighlight: autoHighlightString [
	"Create and schedule a message browser on each method that, when used 
	as the block argument to aBlock gives a true result.   Do not return an #DoIt traces."

	"
	Smalltalk browseAllSelect: [:method | method numLiterals > 10] name: 'Methods with more than 10 literals' autoHighlight: 'Primitive.'
	"

	^ self browseMessageList: (self allSelect: aBlock) name: aName autoHighlight: autoHighlightString allOccurrences: true
]

{ #category : 'browsing' }
SystemDictionary >> browseAllUnSentMessages [
	"Create and schedule a message browser on each method whose message is  not sent in any method in the system."
	
	self browseAllImplementorsOfList: self allUnSentMessages title: 'Messages implemented but not sent'

"Smalltalk browseAllUnSentMessages"
]

{ #category : 'browsing' }
SystemDictionary >> browseAllUnimplementedCalls [
	"Create and schedule a message browser on each method that includes a 
	message that is not implemented in any object in the system.
	Smalltalk browseAllUnimplementedCalls
	"

	^self browseMessageList: self allUnimplementedCalls name: 'Unimplemented calls'
]

{ #category : 'browsing' }
SystemDictionary >> browseClassCommentsWithString: aString [
	"Smalltalk browseClassCommentsWithString: 'my instances' "
	"Launch a message list browser on all class comments containing aString as a substring."
	| caseSensitive suffix list |
	suffix := (caseSensitive := Sensor shiftPressed)
		ifTrue: [ ' (case-sensitive)' ]
		ifFalse: [ ' (use shift for case-sensitive)' ].
	list := Set new.
	self allClassesDo: [ :class |
		(class organization classComment
			findString: aString
			startingAt: 1
			caseSensitive: caseSensitive) > 0 ifTrue: [
			list add: (MethodReference class: class selector: #Comment) ]].
	^ self
		browseMessageList: list
		name: 'Class comments containing ', aString printString, suffix
		autoHighlight: aString
		allOccurrences: true.
]

{ #category : 'browsing' }
SystemDictionary >> browseClassesWithNamesContaining: aString caseSensitive: caseSensitive  [
	"Smalltalk browseClassesWithNamesContaining: 'eMorph' caseSensitive: true "
	"Launch a class-list list browser on all classes whose names containg aString as a substring."

	| suffix aList |
	suffix := caseSensitive
				ifTrue: [' (case-sensitive)']
				ifFalse: [' (use shift for case-sensitive)'].
	aList := OrderedCollection new.
	self allClassesDo: [ :class |
		(class name includesSubstring: aString caseSensitive: caseSensitive)
			ifTrue: [aList add: class name]].
	aList notEmpty
		ifTrue: [HierarchyBrowserWindow forClassesNamed: aList asSet sorted title: 'Classes whose names contain ' , aString , suffix]
]

{ #category : 'browsing' }
SystemDictionary >> browseEqEqSentToCharacterConstants [
	"
	Smalltalk browseEqEqSentToCharacterConstants
	"
	| hasMatch visitor |
	hasMatch := false.
	visitor := ParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#== #~~ ) identityIncludes: node selector key) and: [
					node receiver isLiteralCharacter or: [ node arguments first isLiteralCharacter ]]]) ifTrue: [
						hasMatch := true ]]
		select: [ :node |
			hasMatch not ].
	self browseAllSelect: [ :method |
		hasMatch := false.
		method decompile accept: visitor.
		hasMatch ].
]

{ #category : 'browsing' }
SystemDictionary >> browseEqEqSentToNumericConstants [
	"
	Smalltalk browseEqEqSentToNumericConstants
	"
	| hasMatch visitor |
	hasMatch := false.
	visitor := ParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#== #~~ ) identityIncludes: node selector key) and: [
					node receiver isLiteralNumber or: [ node arguments first isLiteralNumber ]]]) ifTrue: [
						hasMatch := true ]]
		select: [ :node |
			hasMatch not ].
	self browseAllSelect: [ :method |
		hasMatch := false.
		method decompile accept: visitor.
		hasMatch ].
]

{ #category : 'browsing' }
SystemDictionary >> browseEqEqSentToSmallIntegerConstants [
	"
	Smalltalk browseEqEqSentToSmallIntegerConstants
	"
	| hasMatch visitor |
	hasMatch := false.
	visitor := ParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#== #~~ ) identityIncludes: node selector key) and: [
					node receiver isLiteralSmallInteger or: [ node arguments first isLiteralSmallInteger ]]]) ifTrue: [
						hasMatch := true ]]
		select: [ :node |
			hasMatch not ].
	self browseAllSelect: [ :method |
		hasMatch := false.
		method decompile accept: visitor.
		hasMatch ].
]

{ #category : 'browsing' }
SystemDictionary >> browseEqSentToCharacterConstants [
	"
	Smalltalk browseEqSentToCharacterConstants
	"
	| hasMatch visitor |
	hasMatch := false.
	visitor := ParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#= #~= ) identityIncludes: node selector key) and: [
					node receiver isLiteralCharacter or: [ node arguments first isLiteralCharacter ]]]) ifTrue: [
						hasMatch := true ]]
		select: [ :node |
			hasMatch not ].
	self browseAllSelect: [ :method |
		hasMatch := false.
		method decompile accept: visitor.
		hasMatch ].
]

{ #category : 'browsing' }
SystemDictionary >> browseEqSentToNumericConstants [
	"
	Smalltalk browseEqSentToNumericConstants
	"
	| hasMatch visitor |
	hasMatch := false.
	visitor := ParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#= #~= ) identityIncludes: node selector key) and: [
					node receiver isLiteralNumber or: [ node arguments first isLiteralNumber ]]]) ifTrue: [
						hasMatch := true ]]
		select: [ :node |
			hasMatch not ].
	self browseAllSelect: [ :method |
		hasMatch := false.
		method decompile accept: visitor.
		hasMatch ].
]

{ #category : 'browsing' }
SystemDictionary >> browseEqSentToSmallIntegerConstants [
	"
	Smalltalk browseEqSentToSmallIntegerConstants
	"
	| hasMatch visitor |
	hasMatch := false.
	visitor := ParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#= #~= ) identityIncludes: node selector key) and: [
					node receiver isLiteralSmallInteger or: [ node arguments first isLiteralSmallInteger ]]]) ifTrue: [
						hasMatch := true ]]
		select: [ :node |
			hasMatch not ].
	self browseAllSelect: [ :method |
		hasMatch := false.
		method decompile accept: visitor.
		hasMatch ].
]

{ #category : 'browsing - main' }
SystemDictionary >> browseFull: aClass [
	"
	Smalltalk browseFull: Float.
	Smalltalk browseFull: Float class.
	"
	^self browserWindowClass fullOnClass: aClass selector: nil
]

{ #category : 'browsing - main' }
SystemDictionary >> browseFull: aClass selector: aSymbol [
	"
	Smalltalk browseFull: Float selector: nil.
	Smalltalk browseFull: Float selector: #abs.
	Smalltalk browseFull: Float class selector: nil.
	Smalltalk browseFull: Float class selector: #pi.
	"
	^self browserWindowClass fullOnClass: aClass selector: aSymbol
]

{ #category : 'browsing - main' }
SystemDictionary >> browseHierarchy: aClass [
	"
	Smalltalk browseHierarchy: Float.
	Smalltalk browseHierarchy: Float class.
	"
	^self browserWindowClass hierarchyOnClass: aClass selector: nil
]

{ #category : 'browsing - main' }
SystemDictionary >> browseHierarchy: aClass selector: aSymbol [
	"
	Smalltalk browseHierarchy: Float selector: nil.
	Smalltalk browseHierarchy: Float selector: #abs.
	Smalltalk browseHierarchy: Float class selector: nil.
	Smalltalk browseHierarchy: Float class selector: #pi.
	"
	^self browserWindowClass hierarchyOnClass: aClass selector: aSymbol.
]

{ #category : 'browsing' }
SystemDictionary >> browseInstVarRefs: aClass [
	"1/16/96 sw: moved here from Browser so that it could be used from a variety of places.
	 7/30/96 sw: call chooseInstVarThenDo: to get the inst var choice"

	aClass chooseInstVarThenDo: 
		[:aVar | self browseAllAccessesTo: aVar from: aClass]
]

{ #category : 'browsing' }
SystemDictionary >> browseLikelyUnneededRedefinitions [

	"
	Spot unneeded method redefinitions: methods that are equal to what would be inherited if they weren't there
	Smalltalk browseLikelyUnneededRedefinitions
	Be careful with class side #initialize methods that set up instance class variables
	In general, class side #initialize methods can't be removed unless empty.
	"

	self browseAllSelect: [ :cm |
		(cm methodClass superclass ifNotNil: [ :sup | sup lookupSelector: cm selector]) = cm ]
]

{ #category : 'browsing' }
SystemDictionary >> browseMessageList: messageList name: label  [
	"Create and schedule a MessageSet browser on messageList."
	^ self browseMessageList: messageList name: label autoHighlight: nil allOccurrences: false
]

{ #category : 'browsing' }
SystemDictionary >> browseMessageList: methodReferences name: labelString autoHighlight: autoHighlightString allOccurrences: aBoolean [
	"Create and schedule a MessageSet browser on the message list."

	| messageListSize title |

	messageListSize := methodReferences size.
	messageListSize = 0 ifTrue: [
		PopUpMenu inform: ('There are no\' , labelString) withNewLines.
		^nil ].

	title := messageListSize > 1
		ifFalse: [ labelString ]
		ifTrue: [ '[', messageListSize printString, '] ', labelString].

	^ MethodSetWindow
		openMessageList: methodReferences
		label: title
		autoHighlight: autoHighlightString
		allOccurrences: aBoolean
]

{ #category : 'browsing' }
SystemDictionary >> browseMessageListUnsorted: methodReferences name: labelString autoHighlight: autoHighlightString [
	"Create and schedule a MessageSet browser on the message list.
	Don't sort entries by default."

	| messageListSize title |

	messageListSize := methodReferences size.
	messageListSize = 0 ifTrue: [^ PopUpMenu inform: ('There are no\' , labelString) withNewLines ].

	title := messageListSize > 1
		ifFalse: [ labelString ]
		ifTrue: [ '[', messageListSize printString, '] ', labelString].

	MethodSetWindow
		openMessageListUnsorted: methodReferences
		label: title
		autoHighlight: autoHighlightString
		allOccurrences: true
]

{ #category : 'Closure measurements' }
SystemDictionary >> browseMethodsWithEmptyClosures [
	"
	Smalltalk browseMethodsWithEmptyClosures
	"
	| pattern1 pattern2 |
	pattern1 := EncoderForV3PlusClosures emptyClosurePattern.
	pattern2 := EncoderForSistaV1 emptyClosurePattern.
	self
		browseAllSelect: [ :m | | s |
			s := InstructionStream on: m.
			s scanFor: [ :bytecode |
				(s method is: pattern1 subcollectionAt: s pc) or:
					[s method is: pattern2 subcollectionAt: s pc]]]
		name:  'Methods with empty closures'
		autoHighlight: '[]'
]

{ #category : 'browsing' }
SystemDictionary >> browseMethodsWithSourceString: aString [
	"Smalltalk browseMethodsWithSourceString: 'SourceString' "
	"Launch a browser on all methods whose source code contains aString as a substring."

	| caseSensitive suffix selectString |
	Sensor controlKeyPressed ifTrue: [
		selectString := ((aString findTokens: '*') sorted: [ :a :b | a size > b size ]) first.
		^ self 
			browseMessageList: (self allMethodsSourceStringMatching: '*', aString, '*')
			name: 'Methods matched by ' , aString printString , ' (no ctrl for exact match)'
			autoHighlight: selectString
			allOccurrences: true
		].
	suffix := (caseSensitive := Sensor shiftPressed)
		ifTrue: [' (case-sensitive. Use ctrl for $* pattern match.)']
		ifFalse: [' (shift for case-sensitive. ctrl for $* pattern match.)'].
	^ self 
		browseMessageList: (self allMethodsWithSourceString: aString matchCase: caseSensitive)
		name: 'Methods containing ' , aString printString , suffix
		autoHighlight: aString
		allOccurrences: true.
]

{ #category : 'browsing' }
SystemDictionary >> browseMethodsWithSourceString: aString in: aPackage [
	" Browse all methods in aPackage whose source code contains aString as a substring."

	| matchingMethods |
	matchingMethods := Set new.
	aPackage classesDo: [ :aPackageClass |
		(aPackageClass organization classComment
			findString: aString
			startingAt: 1
			caseSensitive: false) > 0 ifTrue: [
				matchingMethods add: (MethodReference class: aPackageClass selector: #Comment) ]].
	aPackage methods do: [ :methodReference |
		(methodReference sourceCode 
			findString: aString
			startingAt: 1
			caseSensitive: false) > 0 ifTrue: [
				matchingMethods add: methodReference ]].
		
	^ self 
		browseMessageList: matchingMethods asArray sort
		name: 'Methods containing ' , aString printString, ' in package: ', aPackage packageName
		autoHighlight: aString
		allOccurrences: true.
]

{ #category : 'browsing' }
SystemDictionary >> browseMethodsWithSourceString: aString [
scopeClass: scopeClass
includeSubclasses: includeSubclasses
includeSuperclasses: includeSuperclasses
	"Smalltalk browseMethodsWithSourceString: 'SourceString' "
	"Launch a browser on all methods whose source code contains aString as a substring."

	| caseSensitive suffix selectString |
	Sensor controlKeyPressed ifTrue: [
		selectString := ((aString findTokens: '*') sorted: [ :a :b | a size > b size ]) first.
		^ self 
			browseMessageList: (self allMethodsSourceStringMatching: '*', aString, '*')
			name: 'Methods matched by ' , aString printString , ' (no ctrl for exact match)'
			autoHighlight: selectString
			allOccurrences: true
		].
	suffix := (caseSensitive := Sensor shiftPressed)
		ifTrue: [' (case-sensitive. Use ctrl for $* pattern match.)']
		ifFalse: [' (shift for case-sensitive. ctrl for $* pattern match.)'].
	^ self 
		browseMessageList: (self allMethodsWithSourceString: aString matchCase: caseSensitive scopeClass: scopeClass includeSubclasses: includeSubclasses includeSuperclasses: includeSuperclasses)
		name: 'Methods containing ' , aString printString , suffix
		autoHighlight: aString
		allOccurrences: true.
]

{ #category : 'browsing' }
SystemDictionary >> browseMethodsWithString: aString [
	"Launch a browser on all methods that contain string literals with aString as a substring. The search is case-insensitive, unless the shift key is pressed, in which case the search is case-sensitive."

	^ self browseMethodsWithString: aString matchCase: false

	"Smalltalk browseMethodsWithString: 'Testing' matchCase: false"
	"Smalltalk browseMethodsWithString: 'Testing' matchCase: true"
]

{ #category : 'browsing' }
SystemDictionary >> browseMethodsWithString: aString matchCase: caseSensitive [
	"Launch a browser on all methods that contain string literals with aString as a substring. Make the search case-sensitive or insensitive as dictated by the caseSensitive boolean parameter"

	self
		browseAllSelect: [ :method |
			method  hasLiteralSuchThat: [ :lit |
				lit isString & lit isSymbol not and:
				[lit includesSubstring: aString caseSensitive: caseSensitive]]]
		name:  'Methods with string ', aString printString, (caseSensitive ifTrue: [' (case-sensitive)'] ifFalse: [' (case-insensitive)'])
		autoHighlight: aString.
]

{ #category : 'browsing' }
SystemDictionary >> browseMyChanges [
	"Browse only the changes (in the changes file) by the current author.
	Smalltalk browseMyChanges
	"
	self browseAllSelect: [ :method |
       method fileIndex > 1 "only look at changes file"
               and: [ method authorAndStamp beginsWith: Utilities authorInitials, ' ' ]]
]

{ #category : 'housekeeping' }
SystemDictionary >> browseObsoleteMethodReferences [
	"Open a browser on all referenced behaviors that are obsolete
		Smalltalk browseObsoleteMethodReferences
	Remember that if no methods reference obsoletes, but
		Smalltalk obsoleteBehaviors inspect
	still finds them, maybe they are referenced by ChangeSets!
	"
	| list |
	list := self obsoleteMethodReferences.
	self browseMessageList: list name: 'Method referencing obsoletes'
]

{ #category : 'browsing' }
SystemDictionary >> browseObsoleteReferences   "Smalltalk browseObsoleteReferences" [
	| references |
	references := OrderedCollection new.
	(LookupKey allSubInstances select:
		[:x | ((x value isKindOf: Behavior) and: ['AnOb*' match: x value name]) or:
		['AnOb*' match: x value class name]]) 
		do: [:x | references addAll: (self allCallsOn: x)].
	self 
		browseMessageList: references 
		name: 'References to Obsolete Classes'
]

{ #category : 'browsing' }
SystemDictionary >> browseSendersOf: aSymbol notTo: classes [
	"
	Smalltalk browseSendersOf: #new: notTo: {}
	Smalltalk browseSendersOf: #new: notTo: {ArrayedCollection. CharacterSequence. Number. ThirtyTwoBitSlotsObject. BlockClosure. AdditionalMethodState. WeakMessageSend. WeakValueAssociation}
	"
	| hasMatch visitor thisMethod |
	thisMethod := nil.
	hasMatch := false.
	visitor := ParseNodeEnumerator
		ofBlock: [ :node | | receiverClass |
			(node isMessageNode and: [node selector key = aSymbol]) ifTrue: [
"thisMethod selector = #bitReverse: ifTrue: [self halt]."
				receiverClass := nil.
				Smalltalk at: node receiver name ifPresent: [ :referencedClass |
					receiverClass := referencedClass ].
				(node receiver name = #self and: [ thisMethod methodClass isMeta ]) ifTrue: [
					receiverClass := thisMethod methodClass theNonMetaClass ].
				node receiver name = '{self class}' ifTrue: [
					receiverClass := thisMethod methodClass ].
				node receiver name = '{self species}' ifTrue: [
					receiverClass := thisMethod methodClass ].
				(node receiver name = #super and: [ thisMethod methodClass isMeta ]) ifTrue: [
					receiverClass := thisMethod methodClass theNonMetaClass superclass ].
				(receiverClass isNil or: [ classes noneSatisfy: [ :c | receiverClass includesBehavior: c ] ]) ifTrue: [
					hasMatch := true ].
			]]
		select: [ :node |
			hasMatch not ].

	self
		browseAllSelect: [ :method |
			hasMatch := false.
			thisMethod := method.
			method decompile accept: visitor.
			hasMatch ]
		name: 'Senders of: ', aSymbol printString, ' but not to: ', classes printString
		autoHighlight: aSymbol.
]

{ #category : 'housekeeping' }
SystemDictionary >> browseUndeclaredReferences [
	"
	Smalltalk browseUndeclaredReferences
	"
	| undeclaredReferences |
	
	undeclaredReferences := self undeclaredReferences.
	undeclaredReferences ifEmpty: [
		 ^(PopUpMenu labels: ' OK ') startUpWithCaption: 'There are no Undeclared at all' ].
	
	undeclaredReferences keysAndValuesDo: [ :undeclared :references |
		self
			browseMessageList: references
			name: 'References to Undeclared: ', undeclared printString ].
]

{ #category : 'browsing' }
SystemDictionary >> browseViewReferencesFromNonViews [
	"
	Smalltalk browseViewReferencesFromNonViews
	"
	| aLiteral aCollection |

	aCollection := OrderedCollection new.

	"Tweak to look just for pluggables or also for menus (or maybe for all morphs)"
"	PopUpMenu withAllSubclasses , MenuMorph withAllSubclasses , PluggableMorph withAllSubclasses do: [ :view |"
	PluggableMorph withAllSubclassesDo: [ :view |
"	MenuMorph withAllSubclassesDo: [ :view |"
		
		aLiteral := view name.
		
		"tweak to linclude refs to SysWindow subhierarchy or not"
		(view includesBehavior: SystemWindow) & false ifFalse: [
			self allBehaviorsDo: [ :class |
				((class includesBehavior: Morph) or: [ class includesBehavior: Morph class ]) ifFalse: [
					class addMethodsTo: aCollection thatReferTo: aLiteral ]]]].
	
	self
		browseMessageList: aCollection asSet
		name: 'References to Views from non-Views'.
]

{ #category : 'browsing - main' }
SystemDictionary >> browserWindowClass [
	^Preferences at: #browserWindowClass
]

{ #category : 'memory space' }
SystemDictionary >> bytesLeft [
	"Answer the number of bytes of space available. Does a full garbage collection."

	^ self garbageCollect

]

{ #category : 'memory space' }
SystemDictionary >> bytesLeft: aBool [
	"Return the amount of available space. If aBool is true, include possibly available swap space. If aBool is false, include possibly available physical memory. For a report on the largest free block currently availabe within Squeak memory but not counting extra memory use #primBytesLeft."
	<primitive: 112>
	^self primBytesLeft
]

{ #category : 'memory space' }
SystemDictionary >> bytesLeftString [
	"Return a string describing the amount of memory available"
	| availInternal availPhysical availTotal |
	self garbageCollect.
	availInternal := self primBytesLeft.
	availPhysical := self bytesLeft: false.
	availTotal := self bytesLeft: true.
	(availTotal > (availInternal + 10000)) "compensate for mini allocations inbetween"
		ifFalse:[^availInternal printStringWithCommas, ' bytes available'].
	^String streamContents:[:s|
		s nextPutAll: availInternal printStringWithCommas, 	' bytes (internal) '; newLine.
		s nextPutAll: availPhysical printStringWithCommas,	' bytes (physical) '; newLine.
		s nextPutAll: availTotal printStringWithCommas, 	' bytes (total)     '].
]

{ #category : 'miscelaneous' }
SystemDictionary >> cachedSystemAttribute: attributeID [
	(attributeID between: 1001 and: 1009)
		ifFalse: [ ^nil ].
	SystemAttributeCache ifNil: [
		SystemAttributeCache := Array new: 9.
		1 to: 9 do: [ :i |
			SystemAttributeCache at: i put: (self primGetSystemAttribute: 1000+i) ]].
	^SystemAttributeCache at: attributeID-1000
]

{ #category : 'sources, change log' }
SystemDictionary >> calcEndianness [
	| bytes word blt |
	"What endian-ness is the current hardware?  The String '1234' will be stored into a machine word.  On BigEndian machines (the Mac), $1 will be the high byte if the word.  On LittleEndian machines (the PC), $4 will be the high byte."
	"Smalltalk endianness"

	bytes := ByteArray withAll: #(0 0 0 0).  "(1 2 3 4) or (4 3 2 1)"
	word := WordArray with: 16r01020304.
	blt := (BitBlt toForm: (Form new hackBits: bytes)) 
				sourceForm: (Form new hackBits: word).
	blt combinationRule: Form over.  "store"
	blt sourceY: 0; destY: 0; height: 1; width: 4.
	blt sourceX: 0; destX: 0.
	blt copyBits.  "paste the word into the bytes"
	bytes first = 1 ifTrue: [^ #big].
	bytes first = 4 ifTrue: [^ #little].
	self error: 'Ted is confused'.
]

{ #category : 'miscellaneous' }
SystemDictionary >> canBeInterrupted: aProcess [
	"Answer nil if aProcess can be interrupted.
	Answer an error message if not."

	aProcess isTerminated ifTrue: [
		^('Process {', aProcess printString, '} isTerminated. Can not interrupt it.') ].

	"The background process can't be interrupted, or Cuis will likely crash."
	aProcess == Processor backgroundProcess ifTrue: [
		^ 'Can not interrupt backgroundProcess' ].

	(aProcess name beginsWith: '[system]') ifTrue: [
		^ ('Process {', aProcess printString, '} is critical for system stability. Can not interrupt it.') ].

	^nil
]

{ #category : 'image, changes name' }
SystemDictionary >> changeImageNameTo: aString [
	"Convert to the format the VM expects: an instance of String that
	holds UTF-8 bytes. (If a pure ascii byte String, that's already the case)"
	| utf8BytesInAStringInstance |
	(aString isByteString and: [ aString isAscii ])
		ifTrue: [ utf8BytesInAStringInstance := aString ]
		ifFalse: [
			utf8BytesInAStringInstance := aString asUtf8Bytes.
			String adoptInstance: utf8BytesInAStringInstance ].
	self primImageName: utf8BytesInAStringInstance.
	LastImageName := self imageName
]

{ #category : 'startup' }
SystemDictionary >> checkIfAlreadyRunningOrStoppedNoExit [
	"
	If the Changes file is not clean during startup, it may mean either:
	- last run crashed or was killed (code recovery may be in order)
	- Cuis is already running. Running it again may mean inconsistent updates to .changes file.
	Smalltalk checkIfAlreadyRunningOrStoppedNoExit
	"
	| imageSaveChunk postImageSaveChunk |
	imageSaveChunk := nil.
	postImageSaveChunk := nil.
	self withChangesFileDo: [ :changesFile |
		changesFile position: self lastQuitLogPosition.
		changesFile atEnd ifFalse: [ imageSaveChunk := changesFile nextChunk ].
		changesFile atEnd ifFalse: [ postImageSaveChunk := changesFile nextChunk ]].

	"If we can't find the image save chunk at the image save position, something is wrong."
	(imageSaveChunk isNil or: [ ((Smalltalk isQuitRecord: imageSaveChunk		) or: [
			Smalltalk isSnapshotRecord: imageSaveChunk ]) not])
		ifTrue: [
			UISupervisor whenUIinSafeState: [
				self notify:
					self currentChangesName asFullFileEntry pathName, String newLineString,
					'Image save chunk not found. Possible image / changes file corruption. Proceed with caution.' ].
				^self ].

	"If there is already a post-imageSave chunk, it may mean Cuis is running in another OS process,
	or in a previous run, it was killed or crashed."
	postImageSaveChunk notNil ifTrue: [
		(Preferences at: #checkLostChangesOnStartUp)
			ifTrue: [
				"Let the user chose automatic or manual recovery from .changes file.
				Used mainly by CuisUniversity."
				UISupervisor whenUIinSafeState: [self restoreLostChanges ]]
			ifFalse: [
				"Notify user about the situation, open changes recovery tool on last User Changes file."
				(Preferences at: #warnAndOfferLastUserChanges) ifTrue: [
					UISupervisor whenUIinSafeState: [self warnAndOfferLastUserChanges]]]].
]

{ #category : 'class names' }
SystemDictionary >> classNamed: className  [
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

	self at: className asSymbol ifPresent: [ :found |
		^ found isBehavior ifTrue: [ found ]].

	(className withoutSuffix: ' class') ifNotNil: [ :baseName |
		self at: baseName asSymbol ifPresent: [ :found |
			^ found isBehavior ifTrue: [ found class ]]].

	^ nil
]

{ #category : 'classes and traits' }
SystemDictionary >> classNames [
	"Answer a sorted collection of all class names. Use the return value of #fillCaches to avoid concurrency issues."
	
	^cachedClassNames ifNil: [ self fillCaches at: 1 ]
]

{ #category : 'sources, change log' }
SystemDictionary >> classRemoved: aClass fromCategory: aCategoryName [

	| classDefinition |
	
	aClass acceptsLoggingOfCompilation 
		ifTrue: [
			"I have to recreate the category because the classs has already been removed form the
			SystemOrganizer - Hernan"
			classDefinition := aClass definitionReplacingCategoryWith: aCategoryName. 
			
			self 
				logChange: classDefinition 
				preamble: 'classRemoval: ', aClass name printString, Utilities changeStampField ]
]

{ #category : 'accessing' }
SystemDictionary >> classes [

	^self classNames collect: [:each | self at: each ]
]

{ #category : 'query' }
SystemDictionary >> classesWithExtensionCategories [
	"For a base image, there should be none.
	Smalltalk classesWithExtensionCategories
	"

	Object withAllSubclasses do: [ :cls | cls organization categories do: [ :cat | cat first == $* ifTrue: [cls print ]]]
]

{ #category : 'housekeeping' }
SystemDictionary >> cleanOutUndeclared  [
	Undeclared removeUnreferencedKeys
]

{ #category : 'special objects' }
SystemDictionary >> clearExternalObjects [
	"Clear the array of objects that have been registered for use in non-Smalltalk code."
	"Smalltalk clearExternalObjects"

	ExternalSemaphoreTable clearExternalObjects

]

{ #category : 'profiling' }
SystemDictionary >> clearProfile [
	"Clear the profile database."

	<primitive: 250>

]

{ #category : 'sources, change log' }
SystemDictionary >> closeSourceFiles [
	"Shut down the source files if appropriate.  1/29/96 sw: changed so that the closing and nilification only take place if the entry was a FileStream, thus allowing stringified sources to remain in the saved image file"

	1 to: 2 do: [ :i |
		(SourceFiles at: i) ifNotNil: [ :strm |
			strm isFileStream
				ifTrue: [
					strm close.
					SourceFiles at: i put: nil ]]]
]

{ #category : 'miscellaneous' }
SystemDictionary >> cogitClass [
	^self getSystemAttribute: 1008
]

{ #category : 'housekeeping' }
SystemDictionary >> condenseChanges [
	"Move all the changes onto a compacted sources file."
	"
	Smalltalk condenseChanges
	"

	| oldChanges classCount oldChangesLocalName oldChangesPathName |
	DirectoryEntry smalltalkImageDirectory // 'ST80.temp' forceWriteStreamDo: [ :f |
		f writeSystemTimeStamp.
		'Condensing Changes File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: self classNames size
			during: [ :barBlock |
				classCount := 0.
				self allClassesDo: [ :class | 
					barBlock value: (classCount := classCount + 1).
					class moveChangesTo: f.
					class putClassCommentToCondensedChangesFile: f.
					class class moveChangesTo: f ]].
		LastQuitLogPosition := f position ].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	oldChanges := SourceFiles at: 2.
	oldChangesPathName := oldChanges name.
	oldChangesLocalName := oldChanges localName.
	oldChanges close.
	(oldChangesPathName, '.old') asFullFileEntry delete.
	oldChangesPathName asFullFileEntry rename: oldChangesLocalName, '.old'.
	DirectoryEntry smalltalkImageDirectory // 'ST80.temp' rename: oldChangesLocalName.
	
	SourceFiles
			at: 2 put: oldChangesPathName asFullFileEntry appendStream.

	self inform: 'Changes file has been rewritten!

Check that all is well, and then save/quit.
 
Otherwise, remove new changes,
replace it with the former one, and
exit without saving the image.
 '
]

{ #category : 'housekeeping' }
SystemDictionary >> condenseSources	 [
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	
	self request: 'Please name the new sources file' initialAnswer: SourceFileVersionString verifying: [:newVersionString| newVersionString ~= SourceFileVersionString] do: [:newVersionString|
		self condenseSources: newVersionString.
		self inform: 'Source files have been rewritten!
 
Check that all is well, and then save/quit.
 
Otherwise, remove new sources/changes,
replace them with the former ones, and
exit without saving the image.
 ']
]

{ #category : 'housekeeping' }
SystemDictionary >> condenseSources: newVersionString [
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| classCount oldChanges oldChangesLocalName oldChangesPathName newChangesPathName newSourcesName |
	SourceFileVersionString := newVersionString.

	"Write all sources with fileIndex 1"
	newSourcesName := self defaultSourcesName.
	newSourcesName asFullFileEntry writeStreamDo: [ :f |
		f writeSystemTimeStamp.
		'Condensing Sources File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: self classNames size
			during: [ :barBlock |
				classCount := 0.
				self allClassesDo: [ :class |
					barBlock value: (classCount := classCount + 1).
					class fileOutOn: f moveSource: true toFile: 1]]].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	"Make a new empty changes file"
	oldChanges := SourceFiles at: 2.
	oldChangesPathName := oldChanges name.
	oldChangesLocalName := oldChanges localName.
	self closeSourceFiles.
	oldChangesPathName ifNotNil: [
		(oldChangesPathName, '.old') asFullFileEntry delete.
	oldChangesPathName asFullFileEntry rename: oldChangesLocalName, '.old' ].
	newChangesPathName := self defaultChangesName.
	newChangesPathName asFullFileEntry writeStreamDo: [ :stream |
		stream writeSystemTimeStamp ].
	LastQuitLogPosition := 0.

	self openSourceFiles.
]

{ #category : 'ui' }
SystemDictionary >> confirmRemovalOf: aSelector on: aClass  [
	"Determine if it is okay to remove the given selector. Answer 1 if it  
	should be removed, 2 if it should be removed followed by a senders  
	browse, and 3 if it should not be removed."

	^self confirmRemovalOf: aSelector on: aClass andSubclasses: false
]

{ #category : 'ui' }
SystemDictionary >> confirmRemovalOf: aSelector on: aClass andSubclasses: aBoolean [
	"Determine if it is okay to remove the given selector. Answer 1 if it  
	should be removed, 2 if it should be removed followed by a senders  
	browse, and 3 if it should not be removed."
	| count aMenu labels answer caption allCalls |
	allCalls := self allCallsOn: aSelector.
	(count := allCalls size) = 0
		ifTrue: [^ 1].
	"no senders -- let the removal happen without warning"
	count = 1
		ifTrue: [(allCalls first actualClass == aClass
					and: [allCalls first methodSymbol == aSelector])
				ifTrue: [^ 1]].
	"only sender is itself"
	labels := aBoolean
		ifTrue: [
'Remove from ', aClass name, ' and subclasses
Remove them, then browse senders
Don''t remove, but show me those senders
Forget it -- do nothing -- sorry I asked']
		ifFalse: [
'Remove it
Remove, then browse senders
Don''t remove, but show me those senders
Forget it -- do nothing -- sorry I asked'].
	aMenu := PopUpMenu labels: labels
		icons: #(acceptIcon acceptIcon cancelIcon cancelIcon).

	caption := 'This message has ' , count printString , ' sender'.
	count > 1
		ifTrue: [caption := caption copyWith: $s].
	answer := aMenu startUpWithCaption: caption.
	answer = 3
		ifTrue: [
			self
				browseMessageList: allCalls
				name: 'Senders of ' , aSelector
				autoHighlight: aSelector asString
				allOccurrences: false ].
	answer = 0
		ifTrue: [answer := 3].
	"If user didn't answer, treat it as cancel"
	^ answer min: 3
]

{ #category : 'code authors' }
SystemDictionary >> contributionsOf: aString [
	"
	Smalltalk contributionsOf: 'JMV'
	"
	| author answer |
	answer := OrderedCollection new.
	self allBehaviorsDo: [ :behavior |
		behavior methodsDo: [ :compiledMethod |
			author := compiledMethod author.
			aString = author ifTrue: [
				answer add: {compiledMethod methodClass. compiledMethod selector}]]].
	^answer
]

{ #category : 'memory space' }
SystemDictionary >> createStackOverflow [
	"For testing the low space handler..."
	"Smalltalk installLowSpaceWatcher; createStackOverflow"

	self createStackOverflow.  "infinite recursion"
]

{ #category : 'image, changes name' }
SystemDictionary >> currentChangesName [
	"Answer the full path to the version-stable source code currently in use.
	Answer nil if not a file (i.e. if internalized with #internalizeChangeLog or #internalizeSources, or closed with #closeSourceFiles)"

	^SourceFiles second ifNotNil: [ :changes |
		changes isFileStream
			ifTrue: [ changes name ]]
]

{ #category : 'image, changes name' }
SystemDictionary >> currentSourcesName [
	"Answer the full path to the version-stable source code currently in use
	Answer nil if not a file (i.e. if internalized with #internalizeSources, or closed with #closeSourceFiles)"

	^SourceFiles first ifNotNil: [ :sources |
		sources isFileStream
			ifTrue: [ sources name ]]
]

{ #category : 'globals' }
SystemDictionary >> declare: aStringOrSymbol [
	"Declare a global variable.
	Do nothing if aSymbol is already a global or class name.
	Smalltalk declare: #Zork
	Zork
	"
	| aSymbol |

	"If name is taken by a class, do nothing and answer nil."
	(self hasClassNamed: aStringOrSymbol)
		ifTrue: [ ^nil ].

	"Add the global name if it wasn't there.
	In any case, answer the Association. May be of use to sender."
	aSymbol := aStringOrSymbol asSymbol.
	self at: aSymbol ifAbsentPut: nil.
	^ self associationAt: aSymbol.
]

{ #category : 'image, changes name' }
SystemDictionary >> defaultChangesName [
	"Answer the default full path to the changes file corresponding to the image file name."
	"
	Smalltalk defaultChangesName
	"
	^(FileIOAccessor default baseNameFor: self imageName), '.changes'
]

{ #category : 'image, changes name' }
SystemDictionary >> defaultSourcesName [
	"Answer the default full path to the sources file corresponding to the image file name."
	"
	Smalltalk defaultSourcesName
	"
	^ self imagePath, '/', SourceFileVersionString, '.sources'
]

{ #category : 'accessing' }
SystemDictionary >> description [
	"Needed by CodeFileBrowser to use Smalltalk as the 'case' source"
	^ self name
]

{ #category : 'miscellaneous' }
SystemDictionary >> detectBuiltInModule: aBlock [
	"
	Smalltalk detectBuiltInModule: [ :n | n beginsWith: 'BitBltPlugin' ]
	Smalltalk detectBuiltInModule: [ :n | n beginsWith: 'Nope' ]
	"
	"Look for a matching builtin module (e.g., plugins). Builtin plugins are those that are compiled with the VM directly, as opposed to plugins residing in an external shared library.
	Answer nil if none."
	| index name |
	index := 1.
	[ (name := self listBuiltinModule: index) notNil ] whileTrue: [
		(aBlock value: name) ifTrue: [ ^name ].
		index := index + 1.
	].
	^nil
]

{ #category : 'startup' }
SystemDictionary >> displayCommandLineUsageOn: aStream [
	"Display command line usage on standard out."

	[ aStream
		nextPutAll: 'Command line options:'; newLine;
		nextPutAll: '  -h Display this help message and quit'; newLine;
		nextPutAll: '  -v Display the system information, containing the version and quit'; newLine;
		nextPutAll: '  -q Just quit'; newLine;
		nextPutAll: '  -e Do not ignore exceptions during startup, but open a Debugger'; newLine;
		nextPutAll: '  -u Install available updates from CoreUpdates directory'; newLine;
		nextPutAll: '  -r <feature> Require <feature>. Find and install a Cuis package file supplying it.'; newLine;
		nextPutAll: '  -d <code> Evaluate code. <code> must be a Smalltalk expression in double quotes. (No comments allowed)'; newLine;
		nextPutAll: '  -s <scriptFile> The file must contain a valid Smalltalk expression. Compile and evaluate it.'; newLine;
		nextPutAll: '  -l <codeFile> File in Smalltalk code. The file must be a valid ChangeSet file.'; newLine;
		nextPutAll: '  -ud <directory> Set User Directory to be <directory>'; newLine;
		nextPutAll: '  -udIsBase Set User Directory to be Cuis Base directory'; newLine;
		nextPutAll: '  -- end of options; stop processing options'; newLine;
		newLine;
		nextPutAll: 'Arguments for scripts, code files, packages (and <code>):'; newLine;
		nextPutAll: '  are available in ''Smalltalk startUpScriptArguments'''; newLine;
		newLine;
		nextPutAll: 'Multiple options:'; newLine;
		nextPutAll: '  any option can be passed multiple times'; newLine;
		flush ]
	on: UnhandledError
	do: [ :exception | exception return ].
]

{ #category : 'startup' }
SystemDictionary >> displaySystemInformationStringOn: aStream [
	"Display the system information string on standard output."

	[ aStream
		nextPutAll: 'System information:';		newLine;
		nextPutAll: self systemInformationString;		newLine;
		flush ]
	on: UnhandledError
	do: [ :exception | exception return ].
]

{ #category : 'startup' }
SystemDictionary >> doStartUp: isARealStartup [
	"
	isARealStartup
		true: system is coming up (VM and image just started)
		false: we have just saved an image snapshot, but didn't quit.
	"
	"Here, startup begins!"
	Cursor defaultCursor activateCursor.
	self setGCParameters.
	isARealStartup ifTrue: [ self clearExternalObjects ].

	"We disable comparison between a Float and a SmallInteger in the VM,
	because the VM converts the float to int, and not the int to float.
	See #numericPrimsMixComparison"
	self numericPrimsMixArithmetic: true.
	self numericPrimsMixComparison: false.

	self readCommandLineArguments.
	self processStartUpList: isARealStartup.
	isARealStartup ifTrue: [
		self setPlatformPreferences.
		self setStartupStamp ].
]

{ #category : 'profiling' }
SystemDictionary >> dumpProfile [
	"Dump the profile database to a file."

	<primitive: 251>

]

{ #category : 'sources, change log' }
SystemDictionary >> endianness [
	"Endianness is cached. If it happens to be nil, we are in the midst of either shutdown or startup. In such cases, compute endiannes but DO NOT cache it. I.e. do not do lazy initialization."
	^EndianCache ifNil: [ self calcEndianness ]
]

{ #category : 'sources, change log' }
SystemDictionary >> evaluated: expression context: aContext [

	self logChange: expression
]

{ #category : 'private' }
SystemDictionary >> exitToDebugger [
	"Enter the machine language debugger, if one exists.  Essential.  See Object 
	documentation whatIsAPrimitive. "

	<primitive: 114>
	self primitiveFailed
]

{ #category : 'special objects' }
SystemDictionary >> externalObjects [
	"Return an array of objects that have been registered for use in non-Smalltalk code. Smalltalk objects should be referrenced by external code only via indirection through this array, thus allowing the objects to move during compaction. This array can be cleared when the VM re-starts, since variables in external code do not survive snapshots. Note that external code should not attempt to access a Smalltalk object, even via this mechanism, while garbage collection is in progress."
	"Smalltalk externalObjects"

	^ ExternalSemaphoreTable externalObjects

]

{ #category : 'sources, change log' }
SystemDictionary >> externalizeSources    [
	"Write the sources and changes streams onto external files."
	"
	Smalltalk externalizeSources
	"

	| sourcesName changesName |

	sourcesName := self defaultSourcesName.
	sourcesName asFullFileEntry writeStreamDo: [ :stream |
		stream nextPutAll: SourceFiles first originalContents ].
	SourceFiles at: 1 put: sourcesName asFullFileEntry readStream.

	changesName := self defaultChangesName.
	changesName asFullFileEntry writeStreamDo: [ :stream |
		stream nextPutAll: SourceFiles last contents ].
	SourceFiles at: 2 put: changesName asFullFileEntry appendStream.

	self inform: 'Sources successfully externalized'
]

{ #category : 'miscellaneous' }
SystemDictionary >> extraVMMemory [
	"Answer the current setting of the 'extraVMMemory' VM parameter. See the comment in extraVMMemory: for details."

	^ self vmParameterAt: 23

]

{ #category : 'miscellaneous' }
SystemDictionary >> extraVMMemory: extraBytesToReserve [
	"Request that the given amount of extra memory be reserved for use by the virtual machine to leave extra C heap space available for things like plugins, network and file buffers, and so on. This request is stored when the image is saved and honored when the image is next started up. Answer the previous value of this parameter."

	extraBytesToReserve < 0
		ifTrue: [self error: 'VM memory reservation must be non-negative'].
	^ self vmParameterAt: 23 put: extraBytesToReserve

]

{ #category : 'snapshot and quit' }
SystemDictionary >> filenameForUpdatedImage: aBoolean [
	"If aBoolean, include highest update number	"
	| fileName newName changesName systemVersion |
	systemVersion := SystemVersion current.
	fileName := String streamContents: [ :strm |
		strm nextPutAll: systemVersion versionString.
		aBoolean ifTrue: [
			strm
				nextPut: $-;
				print: systemVersion highestUpdate ].
		self wordSize = 4 ifTrue: [
			strm nextPutAll: '-32' ]].
	newName := fileName, '.image'.
	(DirectoryEntry smalltalkImageDirectory // newName) exists ifTrue: [
		newName := DirectoryEntry smalltalkImageDirectory
			nextNameFor: fileName
			extension: 'image' ].
	changesName := self fullNameForChangesNamed: newName.
	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	changesName asFullFileEntry exists ifTrue: [
		self inform:
'There is already .changes file of the desired name,
', changesName, '
curiously already present, even though there is no
corresponding .image file.
Please remedy manually and then repeat your request.'.
		^nil  ].
	^newName
]

{ #category : 'class names' }
SystemDictionary >> fillCaches [
	"Fill cachedClassNames and cachedNonClassNames. Return an array with the calculated values."

	| classNames nonClassNames |
	classNames := IdentitySet new.
	nonClassNames := IdentitySet new.
	self keysAndValuesDo: [ :key :value |
		"The key == value name test below addresses two separate issues:
			1) Obsolete classes, where key = #Foo and value name = 'AnObsoleteFoo'
			2) Aliases, i.e., Smalltalk at: #OtherName put: aClass"
		(value class isMeta and: [ key == value name ])
			ifTrue: [ classNames add: key ]
			ifFalse: [ nonClassNames add: key ] ].
	classNames := classNames asArray sort.
	nonClassNames := nonClassNames asArray sort.
	cachedClassNames := classNames.
	cachedNonClassNames := nonClassNames.
	^{ classNames. nonClassNames }
]

{ #category : 'browsing - main' }
SystemDictionary >> findAndBrowseClass [
	"
	Smalltalk findAndBrowseClass.
	"
	^self browserWindowClass findAndBrowseClass.
]

{ #category : 'browsing - main' }
SystemDictionary >> findInSourceCode [
	self request: 'Text to search source code for?' do: [:aString|
		Smalltalk browseMethodsWithSourceString: aString]
]

{ #category : 'system attributes' }
SystemDictionary >> flagInterpretedMethods: aBoolean [
	"The Cog VM can be instructed to set the flag bit of CompiledMethods that
	 it executes but will only interpret.  This can be used e.g. to profile startup.
	 See CompiledMethod>>#flag & CompiledMethod>>#clearFlag.  
	 This flag persists across snapshots, stored in the image header."

	self vmParameterAt: 48 put: ((self vmParameterAt: 48) bitClear: 2) + (aBoolean ifTrue: [2] ifFalse: [0])
]

{ #category : 'globals' }
SystemDictionary >> flushClassNameCache [
	"Smalltalk flushClassNameCache"
	"Forse recomputation of the cached list of class names."

	cachedClassNames := nil.
	cachedNonClassNames := nil
]

{ #category : 'sources, change log' }
SystemDictionary >> forceChangesToDisk [
	"Just flush the buffer and trust the OS to do its job."

	"Expensive and not worth doing, esp. in Windows with antivirus active, when installing large packages"
	ChangeSet notInstallOrTestRun ifTrue: [
		SourceFiles second ifNotNil: [ :changes |
			changes isFileStream ifTrue: [ changes flush ]]].
]

{ #category : 'class names' }
SystemDictionary >> forgetClass: aClass logged: aBool  [
	"Delete the class, aClass, from the system.
	Note that this doesn't do everything required to dispose of a class - to do that use Class>>removeFromSystem."

	| classCategory |
	
	"I have to keep the cateogory becuase it is nil after removing the class -Hernan"
	classCategory := aClass category.
	
	SystemOrganization removeElement: aClass name.
	aBool ifTrue: [SystemChangeNotifier uniqueInstance classRemoved: aClass fromCategory: classCategory].		
	self removeFromStartUpList: aClass.
	self removeFromShutDownList: aClass.
	self removeKey: aClass name ifAbsent: nil.
	self flushClassNameCache
]

{ #category : 'image, changes name' }
SystemDictionary >> fullNameForChangesNamed: aName [
	"
	Smalltalk fullNameForChangesNamed: 'newChanges'
	"
	| newName |
	newName := FileIOAccessor default baseNameFor: ((DirectoryEntry smalltalkImageDirectory // aName) pathName).
	^newName , '.changes'
]

{ #category : 'image, changes name' }
SystemDictionary >> fullNameForImageNamed: aName [
	"
	Smalltalk fullNameForImageNamed: 'newImage'
	"
	| newName |
	newName := FileIOAccessor default baseNameFor: ((DirectoryEntry smalltalkImageDirectory // aName) pathName).
	^newName , '.image'
]

{ #category : 'memory space' }
SystemDictionary >> garbageCollect [
	"Primitive. Reclaims all garbage and answers the number of bytes of available space."
	ActiveModel flushEventSystem.
	^self primitiveGarbageCollect
]

{ #category : 'memory space' }
SystemDictionary >> garbageCollectMost [
	"Primitive. Reclaims recently created garbage (which is usually most of it) fairly quickly and answers the number of bytes of available space."

	<primitive: 131>
	^ self primBytesLeft
]

{ #category : 'miscelaneous' }
SystemDictionary >> getSystemAttribute: attributeID [
  	"Optional. Answer the string for the system attribute with the given 
  	integer ID. Answer nil if the given attribute is not defined on this 
  	platform. On platforms that support invoking programs from command 
  	lines (e.g., Unix), this mechanism can be used to pass command line 
  	arguments to programs written in Squeak.
  
  	By convention, the first command line argument that is not a VM
  	configuration option is considered a 'document' to be filed in. Such a
  	document can add methods and classes, can contain a serialized object,
  	can include code to be executed, or any combination of these.
  
  	Currently defined attributes include: 
  	-1000	1000th command line argument that specify VM options
  	...
  	-1		first command line argument that specify VM options
  	0		the full path name for currently executing VM
  			(or, on some platforms, just the path name of the VM's directory) 
  	1		path name of this image (better use #imageName instead)
  	2		command line argument See #processCommandLineArguments
  	3		command line argument
  	...
  	1000	last possible command line argument
  	1001	this platform's operating system 'Mac OS', 'Win32', 'unix', ...
  	1002	operating system version
  	1003	this platform's processor type
  	1004	vm version
  	1005	window system name
  	1006	vm build id
  	1007	Interpreter class (Cog VM only)
  	1008	Cogit class (Cog VM only)
 	1009	Platform source version (Cog VM only?)
  	1201	max filename length (Mac OS only)
  	1202	file last error (Mac OS only)
  	10001	hardware details (Win32 only)
  	10002	operating system details (Win32 only)
  	10003	graphics hardware details (Win32 only)
  	"
	(self cachedSystemAttribute: attributeID) ifNotNil: [ :cached |
		^cached ].
	^self primGetSystemAttribute: attributeID.
]

{ #category : 'miscellaneous' }
SystemDictionary >> getVMParameters	"Smalltalk getVMParameters" [
	"Answer an Array containing the current values of the VM's internal
	parameter/metric registers.  Each value is stored in the array at the
	index corresponding to its VM register.  (See #vmParameterAt: and
	#vmParameterAt:put:.)"

	<primitive: 254>
	self primitiveFailed
]

{ #category : 'ui' }
SystemDictionary >> globals [
	" 
	Smalltalk  globals
	"
	
	| globals |
	globals := IdentityDictionary new.
	self associationsDo: [ :assoc |
		assoc value class isMeta not ifTrue: [ globals add: assoc ]].
	^globals
]

{ #category : 'memory space' }
SystemDictionary >> growMemoryByAtLeast: numBytes [
	"Grow memory by at least the requested number of bytes.
	 Primitive.  Fail if no memory is available.  Essential.
	 N.B. In Spur, the heap is composed of segments. Growing memory adds a new
	 segment; it does not extend existing segments. Growth is by at least that
	 established by Smalltalk vmParameterAt: 25, which defaults to 16Mb."

	<primitive: 180>
	^(numBytes isInteger and: [numBytes > 0])
		ifTrue: [OutOfMemory signal]
		ifFalse: [self primitiveFailed]
]

{ #category : 'miscellaneous' }
SystemDictionary >> handleUserInterrupt [
	| processToInterrupt oldUIProcess |
	"If the UI process is not ready to run, the UI was not responsive, and most likely that's why the user pressed
	the interrupt key. Make GUI responsive again, and let them debug the process that made UI not responsive.
	'The four Andreas tests for interruptability':
	http://lists.squeakfoundation.org/pipermail/squeak-dev/2022-July/221727.html
		[true] whileTrue
		[[true] whileTrue] forkAt: Processor userSchedulingPriority + 1
		Smalltalk createStackOverflow
		[Smalltalk createStackOverflow] forkAt: Processor userSchedulingPriority + 1
	"
	oldUIProcess := UISupervisor uiProcessIfCantRun.
	"Shift can only be detected on the Mac, due to likely buggy kestroke reporting..."
	processToInterrupt := Sensor shiftPressed | (Preferences at: #cmdDotInterruptTakesStatistics)
		ifTrue: [Utilities reportCPUandRAM]
		ifFalse: [Utilities processTakingMostCPU].
	(Preferences at: #cmdDotEnabled) ifFalse: [
		^self ].
	(processToInterrupt isNil or: [ (self canBeInterrupted: processToInterrupt) notNil]) ifTrue: [
		processToInterrupt := oldUIProcess ifNil: [ Processor preemptedProcess ]].
	processToInterrupt isNil ifTrue: [
		'No process to interrupt found' print.
		^self ].
	(self canBeInterrupted: processToInterrupt) ifNotNil: [ :msg |
		msg print.
		^self ].
	[
		EventSensor install.
		Debugger openProcess: processToInterrupt context: processToInterrupt suspendedContext label: 'User Interrupt'
	] fork "Forked so the #userInterruptWatcher process continues."
]

{ #category : 'testing' }
SystemDictionary >> hasBindingThatBeginsWith: aString [
	"Answer true if the receiver has a key that begins with aString, false otherwise"
	"
	[ 5000 timesRepeat: [Smalltalk hasBindingThatBeginsWith: 'Obj' ]] timeToRun 
		14
	Smalltalk hasBindingThatBeginsWith: 'Obj'
	[ 5000 timesRepeat: [Smalltalk hasBindingThatBeginsWith: 'zxzxObj' ]] timeToRun 
		47
	Smalltalk hasBindingThatBeginsWith: 'zxzxObj'
	"
	
	"Use the cached class and non-class names for better performance."
	| searchBlock |
	searchBlock := [ :element |
		(element beginsWith: aString)
			ifTrue: [ 0 ]
			ifFalse: [
				aString < element
					ifTrue: [ -1 ]
					ifFalse: [ 1 ] ] ].

	self classNames 
		findBinary: searchBlock
		do: [ :found | ^true ]
		ifNone: [ :a :b | ].
	
	self nonClassNames 
		findBinary: searchBlock
		do: [ :found | ^true ]
		ifNone: [ :a :b | ].

	^false
]

{ #category : 'class names' }
SystemDictionary >> hasClassNamed: aStringOrSymbol [
	"Answer whether there is a class of the given name, but don't intern aString if it's not alrady interned.  4/29/96 sw
		Smalltalk hasClassNamed: 'Morph'
		Smalltalk hasClassNamed: 'Display'
		Smalltalk hasClassNamed: 'xMorph'
	Also handle a symbol as argument
		Smalltalk hasClassNamed: #Morph
		Smalltalk hasClassNamed: #Display
		Smalltalk hasClassNamed: #xMorph
	"

	Symbol hasInterned: aStringOrSymbol ifTrue: [ :aSymbol |
		self at: aSymbol ifPresent: [ :global | ^global class isMeta ]].
	^ false
]

{ #category : 'retrieving' }
SystemDictionary >> hierachySortedAllClassesDo: aBlock [
	"Evaluate the argument, aBlock, for each class in the system.
	Smalltalk hierachySortedAllClassesDo: [ :cls | cls print ]
	"

	| s |
	s := self allClasses asSet.
	ProtoObject
		allSubclassesWithLevelDo: [ :cls :l |
			(s includes: cls) ifTrue: [
				aBlock value: cls ]]
		startingLevel: 1
		sortByCategory: true
]

{ #category : 'query' }
SystemDictionary >> hierarchySorted: classes do: aBlock [
	"Sort by hierarchy. Among siblings, sort first by class category and (in same category) by name."
	| s |
	s := classes asSet.
	ProtoObject
		allSubclassesWithLevelDo: [ :cls :l |
			(s includes: cls) ifTrue: [
				aBlock value: cls ].
			(s includes: cls class) ifTrue: [
				aBlock value: cls class ].
			]
		startingLevel: 1
		sortByCategory: true
]

{ #category : 'image format' }
SystemDictionary >> imageFormatVersion [
	"Answer an integer identifying the type of image in memory. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required). This invokes an optional primitive
	that may not be available on all virtual machines.
	Answer nil if unknown."

	"
	Smalltalk imageFormatVersion
	"

	<primitive: 'primitiveImageFormatVersion'>

	"Cog provides a VM parameter"
	^self vmParameterAt: 41
]

{ #category : 'image format' }
SystemDictionary >> imageFormatVersionFromFile [
	"Answer an integer identifying the type of image on file. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required)"

	"
	Smalltalk imageFormatVersionFromFile
	"

	| format |
	format := self imageFormatVersionFromFileAsIs.
	^format <= 16r00FFFFFF
		ifTrue: [  "same endianness as VM"
			format ]
		ifFalse: [ "convert endianness"
			((format bitAnd: 16rFF000000) >> 24)
			+ ((format bitAnd: 16r00FF0000) >> 8)
			+ ((format bitAnd: 16r0000FF00) << 8)
			+ ((format bitAnd: 16r000000FF) << 16)]
]

{ #category : 'image format' }
SystemDictionary >> imageFormatVersionFromFileAsIs [
	"Answer an integer identifying the type of image on file. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required). If the image file has a different
	endianness than the VM, the format version will appear byte-swapped."
	"
	Smalltalk imageFormatVersionFromFileAsIs
	"
	^ self imageName asFullFileEntry readStreamDo: [ :stream |
		(stream binary; next: 4)
			uint32At: 1
			bigEndian: self isBigEndian ]
]

{ #category : 'image, changes name' }
SystemDictionary >> imageName [
	"Answer the full path name for the current image."
	"
	Smalltalk imageName
	"

	| answer |
	"Primitive answers an instance of String containing the UTF-8 bytes. If not pure ASCII,
	contents are invalid! Convert it ASAP into a reasonable ByteArray + UnicodeString."
	answer := self primImageName.
	(String isAscii: answer) ifFalse: [
		ByteArray adoptInstance: answer.
		answer := UnicodeString fromUtf8Bytes: answer ].

	"On some setups (Cog VM in Git Bash under Windows)
		Smalltalk primVmPath 
				answer begins with 'c:\' (lowercase)
	but 
		(FileDirectory on: '') primLookupEntryIn: '' index: 1
				answer is  #('C:' 0 0 true 0) (uppercase)
	Make Windows drive letters be capitalized!
	"
	(answer size > 1 and: [
		answer first isLowercase and: [
			answer second isDriveSeparator ]]) ifTrue: [
					answer := answer capitalized ].
	^answer
]

{ #category : 'image, changes name' }
SystemDictionary >> imagePath [
	"Answer the path for the directory containing the image file."
	"
	Smalltalk imagePath
	"
	| pathName |
	pathName := self imageName.
	^ pathName copyFrom: 1 to: pathName indexOfLastPathSeparator-1
]

{ #category : 'ui' }
SystemDictionary >> inspectGlobals [
	"
	Smalltalk  inspectGlobals
	"

	self globals inspectWithLabel: 'The Globals'
]

{ #category : 'memory space' }
SystemDictionary >> installLowSpaceWatcher [
	"Start a process to watch for low-space conditions."
	"
	Smalltalk installLowSpaceWatcher
	"

	[
		self stopLowSpaceWatcher.
		LowSpaceProcess := [self lowSpaceWatcher] newProcess.
		LowSpaceProcess priority: Processor lowIOPriority + 5.
		LowSpaceProcess name: '[system] Low Space Watcher'.
		LowSpaceProcess resume.
	] valueUnpreemptively

]

{ #category : 'sources, change log' }
SystemDictionary >> internalizeChangeLog     [
		"
		Smalltalk internalizeChangeLog
		"
	"Bring the changes file into a memory-resident filestream, for faster access and freedom from external file system.  1/31/96 sw"

	| reply aFile |
	reply := self confirm:  'CAUTION -- do not undertake this lightly!
If you have backed up your system and
are prepared to face the consequences of
the requested internalization of sources,
hit Yes.  If you have any doubts, hit No
to back out with no harm done.'.

	(reply ==  true) ifFalse: [
		^ self inform: 'Okay - abandoned'].

	(aFile := SourceFiles second) ifNil: [
		^ self error: 'Cannot locate Changes file so cannot proceed.'].
	SourceFiles at: 2 put: (ReadWriteStream with: aFile contentsOfEntireFile).

	self inform: 'Okay, changes file internalized'
]

{ #category : 'sources, change log' }
SystemDictionary >> internalizeSources [
		"
		Smalltalk internalizeSources
		"
	"Bring the sources and changes files into memory-resident filestreams, for faster access and freedom from file-system interface.  1/29/96 sw"

	| reply aFile |
	reply := self confirm:  'CAUTION -- do not undertake this lightly!
If you have backed up your system and
are prepared to face the consequences of
the requested internalization of sources,
hit Yes.  If you have any doubts, hit No
to back out with no harm done.'.

	(reply ==  true) ifFalse: [
		^ self inform: 'Okay - abandoned'].

	(aFile := SourceFiles first) ifNil: [
		^ self error: 'Cannot locate Sources file so cannot proceed.'].
	SourceFiles at: 1 put: (ReadWriteStream with: aFile contentsOfEntireFile).

	(aFile := SourceFiles last) ifNil: [
		^ self error: 'Cannot locate Changes so cannot proceed.'].
	SourceFiles at: 2 put: (ReadWriteStream with: aFile contentsOfEntireFile).

	self inform: 'Okay, sources internalized'
]

{ #category : 'miscellaneous' }
SystemDictionary >> interpreterClass [
	"Interpreter class (Cog VM only)
	nil for classic Interpreter VM
	"
	^self getSystemAttribute: 1007
]

{ #category : 'AndreasProfiler-profiling' }
SystemDictionary >> interruptChecksPerMSec [
"
Smalltalk interruptChecksPerMSec
"
	"Primitive. Answer the number of interrupt checks per milliseconds that we execute
	on this machine. This can be used to adjust the sub-msecs profiler to check (roughly) 
	n times per millisecond."
	<primitive: 'primitiveInterruptChecksPerMSec'>
	^self primitiveFailed
]

{ #category : 'sources, change log' }
SystemDictionary >> isBigEndian [
	^self endianness == #big
]

{ #category : 'miscellaneous' }
SystemDictionary >> isDevelopmentEnvironmentPresent [
	"Or we can't open a Smalltalk debugger"

	^ (self includesKey: #Debugger) and: [self isHeadless not]
]

{ #category : 'miscellaneous' }
SystemDictionary >> isHeadless [
	"Answer true if any of this VM options was specified in the commandline:
		-nodisplay
		-vm-display-null

	Smalltalk isHeadless
	"
	self vmOptionsDo: [ :vmOption :i |
		vmOption = '-vm-display-null' ifTrue: [ ^ true ].
		vmOption = '-nodisplay' ifTrue: [ ^ true ] ].
	^ false
]

{ #category : 'sources, change log' }
SystemDictionary >> isLittleEndian [
	^self endianness == #little
]

{ #category : 'testing' }
SystemDictionary >> isLiveSmalltalkImage [
	^ true
]

{ #category : 'testing' }
SystemDictionary >> isLiveTypingInstalled [
	
	^(FeatureRequirement name: #LiveTyping) isAlreadySatisfied
]

{ #category : 'miscellaneous' }
SystemDictionary >> isModuleAvailable: moduleNamePrefix [
	"
	Smalltalk isModuleAvailable: 'BitBltPlugin'
	Smalltalk isModuleAvailable: 'WrongName'
	"
	"Look for a matching builtin module (e.g., plugins). Builtin plugins are those that are compiled with the VM directly, as opposed to plugins residing in an external shared library."

	^ (self detectBuiltInModule: [ :n | n beginsWith: moduleNamePrefix ]) notNil
]

{ #category : 'startup - restore lost changes' }
SystemDictionary >> isQuitNoSaveRecord: chunk [

	^chunk beginsWith: self tagHeader, self quitNoSaveTag
	
]

{ #category : 'startup - restore lost changes' }
SystemDictionary >> isQuitRecord: chunk [

	^chunk beginsWith: self tagHeader, self quitTag 
]

{ #category : 'system attributes' }
SystemDictionary >> isRunningCog [
	"Returns true if we're running on a Cog VM (JIT or StackInterpreter)
	 (vmParameterAt: 42 is the number of stack pages)
	Smalltalk isRunningCog
	"

	^(self vmParameterAt: 42)
		ifNotNil: [ :stackPages | stackPages > 0 ]
		ifNil: [ false ]
]

{ #category : 'system attributes' }
SystemDictionary >> isRunningCogit [
	"Returns true if we're running on the Cog JIT
	 (vmParameterAt: 46 is the size of the machine code zone)
	Smalltalk isRunningCogit
	"

	^(self vmParameterAt: 46)
		ifNotNil: [ :machineCodeZoneSize  | machineCodeZoneSize > 0 ]
		ifNil: [ false ]
]

{ #category : 'startup - restore lost changes' }
SystemDictionary >> isSnapshotRecord: chunk [

	^chunk beginsWith: self tagHeader, self snapshotTag 
	
]

{ #category : 'retrieving' }
SystemDictionary >> isThereAReferenceTo: aLiteral [
	"Answer a Collection of all the methods that call on aLiteral."
	"
	Smalltalk isThereAReferenceTo: #open:label:
	"

	#(23 48 'fred' (new open:label:)) size.
"Example above should find #open:label:, though it is deeply embedded here."

	self allBehaviorsDo: [ :class |
		class selectorsAndMethodsDo: [ :sel :method |
			(method hasLiteral: aLiteral)
				ifTrue: [ ^true ]]].
	^ false
]

{ #category : 'retrieving' }
SystemDictionary >> isThereAnImplementorOf: aSelector   [
	"Answer true if there is at least one implementor of the selector found in the system, false if there are no implementors"

	self allBehaviorsDo:
		[:class |
			(class includesSelector: aSelector)
				ifTrue: [^ true]].
	^ false
"
Smalltalk isThereAnImplementorOf: #contents.
Smalltalk isThereAnImplementorOf: #nobodyImplementsThis.
"
]

{ #category : 'retrieving' }
SystemDictionary >> isThereAnImplementorOf: aSelector exceptFor: aCollectionOfBehaviors [
	"Answer whether if there is at least some other implementor of the selector besides aBehavior."

	self allBehaviorsDo: [ :class |
		((aCollectionOfBehaviors includes: class) not and: [class includesSelector: aSelector])
			ifTrue: [^ true]].
	^ false
"
Smalltalk isThereAnImplementorOf: #contents.
Smalltalk isThereAnImplementorOf: #nobodyImplementsThis.
"
]

{ #category : 'code authors' }
SystemDictionary >> knownInitialsAndNames [
	"This list could include people who hasn't contributed code to the Cuis image, but to some optional package."
"
| all ok |
all := Smalltalk allContributors asSet.
ok := (Smalltalk knownInitialsAndNames collect: [ :pair | pair first ]) asSet.
(all copyWithoutAll: ok) print

		initials         				name"
^ #(
	#('ab' 						'Alexandre Bergel')
	#('abc' 						'Colin Putney')
	#('acg' 						'Andrew C. Greenberg')
	#('ads' 						'Adam Spitz')
	#('AFi' 						'Alain Fischer')
	#('ajh' 						'Anthony Hannan')
	#('al' 						'Adrian Lienhard')
	#('and' 						'Alan Dao')
	#('aoy' 						'Andres Otaduy')
	#('apb' 						'Andrew P. Black')
	#('ar' 						'Andreas Raab')
	#('asm' 						'Alejandro Magistrello')
	#('avi' 						'Avi Bryant')
	#('AW'						'Alan Wharton')
	#('AY'						'Angel Yan')
	#('BAP'						'Barry Perryman')
	#('BB'						'Brenda Barletta')
	#('BenComan' 				'Ben Coman')
	#('bf' 						'Bert Freudenberg')
	#('BG' 						'Boris Gaertner')
	#('bgs' 						'Boris G. Shingarov')
	#('BJP' 						'Bijan Parsia')
	#('bkv' 						'Brent Vukmer')
	#('bolot' 					'Bolot Kerimbaev')
	#('bp' 						'Bernhard Pieber')
	#('BP' 						'Brent Pinkney') 
	#('brp' 						'Brent Pinkney')
	#('CAP' 						'Cristián Abel Pérez')
	#('cbc' 						'Chris Cunningham')
	#('cbr'						'Casey Ransberger')
	#('ccn' 						'Chris Norton')
	#('chaider'					'Christian Haider')
	#('cmm' 					'Chris Muller')
	#('crl' 						'Craig Latta')
	#('cwp' 						'Colin Putney')
	#('das' 						'David A Smith')
	#('db' 						'Douglas Brebner')
	#('dc' 						'Damien Cassou')
	#('dew' 						'Doug Way')
	#('DF'						'David Faitelson')
	#('dgd' 						'Diego Gomez Deck')
	#('dhn'	 					'Dan Norton')
	#('dhhi' 						'Dan Ingalls')
	#('di' 						'Dan Ingalls')
	#('djp' 						'David J. Pennell')
	#('dkh'						'Dale Henrichs')
	#('DKL'						'Daniel K Lyons')
	#('DM' 						'Duncan Mak')
	#('DSG'						'David Graham')
	#('DSM' 						'Duane Maxwell')
	#('dtl' 						'Dave Lewis')
	#('dvf'	 					'Daniel Vainsencher')
	#('eat' 						'Eric Arseneau Tremblay')
	#('EB'						'Eric Brandwein')
	#('eem'						'Eliot Emilio Miranda')
	#('eliot'						'Eliot Emilio Miranda')
	#('efc' 						'Eddie Cottongim')
	#('em' 						'Ernest Micklei?')
	#('emm' 					'Ernest Micklei')
	#('ER'						'Ezequiel Ramis')
	#('Ez3' 						'Ezequiel Birman')
	#('fbs' 						'Frank Shearar')
	#('FBS' 						'Frank Shearar')
	#('fc' 						'Frank Caggiano')
	#('fcs' 						'Frank Sergeant')
	#('FernandoOlivero' 			'Fernando Olivero')
	#('FernanodOlivero' 			'Fernando Olivero')
	#('FGJ'						'Fernando Gasperi Jabalera')
	#('fgz'						'Felipe Zak')
	#('FJG'				 		'Facundo Javier Gelatti')
	#('GabrielOmarCotelli' 		'Gabriel Omar Cotelli')
	#('GC' 						'Gastón Caruso')
	#('gera' 						'Gerardo Richarte')
	#('gh' 						'Goran Krampe (nee Hultgren)')
	#('gk' 						'Goran Krampe (nee Hultgren)')
	#('gm' 						'German Morales')
	#('go' 						'Georg Gollmann')
	#('gsa' 						'German Arduino')
	#('HAW' 					'Hernan Wilkinson')
	#('HB' 						'Hari Balaraman')
	#('hjh' 						'Hannes Hirzel')
	#('hlsf' 						'Hilaire Fernandes')
	#('hmm' 					'Hans-Martin Mosner')
	#('hsj' 						'Henrik Sperre Johansen')
	#('Igor.Stasenko' 			'Igor Stasenko')
	#('ikp' 						'Ian Piumarta')
	#('jar' 						'Jaromir Matas')
	#('Jb' 						'Jean Baptiste Arnaud')
	#('JC' 						'Juan Cabrera')
	#('jcg' 						'Joshua Gargus')
	#('JCM'						'Juan Cruz Montero')
	#('jdr' 						'Javier Diaz-Reinoso')
	#('je' 						'Joern Eyrich')
	#('JEC'						'Joel Esteban Cámera')
	#('JEP'						'Javier Pimás')
	#('jf' 						'Julian Fitzell')
	#('JF' 						'Julian Fitzell')
	#('jfr' 						'Jon Raiford')
	#('jhm' 						'John Maloney')
	#('jk'						'Jonathan Kelly')
	#('jlb' 						'Jim Benson')
	#('jm' 						'John Maloney')
	#('jmb' 						'Hans Baveco')
	#('JMG'						'Jeff Gonis')
	#('JMM' 						'John McIntosh')
	#('jmv' 						'Juan Vuletich')
	#('JMV' 						'Juan Vuletich')
	#('JO'						'Javier Olaechea')
	#('jp' 						'Joseph Pelrine')
	#('jpb' 						'Josef Philip Bernhart')
	#('jrm' 						'John-Reed Maffeo')
	#('jrp' 						'John Pierce')
	#('jsp' 						'Jeff Pierce')
	#('kdpm' 					'Kevin Pérez Machado')
	#('KenD' 					'Ken Dickey')
	#('kfr' 						'Karl Ramberg')
	#('KLC'			 			'Ken Causey')
	#('KLG'			 			'Gerald Klix')
	#('kph'						'Keith Hodges')
	#('KTT' 						'Kurt Thams')
	#('laza' 						'Alexander Lazarevic')
	#('LC' 						'Leandro Caniglia')
	#('L C' 						'Lisandro Córdoba')
	#('len' 						'Luciano Esteban Notarfrancesco')
	#('LP' 						'Luis Peña')
	#('lpc'						'Laura Perez Cerrato')
	#('LR'						'Luci Ruz')
	#('lr' 						'Lukas Renggli')
	#('Lukas Renggli' 			'Lukas Renggli')
	#('ls' 						'Lex Spoon')
	#('md' 						'Marcus Denker')
	#('MarcusDenker' 			'Marcus Denker')
	#('marcus.denker' 			'Marcus Denker')
	#('mdr' 						'Mike Rutenberg')
	#('MEGL' 					'MarceloLamperti')
	#('mga' 						'Markus Galli')
	#('MGD'						'Matías Dinota')
	#('MH'						'Matías Halperin')
	#('mha' 						'Michael Haupt')
	#('mir' 						'Michael Rueger')
	#('mjg' 						'Mark Guzdial')
	#('mk'	 					'Matej Kosik')
	#('ml' 						'Matías Lang')
	#('MM'	 					'Mariano Montone')
	#('mnqpr'	 				'Szabolcs Komaromi')
	#('MPH'	 					'Michael Hewner')
	#('mpw' 					'Marcel Weiher')
	#('MPW' 					'Marcel Weiher')
	#('mrm' 					'Martin McClure')
	#('mtf' 						'Matthew Fulmer')
	#('mu' 						'Masashi Umezawa')
	#('nb' 						'Naala Brewer')
	#('nh'						'Nicolás Halperin')
	#('nice'					 	'Nicolas Cellier')
	#('nk' 						'Ned Konz')
	#('NM' 						'Nicola Mingotti')
	#('NMS'						'Nicolás Sarfati')
	#('nop' 						'Jay Carlson')
	#('NPM'						'Nicolás Papagna Maldonado')
	#('NS' 						'Nathanael Schaerli')
	#('ovv' 						'Octavio Valentín Vives')
	#('panda' 					'Michael Rueger')
	#('pau'						'Paula Perez Bianchi')
	#('pb'						'Phil Bellalouna')
	#('PB'						'Paolo Belforte')
	#('PHK' 						'Peter Keeler')
	#('pmon'					'Paolo Montrasi')
	#('PM' 						'Peter Mikkelsen')
	#('Pmm' 					'Philippe Marschall')
	#('pnm' 						'Paul McDonough')
	#('r++' 						'Gerardo Richarte')
	#('ra' 						'Robert Aurednik')
	#('raa' 						'Bob Arning')
	#('RAA' 						'Bob Arning')
	#('raok' 						'Richard A. O''Keefe')
	#('rca' 						'Russell Allen')
	#('reThink'				 	'Paul McDonough')
	#('rew' 						'Roger Whitney')
	#('rhi' 						'Robert Hirschfeld')
	#('RJT' 						'Ron Teitelbaum')
	#('RMV' 						'R. Mark Volkmann')
	#('RNG' 						'Nahuel Garbezza')
	#('rr' 						'Romain Robbes')
	#('rs'						'Ramiro Saravia')
	#('rss' 						'Ron Spengler')
	#('rw' 						'Robert Withers')
	#('rww' 						'Robert Withers')
	#('Sames' 					'Samuel S. Shuster')
	#('sbw' 						'Stephan B. Wessels')
	#('sd' 						'Stephane Ducasse')
	#('SD' 						'Stephane Ducasse')
	#('sge' 						'Steve Elkins')
	#('sjd' 						'Santiago Jose Dandois')
	#('SLD'						'Steve Davies')
	#('sma' 						'Stefan Matthias Aust')
	#('sps' 						'Steven Swerling')
	#('SqR' 						'Andres Valloud')
	#('sqr' 						'Andres Valloud')
	#('Squeak1.0'				'Squeak 1.0, September 20, 1996')
	#('Squeak1.1'				'Squeak 1.1, September 23, 1996')
	#('Squeak1.2'				'Squeak 1.2, June 29, 1997')
	#('Squeak1.3'				'Squeak 1.3, January 16, 1998')
	#('sr' 						'Stephan Rudlof')
	#('ss'						'Sebastian Sujarchuk')
	#('SSC' 						'Santiago Salvador Cubino')
	#('SSS' 						'Samuel S. Shuster')
	#('ST-80'					'Smalltalk-80, version 2, of April 1, 1983')
	#('stephane.ducasse' 		'Stephane Ducasse')
	#('stephaneducasse'	 		'Stephane Ducasse')
	#('stp' 						'Stephen Travis Pope')
	#('sumim' 					'Masato Sumi')
	#('S.V.' 						'Sebastián M. Vintoñuke')
	#('svc' 						'Sven Van Caekenberghe')
	#('svp' 						'Stephen Vincent Pair')
	#('sw' 						'Scott Wallace')
	#('TAG' 						'Travis Griggs')
	#('tak' 						'Takashi Yamamiya')
	#('tao' 						'Tim Olson')
	#('TBn' 						'Torsten Bergmann')
	#('tfei' 						'The Fourth Estate, Inc.')
	#('tfel' 						'Tim Felgentreff')
	#('th' 						'Torge Husfeldt')
	#('tk' 						'Ted Kaehler')
	#('tlk' 						'Tom Koenig')
	#('tpr' 						'Tim Rowledge')
	#('TPR' 						'Tim Rowledge')
	#('TS' 						'Tomás Spognardi')
	#('tsl' 						'Thiago da Silva Lino')
	#('tween' 					'Andy Tween')
	#('ul' 						'Levente Uzonyi')
	#('vb' 						'Vassili Bykov')
	#('VGF' 						'Gabriel Fierro')
	#('ward' 					'Ward Cunningham')
	#('wbsl'						'Wéslleymberg Lisboa')
	#('wiz' 						'Jerome Peace')
	#('wod' 						'Bill Dargel')
	#('yo' 						'Yoshiki Ohshima')
	#('zz' 						'Serge Stinckwich'))
]

{ #category : 'startup' }
SystemDictionary >> lastCommandLineArguments [
	"To make all command line arguments after end of options '--' available to scripts applications"

	^startUpArguments copyAfter: '--'.
]

{ #category : 'snapshot and quit' }
SystemDictionary >> lastQuitLogPosition [
	^ LastQuitLogPosition
]

{ #category : 'sources, change log' }
SystemDictionary >> lastUpdateString [
	"Smalltalk lastUpdateString"
	^'latest update: #', SystemVersion current highestUpdate printString
]

{ #category : 'miscelaneous' }
SystemDictionary >> listBuiltinModule: index [
	"Return the name of the n-th builtin module.
	This list is not sorted!"
	<primitive: 572>
	^self primitiveFailed
]

{ #category : 'miscellaneous' }
SystemDictionary >> listBuiltinModules [
	"Smalltalk listBuiltinModules"
	"Return a list of all builtin modules (e.g., plugins). Builtin plugins are those that are compiled with the VM directly, as opposed to plugins residing in an external shared library. The list will include all builtin plugins regardless of whether they are currently loaded or not. Note that the list returned is not sorted!"
	| modules index name |
	modules := WriteStream on: #().
	index := 1.
	[true] whileTrue:[
		name := self listBuiltinModule: index.
		name ifNil:[^modules contents].
		modules nextPut: name.
		index := index + 1.
	].
]

{ #category : 'miscelaneous' }
SystemDictionary >> listLoadedModule: index [
	"Return the name of the n-th loaded module.
	This list is not sorted!"
	<primitive: 573>
	^self primitiveFailed
]

{ #category : 'miscellaneous' }
SystemDictionary >> listLoadedModules [
	"Smalltalk listLoadedModules"
	"Return a list of all currently loaded modules (e.g., plugins). Loaded modules are those that currently in use (e.g., active). The list returned will contain all currently active modules regardless of whether they're builtin (that is compiled with the VM) or external (e.g., residing in some external shared library). Note that the returned list is not sorted!"
	| modules index name |
	modules := WriteStream on: #().
	index := 1.
	[true] whileTrue:[
		name := self listLoadedModule: index.
		name ifNil:[^modules contents].
		modules nextPut: name.
		index := index + 1.
	].
]

{ #category : 'sources, change log' }
SystemDictionary >> logChange: aStringOrText  [
	"Write the argument, aString, onto the changes file."
	^ self logChange: aStringOrText preamble: nil
]

{ #category : 'sources, change log' }
SystemDictionary >> logChange: aStringOrText preamble: preambleOrNil [
	"Write the argument, aString, onto the changes file."
	| aString changesFile |
	aString := aStringOrText asPlainString.
	aString firstNonSeparator = 0 ifTrue: [^ self].  "null doits confuse replay"
	
	(SourceFiles notNil and: [(changesFile := SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			preambleOrNil ifNotNil: [
				changesFile nextPut: $!; nextChunkPut: preambleOrNil; newLine ].
			changesFile nextChunkPut: aString.
			self forceChangesToDisk ]].
	self logToUserChanges: aString preamble: preambleOrNil
]

{ #category : 'miscellaneous' }
SystemDictionary >> logError: errMsg inContext: aContext to: baseFilename [
	"Log the error message and a stack trace to the given file.
	Smalltalk logError: 'test error message' inContext: thisContext to: 'testErr'
	"

	| localFilename file |
	localFilename := (Preferences at: #debugLogTimestamp)
		ifTrue: [ baseFilename, '-',
				(String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ]),
				'.log' ]
		ifFalse: [ baseFilename, '.log' ].
	file := DirectoryEntry logsDirectory // localFilename.
	[
		file forceWriteStreamDo: [ :stream |
	 	 	stream nextPutAll: errMsg; newLine.
			aContext errorReportOn: stream ]
	] on: UnhandledError do: [ :ex | ex return]. "avoid recursive errors"
	[
 	 	StdIOWriteStream stdout newLine; nextPutAll: errMsg.
		StdIOWriteStream stdout newLine; nextPutAll: 'See '; nextPutAll: file pathName.
		StdIOWriteStream stdout newLine.
		aContext shortErrorReportOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout flush
	] on: UnhandledError do: [ :ex | ex return]. "avoid recursive errors"
]

{ #category : 'snapshot and quit' }
SystemDictionary >> logSnapshot: save andQuit: quit [
	"Log quitting to changes file"
	| msg |
	msg := self snapshotMessageFor: save andQuit: quit.
	(SourceFiles at: 2) ifNotNil: [
		save ifTrue: [
			LastQuitLogPosition := (SourceFiles at: 2)
				 setToEnd;
				 position ]].
	self logChange: msg.
	Transcript
		 newLine;
		 show: msg;
		 newLine
]

{ #category : 'startup' }
SystemDictionary >> logStartupDebugAid: stuff [
	"
	Transcript
		showOnDisplay: false;
		logToFile: true;
		logToStdout: true.
	Smalltalk at: #LogStartupDebugAids put: true.
	Smalltalk logStartupDebugAid: 'stuff'
	"
	self logStartupDebugAids ifTrue: [
		self logStartupNotice: stuff ].
]

{ #category : 'startup' }
SystemDictionary >> logStartupDebugAids [
	"
	Smalltalk logStartupDebugAids
	"
	^self at: #LogStartupDebugAids ifAbsent: false
]

{ #category : 'startup' }
SystemDictionary >> logStartupError: stuff [
	"
	Smalltalk logStartupError: 'stuff'
	"
	[
		Transcript
			logToFile: true;
			logToStdout: true.
		stuff print.
		(Delay forSeconds: 0.2) wait.
		Warning signal: stuff.
	] forkAt: Processor activePriority - 1
]

{ #category : 'startup' }
SystemDictionary >> logStartupNotice: stuff [
	"
	Smalltalk logStartupNotice: 'stuff'
	"
	[
		Transcript
			logToFile: true;
			logToStdout: true.
		stuff print.
	] forkAt: Processor activePriority - 1
]

{ #category : 'sources, change log' }
SystemDictionary >> logToUserChanges: aString preamble: preambleOrNil [

	self userChangesAppendDo: [ :stream |
		stream newLine; newLine.
		preambleOrNil ifNotNil: [
			stream nextPut: $!; nextChunkPut: preambleOrNil; newLine ].
		stream nextChunkPut: aString.
		stream flush ].
]

{ #category : 'AndreasProfiler-profiling' }
SystemDictionary >> longRunningPrimitive [
	"Primitive. Answer an Array of the current long-running primitive method identified by the
	 heartbeat, and the number of heartbeats it ran for, or nil if none.  To use this, first install a
	 semaphore to wait upon via SystemDictionary>>longRunningPrimitiveSemaphore:"
	<primitive: 'primitiveLongRunningPrimitive'>
	^self primitiveFailed
]

{ #category : 'AndreasProfiler-profiling' }
SystemDictionary >> longRunningPrimitiveSemaphore: aSemaphoreOrNil [
	"Primitive. Install the semaphore to be used for collecting long-running primitives, 
	 or nil if no semaphore should be used.  The semaphore will be signalled once the
	 heartbeat detects a long-running primitive until the result is collected via
	 primitiveLongRunningPrimitive (see SystemDictionary>>longRunningPrimitive)"
	<primitive: 'primitiveLongRunningPrimitiveSemaphore'>
	^self primitiveFailed
]

{ #category : 'startup - restore lost changes' }
SystemDictionary >> lostChangesDetectedCaption [
	
	^
'Last changes may have been lost
(maybe the VM crashed or you had to kill it)
(or maybe Cuis is already running)
What do you want to do?'
]

{ #category : 'memory space' }
SystemDictionary >> lowSpaceThreshold  [
	"Answer the low space threshold. When the amount of free memory (after garbage collection)
	 falls below this limit, the system is in serious danger of completely exhausting memory and
	 crashing. This limit should be made high enough to allow the user open a debugger to diagnose
	 a problem or to save the image.  In a stack-based VM such as Cog contexts for activations in
	 the stack zone will have to be created as the debugger opens, requiring additional headroom."

	| slotsForDebugger slotsForContextsOnStackPages |
	slotsForDebugger := 65536. "Arbitrary guess"
	slotsForContextsOnStackPages :=
		(self vmParameterAt: 42)
			ifNil: [0]
			ifNotNil:
				[:numStackPages| | headerSize numActivationsPerPage maxContextSize |
				numActivationsPerPage := 40. "Design goal of the Cog VM"
				headerSize := 2. "64-bytes for Spur"
				maxContextSize := MethodContext instSize + CompiledMethod fullFrameSize + headerSize.
				numStackPages * numActivationsPerPage * maxContextSize].
	^slotsForDebugger + slotsForContextsOnStackPages * self wordSize
]

{ #category : 'memory space' }
SystemDictionary >> lowSpaceWatcher [
	"Wait until the low space semaphore is signalled, then take appropriate actions."

	| p |
	self garbageCollectMost <= self lowSpaceThreshold ifTrue: [
		self garbageCollect <= self lowSpaceThreshold ifTrue: [
			"free space must be above threshold before starting low space watcher"
			^ self primitiveBeep ]].

	LowSpaceSemaphore := Semaphore new.
	self primLowSpaceSemaphore: LowSpaceSemaphore.
	self primSignalAtBytesLeft: self lowSpaceThreshold.  "enable low space interrupts"

	LowSpaceSemaphore wait.  "wait for a low space condition..."

	self primSignalAtBytesLeft: 0.  "disable low space interrupts"
	self primLowSpaceSemaphore: nil.
	LowSpaceProcess := nil.
	"Note: user now unprotected until the low space watcher is re-installed"

	p := (self specialObjectsArray at: 23) ifNil: [
		"In case the LowSpaceSemaphore is signaled not by the VM but by someone else."
		Processor preemptedProcess ].
	self specialObjectsArray at: 23 put: nil.
	p isTerminated ifFalse: [
		p suspend.
		Debugger openProcess: p context: p suspendedContext label: 'Space is low' ]
]

{ #category : 'memory space' }
SystemDictionary >> lowSpaceWatcherProcess [
	^LowSpaceProcess
]

{ #category : 'housekeeping' }
SystemDictionary >> macroBenchmark1    "Smalltalk macroBenchmark1" [
	"Decompiles and prettyPrints the source for every method in the system (or less depending on the *FILTER*, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same.  Because it never installs the new method, it should not cause any flusing of the method cache."
	 | methodNode oldMethod newMethod badOnes oldCodeString n classes |
	classes := self allClasses select: [:c | c name < 'B3'].
	badOnes := OrderedCollection new.
'Decompiling and recompiling...'
displayProgressAt: Sensor mousePoint
from: 0 to: (classes sum: [:c | c selectors size] ifEmpty: [0])
during: [:barBlock | n := 0.
	classes do:
		[:cls | 
		"Transcript cr; show: cls name."
		cls selectors do:
			[:selector | barBlock value: (n := n+1).
			oldMethod := cls compiledMethodAt: selector.
			oldCodeString := (cls decompilerClass new
								decompile: selector in: cls method: oldMethod)
							decompileString.
			methodNode := cls compilerClass new
						compile: oldCodeString
						in: cls notifying: nil ifFail: nil.
			newMethod := methodNode generate: #(0 0 0 0).
			oldCodeString = (cls decompilerClass new
								decompile: selector in: cls method: newMethod)
							decompileString ifFalse: [badOnes add: cls name , ' ' , selector]]].
].
	^ badOnes size
]

{ #category : 'housekeeping' }
SystemDictionary >> macroBenchmark3   "Smalltalk macroBenchmark3" [
	| testBlock tallies prev receiver |
	"Runs the stepping simulator with the messageTally tree (like tallySends)."
	testBlock :=
		['Running the context step simulator'
			displayProgressAt: Sensor mousePoint
			from: 0 to: 200
			during:
				[:barBlock |
				1 to: 200 do:
				[:x | barBlock value: x.
				Float pi printString.
				15 factorial printString]]].
	tallies := MessageTally new class: testBlock receiver class
							method: testBlock method.
	receiver := nil.
	prev := testBlock.
	thisContext sender
		runSimulated: testBlock
		contextAtEachStep:
			[:current |
			current == prev ifFalse: [
				"call or return"
				prev sender ifNotNil: [
					"call only"
					(receiver == nil or: [current receiver == receiver])
						ifTrue: [tallies tally: current by: 1]].
				prev := current]].

]

{ #category : 'miscellaneous' }
SystemDictionary >> maxExternalSemaphores [
	"The size of table where external semaphores are registered. Only in Cog.
	nil elsewhere"

	^self vmParameterAt: 49
]

{ #category : 'miscellaneous' }
SystemDictionary >> maxExternalSemaphores: aSize [
	"Changes the size of table where external semaphores are registered. 
	The size can only grow, and will always be the next power of two larger than the parameter.
	
	Setting this at any time other than start-up can potentially lose requests.
	 i.e. during the realloc new storage is allocated, t
	he old contents are copied and then pointers are switched. 
	 Requests occurring during copying won't be seen if they occur to indices already copied. 
	The intended use is to set the table to some adequate maximum at start-up"
	
	self isRunningCog ifFalse: [^0].
	"The vm-header field is a short, maximum 64k entries. Well, on most platforms anyways "
	(aSize < 0 or: [aSize > 16rFFFF]) ifTrue: [^Error signal: 'Must be in the range (0 to: 16rFFFF)'].
	^self vmParameterAt: 49 put: aSize
]

{ #category : 'system attributes' }
SystemDictionary >> maxIdentityHash [
	"Answer the maximum identityHash value supported by the VM.
	Usually the VM is able to answer. It seems that SqueakJS (as of October 5, 2020) isn't.
	On primitive failure, still give a correct answer."

	<primitive: 176>	
	^ 16r3FFFFF							"22 bits in Spur"
]

{ #category : 'sources, change log' }
SystemDictionary >> methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass [

	aClass acceptsLoggingOfCompilation 
		ifTrue: [ self 
			logChange: aMethod sourceCode 
			preamble: 'methodRemoval: ', aClass name, ' ', aSymbol storeString, Utilities changeStampField ]. 

]

{ #category : 'query' }
SystemDictionary >> methodsWithUnboundGlobals [
	"Get all methods that use undeclared global objects that are not listed in Undeclared. For a clean image the result should be empty."
	"
	self assert: Smalltalk methodsWithUnboundGlobals isEmpty
	"
	^ self allSelect: [ :m |
		m literals anySatisfy: [ :l |
			l isVariableBinding and: [
				l key isSymbol and: [
					"avoid class-side methodClass literals"
					(m methodClass bindingOf: l key)
						ifNil: [
							(Undeclared
								associationAt: l key
								ifAbsent: nil) ~~ l ]
						ifNotNil: [ :b |
							b ~~ l ]]]]]
]

{ #category : 'accessing' }
SystemDictionary >> name [
	^ 'Image'
]

{ #category : 'classes and traits' }
SystemDictionary >> nonClassNames [
	"Answer a sorted collection of all non-class names. Use the return value of #fillCaches to avoid concurrency issues."
	
	^cachedNonClassNames ifNil: [ self fillCaches at: 2 ]
]

{ #category : 'snapshot and quit' }
SystemDictionary >> nopTag [

	^ 'NOP'
]

{ #category : 'retrieving' }
SystemDictionary >> numberOfImplementorsOf: aSelector   [
	"Answer a count of the implementors of the given selector found in the system"

	| aCount |
	aCount := 0.
	self allBehaviorsDo:
		[:class |
			(class includesSelector: aSelector)
				ifTrue: [aCount := aCount + 1]].
	^ aCount
"
Smalltalk numberOfImplementorsOf: #contents.
Smalltalk numberOfImplementorsOf: #nobodyImplementsThis. 
Smalltalk numberOfimplementorsOf: #numberOfImplementorsOf:.
"
]

{ #category : 'retrieving' }
SystemDictionary >> numberOfSendersOf: aSymbol [
	"Answer the count of all the methods that call on aLiteral.
	Note: For some selectors, like #==, this method will give an incorrect answer, much larger than the correct one.
	The correct answer would require calling the more expensive #hasLiteralExplicitly: method.
	[ (Smalltalk numberOfSendersOf: #open:label:) ] timeToRun
	[ (Smalltalk numberOfSendersOf: #==) ] timeToRun
	"
	| count |
	count := 0.
	self allBehaviorsDo: [ :class |
		class selectorsAndMethodsDo: [ :sel :method |
			"
			(method hasLiteralExplicitly: aSymbol)
			"
			(method hasLiteral: aSymbol)
				ifTrue: [ count := count + 1 ]]].
	^ count
]

{ #category : 'vm parameters' }
SystemDictionary >> numericPrimsMixArithmetic [
	"If true, then primitives will handle the conversions:
	Float (Small or Boxed) arithmeticOp: SmallInteger
	Else, the primitive fail in case of mixed arithmetic, and conversion will be performed at image side.
	See #numericPrimsMixArithmetic:
	See #numericPrimsMixComparison
	
	Note:
		OpenSmalltalk VMs after November, 2025 (single flag both for arithmetic and comparison since March, 2019) can set the option and will honor it.
		This is just a performance optimization, behavior is the same in any case.

		{Smalltalk numericPrimsMixArithmetic. Smalltalk numericPrimsMixComparison}.
		[1.0 + 1] bench.
			'112.48 M runs per second. 8.89 n seconds per run. 0.00  % GC time.' .

		Smalltalk numericPrimsMixArithmetic: false.
		[1.0 + 1] bench.
			'26.99 M runs per second. 37.05 n seconds per run. 0.00  % GC time.' .
			'28.79 M runs per second. 34.74 n seconds per run. 0.00  % GC time.' .

		Smalltalk numericPrimsMixArithmetic: true.
		[1.0 + 1] bench.
			'120.94 M runs per second. 8.27 n seconds per run. 0.00  % GC time.' .
			'111.51 M runs per second. 8.97 n seconds per run. 0.00  % GC time.' .
	"

	^ (self vmParameterAt: 48) allMask: 64
]

{ #category : 'vm parameters' }
SystemDictionary >> numericPrimsMixArithmetic: aBoolean [
	"If set to true, then primitives will handle the conversions:
	SmallInteger arithmeticOp: Float (Small or Boxed)
	Else, the primitive fail in case of mixed arithmetic, and conversion will be performed at image side.

	Please see  comment at #numericPrimsMixArithmetic"

	"Ignore request if VM doesn't support it"
	[
		self vmParameterAt: 48 put: ((self vmParameterAt: 48) bitClear: 64) + (aBoolean ifTrue: [64] ifFalse: [0]).
	] on: Error do: [].
]

{ #category : 'vm parameters' }
SystemDictionary >> numericPrimsMixComparison [
	"If true, then primitives will handle the conversions:
	Float (Small or Boxed) compareOp: SmallInteger
	Else, the primitive fail in case of mixed comparison, and conversion will be performed at image side.
	See #numericPrimsMixComparison:
	See #numericPrimsMixArithmetic
	
	Note:
		OpenSmalltalk VMs after November, 2025 (single flag both for arithmetic and comparison since March, 2019) can set the option and will honor it. The comparison operation converts the Float to Integer (and not the Integer to Float). This means that some rather big SmallIntegers in 64 bit systems, that can not be represented exactly as a Float will not be equal to any Float. Squeak adopted this critera. Cuis follows the more conventional, Smalltalk-80 tradition to always convert to Float if any operand is Float. Therefore Cuis needs to do 'Smalltalk numericPrimsMixComparison: false'.
		Previous VMs can not set the option, and will answer true when queried. But these VMs did the conversion to Float, and the requested operation in Floats. So, with these VMs, Cuis will also have the desired behavior.
		
		{Smalltalk numericPrimsMixArithmetic. Smalltalk numericPrimsMixComparison}.
		SmallInteger maxVal asFloat = SmallInteger maxVal.

		Smalltalk numericPrimsMixComparison: true.
		SmallInteger maxVal asFloat = SmallInteger maxVal.
		[SmallInteger maxVal asFloat = SmallInteger maxVal] bench.
			'67.76 M runs per second. 14.76 n seconds per run. 0.00  % GC time.' .
			'68.53 M runs per second. 14.59 n seconds per run. 0.00  % GC time.' .

		Smalltalk numericPrimsMixComparison: false.
		SmallInteger maxVal asFloat = SmallInteger maxVal.
		[SmallInteger maxVal asFloat = SmallInteger maxVal] bench.
			'37.27 M runs per second. 26.83 n seconds per run. 0.00  % GC time.' .
			'37.81 M runs per second. 26.45 n seconds per run. 0.00  % GC time.' .
		"

	^ (self vmParameterAt: 48) allMask: 512
]

{ #category : 'vm parameters' }
SystemDictionary >> numericPrimsMixComparison: aBoolean [
	"If set to true, then primitives will handle the conversions:
	SmallInteger compareOp: Float (Small or Boxed)
	Else, the primitive fail in case of mixed comparison, and conversion will be performed at image side.

	Please see  comment at #numericPrimsMixComparison"

	"Ignore request if VM doesn't support it"
	[
		self vmParameterAt: 48 put: ((self vmParameterAt: 48) bitClear: 512) + (aBoolean ifTrue: [512] ifFalse: [0]).
	] on: Error do: [].
]

{ #category : 'object serialization' }
SystemDictionary >> objectForDataStream: refStrm [
	| dp |
	"I am about to be written on an object file.  Write a reference to Smalltalk instead."

	dp := DiskProxy global: #Smalltalk selector: #yourself
			args: #().
	refStrm replace: self with: dp.
	^ dp
]

{ #category : 'housekeeping' }
SystemDictionary >> obsoleteBehaviors [
	"
	Smalltalk obsoleteBehaviors inspect
	Find all obsolete behaviors including meta classes
	"
	| obs |
	obs := OrderedCollection new.
	self garbageCollect.
	self allObjectsDo: [ :cl |
		(cl isBehavior and: [cl isObsolete]) ifTrue: [obs add: cl]].
	^ obs asArray
]

{ #category : 'housekeeping' }
SystemDictionary >> obsoleteClasses   "Smalltalk obsoleteClasses inspect" [
	"NOTE:  Also try inspecting comments below"
	| obs c |
	obs := OrderedCollection new.  
	self garbageCollect.
	Metaclass allInstancesDo:
		[:m | c := m soleInstance.
		(c notNil and: ['AnOb*' match: c name asPlainString])
			ifTrue: [obs add: c]].
	^ obs asArray

"Likely in a ClassDict or Pool...
(Association allInstances select: [:a | (a value isKindOf: Class) and: ['AnOb*' match: a value name]]) asArray
"
"Obsolete class refs or super pointer in last lit of a method...
| n l found |
Smalltalk browseAllSelect:
	[:m | found := false.
	1 to: m numLiterals do:
		[:i | (((l := m literalAt: i) isMemberOf: Association)
				and: [(l value isKindOf: Behavior)
				and: ['AnOb*' match: l value name]])
			ifTrue: [found := true]].
	found]
"
]

{ #category : 'housekeeping' }
SystemDictionary >> obsoleteMethodReferences [
	"
	Smalltalk obsoleteMethodReferences
	Smalltalk browseObsoleteMethodReferences
	Open a browser on all referenced behaviors that are obsolete"

	| obsClasses obsRefs |
	^Array streamContents: [ :references |
		obsClasses := self obsoleteBehaviors.
		'Scanning for methods referencing obsolete classes' displayProgressAt: Sensor mousePoint
			from: 1 to: obsClasses size during: [ :barBlock |
		obsClasses keysAndValuesDo: [ :index :each |
			barBlock value: index.
			obsRefs := self pointersTo: each except: obsClasses.
			obsRefs do: [ :ref |
				"Figure out if it may be a global"
				(ref isVariableBinding and: [ ref key isString "or Symbol" ]) ifTrue: [
					(self pointersTo: ref) do: [ :meth |
						(meth is: #CompiledMethod) ifTrue: [
							meth methodReference ifNotNil: [ :mref |
								(mref isValid and: [ mref compiledMethod == meth]) ifTrue: [
									references nextPut: mref ]]]]]]]]].
]

{ #category : 'memory space' }
SystemDictionary >> okayToProceedEvenIfSpaceIsLow [
	"Return true if either there is enough memory to do so safely or if the user gives permission after being given fair warning."

	self garbageCollectMost > self lowSpaceThreshold ifTrue: [^ true].  "quick"
	self garbageCollect > self lowSpaceThreshold ifTrue: [^ true].  "work harder"

	^ self confirm:
'WARNING: There is not enough space to start the low space watcher.
If you proceed, you will not be warned again, and the system may
run out of memory and crash. If you do proceed, you can start the
low space notifier when more space becomes available simply by
opening and then closing a debugger (e.g., by hitting Cmd-period.)
Do you want to proceed?'

]

{ #category : 'sources, change log' }
SystemDictionary >> openSourceFiles [
	self imageName = LastImageName ifFalse: [
		LastImageName := self imageName.
		"Reset the author initials to blank when the image gets moved"
		Utilities clearAuthor.
		"And clear any absolute path that is most likely invalid now."
		(Preferences at: #clearPackagePathsOnImageMove) ifTrue: [
			CodePackage clearPaths ]].
	"Warning: Do open the source files only if nil.
	If not nil, it is because they are internalized and the files should not be opened"
	self openSourcesAndChanges.
	CuisSourceFileArray install
]

{ #category : 'sources, change log' }
SystemDictionary >> openSourcesAndChanges [
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems.
	Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sources changes entry msg lastQuitLogPosition snapshotChunk entryPathName |

	"Do not open source files if internalized (i.e. notNil)"
	sources := SourceFiles at: 1.
	sources ifNil: [
		entry := self defaultSourcesName asFullFileEntry.
		entry exists ifFalse: [
			entry := self alternativeSourcesName asFullFileEntry ].
		entry exists ifTrue: [
			sources := [ entry readStream ] on: FileDoesNotExistException do: [ nil ]]].
	(sources isNil and: [ Preferences at: #warnIfNoSourcesFile ])
		ifTrue: [
				msg := 'Cuis cannot locate the sources file named ' , entry pathName, '.', String newLineString,
					'Please check that the file is properly named and is in the same directory as this image.'.
			self logStartupError: msg.
		].

	"Do not open source files if internalized (i.e. notNil)"
	changes := SourceFiles at: 2.
	changes ifNil: [
		entry := self defaultChangesName asFullFileEntry.
		entryPathName := entry pathName.
		"Read the last snapshot chunk. Validate that the .changes file is appropriate for this image."
		entry exists
			ifTrue: [
				lastQuitLogPosition := self lastQuitLogPosition.
				lastQuitLogPosition > 0 ifTrue: [
					entry readStreamDo: [ :changesFile |
						changesFile position: lastQuitLogPosition.
						snapshotChunk := changesFile nextChunk ].
					((snapshotChunk beginsWith: self tagHeader) and: [
						snapshotChunk includesSubString: 'priorSource: ']) ifFalse: [
							(Preferences at: #warnIfNoChangesFile) ifTrue: [
								self logStartupError:
									'Incorrect changes file: ', entryPathName, String newLineString,
									'Missing code will be decompiled', String newLineString,
									'New source code will not be saved' ].
							entry := nil ]]]
			ifFalse: [
				(Preferences at: #warnIfNoChangesFile) ifTrue: [
					self logStartupError:
						'Could not find changes file: ', entryPathName, String newLineString,
						'Missing code will be decompiled', String newLineString,
						'New source code will not be saved' ].
				entry := nil ].
		entry ifNotNil: [
			changes := [ entry appendStream ]
				on: FileWriteError
				do: [
					self logStartupError:
						'Could not write to changes file: ', entryPathName, String newLineString,
						'Changes file will not be used.', String newLineString,
						'Missing code will be decompiled', String newLineString,
						'New source code will not be saved'.
					nil ]]].
	ChangesInitialFileSize := changes ifNotNil: [ changes position ].

	SourceFiles := Array with: sources with: changes.
]

{ #category : 'accessing' }
SystemDictionary >> organization [
	"Return the organizer for the receiver"
	^SystemOrganization
]

{ #category : 'miscellaneous' }
SystemDictionary >> osVersion [
	"Return the version number string of the platform we're running on"
	^(self getSystemAttribute: 1002) asString
]

{ #category : 'miscelaneous' }
SystemDictionary >> platformName [
	"Return the name of the platform we're running on"
	^self getSystemAttribute: 1001
]

{ #category : 'miscellaneous' }
SystemDictionary >> platformSubtype [
	"Return the subType of the platform we're running on"
	^self getSystemAttribute: 1003
]

{ #category : 'retrieving' }
SystemDictionary >> pointersTo: anObject [
	"Find all occurrences in the system of pointers to the argument anObject."
	"(Smalltalk pointersTo: Browser) inspect."

	^ self pointersTo: anObject except: #()

]

{ #category : 'retrieving' }
SystemDictionary >> pointersTo: anObject except: objectsToExclude  [
	"Find all occurrences in the system of pointers to the argument
	anObject. Remove objects in the exclusion list from the
	results. "
	^ anObject inboundPointersExcluding: objectsToExclude
]

{ #category : 'retrieving' }
SystemDictionary >> poolUsers [
	"Answer a dictionary of pool name -> classes that refer to it. Also includes any globally know dictionaries (such as Smalltalk, Undeclared etc) which although not strictly accurate is potentially useful information "
	"Smalltalk poolUsers"
	| poolUsers pool refs |
	poolUsers := Dictionary new.
	self keys
		do: [ :k |
			 (((pool := self at: k) is: #Dictionary)
					or: [pool isKindOf: SharedPool class])
				ifTrue: [refs := self allClasses
								select: [:c | c sharedPools identityIncludes: pool]
								thenCollect: [:c | c name].
					refs := refs asOrderedCollection.
					refs
						add: (self
								allCallsOn: (self associationAt: k)).
					poolUsers at: k put: refs]].
	^ poolUsers
]

{ #category : 'class names' }
SystemDictionary >> prepareToRenameClass: aClass as: newName  [

	^self prepareToRenameClass: aClass from: aClass name to: newName
]

{ #category : 'class names' }
SystemDictionary >> prepareToRenameClass: aClass from: oldName to: newName [

	"Rename the class, aClass, to have the title newName."

	| oldref i category |

	category := SystemOrganization categoryOfElement: oldName.
	SystemOrganization classify: newName under: category.
	SystemOrganization removeElement: oldName.
	oldref := self associationAt: oldName.
	self removeKey: oldName.
	oldref key: newName.
	self add: oldref.  "Old association preserves old refs"
	(Array with: StartUpList with: ShutDownList) do:
		[:list |  i := list indexOf: aClass name ifAbsent: [0].
		i > 0 ifTrue: [list at: i put: newName]].
	self flushClassNameCache.
	
	SystemChangeNotifier uniqueInstance aboutToRenameClass: aClass from: oldName to: newName inCategory: category.
	
]

{ #category : 'shrinking' }
SystemDictionary >> presumedSentMessages [
	| sent |
	"
	In addition to those here, if it is desired to preserve some methods from deletion, see #nominallyUnsent:
	Smalltalk presumedSentMessages
	"

	"The following should be preserved for doIts, etc"
	sent := IdentitySet new.
	#( rehashWithoutBecome
		browseAllSelect:  lastRemoval
		vScrollBarValue: hScrollBarValue: 
		to: removeClassNamed:
		dragon: hilberts: mandala: web test3 factorial tinyBenchmarks benchFib
		newDepth: restoreAfter: zapAllMethods obsoleteClasses
		removeAllUnSentMessages abandonSources removeUnreferencedKeys
		zapOrganization condenseSources condenseChanges browseObsoleteReferences
		subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		methodsFor:stamp: methodsFor:stamp:prior: instanceVariableNames:
		startTimerEventLoop unusedClasses allClasses
		unimplemented
		reduceCuis
		variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		printSpaceAnalysis:on:) do: [ :sel |
			sent add: sel].
	"The following may be sent by perform: in dispatchOnChar..."
	Editor withAllSubclassesDo: [ :c |
		c keyboardActions asSet do: [ :sel | sent add: sel ].
		c new defaultMenuSpec do: [ :spec |
			spec size >= 5 ifTrue: [
				spec fifth isSymbol ifTrue: [ sent add: spec fifth ]]]].
	#(beReadOnlyBinding beReadWriteBinding) do: [ :sel |
		sent add: sel].
	AppLauncher appSelector ifNotNil: [ :sel |
		sent add: sel ].
	^ sent
]

{ #category : 'memory space' }
SystemDictionary >> primBytesLeft [
	"Primitive. Answer the number of bytes available for new object data.
	Not accurate unless preceded by
		Smalltalk garbageCollectMost (for reasonable accuracy), or
		Smalltalk garbageCollect (for real accuracy).
	See Object documentation whatIsAPrimitive."

	<primitive: 112>
	^ 0
]

{ #category : 'miscelaneous' }
SystemDictionary >> primGetSystemAttribute: attributeID [
  	"Optional. Answer the string for the system attribute with the given 
  	integer ID. Answer nil if the given attribute is not defined on this 
  	platform. On platforms that support invoking programs from command 
  	lines (e.g., Unix), this mechanism can be used to pass command line 
  	arguments to programs written in Squeak.
  
  	By convention, the first command line argument that is not a VM
  	configuration option is considered a 'document' to be filed in. Such a
  	document can add methods and classes, can contain a serialized object,
  	can include code to be executed, or any combination of these.
  
  	Currently defined attributes include: 
  	-1000	1000th command line argument that specify VM options
  	...
  	-1		first command line argument that specify VM options
  	0		the full path name for currently executing VM
  			(or, on some platforms, just the path name of the VM's directory) 
  	1		path name of this image (better use #imageName instead)
  	2		command line argument See #processCommandLineArguments
  	3		command line argument
  	...
  	1000	last possible command line argument
  	1001	this platform's operating system 'Mac OS', 'Win32', 'unix', ...
  	1002	operating system version
  	1003	this platform's processor type
  	1004	vm version
  	1005	window system name
  	1006	vm build id
  	1007	Interpreter class (Cog VM only)
  	1008	Cogit class (Cog VM only)
 	1009	Platform source version (Cog VM only?)
  	1201	max filename length (Mac OS only)
  	1202	file last error (Mac OS only)
  	10001	hardware details (Win32 only)
  	10002	operating system details (Win32 only)
  	10003	graphics hardware details (Win32 only)
  	"

	<primitive: 149>
	^ nil
]

{ #category : 'primitives' }
SystemDictionary >> primImageName [
	"Answer the full path name for the current image."
	"
	Smalltalk primImageName
	"

	<primitive: 121>
	self primitiveFailed
]

{ #category : 'image, changes name' }
SystemDictionary >> primImageName: newName [
	"Set the the full path name for the current image.  All further snapshots will use this."

	<primitive: 121>
	^ self primitiveFailed
]

{ #category : 'memory space' }
SystemDictionary >> primLowSpaceSemaphore: aSemaphore [
	"Primitive. Register the given Semaphore to be signalled when the
	number of free bytes drops below some threshold. Disable low-space
	interrupts if the argument is nil."

	<primitive: 124>
	self primitiveFailed
]

{ #category : 'memory space' }
SystemDictionary >> primSignalAtBytesLeft: numBytes [
	"Tell the interpreter the low-space threshold in bytes. When the free
	space falls below this threshold, the interpreter will signal the low-space
	semaphore, if one has been registered.  Disable low-space interrupts if the
	argument is zero.  Fail if numBytes is not an Integer."

	<primitive: 125>
	self primitiveFailed
]

{ #category : 'primitives' }
SystemDictionary >> primVmFileName [
	"Answer the absolute file name of the the Smalltalk virtual machine"

	^ self getSystemAttribute: 0
]

{ #category : 'primitives' }
SystemDictionary >> primVmPath [
	"Answer the path for the directory containing the Smalltalk virtual machine. Return nil if this primitive is not implemented."
	"Smalltalk primVmPath"

	<primitive: 142>
]

{ #category : 'ui' }
SystemDictionary >> primitiveBeep [
	"Make a primitive beep. Not to be called directly."
	"
	Smalltalk primitiveBeep
	"

	<primitive: 140>
	self primitiveFailed
]

{ #category : 'special objects' }
SystemDictionary >> primitiveErrorTable [
	"Smalltalk primitiveErrorTable"
	^self specialObjectsArray at: 52
]

{ #category : 'memory space' }
SystemDictionary >> primitiveGarbageCollect [
	"Primitive. Reclaims all garbage and answers the number of bytes of available space."

	<primitive: 130>
	^ self primBytesLeft
]

{ #category : 'printing' }
SystemDictionary >> printElementsOn: aStream [
	aStream nextPutAll:'(lots of globals)'
]

{ #category : 'printing' }
SystemDictionary >> printOn: aStream [
	self == Smalltalk
		ifTrue: [ aStream nextPutAll: 'Smalltalk' ]
		ifFalse: [ super printOn: aStream ]
]

{ #category : 'snapshot and quit' }
SystemDictionary >> printStuffToCleanOnImageSave [
	"So far, to serve experiments.
	Some day, when this is empty, we'll be closer to bootstrap from sources."
	| n nonNilVars m classInstVars v|
	n := 0.
	""
	'--------' print.
	'Proceeses: ' print.
	(Processor processes
		sort: [ :a :b | a priority >= b priority ]) do: [ :p | p print ].
	'======' print.
	'Globals: ' print.
	self globals associationsDo: [ :g | " {g key. g value class }" g  print ].
	'======' print.
	'ClassVars (notNil):  ' print.
	n := 0.
	self hierachySortedAllClassesDo: [ :cls |
		"Cleared, but come again for lazy init during this report.
		therefore, not required for bootstrap from sources"
		({ Scanner } includes: cls) ifFalse: [
			nonNilVars := cls classPool select: [ :var | var notNil ].
			nonNilVars notEmpty ifTrue: [
				n := n + 1.
				{ cls. nonNilVars keys} print ]]].
	'======' print.
	'InstClassVars (notNil): ' print.
	m := 0.
	self hierachySortedAllClassesDo: [ :cls |
		classInstVars := Dictionary new.
		cls class allRegularInstVarNames do: [ :nam |
			v := cls instVarNamed: nam.
			v ifNotNil: [
				classInstVars at: nam put: v ]].
		classInstVars notEmpty ifTrue: [
			m := m + 1.
			{ cls class. classInstVars keys }  print ]].
	'======' print.
	
	{ n. m}  print
]

{ #category : 'startup' }
SystemDictionary >> processCommandLineArguments: initialOptions [
	"
	Smalltalk processCommandLineArguments: true
	Smalltalk processCommandLineArguments: false

	Note: By default, any exceptions thrown during the evaluation of arguments are simply ignored.
	To ease debugging, use the -e argument: Do Not Ignore Exceptions. 
	For example:
		squeak Cuis.image -e -s myScript.st
	If ``myScript.st`` contains an error, a Debugger will pop up.
	
	See comment at
		processInitialCommandLineOption:optionArgument:
		processFinalCommandLineOption:optionArgument:raiseExceptions:
	See displayCommandLineUsageOn:
	"
	| optionString optionArgument optionIndex nextOptionIndex optionLastParameterIndex raiseExceptions dashFinder |

	"Any command line argument that starts with $- is an Option.
	non-Option arguments are parameters for the preceding Option."
	raiseExceptions := false.
	startUpScriptArguments := #().
	"Empty Strings are valid command line arguments, generated by two double quotes. Handle them."
	dashFinder := [ :any | any size > 1 and: [ any first == $- ] ].
	optionIndex := startUpArguments findFirst: dashFinder startingAt: 1.
	[ optionIndex > 0 ] whileTrue:
	[
		"Find parameters for this option. Find the next option after this one."
		nextOptionIndex := startUpArguments findFirst: dashFinder startingAt: optionIndex+1.
		optionLastParameterIndex := nextOptionIndex = 0
			ifTrue: [startUpArguments size]
			ifFalse: [nextOptionIndex-1].
			
		optionString := startUpArguments at: optionIndex.
		optionString = '--' 						"Including    --    as an option means 'ignore everything after here'"
			ifTrue: [^self].
		optionString = '-e'
			ifTrue: [ raiseExceptions := true ]
			ifFalse:
			[
				optionArgument := optionLastParameterIndex > optionIndex ifTrue: [startUpArguments at: optionIndex+1].
				"optionArgument (at: optionIndex+1) would be the startUpScript fileName. It's arguments follow."
				startUpScriptArguments := startUpArguments copyFrom: optionIndex+2 to: optionLastParameterIndex.
				initialOptions
					ifTrue: [ self processInitialCommandLineOption: optionString optionArgument: optionArgument ]
					ifFalse: [ self processFinalCommandLineOption: optionString optionArgument: optionArgument raiseExceptions: raiseExceptions ].
				startUpScriptArguments := #().
			].
		optionIndex := nextOptionIndex.
	].
]

{ #category : 'startup' }
SystemDictionary >> processFinalCommandLineOption: optionString optionArgument: optionArgument raiseExceptions: aBoolean [
	"
	A possible example (duplicated single quotes: '' should be double quotes, but not allowed in a Smalltalk comment):
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -e -d''0 zope''
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -r YAXO -rJSON -d ''Smalltalk startUpScriptArguments print'' xx YY 3492 -d'' 'here comes another' print'' -d ''{'The answer is: '. Smalltalk startUpScriptArguments first asNumber + Smalltalk startUpScriptArguments second asNumber} print'' 3 4 -- -d'' 'not this' print' ''

Squeak.exe Cuis4.2-2211x.image -r RequiredFeature1 -r RequiredFeature2 -d ''Transcript show: 'popo1'; newLine'' -d ''Transcript show: 'popo2'; newLine'' -s smalltalkScript.st param1ToScript param2ToSCript ''parametro 3 to script ->>>--// 3''
	"
	"You can use 'Smalltalk startUpScriptArguments' inside the startUp script:"


	| exceptionToIgnoreOrNil file |
	exceptionToIgnoreOrNil := aBoolean ifFalse: [UnhandledError] ifTrue: [nil].
	optionString caseOf: {

		[ '-u' ] -> [ ChangeSet installNewUpdates ].

		[ '-r' ] -> [		"as in 		-rRequiredFeature2        or         -r RequiredFeature2"
			{ 'Feature require: '. optionArgument } print.
			[ Feature require: optionArgument ] on: FeatureRequirementUnsatisfied do: [ :ex |
				StdIOWriteStream stdout nextPutAll: ex messageText; newLine; flush.
				ex messageText print.
				self quit ] ].

		[ '-d' ] -> [		"as in 		-d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine''        (duplicated singleQuotes should read doubleQuote)"
			{ 'Compiler evaluate: '. optionArgument } print.
			[ Compiler evaluate: optionArgument ] on: exceptionToIgnoreOrNil do: [ :ex | ex return] ].

		['-l' ] -> ["file in the file"
			file := optionArgument isAbsolutePathName
				ifTrue: [ FileEntry withAbsolutePathName: optionArgument ]
				ifFalse: [FileEntry withRelativePathName: optionArgument to: DirectoryEntry currentDirectory ].
			{ 'File in: '. optionArgument. ' -> '. file} print.
			[file readStreamDo: [ :stream | stream fileIn ]] on: exceptionToIgnoreOrNil do: [ :ex | ex return]].

		[ '-s' ] -> [		"as in 		-s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parameter to script script ->>>--// 3'' 			(duplicated singleQuotes should read doubleQuote)"
			file := optionArgument isAbsolutePathName
				ifTrue: [ FileEntry withAbsolutePathName: optionArgument ]
				ifFalse: [FileEntry withRelativePathName: optionArgument to: DirectoryEntry currentDirectory ].
			{ 'Compiler evaluate contents of file named: '. optionArgument. ' -> '. file. ' arguments: '. self startUpScriptArguments } print.
			file exists ifTrue: [
				file readStreamDo: [ :stream |
					[ Compiler evaluate: stream contentsOfEntireFile ] on: exceptionToIgnoreOrNil do: [ :ex | ex return]]]].
	}
	otherwise: [].
]

{ #category : 'system attributes' }
SystemDictionary >> processHasThreadAffinityInstVar: aBoolean [
	"The threaded VM needs to know if the 4th inst var of Process
	 is threadAffinity which it uses to control process-to-thread binding.
	 This flag persists across snapshots, stored in the image header.
	Smalltalk processHasThreadAffinityInstVar: true
	"
	
	aBoolean ifTrue: [self assert: (Process instVarNames at: 4) ='threadAffinity'].
	self vmParameterAt: 48 put: ((self vmParameterAt: 48) bitClear: 1) + (aBoolean ifTrue: [1] ifFalse: [0])
]

{ #category : 'startup' }
SystemDictionary >> processInitialCommandLineOption: optionString optionArgument: optionArgument [
	"
	A possible example:
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -h
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -v
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -q
	"

	optionString caseOf: {
		[ '-h' ] -> [
			self displayCommandLineUsageOn: StdIOWriteStream stdout.
			self quit ].

		[ '-v' ] -> [ 
			self displaySystemInformationStringOn: StdIOWriteStream stdout.
			self quit ].

		[ '-q' ] -> [ self quit ].

		[ '-ud' ] -> [
			DirectoryEntry setUserBaseDirectory: optionArgument asDirectoryEntry ].

		[ '-udIsBase' ] -> [ DirectoryEntry setUserBaseDirectoryAsCuisBase ].
	}
	otherwise: [].
]

{ #category : 'system attributes' }
SystemDictionary >> processPreemptionYields [
	"Answer whether the VM causes a process to yield on process preemption,
	 i.e. to put a preempted process at the back of its run queue.  If the parameter
	 is unavailable (non-Cog VMs) or bit 2 (4) is 0 then preemption yields.
	Smalltalk processPreemptionYields
	"

	^ (self vmParameterAt: 48)
		ifNil: [ true ]
		ifNotNil: [ :properties |
			(properties allMask: 4) not ]
]

{ #category : 'system attributes' }
SystemDictionary >> processPreemptionYields: aBoolean [
	"The Cog VM can be instructed not to yield on process preemption,
	 i.e. not to put a preempted process at the back of its run queue.  By
	 default preempting a process causes it to yield (Blue Book semantics)
	 which can have unfortunate effects.
	 This flag persists across snapshots, stored in the image header."

	self vmParameterAt: 48 put: ((self vmParameterAt: 48) bitClear: 4) + (aBoolean ifTrue: [0] ifFalse: [4])
]

{ #category : 'snapshot and quit' }
SystemDictionary >> processShutDownList: quitting [
	"Send #shutDown to each class that needs to wrap up before a snapshot."

	self send: #shutDown: toClassesNamedIn: ShutDownList with: quitting.
	EndianCache := nil.
	SystemAttributeCache := nil.
	PreviousUserChangesName := nil.
]

{ #category : 'snapshot and quit' }
SystemDictionary >> processStartUpList: isARealStartup [
	"Send #startUp to each class that needs to run initialization after a snapshot."

	EndianCache := self calcEndianness.
	self send: #startUp: toClassesNamedIn: StartUpList with: isARealStartup
]

{ #category : 'system attributes' }
SystemDictionary >> processSuspensionUnblocks [
	"Answer whether the suspend primitive backs up a process waiting on a
	 condition variable to before the send or not. The original behaviour was to
	 remove a process from its condition variable (Semaphore, Mutex; both kinds
	 of LinkedList).  Hence resuming would allow the process past the condition
	 variable, and hence required special behaviour in Process>>signalException:.
	 The new behaviour is to back up a blocked process one bytecode so that if
	 and when resumed it enters the wait state.  It also implies that the suspend
	 primitive answers nil if the process is blocked, and only answers a list when
	 suspending a runnable process that is not the active process.

	 To be clear, this method answers true on VMs with the old behaviour and
	  false on those with the new."

	^((self vmParameterAt: 65) ifNil: 0) noMask: 32
]

{ #category : 'profiling' }
SystemDictionary >> profile: aBlock [
	"Make a virtual machine profile of the given block."
	"Note: Profiling support is provided so that VM implementors
	 can better understand and improve the efficiency of the virtual
	 machine. To use it, you must be running a version of the
	 virtual machine compiled with profiling enabled (which
	 makes it much slower than normal even when not profiling).
	 You will also need the CodeWarrior profile reader application."

	self stopProfiling.
	self clearProfile.
	self startProfiling.
	aBlock value.
	self stopProfiling.
	self dumpProfile.
]

{ #category : 'AndreasProfiler-profiling' }
SystemDictionary >> profilePrimitive [
	"Primitive. Answer the primitive sample by the profiler, or nil if the profiler isn't active.
	See also primitiveProfileStart."
	<primitive: 'primitiveProfilePrimitive'>
	^self primitiveFailed
]

{ #category : 'AndreasProfiler-profiling' }
SystemDictionary >> profileSample [
	"Primitive. Answer the last sample taken by the profiler, or nil if the profiler isn't active.
	See also primitiveProfileStart."
	<primitive: 'primitiveProfileSample'>
	^self primitiveFailed
]

{ #category : 'AndreasProfiler-profiling' }
SystemDictionary >> profileSemaphore: aSemaphore [
	"Primitive. Install the semaphore to be used for profiling, 
	or nil if no semaphore should be used.
	See also primitiveProfileStart."
	<primitive: 'primitiveProfileSemaphore'>
	^self primitiveFailed
]

{ #category : 'AndreasProfiler-profiling' }
SystemDictionary >> profileStart: counter [
	"Primitive. Begin profiling execution every by using the interrupt check-counter instead of a time-based process (which is limited to timing resolution and triggers off the same signal that many of the processes being profiled trigger off leading to consistently wrong results).
	The argument is the number of interrupt checks (method activations) to let go by before taking a sample. The sample is being stored in the profileSample iVar which can be retrieved by executing primitiveProfileSample. When a sample is taken, it signals the semaphore specified in primitiveProfileSemaphore.
	If the argument is less or equal to zero, it disables profiling."
	"Not an interrupt check-counter, but #primHighResClock"
	<primitive: 'primitiveProfileStart'>
	^self primitiveFailed
]

{ #category : 'snapshot and quit' }
SystemDictionary >> quit [
	"Just quit. No questions asked. No validations done.
	Smalltalk quit.
	"

	(SourceFiles at: 2) ifNotNil: [ :changes |
		ChangesInitialFileSize ifNotNil: [ changes truncate: ChangesInitialFileSize ]].
	"These could be moved to some #shutDown"
	self stopLowSpaceWatcher.
	ProcessorScheduler stopBackgroundProcess.

	self processShutDownList: true.
	self closeSourceFiles.

	"Be sure all unreferenced objects are finalized as approriate.
	Do this as close to the quit / snapshot primitive as possible."
	self garbageCollect.
	EphemeronFinalizationProcess ensureFinalizationComplete.
	EphemeronFinalizationProcess stopFinalizationProcess.

	"Do image save & quit as apropriate"
	self quitPrimitive.
]

{ #category : 'snapshot and quit' }
SystemDictionary >> quitNoSaveTag [

	^ 'QUIT/NOSAVE' 
]

{ #category : 'private' }
SystemDictionary >> quitPrimitive [
	"Exit to another operating system on the host machine, if one exists.  All 
	state changes in the object space since the last snapshot are lost.  Essential.  
	See Object documentation whatIsAPrimitive."

	<primitive: 113>
	self primitiveFailed
]

{ #category : 'snapshot and quit' }
SystemDictionary >> quitPrimitive: exitCode [
	"Primitive. Exit with exitCode to another operating system on the host machine, if one exists.
	All state changes in the object space since the last snapshot are lost.
	Ignore exitCode if it's not supported by the VM."

	<primitive: 113>
	self quitPrimitive
]

{ #category : 'snapshot and quit' }
SystemDictionary >> quitTag [
		
	^'QUIT'
]

{ #category : 'snapshot and quit' }
SystemDictionary >> readAndApplyUserPrefs [
	"
	Smalltalk readAndApplyUserPrefs
	"
	(Preferences at: #useUserPrefsFile) ifTrue: [
		PreferenceSet readAndApplyUserPrefs ].
]

{ #category : 'startup' }
SystemDictionary >> readCommandLineArguments [
	"
	Smalltalk readCommandLineArguments
	"
	| arg i |
	startUpArguments := Array streamContents: [ :strm |
		i := 2.
		[ i <= 1000 and: [ (arg := self getSystemAttribute: i) notNil ] ] whileTrue: [
			strm nextPut: arg.
			i := i + 1 ]]
]

{ #category : 'housekeeping' }
SystemDictionary >> recompileAllFrom: firstName  [
	"Recompile all classes, starting with given name."

	self allClassesDo: [ :class |
		class name >= firstName
			ifTrue: [
				Transcript show: class name; newLine.
				class compileAll]]

	"Smalltalk recompileAllFrom: 'AAABodyShop'."

]

{ #category : 'special objects' }
SystemDictionary >> recreateSpecialObjectsArray [
	"Smalltalk recreateSpecialObjectsArray"
	
	"To external package developers:
	**** DO NOT OVERRIDE THIS METHOD.  *****
	If you are writing a plugin and need additional special object(s) for your own use, 
	use addGCRoot() function and use own, separate special objects registry "
	
	"The Special Objects Array is an array of objects used by the Squeak virtual machine.
	 Its contents are critical and accesses to it by the VM are unchecked, so don't even
	 think of playing here unless you know what you are doing."
	| newArray |
	newArray := Array new: 60.
	"Nil false and true get used throughout the interpreter"
	newArray at: 1 put: nil.
	newArray at: 2 put: false.
	newArray at: 3 put: true.
	"This association holds the active processor (a ProcessScheduler)"
	newArray at: 4 put: (self associationAt: #Processor).
	"Numerous classes below used for type checking and instantiation"
	newArray at: 5 put: Bitmap.
	newArray at: 6 put: SmallInteger.
	newArray at: 7 put: String.
	newArray at: 8 put: Array.
	newArray at: 9 put: Smalltalk.
	newArray at: 10 put: BoxedFloat64.
	newArray at: 11 put: MethodContext.
	newArray at: 11 put: (self at: #MethodContext ifAbsent: [self at: #Context]).
	newArray at: 12 put: nil. "was BlockContext."
	newArray at: 13 put: Point.
	newArray at: 14 put: LargePositiveInteger.
	newArray at: 15 put: Display.
	newArray at: 16 put: Message.
	newArray at: 17 put: CompiledMethod.
	newArray at: 18 put: ((self specialObjectsArray at: 18) ifNil: [Semaphore new]). "low space Semaphore"
	newArray at: 19 put: Semaphore.
	newArray at: 20 put: Character.
	newArray at: 21 put: #doesNotUnderstand:.
	newArray at: 22 put: #cannotReturn:.
	newArray at: 23 put: nil. "This is the process signalling low space."
	"An array of the 32 selectors that are compiled as special bytecodes,
	 paired alternately with the number of arguments each takes."
	newArray at: 24 put: #(	#+ 1 #- 1 #< 1 #> 1 #<= 1 #>= 1 #= 1 #~= 1
							#* 1 #/ 1 #\\ 1 #@ 1 #bitShift: 1 #// 1 #bitAnd: 1 #bitOr: 1
							#at: 1 #at:put: 2 #size 0 #next 0 #nextPut: 1 #atEnd 0 #== 1 #class 0
							#blockCopyNOWUNUSED: 1 #value 0 #value: 1 #do: 1 #new 0 #new: 1 #x 0 #y 0 ).
	newArray at: 25 put: nil. "This is nil in Spur, which has immediate Characters."
	newArray at: 26 put: #mustBeBoolean.
	newArray at: 27 put: ByteArray.
	newArray at: 28 put: Process.
	newArray at: 29 put: nil. "This is nil in Spur, no compact classes."
	newArray at: 30 put: ((self specialObjectsArray at: 30) ifNil: [Semaphore new]). "delay Semaphore"
	newArray at: 31 put: ((self specialObjectsArray at: 31) ifNil: [Semaphore new]). "user interrupt Semaphore"

	"Entries 32 - 34 unreferenced. Previously these contained prototype instances to be copied for fast initialization.
	In Squeak these are: DoubleByteArray, WordArray, DoubleWordArray.
	Understand why."
	newArray at: 32 put: nil. "was the prototype Float"
	newArray at: 33 put: nil. "was the prototype 4-byte LargePositiveInteger"
	newArray at: 34 put: nil. "was the prototype Point"

	newArray at: 35 put: #cannotInterpret:.
	newArray at: 36 put: nil. "was the prototype MethodContext"
	newArray at: 37 put: BlockClosure.
	newArray at: 38 put: nil. "was the prototype BlockContext"

	"array of objects referred to by external code"
	newArray at: 39 put: (self specialObjectsArray at: 39).	"preserve external semaphores"
	newArray at: 40 put: nil. "Reserved for Mutex in Cog VMs"
	newArray at: 41 put: ((self specialObjectsArray at: 41) ifNil: [LinkedList new]). "Reserved for a LinkedList instance for overlapped calls in CogMT"
	newArray at: 42 put: ((self specialObjectsArray at: 42) ifNil: [Semaphore new]). "finalization Semaphore"
	newArray at: 43 put: LargeNegativeInteger.
	"External objects for callout.
	 Note: Written so that one can actually completely remove the FFI."
	newArray at: 44 put: (self at: #ExternalAddress ifAbsent: nil).
	newArray at: 45 put: (self at: #ExternalStructure ifAbsent: nil).
	newArray at: 46 put: (self at: #ExternalData ifAbsent: nil).
	newArray at: 47 put: (self at: #ExternalFunction ifAbsent: nil).
	newArray at: 48 put: (self at: #ExternalLibrary ifAbsent: nil).
	newArray at: 49 put: #aboutToReturn:through:.
	newArray at: 50 put: #run:with:in:.
	"51 reserved for immutability message"
	newArray at: 51 put: #attemptToAssign:withIndex:.
	newArray at: 52 put: #(nil "nil => generic error" #'bad receiver'
							#'bad argument' #'bad index'
							#'bad number of arguments'
							#'inappropriate operation'  #'unsupported operation'
							#'no modification' #'insufficient object memory'
							#'insufficient C memory' #'not found' #'bad method'
							#'internal error in named primitive machinery'
							#'object may move' #'resource limit exceeded'
							#'object is pinned' #'primitive write beyond end of object').
	"53 to 55 are for Alien"
	newArray at: 53 put: (self at: #Alien ifAbsent: nil).
	newArray at: 54 put: #invokeCallbackContext:. "use invokeCallback:stack:registers:jmpbuf: for old Alien callbacks."
	newArray at: 55 put: (self at: #UnsafeAlien ifAbsent: nil).

	"Used to be WeakFinalizationList for WeakFinalizationList hasNewFinalization, obsoleted by ephemeron support."
	newArray at: 56 put: nil.

	"reserved for the foreign callback process prototype"
	newArray at: 57 put: (self specialObjectsArray at: 57 ifAbsent: nil).

	newArray at: 58 put: #unusedBytecode.
	"59 reserved for Sista counter tripped message"
	newArray at: 59 put: #conditionalBranchCounterTrippedOn:.
	"60 reserved for Sista class trap message"
	newArray at: 60 put: #classTrapFor:.

	"Now replace the interpreter's reference in one atomic operation"
	self specialObjectsArray become: newArray.
]

{ #category : 'shrinking' }
SystemDictionary >> reduceCuis [
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	self at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	WorldMorph allInstancesDo: [ :w |
		w backgroundImageData: nil.
		w submorphsDo: [ :a | a delete ]].
	Preferences at: #wantsMenuIcons put: false.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	BitBltCanvas releaseClassCachedState.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	self garbageCollect.

	self removeEmptyMessageCategories.
	self organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(SpaceTally DynamicTypingSmalltalkCompleter).
	AppLauncher appGlobalName ifNotNil: [ :any |
		keep add: any ].
	unused := self unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(self at: c) removeFromSystem]. 
		n := self removeAllUnSentMessages.
		unused := self unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSet zapAllChangeSets.
	self garbageCollect.

	self removeEmptyMessageCategories.
	self organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	self garbageCollect.
	oldDicts := MethodDictionary allInstances.
	newDicts := Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts := newDicts := nil.

	 SmalltalkCompleter initialize .

   "Sanity checks"
"   Undeclared
   Smalltalk cleanOutUndeclared
   Smalltalk browseUndeclaredReferences
   Smalltalk obsoleteClasses
   Smalltalk obsoleteBehaviors 
   Smalltalk browseObsoleteMethodReferences
   Smalltalk browseAllUnimplementedCalls"
]

{ #category : 'special objects' }
SystemDictionary >> registerExternalObject: anObject [
	"Register the given object in the external objects array and return its index. If it is already there, just return its index."

	^ExternalSemaphoreTable registerExternalObject: anObject
]

{ #category : 'shrinking' }
SystemDictionary >> removeAllUnSentMessages [
	"Smalltalk removeAllUnSentMessages"
	"[Smalltalk unusedClasses do: [:c | (Smalltalk at: c) removeFromSystem]. 
	Smalltalk removeAllUnSentMessages > 0] whileTrue."
	"Remove all implementations of unsent messages."
	| sels n |
	sels := self allUnSentMessages.
	self presumedSentMessages
		do: [:sel | sels
				remove: sel
				ifAbsent: nil].
	sels isEmpty
		ifTrue: [^ 0].
	n := 0.
	self
		allBehaviorsDo: [:x | n := n + 1].
	'Removing ' , sels size printString , ' messages . . .'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: n
		during: [:barBlock | 
			n := 0.
			self
				allBehaviorsDo: [:class | 
					barBlock value: (n := n + 1).
					sels
						do: [:sel | class removeSelector: sel]]].
	^ sels size
]

{ #category : 'class names' }
SystemDictionary >> removeClassNamed: aName [
	"Invoked from fileouts:  if there is currently a class in the system named aName, then remove it.  If anything untoward happens, report it in the Transcript.  "

	| oldClass |
	oldClass := self at: aName asSymbol ifAbsent: [
		Transcript newLine; show: 'Removal of class named ', aName, ' ignored because ', aName, ' does not exist.'.
		^ self].

	oldClass removeFromSystem
]

{ #category : 'class names' }
SystemDictionary >> removeClassNamedIfInBaseSystem: aName [
	"Invoked from fileouts:  if there is currently a class in the system named aName, and it is not part of a package, then remove it.
	If anything untoward happens, report it in the Transcript.  "

	| oldClass |
	oldClass := self at: aName asSymbol ifAbsent: [
"		Transcript newLine; show: 'Removal of class named ', aName, ' ignored because ', aName, ' does not exist.'."
		^ self].

	CodePackage
		packageOfClass: oldClass
		ifNone: [
			"If remove is actually done, then include it in the current change set for the base system,
			as a regular remove."
			oldClass removeFromSystem ]
]

{ #category : 'housekeeping' }
SystemDictionary >> removeEmptyMessageCategories [
	"Smalltalk removeEmptyMessageCategories"
	self garbageCollect.
	(ClassOrganizer allInstances copyWith: SystemOrganization) do:
		[:org | org removeEmptyCategories]
]

{ #category : 'snapshot and quit' }
SystemDictionary >> removeFromShutDownList: aClass [

	ShutDownList remove: aClass name ifAbsent: nil
]

{ #category : 'snapshot and quit' }
SystemDictionary >> removeFromStartUpList: aClass [

	StartUpList remove: aClass name ifAbsent: nil
]

{ #category : 'removing' }
SystemDictionary >> removeKey: key ifAbsent: aBlock [
	"Remove key (and its associated value) from the receiver. If key is not in
	the receiver, answer the result of evaluating aBlock. Otherwise, answer
	the value externally named by key."
	self flushClassNameCache.
	^super removeKey: key ifAbsent: aBlock
]

{ #category : 'shrinking' }
SystemDictionary >> removeSelector: descriptor [
	"Safely remove a selector from a class (or metaclass). If the class
	or the method doesn't exist anymore, never mind and answer nil.
	This method should be used instead of 'Class removeSelector: #method'
	to omit global class references."

	| class sel |
	class := self at: descriptor first ifAbsent: [^ nil].
	(descriptor size > 2 and: [descriptor second == #class])
		ifTrue:
			[class := class class.
			sel := descriptor third]
		ifFalse: [sel := descriptor second].
	^ class removeSelector: sel
]

{ #category : 'shrinking' }
SystemDictionary >> removedUnusedClassesAndMethods [
	[
		#hereWeGo print.
		self unusedClasses do: [:c | 
			c print.
			(self at: c) removeFromSystem]. 
		self removeAllUnSentMessages > 0 or: [ self unusedClasses notEmpty ]] whileTrue.
]

{ #category : 'class names' }
SystemDictionary >> renameClassNamed: oldName as: newName [
	"Invoked from fileouts:  if there is currently a class in the system named oldName, then rename it to newName.  If anything untoward happens, report it in the Transcript.  "

	| oldClass |
	(oldClass := self at: oldName asSymbol ifAbsent: nil)
		ifNil: [
			Transcript newLine; show: 'Class-rename for ', oldName, ' ignored because ', oldName, ' does not exist.'.
			^ self].

	oldClass rename: newName
]

{ #category : 'class names' }
SystemDictionary >> renamedClass: aClass from: oldName  [
	
	| newName |
	
	newName := aClass name.
	
	SystemChangeNotifier uniqueInstance classRenamed: aClass from: oldName to: newName inCategory: aClass category.
	self 
		logChange: 'Smalltalk renameClassNamed: #', oldName, ' as: #', newName
		preamble: 'classRenamed: #', oldName, ' as: #', newName, Utilities changeStampField
]

{ #category : 'shrinking' }
SystemDictionary >> reportClassAndMethodRemovalsFor: collectionOfClassNames [
	| initialClassesAndMethods finalClassesAndMethods |
	"Smalltalk reportClassAndMethodRemovalsFor: #(Celeste Scamper MailMessage)"

	initialClassesAndMethods := self unusedClassesAndMethodsWithout: #(#() #()).
	finalClassesAndMethods := self unusedClassesAndMethodsWithout: {collectionOfClassNames. #()}.
	^ {finalClassesAndMethods first copyWithoutAll: initialClassesAndMethods first.
		finalClassesAndMethods second copyWithoutAll: initialClassesAndMethods second}
]

{ #category : 'startup - restore lost changes' }
SystemDictionary >> restoreLostChanges [

 	| decision |
	
	decision := PopUpMenu withCaption: self lostChangesDetectedCaption chooseFrom: self restoreLostChangesOptions.

	decision = 1 ifTrue: [ ^self restoreLostChangesAutomatically ].
	decision = 2 ifTrue: [ ^self restoreLostChangesManually ]
]

{ #category : 'startup - restore lost changes' }
SystemDictionary >> restoreLostChangesAutomatically [
	
	self withChangesFileDo: [ :aChangesFile | self restoreLostChangesAutomaticallyFrom: aChangesFile ]
]

{ #category : 'startup - restore lost changes' }
SystemDictionary >> restoreLostChangesAutomaticallyFrom: aChangesFile [

	| changeList |

	changeList := ChangeListWithFileInErrors new.
	changeList scanFile: aChangesFile from: LastQuitLogPosition to: aChangesFile size.
	changeList fileInAllKeepingErrors.
	(changeList hasFileInErrors and: [ self shouldShowFileInErrors ]) ifTrue: [ changeList showChangesWithFileInErrors ]

]

{ #category : 'startup - restore lost changes' }
SystemDictionary >> restoreLostChangesManually [

	ChangeList browseFrom: LastQuitLogPosition on: self currentChangesName asFullFileEntry labeled: 'Lost changes'

]

{ #category : 'startup - restore lost changes' }
SystemDictionary >> restoreLostChangesOptions [
	
	^{'Restore lost changes automatically'. 'Restore lost changes manually'. 'Nothing'}.
]

{ #category : 'startup - restore lost changes' }
SystemDictionary >> restoringChangesHasErrorsCaption [
	
	^'There were errors filing in the lost changes. Do you want to see them?'
]

{ #category : 'retrieving' }
SystemDictionary >> rootClasses [
	"
	Answer class hierarchy roots. This is, classes without superclass.
	Smalltalk rootClasses
	"
	^Array streamContents: [ :strm |
		self do: [ :root |
			(root isBehavior and: [root superclass isNil]) ifTrue: [	"Grab ProtoObject and any other alike"
				strm nextPut: root ]]]
]

{ #category : 'snapshot and quit' }
SystemDictionary >> saveAndQuit [
	"Save image and quit. No questions asked.
	Smalltalk saveAndQuit.
	"
	ChangeSet zapAllChangeSets.
	self garbageCollect.
	^ self
		snapshot: true
		andQuit: true
		embedded: false
		clearAllClassState: false.
]

{ #category : 'snapshot and quit' }
SystemDictionary >> saveAndQuitAs: newName clearAllClassState: clearAllStateFlag [
	"Save the image  under a new name. Don't quit.
	See senders."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName := self fullNameForChangesNamed: newName.
		FileIOAccessor default copy: oldChangesName asFullFileEntry to: newChangesName asFullFileEntry.
		ChangesInitialFileSize ifNotNil: [
			oldChangesName asFullFileEntry appendStreamDo: [ :strm | strm truncate: ChangesInitialFileSize ]]].

	ChangeSet zapAllChangeSets.
	FontFamily releaseClassCachedState.
	self garbageCollect.
	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: true embedded: false clearAllClassState: clearAllStateFlag.
]

{ #category : 'snapshot and quit' }
SystemDictionary >> saveAndStayAs: newName clearAllClassState: clearAllStateFlag [
	"Save the image  under a new name. Don't quit.
	See senders."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName := self fullNameForChangesNamed: newName.
		FileIOAccessor default copy: oldChangesName asFullFileEntry to: newChangesName asFullFileEntry.
		ChangesInitialFileSize ifNotNil: [
			oldChangesName asFullFileEntry appendStreamDo: [ :strm | strm truncate: ChangesInitialFileSize ]]].

	ChangeSet zapAllChangeSets.
	self garbageCollect.
	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: false embedded: false clearAllClassState: clearAllStateFlag.
]

{ #category : 'snapshot and quit' }
SystemDictionary >> saveAs [
	"Put up the 'saveAs' prompt, obtain a name, and save the image  under that new name."

	self request: 'New file name?' initialAnswer: self imageName asFullFileEntry name do: [ :newName |
		(
			"Either both files are new,"
			((self fullNameForImageNamed: newName) asFullFileEntry exists not
				and: [(self fullNameForChangesNamed: newName) asFullFileEntry exists not])
			"Or user confirms overwriting them."
			or: [self confirm: ('{1} already exists. Overwrite?' format: {newName})]
		)
			ifTrue: [
				self saveAs: newName ]].

]

{ #category : 'snapshot and quit' }
SystemDictionary >> saveAs: newName [
	"Save the image under newName. Don't quit.
	See senders."

	(self fullNameForImageNamed: newName) = self imageName
		ifTrue: [
			"If saving over the our own image file, call #saveImage instead."
			self saveImage ]
		ifFalse: [
			"This method requires saving on a different file."
			self saveAs: newName clearAllClassState: false ].
]

{ #category : 'snapshot and quit' }
SystemDictionary >> saveAs: newName clearAllClassState: clearAllStateFlag [
	"Save the image under a new name. Don't quit.
	Note: newName must be different from current name.
	See senders."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName := self fullNameForChangesNamed: newName.
		FileIOAccessor default copy: oldChangesName asFullFileEntry to: newChangesName asFullFileEntry.
		ChangesInitialFileSize ifNotNil: [
			oldChangesName asFullFileEntry appendStreamDo: [ :strm | strm truncate: ChangesInitialFileSize ]]].

	ChangeSet zapAllChangeSets.
	self garbageCollect.
	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: false embedded: false clearAllClassState: clearAllStateFlag.
]

{ #category : 'snapshot and quit' }
SystemDictionary >> saveImage [
	"Save image. Don't quit. No questions asked.
	Smalltalk saveImage.
	"
	ChangeSet zapAllChangeSets.
	self garbageCollect.
	^ self
		snapshot: true
		andQuit: false
		embedded: false
		clearAllClassState: false.
]

{ #category : 'snapshot and quit' }
SystemDictionary >> saveRollingReleaseAndQuit [
	"Save the image/changes using the next available version number.
	This is usually only used to prepare updated Cuis images for distribution."
	"
	Smalltalk saveRollingReleaseAndQuit
	"

	"If we are in a Stable Release, increment minor version.
	For instance, after releasing Cuis 6.2, next rolling release image should be Cuis 6.3.
	Note: Version number for next Stable Release is hardcoded at #saveStableRelease"
	| sv |
	sv := SystemVersion current.
	sv isStableRelease ifTrue: [
		SystemVersion
			newVersionMajor: sv versionMajor
			versionMinor: sv versionMinor + 1 ].

	(self filenameForUpdatedImage: true) ifNotNil: [ :newName |
		Preferences at: #saveReleaseOptionsInWorldMenu put: false.
		SystemVersion current beRollingRelease.
		self setDefaultGCParameters.
		"Try to clear all user state, including all class vars, preferences, etc"
		self saveAndQuitAs: newName clearAllClassState: true ]
]

{ #category : 'snapshot and quit' }
SystemDictionary >> saveStableRelease [
	"Save the image/changes using the next release version number."
	"
	Smalltalk saveStableRelease
	"
	| versionMajor versionMinor previousValue |
	versionMajor := 7.
	versionMinor := 6.
	SystemVersion newVersionMajor: versionMajor versionMinor: versionMinor.
	"While condensing sources, convert all variable assignment to ANSI Smalltalk form, i.e. :="
	previousValue := Preferences at: #fileOutANSIassignment.
	Preferences at: #fileOutANSIassignment put: true.
	self condenseSources: SystemVersion current versionString.
	Preferences at: #fileOutANSIassignment put: previousValue.
	self setDefaultGCParameters.

	(self filenameForUpdatedImage: false) ifNotNil: [ :newName |
		Preferences at: #saveReleaseOptionsInWorldMenu put: false.
		SystemVersion current beStableRelease.
		"Try to clear all user state, including all class vars, preferences, etc"
		self saveAndStayAs: newName clearAllClassState: true ].
]

{ #category : 'snapshot and quit' }
SystemDictionary >> send: startUpOrShutDown toClassesNamedIn: startUpOrShutDownList with: argument [
	"Send the message #startUp: or #shutDown: to each class named in the list.
	The argument indicates if the system is about to quit (for #shutDown:) or if
	the image is ia real startup (or just continue after image save) (for #startUp:).
	If any name cannot be found, then remove it from the list."

	| removals class |
	removals := OrderedCollection new.
	startUpOrShutDownList do:
		[:name |
		class := self at: name ifAbsent: nil.
		class
			ifNil: [removals add: name]
			ifNotNil: [class perform: startUpOrShutDown with: argument]].

	"Remove any obsolete entries, but after the iteration"
	"Well, not. Better just ignore them. Maybe it is stuff, like SoundPlayer, that was moved to optional packages, and can be loaded again anytime."
	"startUpOrShutDownList removeAll: removals"
]

{ #category : 'vm parameters' }
SystemDictionary >> sendMouseWheelEvents [
	"The Cog VM can be instructed to deliver mouse wheel events as mouse wheel events.
	 By default mouse wheel events are mapped to arrow events.
	 This flag persists across snapshots, stored in the image header.

	This implementation was copied from Squeak 5.3 and modified:
	Non-Cog VMs might not support this flag. If this is the case, just answer false."
	
	^ (self vmParameterAt: 48)
		ifNil: [ false ]
		ifNotNil: [ :properties | properties allMask: 32 ].
]

{ #category : 'vm parameters' }
SystemDictionary >> sendMouseWheelEvents: aBoolean [
	"The Cog VM can be instructed to deliver mouse wheel events as mouse wheel events.
	 By default mouse wheel events are mapped to arrow events.
	 This flag persists across snapshots, stored in the image header.

	This implementation was copied from Squeak 5.3 and modified:
	Non-Cog VMs might not support this flag. If this is the case, just ignore it."

	(self vmParameterAt: 48) ifNotNil: [ :properties |
		self vmParameterAt: 48 put: (properties bitClear: 32) + (aBoolean ifTrue: [32] ifFalse: [0]) ].
]

{ #category : 'snapshot and quit' }
SystemDictionary >> setDefaultGCParameters [
	"Adjust the VM's default GC parameters to avoid premature tenuring, and too frequent scavenging.
	Parameters set here persist in saved images, so we set them image save for release.
	See #setGCParameters"

	"Desired Eden size: "
	self vmParameterAt: 45 put: `16*1024*1024`.
]

{ #category : 'snapshot and quit' }
SystemDictionary >> setGCParameters [
	"Adjust the VM's default GC parameters to avoid premature tenuring, and too frequent scavenging.
	Parameters set here don't persist, so we set them on startup.
	See #setDefaultGCParameters"

	| proportion edenSize survivorSize averageObjectSize numObjects |

	"Grow old memory in chunks of: "
	self vmParameterAt: 25 put: `32*1024*1024`.
	"Shrink heap when unused memory is at least: "
	self vmParameterAt: 24 put: `64*1024*1024`.

	"Note: (jmv, 9/2021) It seems modern Spur VMs ignore parameter 6. This all seems to be bogus."
	proportion := 0.9. "tenure when 90% of pastSpace is full"
	edenSize := self vmParameterAt: 44.
	survivorSize := edenSize / 5.0. "David's paper uses 140Kb eden + 2 x 28kb survivor spaces; Spur uses the same ratios :-)"
	averageObjectSize := 8 * self wordSize. "a good approximation"
	numObjects := (proportion * survivorSize / averageObjectSize) rounded.
	self vmParameterAt: 6 put: numObjects.  "tenure when more than this many objects survive the GC"
	"/Note: (jmv, 9/2021)"
	
	"Do a full GC when used memory grows by this factor.
	Default is 0.333.
	2.0 means gull GC when heap size triples."
	self vmParameterAt: 55 put: 2.0.
]

{ #category : 'snapshot and quit' }
SystemDictionary >> setPlatformPreferences [
	"Set some platform specific preferences on system startup"
	| platform specs |	

	(Preferences at: #automaticPlatformSettings) ifFalse:[^self].
	platform := self platformName.
	specs := 	#(	
					(soundStopWhenDone false)
					(soundQuickStart false)
			).
	platform = 'Win32' ifTrue:[
		specs := #(	
					(soundStopWhenDone true)
					(soundQuickStart false)
				)].
	platform = 'Mac OS' ifTrue:[
		specs := #(	
					(soundStopWhenDone false)
					(soundQuickStart true)
				)].
	specs do: [:tuple |
		Preferences name: tuple first category: #system value: tuple last ]
]

{ #category : 'startup' }
SystemDictionary >> setStartupStamp [

	| dateAndTime |
	dateAndTime := DateAndTime now.
	StartupStamp := String streamContents: [ :stream |
		stream nextPutAll: '----STARTUP---- ('.
		dateAndTime date printOn: stream.
		stream space.
		dateAndTime time print24: true showSeconds: true on: stream.
		stream
			nextPutAll: ') as ';
			nextPutAll: self imageName ] 

]

{ #category : 'startup - restore lost changes' }
SystemDictionary >> shouldShowFileInErrors [
	
 	^self confirm: self restoringChangesHasErrorsCaption
	
]

{ #category : 'browsing' }
SystemDictionary >> showMenuOf: selectorCollection withFirstItem: firstItem ifChosenDo: choiceBlock [
	"Show a sorted menu of the given selectors, preceded by firstItem, and all
	abbreviated to 40 characters.  Evaluate choiceBlock if a message is chosen."

	^ self showMenuOf: selectorCollection withFirstItem: firstItem ifChosenDo: choiceBlock withCaption: nil
]

{ #category : 'browsing' }
SystemDictionary >> showMenuOf: selectorCollection withFirstItem: firstItem ifChosenDo: choiceBlock withCaption: aCaption [
	"Show a sorted menu of the given selectors, preceded by firstItem, and all abbreviated to 40 characters.  Use aCaption as the menu title, if it is not nil.  Evaluate choiceBlock if a message is chosen."
	| index menuLabels orderedList aMenu |
	orderedList := selectorCollection isSequenceable
		ifTrue: [ selectorCollection ]
		ifFalse: [ selectorCollection asArray sort ].
	menuLabels := String streamContents: [ :strm |
		strm nextPutAll: (firstItem contractTo: 40).
		orderedList do: [ :sel |
			strm
				 newLine;
				 nextPutAll: (sel contractTo: 40) ]].
	aMenu := PopUpMenu
		labels: menuLabels
		lines: #(1 ).
	index := aCaption
		ifNil: [ aMenu startUpMenu ]
		ifNotNil: [ aMenu startUpWithCaption: aCaption ].
	index = 1 ifTrue: [ choiceBlock value: firstItem ].
	index > 1 ifTrue: [ choiceBlock value: (orderedList at: index - 1) ].
]

{ #category : 'memory space' }
SystemDictionary >> signalLowSpace [
	"Signal the low-space semaphore to alert the user that space is running low."

	LowSpaceSemaphore signal.
]

{ #category : 'snapshot and quit' }
SystemDictionary >> snapshot: save andQuit: quit embedded: embeddedFlag clearAllClassState: clearAllStateFlag [
	"This is the main method for image save and / or quit.
	See senders."
	"WARNING: Current process will be killed. UI Process will be restarted"
	"Mark the changes file and close all files as part of #processShutdownList.
	If save is true, save the current state of this Smalltalk in the image file.
	If quit is true, then exit to the outer OS shell.
	The latter part of this method runs when resuming a previously saved image. This resume logic
	checks for a document file to process when starting up."
	"
	To test the full cleanup and startup procedures, evaluate:
		Smalltalk snapshot: false andQuit: false embedded: false clearAllClassState: true

	To test the cleanup done when saving the image, evaluate:
		Smalltalk snapshot: false andQuit: false embedded: false clearAllClassState: false
	"
	| activeProc |
	"If it is just about quitting, do it the simpler way."
	save not & quit ifTrue: [
		self quit.
		"Execution doesn't reach this point"
		^self.
	].
	self logSnapshot: save andQuit: quit.
	clearAllStateFlag ifTrue: [
		TranscriptWindow allInstancesDo: [ :each |
			each isInWorld ifTrue: [ each delete.]].
		UISupervisor ui tearDownDesktop.
		Transcript logToFile: false ].
	"These could be moved to some #shutDown"
	self stopLowSpaceWatcher.
	ProcessorScheduler stopBackgroundProcess.
	"Be sure all unreferenced objects are finalized as approriate.
	Do this again later, as close to the snapshot / quit primitive as possible."
	self garbageCollect.
	EphemeronFinalizationProcess ensureFinalizationComplete.
	EphemeronFinalizationProcess stopFinalizationProcess.
	activeProc := Processor activeProcess.
[
	| isARealStartup guiRootObject guiRootObjectClass userBaseDirectory |
	"Stuff needed to relaunch UI on startup"
	guiRootObjectClass := UISupervisor ui class.
	"Replace with this to create a new world at startup after 'saveAsNewVersion'"
	guiRootObject := clearAllStateFlag ifFalse: [ UISupervisor ui ].

	UISupervisor stopUIProcess.
	activeProc isTerminated ifFalse: [ activeProc terminate ].
	guiRootObject ifNotNil: [ guiRootObject releaseCachedState ].
	clearAllStateFlag ifTrue: [
		Processor processesDo: [ :p |
			(p name = 'Formerly: Morphic UI') ifTrue: [ p terminate ]]].
	MorphicCanvas allSubInstancesDo: [ :c |
		c nilState ].
	self processShutDownList: quit.

	"Clean Globals"
	userBaseDirectory := DirectoryEntry userBaseDirectory.
	self at: #Sensor put: nil.
	self closeSourceFiles.
	self at: #SourceFiles put: nil.
	self allClassesDo: [ :cls |
		cls releaseClassCachedState ].
	clearAllStateFlag ifTrue: [
		self allClassesDo: [ :cls |
			cls releaseClassState ]].
	"To keep cleaning stuff that shouldn't be saved..."
	clearAllStateFlag ifTrue: [
		Transcript clear.
		"Enable this call to actually see the image clean report."
		false ifTrue: [self printStuffToCleanOnImageSave]].
	(Cursor cursorAt: #writeCursor) activateCursor.
	"The snapshot primitive will run a GC, but any detected Ephemeron will not be finalized, and the finalization
	queue will be cleared by the VM both in running and snapshotted image. This means they will never be finalized.
	To avoid that, just before snapshot, do a GC.
	We also need to do #mournAllPending because TheFinalizationProcess has been already stopped. "
	self garbageCollect.
	EphemeronFinalizationProcess mournAllPending.
	"Do image save & quit as apropriate"
	isARealStartup := false.
	save ifTrue: [
		"The snapshot primitive answers false if it was just called to do the snapshot.
		But image startup is resumed by returning (again) from the primitive, but this time answering true.
		nil means saving image failed"
		isARealStartup := embeddedFlag
			ifTrue: [ self snapshotEmbeddedPrimitive ]
			ifFalse: [ self snapshotPrimitive ]].
	quit & (isARealStartup == false) ifTrue: [ self quitPrimitive ]. "Don't quit on nil (image save failed)"

	isARealStartup == true ifFalse: [ 									"Keep current #userBaseDirectory unless a real startup"
		DirectoryEntry setUserBaseDirectory: userBaseDirectory ].

	"If starting from absolute scratch, this would be a good time to recreate Global names"
	self at: #Sensor put: nil.
	self at: #SourceFiles put: (Array new: 2).
	self logStartupDebugAid: 'To #openSourceFiles'.
	self openSourceFiles.
	"Here, startup begins! (isARealStartup might be nil)"
	self logStartupDebugAid: 'To #initClassCachedState'.
	self allClassesDo: [ :cls | cls initClassCachedState ].
	self logStartupDebugAid: 'To #doStartUp:'.
	self doStartUp: isARealStartup == true.
	self logStartupDebugAid: 'To #spawnNewMorphicProcessFor: ', guiRootObject printString.
	UISupervisor spawnNewMorphicProcessFor: (guiRootObject ifNil: [ guiRootObject := guiRootObjectClass newWorld ]).
	isARealStartup == true
		ifTrue: [
			self logStartupDebugAid: 'To #checkIfAlreadyRunningOrStoppedNoExit'.
			self checkIfAlreadyRunningOrStoppedNoExit.
			"Some command line arguments need to be processed before starting the UI"
			self processCommandLineArguments: true.
			self assureStartupStampLogged ].
	clearAllStateFlag ifTrue: [
		UISupervisor whenUIinSafeState: [
			self logStartupDebugAid: 'To #recreateDefaultDesktop'.
			guiRootObject recreateDefaultDesktop; restoreDisplay ]]
	ifFalse: [
		UISupervisor whenUIinSafeState: [
			self logStartupDebugAid: 'To #restoreDisplay'.
			guiRootObject restoreDisplay ]].
	"It is needed to do this at least once on MacVMs, where it may not be needed later.
	For instance #forceToScreen: is usually not called in #displayWorldOn: if running on Mac."
	Display forceToScreen.
	isARealStartup == true
		ifTrue: [
			"If system is coming up (VM and image just started)"
			UISupervisor whenUIinSafeState: [
				self logStartupDebugAid: 'To #readAndApplyUserPrefs'.
				self readAndApplyUserPrefs.
				self logStartupDebugAid: 'To #processCommandLineArguments'.
				"Some command line arguments need to be processed after the UI is up, and in the UI process"
				self processCommandLineArguments: false.
				self logStartupDebugAid: 'To #launchApp'.
				AppLauncher launchApp ]]
		ifFalse: [
			"If we are just saving the image"
			UISupervisor whenUIinSafeState: [
				self logStartupDebugAid: 'To #readAndApplyUserPrefs'.
				self readAndApplyUserPrefs ]].
	"Now it's time to raise an error"
	isARealStartup ifNil: [ self logStartupError: 'Failed to write image file (disk full?)' ]
]
	forkAt: Processor timingPriority - 1
	named: 'Startup process'.
]

{ #category : 'snapshot and quit' }
SystemDictionary >> snapshotEmbeddedPrimitive [
	<primitive: 247>
	^nil "indicates error writing embedded image file"
]

{ #category : 'snapshot and quit' }
SystemDictionary >> snapshotMessageFor: save andQuit: quit [

	| dateAndTime |
	dateAndTime := DateAndTime now.
	^String streamContents: [ :stream |
		stream 
			nextPutAll: self tagHeader;
			nextPutAll: (self snapshotTagFor: save andQuit: quit);
			nextPutAll: self tagTail.
		stream nextPut: $(.
		dateAndTime date printOn: stream.
		stream space.
		dateAndTime time print24: true showSeconds: true on: stream.
		stream nextPut: $).
		stream
			space;
			nextPutAll: self imageName asFullFileEntry name;
			nextPutAll: ' priorSource: ';
			print: LastQuitLogPosition ].
]

{ #category : 'snapshot and quit' }
SystemDictionary >> snapshotPrimitive [
	"Primitive. Write the current state of the object memory on a file in the
	same format as the Smalltalk-80 release. The file can later be resumed,
	returning you to this exact state. Return normally after writing the file.
	Essential. See Object documentation whatIsAPrimitive."

	<primitive: 97>
	^nil "indicates error writing image file"
]

{ #category : 'snapshot and quit' }
SystemDictionary >> snapshotTag [
		
	^'SNAPSHOT'
]

{ #category : 'snapshot and quit' }
SystemDictionary >> snapshotTagFor: save andQuit: quit [
		
	^save
		ifTrue: [ quit
			ifTrue: [ self quitTag ]
			ifFalse: [ self snapshotTag ]]
		ifFalse: [ quit
			ifTrue: [ self quitNoSaveTag ]
			ifFalse: [ self nopTag ]]
]

{ #category : 'special objects' }
SystemDictionary >> specialNargsAt: anInteger  [
	"Answer the number of arguments for the special selector at: anInteger."

	^ (self specialObjectsArray at: 24) at: anInteger * 2
]

{ #category : 'special objects' }
SystemDictionary >> specialObjectsArray  "Smalltalk specialObjectsArray at: 1" [
	<primitive: 129>
	^ self primitiveFailed
]

{ #category : 'special objects' }
SystemDictionary >> specialSelectorAt: anInteger  [
	"Answer the special message selector from the interleaved specialSelectors array."

	^ (self specialObjectsArray at: 24) at: anInteger * 2 - 1
]

{ #category : 'special objects' }
SystemDictionary >> specialSelectorSize [
	"Answer the number of special selectors in the system."

	^ (self specialObjectsArray at: 24) size // 2
]

{ #category : 'special objects' }
SystemDictionary >> specialSelectors [
	"Return an array with flattened special selector - argument count pairs. VMMaker relies on this."
	
	^self specialObjectsArray at: 24
]

{ #category : 'profiling' }
SystemDictionary >> startProfiling [
	"Start profiling the virtual machine."

	<primitive: 252>

]

{ #category : 'startup' }
SystemDictionary >> startUpArguments [
	"To make all raw command line arguments available to applications"

	^startUpArguments
]

{ #category : 'startup' }
SystemDictionary >> startUpScriptAllArguments [
	"To make command line arguments to startup scripts available to them.
	Also include #lastCommandLineArguments"

	^ self startUpScriptArguments, self lastCommandLineArguments
]

{ #category : 'startup' }
SystemDictionary >> startUpScriptArguments [
	"To make command line arguments to startup scripts available to them"

	^startUpScriptArguments
]

{ #category : 'memory space' }
SystemDictionary >> stopLowSpaceWatcher [
	"Stop the process to watch for low-space conditions."
	"
	Smalltalk
		stopLowSpaceWatcher;
		installLowSpaceWatcher
	"

	self primSignalAtBytesLeft: 0.  "disable low-space interrupts"
	LowSpaceProcess ifNotNil: [LowSpaceProcess terminate].
	LowSpaceProcess := nil
]

{ #category : 'profiling' }
SystemDictionary >> stopProfiling [
	"Stop profiling the virtual machine."

	<primitive: 253>

]

{ #category : 'object serialization' }
SystemDictionary >> storeDataOn: aDataStream [
	"I don't get stored.  Use a DiskProxy"

	self error: 'use a DiskProxy to store me'
]

{ #category : 'snapshot and quit' }
SystemDictionary >> storeOn: aStream [
	self == Smalltalk ifTrue: [
		^ aStream nextPutAll: 'Smalltalk'].
	^ super storeOn: aStream
]

{ #category : 'accessing' }
SystemDictionary >> summary [
	"Needed by CodeFileBrowser to use Smalltalk as the 'case' source"
	^ self name
]

{ #category : 'ui' }
SystemDictionary >> systemCategoryFromUserWithPrompt: aString [
	"Prompt the user to select an existing system category (i.e. the ones that appear in the top left pane in the Browser window)"
	| allCats menuIndex |
	allCats := self organization categories sorted.
	menuIndex := (PopUpMenu labelArray: allCats) startUpWithCaption: aString.
	^ menuIndex = 0 ifTrue: [nil] ifFalse: [allCats at: menuIndex]
]

{ #category : 'sources, change log' }
SystemDictionary >> systemInformationString [
	"Identify software version"
	^ self versionString, String newLineString,
		self lastUpdateString, String newLineString,
		'Running at :', self imageName.
]

{ #category : 'snapshot and quit' }
SystemDictionary >> tagHeader [

	^ '----'
]

{ #category : 'snapshot and quit' }
SystemDictionary >> tagTail [

	^ self tagHeader
]

{ #category : 'housekeeping' }
SystemDictionary >> testDecompiler [
	"
	Smalltalk testDecompiler
	"
	"Decompiles the source for every method in the system, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same."
	| methodNode oldMethod newMethod badOnes oldCodeString n |
	badOnes := OrderedCollection new.
	'Decompiling all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :barBlock |
			n := 0.
			self allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n := n + 1) \\ 100 = 0 ifTrue: [ barBlock value: n ].
					oldMethod := cls compiledMethodAt: selector.
					oldCodeString := (cls decompilerClass new
						decompile: selector
						in: cls
						method: oldMethod) decompileString.
					methodNode := cls compilerClass new
						compile: oldCodeString
						in: cls
						notifying: nil
						ifFail: nil.
					newMethod := methodNode generate: #(0 0 0 0 ).
					oldCodeString =
						(cls decompilerClass new
							decompile: selector
							in: cls
							method: newMethod) decompileString ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: (MethodReference class: cls selector: selector) ]]]].
	self
		browseMessageList: badOnes
		name: 'Decompiler Discrepancies'.
]

{ #category : 'housekeeping' }
SystemDictionary >> testFormatter [
	"Smalltalk testFormatter"
	"Reformats the source for every method in the system, and then compiles that source and verifies that it generates identical code.
	The formatting used will be classic monochrome."
	| newCodeString methodNode oldMethod newMethod badOnes n |
	badOnes := OrderedCollection new.
	'Formatting all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :barBlock |
			n := 0.
			self allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n := n + 1) \\ 100 = 0 ifTrue: [ barBlock value: n ].
					newCodeString := cls compilerClass new
						format: (cls sourceCodeAt: selector)
						in: cls
						notifying: nil.
					methodNode := cls compilerClass new
						compile: newCodeString
						in: cls
						notifying: nil
						ifFail: nil.
					newMethod := methodNode generate: #(0 0 0 0 ).
					oldMethod := cls compiledMethodAt: selector.
					oldMethod = newMethod ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: cls name , ' ' , selector ]]]].
	self
		browseMessageList: badOnes
		name: 'Formatter Discrepancies'.
]

{ #category : 'housekeeping' }
SystemDictionary >> testFormatter2 [
	"Smalltalk testFormatter2"
	"Reformats the source for every method in the system, and then verifies that the order of source tokens is unchanged.
	The formatting used will be classic monochrome"
	| newCodeString badOnes n oldCodeString oldTokens newTokens |
	badOnes := OrderedCollection new.
	'Formatting all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :barBlock |
			n := 0.
			self allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n := n + 1) \\ 100 = 0 ifTrue: [ barBlock value: n ].
					oldCodeString := (cls sourceCodeAt: selector) asPlainString.
					newCodeString := cls compilerClass new
						format: oldCodeString
						in: cls
						notifying: nil.
					oldTokens := oldCodeString findTokens: Character separators.
					newTokens := newCodeString findTokens: Character separators.
					oldTokens = newTokens ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: cls name , ' ' , selector ]]]].
	self
		browseMessageList: badOnes
		name: 'Formatter Discrepancies'.
]

{ #category : 'snapshot and quit' }
SystemDictionary >> unbindExternalPrimitives [
	"Primitive. Force all external primitives to be looked up again afterwards. Since external primitives that have not found are bound for fast failure this method will force the lookup of all primitives again so that after adding some plugin the primitives may be found."
	<primitive: 570>
	"Do nothing if the primitive fails for compatibility with older VMs"
]

{ #category : 'housekeeping' }
SystemDictionary >> undeclaredReferences [
	"
	Smalltalk undeclaredReferences
	"
	| undeclaredReferences |
	
	undeclaredReferences := Dictionary new.
	self cleanOutUndeclared.
	Undeclared associationsDo: [ :undeclaredAssoc |
		undeclaredReferences
			at: undeclaredAssoc key
			put: (self allCallsOn: undeclaredAssoc) ].

	^undeclaredReferences
]

{ #category : 'retrieving' }
SystemDictionary >> unimplemented [
	"Answer an Array of each message that is sent by an expression in a method but is not implemented by any object in the system."

	| all unimplemented entry |
	all := IdentitySet new.
	self allBehaviorsDo: [ :cl | cl selectorsDo: [ :aSelector | all add: aSelector]].

	unimplemented := IdentityDictionary new.
	self allBehaviorsDo: [:cl |
		 cl selectorsDo: [:sel |
			(cl compiledMethodAt: sel) messagesDo: [ :m |
				(all includes: m) ifFalse: [
					entry := unimplemented at: m ifAbsent: [#()].
					entry := entry copyWith: (cl name, '>', sel).
					unimplemented at: m put: entry]]]].

	"remove some clutter from the result:"
	#(doPrimitive: primitiveFail success:) do: [ :sel |
		unimplemented removeKey: sel ifAbsent: nil].

	^ unimplemented
]

{ #category : 'code authors' }
SystemDictionary >> unknownContributors [
	"Answer a collection of authorInitials for whom there is code in the system 
	(either in core or in loaded packages), but we don't knwo their full name.
	Smalltalk unknownContributors
	"

	| all ok |
	all := self allContributors asSet.
	ok := (self knownInitialsAndNames collect: [ :pair | pair first ]) asSet.
	^(all difference: ok) asArray sort
]

{ #category : 'miscellaneous' }
SystemDictionary >> unloadModule: aString [
	"Primitive. Unload the given module.
	This primitive is intended for development only since some
	platform do not implement unloading of DLL's accordingly.
	Also, the mechanism for unloading may not be supported
	on all platforms."
	<primitive: 571>
	^self primitiveFailed
]

{ #category : 'special objects' }
SystemDictionary >> unregisterExternalObject: anObject [
	"Unregister the given object in the external objects array. Do nothing if it isn't registered."

	ExternalSemaphoreTable unregisterExternalObject: anObject
]

{ #category : 'shrinking' }
SystemDictionary >> unusedClasses [
	"Enumerates all classes in the system and returns a list of those that are apparently unused. A class is considered in use if it (a) has subclasses or (b) is referred to by some method or (c) has its name in use as a literal (but not in the same class) or (d) some instance is a global."
	"Smalltalk unusedClasses asSortedCollection"

	^ self allUnusedClassesWithout: #(#() #())
]

{ #category : 'shrinking' }
SystemDictionary >> unusedClassesAndMethodsWithout: classesAndMessagesPair [
	| classRemovals messageRemovals nClasses nMessages |
	"Accepts and returns a pair: {set of class names. set of selectors}.
	It is expected these results will be diff'd with the normally unused results."

	(classRemovals := IdentitySet new) addAll: classesAndMessagesPair first.
	(messageRemovals := IdentitySet new) addAll: classesAndMessagesPair second.
	nClasses := nMessages := -1.
	["As long as we keep making progress..."
	classRemovals size > nClasses or: [messageRemovals size > nMessages]]
		whileTrue:
			["...keep trying for bigger sets of unused classes and selectors."
			nClasses := classRemovals size.
			nMessages := messageRemovals size.
			Utilities informUser: 'Iterating removals ' ,
					(classesAndMessagesPair first isEmpty
						ifTrue: ['for baseline...']
						ifFalse: ['for ', classesAndMessagesPair first first, ' etc...']) , String newLineString ,
					nClasses printString , ' classes, ' , nMessages printString , ' messages.
|
|' "spacers move menu off cursor"
				during:
				[classRemovals addAll: (self allUnusedClassesWithout: {classRemovals. messageRemovals}).
				messageRemovals addAll: (self allUnSentMessagesWithout: {classRemovals. messageRemovals})]].
	^ {classRemovals. self allUnSentMessagesWithout: {classRemovals. messageRemovals}}
]

{ #category : 'memory space' }
SystemDictionary >> useUpMemory [
	"For testing the low space handler..."
	"Smalltalk installLowSpaceWatcher; useUpMemory"

	| lst |
	lst := nil.
	[true] whileTrue: [
		lst := Link nextLink: lst.
	].
]

{ #category : 'memory space' }
SystemDictionary >> useUpMemoryWithArrays  [
	"For testing the low space handler..."
	"Smalltalk installLowSpaceWatcher; useUpMemoryWithArrays"

	| b |  "First use up most of memory."
	b := String new: self bytesLeft - self lowSpaceThreshold - 100000.
	b := b.  "Avoid unused value warning"
	(1 to: 10000) collect: [:i | Array new: 10000]
]

{ #category : 'memory space' }
SystemDictionary >> useUpMemoryWithContexts  [
	"For testing the low space handler..."
	"Smalltalk installLowSpaceWatcher; useUpMemoryWithContexts"

	self useUpMemoryWithContexts
]

{ #category : 'memory space' }
SystemDictionary >> useUpMemoryWithTinyObjects  [
	"For testing the low space handler..."
	"Smalltalk installLowSpaceWatcher; useUpMemoryWithTinyObjects"

	| b |  "First use up most of memory."
	b := String new: self bytesLeft - self lowSpaceThreshold - 100000.
	b := b.  "Avoid unused value warning"
	(1 to: 10000) collect: [:i | BitBlt new]
]

{ #category : 'sources, change log' }
SystemDictionary >> userChangesAppendDo: aBlock [
	"Convenience. On write errors, write a message to Transcript and continue."

	[
		Utilities logsUserChanges ifTrue: [
			DirectoryEntry userChangesDirectory // self userChangesFileName appendStreamDo: aBlock ]]
		on: Error do: [ :ex |
			Transcript show: 'Can not log to User changes. '; show: ex  printString; newLine ].
]

{ #category : 'image, changes name' }
SystemDictionary >> userChangesFileName [
	"
	Smalltalk userChangesFileName
	"
	^(FileIOAccessor default baseNameFor: self imageName pathAndLocalName second), 
		(Preferences at: #userChangesFileNameExtension)
]

{ #category : 'housekeeping' }
SystemDictionary >> verifyChanges		"Smalltalk verifyChanges" [
	"Recompile all methods in the changes file."
	self allBehaviorsDo: [:class | class recompileChanges].

]

{ #category : 'sources, change log' }
SystemDictionary >> versionString [
	"Answer the version of this release."

	^SystemVersion current versionString
]

{ #category : 'miscellaneous' }
SystemDictionary >> vmFileName [
	"Answer the absolute file name of the the Smalltalk virtual machine"
	"
	Smalltalk vmFileName
	"

	| answer |
	"Primitive answers an instance of String containing the UTF-8 bytes. If not pure ASCII,
	contents are invalid! Convert it ASAP into a reasonable ByteArray + UnicodeString."
	answer := self primVmFileName.
	(String isAscii: answer) ifFalse: [
		ByteArray adoptInstance: answer.
		answer := UnicodeString fromUtf8Bytes: answer ].
	^answer
]

{ #category : 'miscellaneous' }
SystemDictionary >> vmOptionsDo: aBlock [
	"Repeatedly evaluate aBlock for each vm option specified by the commandline that started Cuis.
	aBlock has two arguments: the vm option itself and the index (position)
	
	Smalltalk vmOptionsDo: [ :option :i | {i. option} print ]
	"
	| i vmOption |
	i := -1.
	[vmOption := self getSystemAttribute: i.
	vmOption notNil ] whileTrue: [
		aBlock value: vmOption value: i.
		i := i-1 ]
]

{ #category : 'vm parameters' }
SystemDictionary >> vmParameterAt: parameterIndex [
	"parameterIndex is a positive integer corresponding to one of the VM's internal parameter/metric registers.
	 Answer with the current value of that register.  Primitive fails if parameterIndex is invalid or has no corresponding register.
	Answer nil if the VM doesn't provide the register, and the primitive fails.
	
	Also see #getVMParameters and #vmParameterAt:put: These 3 methods call the 
	same primitive, whose behavior depends on argument count:
		0 args:	return an Array of VM parameter values;
  		1 arg:	return the indicated VM parameter;
  		2 args:	set the VM indicated parameter.

	VM parameters are numbered as follows:
		1	byte size of old-space (read-only)
		2	byte size of young-space (read-only)
		3	byte size of object memory (read-only)
		4	nil (was allocationCount (read-only))
		5	nil (was allocations between GCs (read-write)
		6	survivor count tenuring threshold (read-write)
		7	full GCs since startup (read-only)
		8	total milliseconds in full GCs since startup (read-only)
		9	Scavenging GCs
		10	total milliseconds in scavenging GCs since startup (read-only) since startup (read-only)
		11	tenures of surving objects since startup/last write (read-write)
		12	event trace mask; if 1 << eventType is set in the mask then event received by primGetNextEvent: will be printed to stderr.
		13	VM ticker start microseconds (Croquet/QwaqVM only)
		14	VM ticker count (Croquet/QwaqVM only)
		15	VM ticker call count (Croquet/QwaqVM only)
		16	total microseconds at idle since start-up (if non-zero)
		17	fraction of the code zone to use (Sista only; used to control code zone use to preserve sendAndBranchData on counter tripped callback)
		18	total milliseconds in full GC compaction since startup (a portion of parameter 8)
		19	scavenge threshold, the effective size of eden.  When eden fills to the threshold a scavenge is scheduled.
		20	utc microseconds at VM start-up (actually at time initialization, which precedes image load).
		21	root/remembered table size (occupancy) (read-only)
		22	root table overflows since startup (read-only)
		23	bytes of extra memory to reserve for VM buffers, plugins, etc (stored in image file header).
		24	memory threshold above which to shrink object memory (read-write)
		25	ammount to grow by when growing object memory (read-write)
		26	interruptChecksEveryNms - force an ioProcessEvents every N milliseconds (read-write)
		27	number of times mark loop iterated for current IGC/FGC (read-only) includes ALL marking
		28	number of times sweep loop iterated for current IGC/FGC (read-only)
		29	number of times make forward loop iterated for current IGC/FGC (read-only)
		30	number of times compact move loop iterated for current IGC/FGC (read-only)
		31	number of grow memory requests (read-only)
		32	number of shrink memory requests (read-only)
		33	number of root table entries used for current IGC/FGC (read-only)
		34	bytes allocated in total since start-up or reset (read-write)
		35	number of survivor objects after current IGC/FGC (read-only)
		36	millisecond clock when current IGC/FGC completed (read-only)
		37	number of marked objects for Roots of the world, not including Root Table entries for current IGC/FGC (read-only)
		38	milliseconds taken by current IGC (read-only)
		39	Number of finalization signals for Weak Objects pending when current IGC/FGC completed (read-only)
		40	BytesPerOop for this image
		41	imageFormatVersion for the VM
		42	number of stack pages in use (Cog Stack VM only, otherwise nil)
		43	desired number of stack pages (stored in image file header, max 65535; Cog VMs only, otherwise nil)
		44	size of eden, in bytes (Cog VMs only, otherwise nil)
		45	desired size of eden, in bytes (stored in image file header; Cog VMs only, otherwise nil)
		46	size of machine code zone, in bytes (stored in image file header; Cog JIT VM only, otherwise nil)
		47	desired size of machine code zone, in bytes (applies at startup only, stored in image file header; Cog JIT VM only)
		48	various properties stored in the image header (that instruct the VM) as an integer encoding an array of bit flags. 
			Bit 0: if set, tells the VM that the image's Process class has 'threadAffinity' as its 5th inst var 
					(after nextLink, suspendedContext, priority & myList)
			Bit 1: on Cog JIT VMs asks the VM to set the flag bit in interpreted methods. This is useful when profiling a system to set the number
					of literals beyond which CompiledMethods are not jitted.
			Bit 2: if set, preempting does not yield. If not set, preempting a process puts it at the back of the run queue, yielding to all other
					process of the same priority. This was the Smalltalk-80 behavior, but it makes sharing structures between processes of 
					the same priority unnecesarily more complex.
			Bit 3: in a VM that supports muilt-threaded FFI, if set, the Window system will only be accessed from the first VM thread:
					'noThreadingOfGUIThread'
			Bit 4: in a Spur vm, if set, new finalization enabled. Enable for images with Ephemeron support.
					causes weaklings and ephemerons to be queued individually for finalization
			Bit 5: If set, wheel events will be delivered as such and not mapped to arrow key events.
					If unset, create fake keyboard events for wheel events. 
			Bit 6: See #numericPrimsMixArithmetic:
				If unset, arithmetic primitives won't perform conversion in case of mixed SmallInteger/Float, and fail, letting the image decide what to do.
				If set, the VM will make the conversion.
			Bit 7: if set use the unix epoch for dates and times in files (FilePlugin / FileAttributesPlugin) / or is it about UTC???- If set UTC seconds from epoch, rather than local seconds
			Bit 8: if set upscaleDisplayIfHighDPI
			Bit 9: See #numericPrimsMixComparison:
				If unset, numeric comparison primitives won't perform conversion in case of mixed SmallInteger/Float, and fail, letting the image decide what to do.
				If set, the VM will make the conversion, meaning that `SmallInteger maxVal asFloat = SmallInteger maxVal` could evaluate to false.
		49	the size of the external semaphore table (read-write; Cog VMs only)
		50-51 reserved for VM parameters that persist in the image (such as eden above)
		52	root (remembered) table maximum size (read-only)
		53	the number of oldSpace segments (Spur only, otherwise nil)
		54	total size of free old space (Spur only, otherwise nil)
		55	ratio of growth and image size at or above which a GC will be performed post scavenge
		56	number of process switches since startup (read-only)
		57	number of ioProcessEvents calls since startup (read-only)
		58	number of forceInterruptCheck (Cog VMs) or quickCheckInterruptCalls (non-Cog VMs) calls since startup (read-only)
		59	number of check event calls since startup (read-only)
		60	number of stack page overflows since startup (read-only; Cog VMs only)
		61	number of stack page divorces since startup (read-only; Cog VMs only)
		62	number of machine code zone compactions since startup (read-only; Cog VMs only)
		63	milliseconds taken by machine code zone compactions since startup (read-only; Cog VMs only)
		64	current number of machine code methods (read-only; Cog VMs only)		
		65	In newer Cog VMs a set of flags describing VM features,
				if non-zero bit 0 implies multiple bytecode set support;
				if non-zero bit 1 implies read-only object support;
				if non-zero bit 2 implies the VM suffers from using an ITIMER heartbeat (if 0 it has a thread that provides the heartbeat)
			(read-only; Cog VMs only; nil in older Cog VMs, a boolean answering multiple bytecode support in not so old Cog VMs)
				if non-zero bit 3 implies the VM supports cross-platform BIT_IDENTICAL_FLOATING_POINT arithmetic
				if non-zero bit 4 implies the VM can catch exceptions in FFI calls and answer them as primitive failures
				if non-zero bit 5 implies the VM's suspend primitive backs up the process to before the wait if it was waiting on a condition variable
		66	the byte size of a stack page in the stack zone  (read-only; Cog VMs only)
		67	the maximum allowed size of old space in bytes, 0 implies no internal limit (Spur VMs only).
		68	the average number of live stack pages when scanned by GC (at scavenge/gc/become et al)
		69	the maximum number of live stack pages when scanned by GC (at scavenge/gc/become et al)
		70	the value of VM_PROXY_MAJOR (the interpreterProxy major version number)
		71	the value of VM_PROXY_MINOR (the interpreterProxy minor version number)
		72	total milliseconds in full GCs Mark phase since startup (read-only)
		73	total milliseconds in full GCs Sweep phase since startup (read-only, can be 0 depending on compactors)
		74	maximum pause time due to segment allocation
		75	whether the arithmetic primitives perform conversion in case of mixed SmallInteger/Float (true) or fail (false)
		76	the minimum unused headroom in all stack pages; Cog VMs only
		77	whether the numeric comparison primitives perform conversion in case of mixed SmallInteger/Float (true) or fail (false)"

	<primitive: 254>
	(parameterIndex isInteger
	 and: [parameterIndex > 0]) ifTrue:
		[^nil].
	self primitiveFailed
]

{ #category : 'vm parameters' }
SystemDictionary >> vmParameterAt: parameterIndex put: newValue [
	"parameterIndex is a positive integer corresponding to one of the VM's internal
	parameter/metric registers.  Store newValue (a positive integer) into that
	register and answer with the previous value that was stored there.
	Fail if newValue is out of range, if parameterIndex has no corresponding
	register, or if the corresponding register is read-only.

	As of late 2020 the parameters which can be set are
		5	allocations between GCs (read-write; nil in Cog VMs)
		6	survivor count tenuring threshold (read-write)
		11	tenures of surving objects since startup/last write (read-write)
		12	event trace mask; if 1 << eventType is set in the mask then event received by primGetNextEvent: will be printed to stderr.
		17	proportion of code zone available for use (Sista VMs only)
		23	bytes of extra memory to reserve for VM buffers, plugins, etc.
		24	memory threshold above whichto shrink object memory (read-write)
		25	memory headroom when growing object memory (read-write)
		26	interruptChecksEveryNms - force an ioProcessEvents every N milliseconds (read-write)
		34	bytes allocated in total since start-up or reset (read-write)
		43	desired number of stack pages (stored in image file header, max 65535; Cog VMs only, otherwise nil)
		45	desired size of eden, in bytes (stored in image file header; Cog VMs only, otherwise nil)
		47	desired size of machine code zone, in bytes (applies at startup only, stored in image file header; Cog JIT VM only)
		48	various properties of the Cog VM as an integer encoding an array of bit flags.
			Bit 0: tells the VM that the image's Process class has 'threadAffinity' as its 5th inst var (after nextLink, suspendedContext, priority & myList)
			Bit 1: on Cog JIT VMs asks the VM to set the flag bit in interpreted methods
			Bit 2: if set, preempting a process puts it to the head of its run queue, not the back,
					i.e. preempting a process by a higher priority one will not cause the preempted process to yield
						to others at the same priority.
			Bit 3: in a muilt-threaded VM, if set, the Window system will only be accessed from the first VM thread
			Bit 4: in a Spur vm, if set, causes weaklings and ephemerons to be queued individually for finalization
			Bit 5: (on VMs that support it) if set, implies wheel events will be delivered as such and not mapped to arrow key events
			Bit 6: (on VMs that support it). See #numericPrimsMixArithmetic:
				If unset, arithmetic primitives won't perform conversion in case of mixed SmallInteger/Float, and fail, letting the image decide what to do.
				If set, the VM will make the conversion.
				(on VMs that don't support it, those primitives will fail in those cases)
			Bit 9: (on VMs that support it). See #numericPrimsMixComparison:
				If unset, numeric comparison primitives won't perform conversion in case of mixed SmallInteger/Float, and fail, letting the image decide what to do.
				If set, the VM will make the conversion, meaning that `SmallInteger maxVal asFloat = SmallInteger maxVal` could evaluate to false.
		49	the size of the external semaphore table (read-write; Cog VMs only)
				(on VMs that don't support it, those primitives will fail in those cases)
		55	ratio of growth and image size at or above which a GC will be performed post scavenge (Spur only, otherwise nil)
		67	the maximum allowed size of old space in bytes, 0 implies no internal limit (Spur only).
		68	the average number of live stack pages when scanned by GC (at scavenge/gc/become et al)
		69	the maximum number of live stack pages when scanned by GC (at scavenge/gc/become et al)
		74 maximum pause time due to segment allocation
		75 whether arithmetic primitives will do mixed type arithmetic; if false they fail for different receiver and argument types"

	<primitive: 254>
	self primitiveFailed
]

{ #category : 'image, changes name' }
SystemDictionary >> vmPath [
	"Answer the path for the directory containing the Smalltalk virtual machine. Return the empty string if this primitive is not implemented."
	"
	Smalltalk vmPath
	"

	| answer |

	"Primitive answers an instance of String containing the UTF-8 bytes. If not pure ASCII,
	contents are invalid! Convert it ASAP into a reasonable ByteArray + UnicodeString."
	answer := self primVmPath.
	(String isAscii: answer) ifFalse: [
		ByteArray adoptInstance: answer.
		answer := UnicodeString fromUtf8Bytes: answer ].

	"On some setups (Cog VM in Git Bash under Windows)
		Smalltalk primVmPath 
				answer begins with 'c:\' (lowercase)
	but 
		(FileDirectory on: '') primLookupEntryIn: '' index: 1
				answer is  #('C:' 0 0 true 0) (uppercase)
	Make Windows drive letters be capitalized!
	"
	(answer size > 1 and: [
		answer first isLowercase and: [
			answer second isDriveSeparator ]]) ifTrue: [
					answer := answer capitalized ].
	^answer
]

{ #category : 'miscellaneous' }
SystemDictionary >> vmVersion	"Smalltalk vmVersion" [
	"Return a string identifying the interpreter version"
	^self getSystemAttribute: 1004
]

{ #category : 'miscellaneous' }
SystemDictionary >> voidCogVMState [
	"Void any internal caches the VM maintains other than the method lookup caches.
		 These comprise
				- the stack zone, where method activations are stored, and
				- the machine code zone, where the machine code form of CompiledMethods is held."
	<primitive: 214>
	^self primitiveFailed

	"Time millisecondsToRun: [Smalltalk voidCogVMState]"
	"(1 to: 10) collect: [:ign| Time millisecondsToRun: [Smalltalk voidCogVMState]]"
]

{ #category : 'startup - recover user changes' }
SystemDictionary >> warnAndOfferLastUserChanges [
"
Smalltalk warnAndOfferLastUserChanges
"
 	| userChanges |
	PreviousUserChangesName ifNil: [
		self notify: 
			self currentChangesName asFullFileEntry pathName, ' is not clean.', String newLineString, String newLineString,
			'- Cuis may already be running (and running it twice may lead to .changes file corruption).', String newLineString,
			'		Suggestion: Exit this session and resume work on the other, already running, one.', String newLineString, String newLineString,
			'- Cuis may have crashed in a previous run.', String newLineString,
			'		However the appropriate User Changes file could not be automatically determined.', String newLineString, String newLineString,
			'- The .changes file may have been accidentally modified.', String newLineString,
			'		Suggestion: Exit this session and recover it using the `git checkout` command.',String newLineString, String newLineString,
			'Proceed with caution.' .
		^ self ].
		
	userChanges := DirectoryEntry userChangesDirectory // PreviousUserChangesName.
	self notify: 
		self currentChangesName asFullFileEntry pathName, ' is not clean.', String newLineString, String newLineString,
		'- Cuis may already be running (and running it twice may lead to .changes file corruption).', String newLineString,
		'		Suggestion: Exit this session and resume work on the other, already running, one.', String newLineString, String newLineString,
		'- Cuis may have crashed in a previous run.', String newLineString,
		'		Proceed to review and recover code from last saved User Changes file:', String newLineString,
		'		', userChanges pathName, String newLineString, String newLineString,
		'- The .changes file may have been accidentally modified.', String newLineString,
		'		Suggestion: Exit this session and recover it using the `git checkout` command.',String newLineString, String newLineString,
		'Proceed with caution.' .
	ChangeList browseCondensedChanges: userChanges.
]

{ #category : 'startup - restore lost changes' }
SystemDictionary >> withChangesFileDo: aBlock [

	^self currentChangesName asFullFileEntry readStreamDo: aBlock
]

{ #category : 'vm parameters' }
SystemDictionary >> wordSize [
	"Answer the size in bytes of an object pointer or word in the object memory.
	Smalltalk wordSize
	"

	^self vmParameterAt: 40
]

{ #category : 'sources, change log' }
SystemDictionary >> writeSystemTimeStamp: aStream  [
	"Writes system version and current time on stream aStream."

	| dateTime |
	dateTime := DateAndTime now.
	aStream
		nextPutAll: 'From ';
		nextPutAll: self versionString;
		nextPutAll: ' [';
		nextPutAll: self lastUpdateString;
		nextPutAll: '] on '.
	dateTime date printOn: aStream.
	aStream
		nextPutAll: ' at '.
	dateTime time print24: false showSeconds: true on: aStream
]
