"
My instances describe the behavior of other objects. I provide the minimum state necessary for compiling methods, and creating and running instances. Most objects are created as instances of the more fully supported subclass, Class, but I am a good starting point for providing instance-specific behavior (as in Metaclass).
"
Class {
	#name : 'Behavior',
	#superclass : 'Object',
	#instVars : [
		'superclass',
		'methodDict',
		'format'
	],
	#classVars : [
		'ObsoleteSubclasses'
	],
	#category : 'Kernel-Classes',
	#package : 'Kernel',
	#tag : 'Classes'
}

{ #category : 'testing' }
Behavior class >> canZapMethodDictionary [
	"Return false since zapping the method dictionary of Behavior class or its subclasses will cause the system to fail."
	^false
]

{ #category : 'class initialization' }
Behavior class >> releaseClassCachedState [
	ObsoleteSubclasses ifNotNil: [
		ObsoleteSubclasses ifEmpty: [
			ObsoleteSubclasses := nil ]]
]

{ #category : 'accessing method dictionary' }
Behavior >> >> selector  [
	"Answer the compiled method associated with the argument, selector (a 
	Symbol), a message selector in the receiver's method dictionary. If the 
	selector is not in the dictionary, create an error notification."

	^self compiledMethodAt: selector 

]

{ #category : 'methods' }
Behavior >> addMethodsTo: methodsReferencingLiteral thatReferExplicitlyTo: aLiteral [
	
	| selectors |
	
	selectors := self whichSelectorsReferExplicitlyTo: aLiteral.
	selectors do: [ :sel | methodsReferencingLiteral add: (MethodReference class: self selector: sel) ]
]

{ #category : 'methods' }
Behavior >> addMethodsTo: methodsReferencingLiteral thatReferTo: aLiteral [

	(self whichSelectorsReferTo: aLiteral)
		do: [ :sel |
			methodsReferencingLiteral add: (MethodReference class: self selector: sel) ]
]

{ #category : 'obsolete subclasses' }
Behavior >> addObsoleteSubclass: aClass [
	"Weakly remember that aClass was a subclass of the receiver and is now obsolete"

	ObsoleteSubclasses ifNil: [
		ObsoleteSubclasses := WeakIdentitySet new ].
	ObsoleteSubclasses add: aClass.
]

{ #category : 'accessing instances and variables' }
Behavior >> addReferencesOf: anInstVarName at: anInstVarIndex to: references [

	| reference |
	
	self methodsDo: [ :aMethod |
		(aMethod accessorDescriptionOf: anInstVarName at: anInstVarIndex) ifNotEmpty: [ :description |
			reference := MethodReference method: aMethod.
			reference prefixStringVersionWith: description.
			references add: reference ]].
	
]

{ #category : 'method dictionary' }
Behavior >> addSelectorSilently: selector withMethod: compiledMethod  [
	"Add the message selector with the corresponding compiled method to the 
	receiver's method dictionary.
	Do this without sending system change notifications"

	| oldMethodOrNil |
	oldMethodOrNil := self lookupSelector: selector.
	self methodDict at: selector put: compiledMethod.

	"Now flush Squeak's method cache, either by selector or by method"
	oldMethodOrNil ifNotNil: [oldMethodOrNil flushCache].
	selector flushCache.
]

{ #category : 'methods' }
Behavior >> addTo: aSet methodsThatReferInHierarchyTo: aSymbol [
	
	self withAllSuperAndSubclassesDo: [ :class |
		class addMethodsTo: aSet thatReferExplicitlyTo: aSymbol ] 
		
]

{ #category : 'instance creation' }
Behavior >> adoptInstance: anInstance [
	"Change the class of anInstance to me.
	Primitive (found in Cog and new VMs)  follows the same rules as primitiveChangeClassTo:, but returns the class rather than the modified instance"

	<primitive: 160 error: ec>
	anInstance primitiveChangeClassTo: self basicNew.
	^self
]

{ #category : 'accessing instances and variables' }
Behavior >> allAccessesTo: instVarName  [

	| references instVarIndex |

	instVarIndex := self indexOfInstanceVariable: instVarName.
	references := SortedCollection sortBlock: [ :left :right | left stringVersion <= right stringVersion ].
	
	self withAllSubAndSuperclassesDo: [:class |  class addReferencesOf: instVarName at: instVarIndex to: references ].
	
	^ references 
]

{ #category : 'accessing instances and variables' }
Behavior >> allBroadAccessesTo: instVarName [

	| references instVarIndex definingClass |

	definingClass := self whichClassDefinesInstanceVariable: instVarName ifNone: [ ^#() ].
	instVarIndex := self indexOfInstanceVariable: instVarName.
	references := SortedCollection sortBlock: [ :left :right | left stringVersion <= right stringVersion ].

	definingClass withAllSubclassesDo: [ :class | class addReferencesOf: instVarName at: instVarIndex to: references ].

	^ references 
]

{ #category : 'system-support' }
Behavior >> allCallsOn [
	"Answer a SortedCollection of all the methods that refer to me by name or 
	as part of an association in a global dict."
	"
	^ (Smalltalk
		allCallsOn: (Smalltalk associationAt: self theNonMetaClass name))
		, (Smalltalk allCallsOn: self theNonMetaClass name)
	"

	^ Smalltalk allCallsOn: self theNonMetaClass name
]

{ #category : 'accessing instances and variables' }
Behavior >> allClassVarNames [
	"Answer a Set of the names of the receiver's and the receiver's ancestor's
	class variables."

	self subclassResponsibility 
]

{ #category : 'accessing instances and variables' }
Behavior >> allInstVarNames [
	"Answer an Array of the names of the receiver's instance variables. The 
	Array ordering is the order in which the variables are stored and 
	accessed by the interpreter."

	^superclass
		ifNotNil: [superclass allInstVarNames , self instVarNames]
		ifNil: [self instVarNames copy]	"Guarantee a copy is answered."
]

{ #category : 'accessing instances and variables' }
Behavior >> allInstances [
	"Answer all instances of the receiver."
	<primitive: 177>
	self primitiveFailed
]

{ #category : 'enumerating' }
Behavior >> allInstancesDo: aBlock [
	"Evaluate aBlock with each of the current instances of the receiver."
	self allInstances do: aBlock
]

{ #category : 'instances' }
Behavior >> allInstancesOrNil [
	"Answer all instances of the receiver, or nil if the primitive
	 fails, which it may be due to being out of memory."
	<primitive: 177>
	^nil
]

{ #category : 'user interface' }
Behavior >> allLocalCallsOn: aSymbol [
	"Answer a SortedCollection of all the methods that call on aSymbol, anywhere in my class hierarchy."

	| aSet cls |
	
	aSet := Set new.
	cls := self theNonMetaClass.
	
	cls addTo: aSet methodsThatReferInHierarchyTo: aSymbol.
	cls class addTo: aSet methodsThatReferInHierarchyTo: aSymbol.
	
	^aSet
]

{ #category : 'accessing instances and variables' }
Behavior >> allRegularInstVarNames [
	"Answer an Array of the names of the receiver's instance variables. The
	Array ordering is the order in which the variables are stored and
	accessed by the interpreter.

	Quite like asking #allInstVarNames, but do not include Behavior state (i.e. Smalltalk internals)"

	^ (self == ProtoObject class or: [ superclass isNil ])
		ifTrue: [self instVarNames copy]	"Guarantee a copy is answered."
		ifFalse: [superclass allRegularInstVarNames , self instVarNames].
]

{ #category : 'accessing method dictionary' }
Behavior >> allSelectors [
	"Answer all selectors understood by instances of the receiver"

	| coll |
	coll := OrderedCollection new.
	self withAllSuperclasses do:
		[:aClass | coll addAll: aClass selectors].
	^ coll asIdentitySet
]

{ #category : 'accessing instances and variables' }
Behavior >> allSubInstances  [
	"Answer a list of all current instances of the receiver and all of its subclasses."
	| aCollection |
	aCollection := OrderedCollection new.
	self allSubInstancesDo:
		[:x | x == aCollection ifFalse: [aCollection add: x]].
	^ aCollection
]

{ #category : 'enumerating' }
Behavior >> allSubInstancesDo: aBlock  [
	"Evaluate the argument, aBlock, for each of the current instances of the 
	receiver and all its subclasses."

	self allInstancesDo: aBlock.
	self allSubclassesDo: [:sub | sub allInstancesDo: aBlock]
]

{ #category : 'accessing class hierarchy' }
Behavior >> allSubclasses [
	"A breadth-first iterative algorithm. Significantly faster than a recursive, depth-first implementation."

	| answer finger fingerLimit each |
	answer := OrderedCollection new.
	self subclassesDo: [:some | answer add: some].
	finger := 0.
	fingerLimit := answer size.
	[finger < fingerLimit] whileTrue:
		[
			finger + 1 to: fingerLimit do:
				[:index |
					each := answer at: index.
					each subclassesDo: [:some | answer add: some]
				].
			finger := fingerLimit.
			fingerLimit := answer size.
		].
	^answer
]

{ #category : 'enumerating' }
Behavior >> allSubclassesDo: aBlock  [
	"Evaluate the argument, aBlock, for each of the receiver's subclasses."

	self subclassesDo: 
		[:cl | 
		aBlock value: cl.
		cl allSubclassesDo: aBlock]
]

{ #category : 'accessing class hierarchy' }
Behavior >> allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level  [
	"Walk the tree of subclasses, giving the class and its level.
	Sort nicely."

	^ self allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level sortByCategory: false
]

{ #category : 'accessing class hierarchy' }
Behavior >> allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level sortByCategory: sortByCategoryBoolean [
	"Walk the tree of subclasses, giving the class and its level.
	 Sort nicely: major order is alphabetical by category name, and within a category alphabetical by class name.
	 Earlier versions used the order of the category within the system organization, but that gives different orders
	 when programmers reorder the system categories to suit their wishes. Since this order decides the order of
	 package file-outs, ignoring the speicifc system category order and using its alphabetical order is important
	 for stable package file-out. eem; '24/6/17"

	classAndLevelBlock value: self value: level.
	self == Class ifTrue:  [^ self].  "Don't visit all the metaclasses"
	"Visit subclasses in alphabetical order, either by category and name, or just by name"
	self subclasses
		sort: [ :a :b |
			sortByCategoryBoolean
				ifTrue: [ (a category ifNil: ['']) < (b category ifNil: ['']) or: [ a category = b category and: [ a name <= b name ]]]
				ifFalse: [ a name <= b name ]
			];
		do: [ :subclass |
			subclass
				allSubclassesWithLevelDo: classAndLevelBlock
				startingLevel: level + 1
				sortByCategory: sortByCategoryBoolean ]
]

{ #category : 'accessing class hierarchy' }
Behavior >> allSuperclasses [
	"Answer an OrderedCollection of the receiver's and the receiver's  
	ancestor's superclasses. The first element is the receiver's immediate  
	superclass, followed by its superclass and subsequent superclasses,
	and proceeding as long as there is a non-nil superclass."

	| answer pivot |
	answer := OrderedCollection new.
	pivot := superclass.
	[pivot == nil] whileFalse:
		[
			answer add: pivot.
			pivot := pivot superclass
		].
	^answer
]

{ #category : 'enumerating' }
Behavior >> allSuperclassesDo: aBlock  [
	"Evaluate the argument, aBlock, for each of the receiver's superclasses."

	superclass ifNotNil: [
		aBlock value: superclass.
		superclass allSuperclassesDo: aBlock]
]

{ #category : 'accessing class hierarchy' }
Behavior >> allSuperclassesUpTo: aSuperclass [
	
	| superclasses |
	
	^ superclass = aSuperclass
		ifTrue: [ OrderedCollection with: aSuperclass]
		ifFalse: [superclasses := superclass allSuperclassesUpTo: aSuperclass.
			superclasses addFirst: superclass.
			superclasses]
]

{ #category : 'testing method dictionary' }
Behavior >> allUnsentMessages [
	"Answer an array of all the messages defined by the receiver that are not sent anywhere in the system.  5/8/96 sw"

	^ Smalltalk allUnSentMessagesIn: self selectors
]

{ #category : 'accessing instances and variables' }
Behavior >> allowsSubInstVars [
	"Classes that allow instances to change classes among its subclasses will want to override this and return false, so inst vars are not accidentally added to its subclasses."

	^ true
]

{ #category : 'private' }
Behavior >> basicCompile: code notifying: requestor trailer: bytes ifFail: failBlock [
	"Compile code without logging the source in the changes file"

	^self basicCompile: code notifying: requestor trailer: bytes ifFail: failBlock optimizeSpecialSends: true
]

{ #category : 'private' }
Behavior >> basicCompile: code notifying: requestor trailer: bytes ifFail: failBlock optimizeSpecialSends: doOptimizeSpecialSends [
	"Compile code without logging the source in the changes file"

	| methodNode |
	methodNode := self compilerClass new
				compile: code
				in: self
				notifying: requestor
				ifFail: failBlock
				optimizeSpecialSends: doOptimizeSpecialSends.
	methodNode encoder requestor: requestor.
	^ CompiledMethodWithNode generateMethodFromNode: methodNode trailer: bytes.
]

{ #category : 'instance creation' }
Behavior >> basicNew [
	"Primitive. Answer an instance of the receiver (which is a class) with no 
	 indexable variables. Fail if the class is indexable. Essential. See Object 
	 documentation whatIsAPrimitive.
	
	 If the primitive fails because space is low then the scavenger will run
	 before the method is activated.  Check that space was low and retry
	 via handleFailingBasicNew if so."

	<primitive: 70 error: ec>
	ec == #'insufficient object memory' ifTrue:
		[^self handleFailingBasicNew].
	self isVariable ifTrue: [^self basicNew: 0].
	self primitiveFailed
]

{ #category : 'instance creation' }
Behavior >> basicNew: sizeRequested [
	"Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive.
	
	 If the primitive fails because space is low then the scavenger will run before the
	 method is activated.  Check args and retry via handleFailingBasicNew: if they're OK."

	<primitive: 71 error: ec>
	(ec == #'insufficient object memory' or: [ec == #'bad argument']) ifTrue:
		[^self handleFailingBasicNew: sizeRequested].
	self isVariable ifFalse:
		[self error: self printString, ' cannot have variable sized instances'].
	self primitiveFailed
]

{ #category : 'private' }
Behavior >> basicRemoveSelector: selector  [
	"Assuming that the argument, selector (a Symbol), is a message selector 
	in my method dictionary, remove it and its method."

	| oldMethod |
	oldMethod := self methodDict at: selector ifAbsent: [^ self].
	self methodDict removeKey: selector.

	"Now flush Squeak's method cache, either by selector or by method"
	oldMethod flushCache.
	selector flushCache.
]

{ #category : 'accessing' }
Behavior >> binding [
	^ nil -> self
]

{ #category : 'accessing' }
Behavior >> bindingOf: varName [
	"Answer the binding of some variable resolved in the scope of the receiver"
	^superclass bindingOf: varName
]

{ #category : 'accessing instances and variables' }
Behavior >> byteSizeOfInstance [
	"Answer the total memory size of an instance of the receiver."

	<primitive: 181 error: ec>
	self isVariable ifTrue:
		[^self byteSizeOfInstanceOfSize: 0].
	self primitiveFailed
]

{ #category : 'accessing instances and variables' }
Behavior >> byteSizeOfInstanceOfSize: basicSize [
	"Answer the total memory size of an instance of the receiver
	 with the given number of indexable instance variables."

	<primitive: 181 error: ec>
	self isVariable
		ifTrue: "If the primitive overflowed answer a close approximation"
			[(basicSize isInteger
			  and: [basicSize >= 16r1000000]) ifTrue:
				[^2 * (self byteSizeOfInstanceOfSize: basicSize + 1 // 2)
				   - (self byteSizeOfInstanceOfSize: 0)]]
		ifFalse:
			[basicSize = 0 ifTrue:
				[^self byteSizeOfInstance]].
	self primitiveFailed
]

{ #category : 'testing method dictionary' }
Behavior >> canUnderstand: selector  [
	"Answer whether the receiver can respond to the message whose selector 
	is the argument. The selector can be in the method dictionary of the 
	receiver's class or any of its superclasses."

	(self includesSelector: selector) ifTrue: [^true].
	superclass ifNil: [^false].
	^superclass canUnderstand: selector
]

{ #category : 'testing' }
Behavior >> canZapMethodDictionary [
	"Return true if it is safe to zap the method dictionary on #obsolete"
	^true
]

{ #category : 'accessing' }
Behavior >> classBindingOf: varName [
	"Answer the binding of some variable resolved in the scope of the receiver's class"
	^self bindingOf: varName
]

{ #category : 'accessing' }
Behavior >> classDepth [

	superclass ifNil: [^ 1].
	^ superclass classDepth + 1
]

{ #category : 'accessing instances and variables' }
Behavior >> classVarNames [
	"Answer a Set of the receiver's class variable names.  Since the receiver does
	not retain knowledge of class variables, the method fakes it by creating an empty set."

	^Set new
]

{ #category : 'compiling' }
Behavior >> compile: code  [
	"Compile the argument, code, as source code in the context of the 
	receiver. Create an error notification if the code can not be compiled. 
	The argument is either a string or an object that converts to a string or a 
	PositionableStream on an object that converts to a string."

	^self compile: code notifying: nil
]

{ #category : 'compiling' }
Behavior >> compile: code notifying: requestor  [
	"Compile the argument, code, as source code in the context of the 
	receiver and install the result in the receiver's method dictionary. The 
	second argument, requestor, is to be notified if an error occurs. The 
	argument code is either a string or an object that converts to a string or 
	a PositionableStream. This method also saves the source code."
	
	| methodAndNode sel compiled updatedCode |
	methodAndNode := self
		basicCompile: code
		notifying: requestor
		trailer: self defaultMethodTrailer
		ifFail: [^nil].
	sel := methodAndNode selector.
	compiled := methodAndNode method.
	(self includesSelector: sel) ifTrue: [
		((self compiledMethodAt: sel) propertyValueAt: #libraryName) ifNotNil: [ :libName |
			compiled propertyValueAt: #libraryName put: libName ]].
	"Re-fetching source code may be needed if, for example, some variable declaration was corrected during compilation."
	updatedCode := CodeProvider normalizeCodeForSaving: (requestor ifNil: [code] ifNotNil: [requestor text]) in: self.
	compiled putSource: updatedCode fromParseNode: methodAndNode node inFile: 2
			withPreamble: [ :f | f newLine; nextPut: $!; nextChunkPut: 'Behavior method'; newLine].
	self addSelectorSilently: sel withMethod: compiled.
	^ methodAndNode selector
]

{ #category : 'compiling' }
Behavior >> compileAll [
	^ self compileAllFrom: self
]

{ #category : 'compiling' }
Behavior >> compileAllFrom: oldClass [
	"Compile all the methods in the receiver's method dictionary.
	This validates sourceCode and variable references and forces
	all methods to use the current bytecode set"
	"ar 7/10/1999: Use oldClass selectors not self selectors"
	oldClass selectorsDo: [:sel | self recompile: sel from: oldClass]
]

{ #category : 'method dictionary' }
Behavior >> compiledMethodAt: selector  [
	"Answer the compiled method associated with the argument, selector (a 
	Symbol), a message selector in the receiver's method dictionary. If the 
	selector is not in the dictionary, create an error notification."

	^ self methodDict at: selector
]

{ #category : 'accessing method dictionary' }
Behavior >> compiledMethodAt: selector ifAbsent: aBlock [
	"Answer the compiled method associated with the argument, selector (a Symbol), a message selector in the receiver's method dictionary. If the selector is not in the dictionary, return the value of aBlock"

	^ self methodDict at: selector ifAbsent: aBlock
]

{ #category : 'accessing method dictionary' }
Behavior >> compiledMethodAt: aSelector ifPresent: aFoundBlock ifAbsent: anAbsentBlock  [
	
	^self methodDict at: aSelector ifPresent: aFoundBlock ifAbsent: anAbsentBlock.
]

{ #category : 'compiling' }
Behavior >> compilerClass [
	"Return a compiler class appropriate for source methods of this class."

	^Compiler
]

{ #category : 'accessing method dictionary' }
Behavior >> compress [
	"Compact the method dictionary of the receiver."

	self methodDict rehash
]

{ #category : 'copying' }
Behavior >> copyOfMethodDictionary [
	"Return a copy of the receiver's method dictionary"

	^ self methodDict copy
]

{ #category : 'user interface' }
Behavior >> crossReference [
	"Answer an Array of arrays of size 2 whose first element is a message selector in the receiver's method dictionary and whose second element is a set of all message selectors in the method dictionary whose methods send a message with that selector. Subclasses are not included."

	^self selectors asArray sort collect: [ :x |
		Array 
			with: String newLineString, x 
			with: (self whichSelectorsReferTo: x) ]

	"
	Point crossReference.
	"
]

{ #category : 'compiling' }
Behavior >> decompile: selector  [
	"Find the compiled code associated with the argument, selector, as a message selector
	in the receiver's method dictionary and decompile it.  Answer the resulting source
	code as a string.  Create an error if the selector is not in the receiver's method
	dictionary."

	^self decompilerClass new decompile: selector in: self
]

{ #category : 'accessing' }
Behavior >> decompilerClass [
	"Answer a decompiler class appropriate for compiled methods of this class."

	^self compilerClass decompilerClass
]

{ #category : 'class hierarchy' }
Behavior >> deepSubclassNamed: aName [
	"Or nil"
	self allSubclassesDo: [ :cls | cls name = aName ifTrue: [ ^cls ]].
	^nil.
]

{ #category : 'compiling' }
Behavior >> defaultMethodTrailer [
	^ #(0 0 0 0)
]

{ #category : 'accessing instances and variables' }
Behavior >> definesClassVariableNamedInHierarchy: aClassVariableName [

	^self allClassVarNames includes: aClassVariableName 
]

{ #category : 'accessing' }
Behavior >> definesInstanceVariableNamed: anInstanceVariableName [

	^self instVarNames includes: anInstanceVariableName
]

{ #category : 'accessing instances and variables' }
Behavior >> definesInstanceVariableNamedInHierarchy: anInstanceVariableName [

	^self allInstVarNames includes: anInstanceVariableName
]

{ #category : 'accessing instances and variables' }
Behavior >> elementSize [
	"Answer the size in bytes of an element in the receiver.  The formats are
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"
	| instSpec |
	instSpec := self instSpec.
	instSpec < 9 ifTrue: [^Smalltalk wordSize].
	instSpec >= 16 ifTrue: [^1].
	instSpec >= 12 ifTrue: [^2].
	instSpec >= 10 ifTrue: [^4].
	^8
]

{ #category : 'accessing method dictionary' }
Behavior >> firstCommentAt:  selector [
	"Answer a string representing the first comment in the method associated with selector.  Return an empty string if the relevant source file is not available, or if the method's source code does not contain a comment.  Not smart enough to bypass quotes in string constants, but does map doubled quote into a single quote."

	| sourceString commentStart  pos nextQuotePos |

	sourceString := (self sourceCodeAt: selector) asPlainString.
	sourceString isEmpty ifTrue: [^ ''].
	commentStart := sourceString findString: '"' startingAt: 1.
	commentStart = 0 ifTrue: [^ ''].
	pos := commentStart + 1.
	[(nextQuotePos := sourceString findString: '"' startingAt: pos) = (sourceString findString: '""' startingAt: pos)]
		whileTrue:
			[pos := nextQuotePos + 2].
	
	commentStart = nextQuotePos ifTrue: [^ ''].  "Must have been a quote in string literal"

	^ (sourceString copyFrom: commentStart + 1 to: nextQuotePos - 1) copyReplaceAll: '""' with: '"'


"Behavior firstCommentAt: #firstCommentAt:"
]

{ #category : 'accessing method dictionary' }
Behavior >> firstPrecodeCommentFor:  selector [
	"If there is a comment in the source code at the given selector that preceeds the body of the method, return it here, else return nil"

	| parser source tree |
	"Behavior firstPrecodeCommentFor: #firstPrecodeCommentFor:"
	(MethodSet isPseudoSelector: selector)
		ifTrue: [
			"Not really a selector"
			^ nil].
	source := self sourceCodeAt: selector asSymbol ifAbsent: [^ nil].
	parser := self parserClass new.
	tree := 
		parser
			parse: source readStream
			class: self
			noPattern: false
			context: nil
			notifying: nil
			ifFail: [^ nil].
	^ (tree comment ifNil: [^ nil]) first
]

{ #category : 'private' }
Behavior >> flushCache [
	"Tell the interpreter to remove the contents of its method lookup cache, if it has 
	one.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 89>
	self primitiveFailed
]

{ #category : 'accessing' }
Behavior >> format [
	"Answer an Integer that encodes the kinds and numbers of variables of instances
	of the receiver."

	^format
]

{ #category : 'private' }
Behavior >> handleFailingBasicNew [
	"handleFailingBasicNew gets sent after basicNew has failed and allowed
	 a scavenging garbage collection to occur.  The scavenging collection
	 will have happened as the VM is activating the (failing) basicNew.  If
	 handleFailingBasicNew fails then the scavenge failed to reclaim sufficient
	 space and a global garbage collection is required.  Retry after garbage
	 collecting and growing memory if necessary.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 70>
	Smalltalk garbageCollect < 1048576 ifTrue:
		[Smalltalk growMemoryByAtLeast: 1048576].
	^self handleFailingFailingBasicNew "retry after global garbage collect"
]

{ #category : 'private' }
Behavior >> handleFailingBasicNew: sizeRequested [
	"handleFailingBasicNew: gets sent after basicNew: has failed and allowed
	 a scavenging garbage collection to occur.  The scavenging collection
	 will have happened as the VM is activating the (failing) basicNew:.  If
	 handleFailingBasicNew: fails then the scavenge failed to reclaim sufficient
	 space and a global garbage collection is required.  Retry after garbage
	 collecting and growing memory if necessary.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 71>
	| bytesRequested |
	bytesRequested := self byteSizeOfInstanceOfSize: sizeRequested.
	Smalltalk garbageCollect < bytesRequested ifTrue:
		[Smalltalk growMemoryByAtLeast: bytesRequested].
	"retry after global garbage collect and possible grow"
	^self handleFailingFailingBasicNew: sizeRequested
]

{ #category : 'private' }
Behavior >> handleFailingFailingBasicNew [
	"This basicNew gets sent after handleFailingBasicNew: has done a full
	 garbage collection and possibly grown memory.  If this basicNew fails
	 then the system really is low on space, so raise the OutOfMemory signal.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 70>
	"space must be low"
	OutOfMemory signal.
	^self basicNew  "retry if user proceeds"
]

{ #category : 'private' }
Behavior >> handleFailingFailingBasicNew: sizeRequested [
	"This basicNew: gets sent after handleFailingBasicNew: has done a full
	 garbage collection and possibly grown memory.  If this basicNew: fails
	 then the system really is low on space, so raise the OutOfMemory signal.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive."
	"space must be low."
	<primitive: 71>
	(sizeRequested isInteger and: [ sizeRequested > 0 ])
		ifTrue: [ OutOfMemory signal ]
		ifFalse: [ self error: 'sizeRequested must be a positive integer' ].
	^ self basicNew: sizeRequested"retry if user proceeds".
]

{ #category : 'private' }
Behavior >> handleFailingFailingUninitializedPinnedNew: sizeRequested [
	"This uninitializedPinnedNew: gets sent after handleFailingUninitializedPinnedNew: has
	 done a full garbage collection and possibly grown memory.  If this uninitializedPinnedNew:
	 fails then the system really is low on space, so raise the OutOfMemory signal.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested. In contrast with basicNew:
	 do not initialize the new instance with zeros. Fail if this class is not
	 bits indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Optional."
	<primitive: 583>
	(sizeRequested isInteger and: [ sizeRequested > 0 ])
		ifTrue: [ OutOfMemory signal ]
		ifFalse: [ self error: 'sizeRequested must be a positive integer' ].
	^ self uninitializedPinnedNew: sizeRequested"retry if user proceeds".
]

{ #category : 'private' }
Behavior >> handleFailingUninitializedPinnedNew: sizeRequested [
	"handleFailingUninitializedPinnedNew: gets sent after uninitializedPinnedNew: has
	 failed and allowed a scavenging garbage collection to occur.  The scavenging collection
	 will have happened as the VM is activating the (failing) uninitializedPinnedNew:.  If
	 handleFailingUninitializedPinnedNew: fails then the scavenge failed to reclaim sufficient
	 space and a global garbage collection is required.  Retry after garbage
	 collecting and growing memory if necessary.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested. In contrast with basicNew:
	 do not initialize the new instance with zeros. Fail if this class is not
	 bits indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Optional."

	<primitive: 583>
	| bytesRequested |
	bytesRequested := self byteSizeOfInstanceOfSize: sizeRequested.
	Smalltalk garbageCollect < bytesRequested ifTrue:
		[Smalltalk growMemoryByAtLeast: bytesRequested].
	"retry after global garbage collect and possible grow"
	^self handleFailingFailingUninitializedPinnedNew: sizeRequested
]

{ #category : 'testing' }
Behavior >> hasChangedComparedTo: anotherClass [

	^self superclass ~~ anotherClass superclass
		or: [ self instVarNames ~= anotherClass instVarNames
		or: [ self classVarNames ~= anotherClass classVarNames
		or: [ self sharedPools ~= anotherClass sharedPools ]]]
]

{ #category : 'testing method dictionary' }
Behavior >> hasMethods [
	"Answer whether the receiver has any methods in its method dictionary."

	^ self methodDict size > 0
]

{ #category : 'accessing instances and variables' }
Behavior >> hasReferencesToInstanceVariableNamed: anInstanceVariableName [

	"Returns true if only self has one or more methods referencing anInstanceVariableName - Hernan"

	^(self whichSelectorsAccess: anInstanceVariableName) notEmpty
]

{ #category : 'accessing class hierarchy' }
Behavior >> hasSubclasses [

	"Subclasses should redefine it with better implementations - Hernan"
	
	^self subclasses notEmpty
]

{ #category : 'accessing method dictionary' }
Behavior >> highestClassImplementing: aSelector ifNone: aNoneBlock [

	| highestImplementorClass |

	self withAllSuperclassesDo: [ :aBehavior | (aBehavior includesSelector: aSelector) ifTrue: [ highestImplementorClass := aBehavior ]].
	
	^ highestImplementorClass ifNil: aNoneBlock 
]

{ #category : 'comparing' }
Behavior >> identityHash [
	"Answer a SmallInteger whose value is related to the receiver's identity.
	 Behavior implements identityHash to allow the VM to use an object representation which
	 does not include a direct reference to an object's class in an object.  If the VM is using
	 this implementation then classes are held in a class table and instances contain the index
	 of their class in the table.  A class's class table index is its identityHash so that an instance
	 can be created without searching the table for a class's index.  The VM uses this primitive
	 to enter the class into the class table, assigning its identityHash with an as yet unused
	 class table index. If this primitive fails it means that the class table is full.
	 There are 22 bits of classTable index and 22 bits of identityHash per object.

	 Primitive. Essential. Do not override. See Object documentation whatIsAPrimitive."

	<primitive: 175>
	self primitiveFailed.
]

{ #category : 'testing class hierarchy' }
Behavior >> includesBehavior: aClass [
	^self == aClass or:[self inheritsFrom: aClass]
]

{ #category : 'method dictionary' }
Behavior >> includesSelector: aSymbol  [
	"Answer whether the message whose selector is the argument is in the 
	method dictionary of the receiver's class."

	^ self methodDict includesKey: aSymbol
]

{ #category : 'accessing instances and variables' }
Behavior >> indexOfInstanceVariable: aName [

	^self allInstVarNames indexOf: aName
]

{ #category : 'testing class hierarchy' }
Behavior >> inheritsFrom: aClass  [
	"Answer whether the argument, aClass, is on the receiver's superclass 
	chain."

	| aSuperclass |
	aSuperclass := superclass.
	[aSuperclass == nil]
		whileFalse: 
			[aSuperclass == aClass ifTrue: [^true].
			aSuperclass := aSuperclass superclass].
	^false
]

{ #category : 'system startup' }
Behavior >> initClassCachedState [
	"Will be called for each class on startup.
	See implementors for examples

	Can be used for eagerly initializing stuff that was cleared with releaseClassCachedState"
]

{ #category : 'instance creation' }
Behavior >> initializedInstance [
	"Answer an instance of the receiver which in some sense is initialized.  In the case of Morphs, this will yield an instance that can be attached to the Hand after having received the same kind of basic initialization that would be obtained from an instance chosen from the 'new morph' menu.   Return nil if the receiver is reluctant for some reason to return such a thing"

	^ self new
]

{ #category : 'accessing instances and variables' }
Behavior >> inspectAllInstances  [
	"Inpsect all instances of the receiver.  1/26/96 sw"

	| all allSize prefix |
	all := self allInstances.
	(allSize := all size) = 0 ifTrue: [^ self inform: 'There are no 
instances of ', self name].
	prefix := allSize = 1
		ifTrue: 	['The lone instance']
		ifFalse:	['The ', allSize printString, ' instances'].
	
	all asArray inspectWithLabel: (prefix, ' of ', self name)
]

{ #category : 'accessing instances and variables' }
Behavior >> inspectSubInstances  [
	"Inspect all instances of the receiver and all its subclasses.  CAUTION - don't do this for something as generic as Object!  1/26/96 sw"

	| all allSize prefix |
	all := self allSubInstances.
	(allSize := all size) = 0 ifTrue: [^ self inform: 'There are no 
instances of ', self name, '
or any of its subclasses'].
	prefix := allSize = 1
		ifTrue: 	['The lone instance']
		ifFalse:	['The ', allSize printString, ' instances'].
	
	all asArray inspectWithLabel: (prefix, ' of ', self name, ' & its subclasses')
]

{ #category : 'accessing' }
Behavior >> instSize [
	"Answer the number of named instance variables
	(as opposed to indexed variables) of the receiver.
	Class format is
		<5 bits inst spec><16 bits inst size>"

	^ format bitAnd: 16rFFFF
]

{ #category : 'accessing' }
Behavior >> instSpec [
	"Answer the instance specification part of the format that defines what kind of object
	 an instance of the receiver is.  The formats are
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (Context BlockClosure AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable	(DoubleWordArray et al)
		10-11	= 32-bit indexable	(WordArray et al)			(includes one odd bit, unused in 32-bit instances)
		12-15	= 16-bit indexable	(DoubleByteArray et al)		(includes two odd bits, one unused in 32-bit instances)
		16-23	= 8-bit indexable	(ByteArray et al)			(includes three odd bits, one unused in 32-bit instances)
		24-31	= compiled code	(CompiledCode et al)		(includes three odd bits, one unused in 32-bit instances)

	 Note that in the VM instances also have a 5 bit format field that relates to their class's format.
	 Formats 11, 13-15, 17-23 & 25-31 are unused in classes but used in instances to define the
	 number of elements missing up to the slot size.  For example, a 2-byte ByteString instance
	 has format 18 in 32 bits, since its size is one 32-bit slot - 2 bytes ((18 bitAnd: 3) = 2), and
	 22 in 64 bits, since its size is one 64-bit slot - 6 bytes ((22 bitAnd: 7) = 6).
	 Formats 24-31 are for compiled code which is a combination of pointers and bytes.  The number of pointers is
	 determined by literal count field of the method header, which is the first field of the object and must be a SmallInteger. 
	 The literal count field occupies the least significant 15 bits of the method header, allowing up to 32,767 pointer fields,
	 not including the header."
	^(format bitShift: -16) bitAnd: 16r1F
]

{ #category : 'accessing' }
Behavior >> instVarNames [
	"Answer an Array of the instance variable names. Behaviors must make 
	up fake local instance variable names because Behaviors have instance 
	variables for the purpose of compiling methods, but these are not named 
	instance variables."

	| mySize superSize |
	mySize := self instSize.
	superSize := 
		superclass
			ifNil: [0]
			ifNotNil: [superclass instSize].
	mySize = superSize ifTrue: [^#()].	
	^(superSize + 1 to: mySize) collect: [:i | 'inst' , i printString]
]

{ #category : 'compiling' }
Behavior >> instVarNamesAndOffsetsDo: aBinaryBlock [
	"This is part of the interface between the compiler and a class's instance or field names.
	 The class should enumerate aBinaryBlock with the instance variable name strings and
	 their integer offsets.  The order is important. Names evaluated later will override the
	 same names occurring earlier."

	"Nothing to do here; ClassDescription introduces named instance variables"
	^self
]

{ #category : 'accessing instances and variables' }
Behavior >> instanceCount [
	"Answer the number of instances of the receiver that are currently in use."

	| count |
	count := 0.
	self allInstancesDo: [:x | count := count + 1].
	^count
]

{ #category : 'testing' }
Behavior >> isBehavior [
	"Return true if the receiver is a behavior"
	^true
]

{ #category : 'testing' }
Behavior >> isBits [
	"Answer whether the receiver contains just bits (not pointers).
	The class format is
		<5 bits inst spec><16 bits inst size>
	where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ self instSpec >= 7
]

{ #category : 'testing' }
Behavior >> isBytes [
	"Answer whether the receiver's instances have indexed 8-bit integer slots.
	If true, #isBits is also true.
	The class format is
		<5 bits inst spec><16 bits inst size>
	where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ self instSpec >= 16
]

{ #category : 'testing' }
Behavior >> isCompiledMethodClass [
	"Answer whether the receiver has compiled method instances that mix pointers and bytes."
	^self instSpec >= 24
]

{ #category : 'testing' }
Behavior >> isDoubleBytes [
	"Answer whether the receiver's instances have indexed 16-bit integer slots.
	If true, #isBits is also true.
	The class format is
		<5 bits inst spec><16 bits inst size>
	where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ self instSpec = 12
]

{ #category : 'testing' }
Behavior >> isDoubleWords [
	"Answer whether the receiver's instances have indexed 64-bit integer slots.
	If true, #isBits is also true.
	 The class format is
		<5 bits inst spec><16 bits inst size>
	 where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ self instSpec = 9
]

{ #category : 'testing' }
Behavior >> isEphemeronClass [
	"Answer whether the receiver has ephemeral instance variables.  The garbage collector will
	 fire (queue for finalization) any ephemeron whose first instance variable is not referenced
	 other than from the transitive closure of references from ephemerons. Hence referring to
	 an object from the first inst var of an ephemeron will cause the ephemeron to fire when
	 the rest of the system does not refer to the object and that object is ready to be collected.
	 Since references from the remaining inst vars of an ephemeron will not prevent the ephemeron
	 from firing, ephemerons may act as the associations in weak dictionaries such that the value
	 (e.g. properties attached to the key) will not prevent firing when the key is no longer referenced
	 other than from ephemerons.  Ephemerons can therefore be used to implement instance-based
	 pre-mortem finalization."
	^self instSpec = 5
]

{ #category : 'testing' }
Behavior >> isFixed [
	"Answer whether the receiver does not have a variable (indexable) part."

	^self isVariable not
]

{ #category : 'testing' }
Behavior >> isImmediateClass [
	"Answer whether the receiver has immediate instances.  Immediate instances
	 store their value in their object pointer, not in an object body.  Hence immediates
	 take no space and are immutable.  The immediates are distinguished by tag bits
	 in the pointer. They include SmallIntegers and Characters.  Hence in the 32-bit
	 system SmallIntegers are 31-bit signed integers and Characters are 30-bit
	 unsigned character codes."
	^self instSpec = 7
]

{ #category : 'accessing instances and variables' }
Behavior >> isInstanceVariableNamedReferencedInHierarchy: anInstanceVariableName [

	"Returns true if self or any subclass has one or more methods referencing anInstanceVariableName - Hernan"

	^self withAllSubclasses anySatisfy: [ :aClass | aClass hasReferencesToInstanceVariableNamed: anInstanceVariableName ]
]

{ #category : 'testing' }
Behavior >> isMeta [
	^ false
]

{ #category : 'testing' }
Behavior >> isObsolete [
	"Return true if the receiver is obsolete."
	^self instanceCount = 0
]

{ #category : 'testing' }
Behavior >> isPointers [
	"Answer whether the receiver contains just pointers (not bits)."

	^self isBits not
]

{ #category : 'testing' }
Behavior >> isVariable [
	"Answer whether the receiver has indexable variables.
	 Class format is
		<5 bits inst spec><16 bits inst size>
	 where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"
	| instSpec |
	instSpec := self instSpec.
	(instSpec between: 2 and: 4) ifTrue: [ ^true ].
	^instSpec >= 9
]

{ #category : 'testing' }
Behavior >> isWeak [
	"Answer whether the receiver has contains weak references."
	^ self instSpec = 4
]

{ #category : 'testing' }
Behavior >> isWords [
	"Answer whether the receiver's instances have indexed 32-bit integer slots.
	If true, #isBits is also true.
	The class format is
		<5 bits inst spec><16 bits inst size>
	where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ self instSpec = 10
]

{ #category : 'testing class hierarchy' }
Behavior >> kindOfSubclass [
	"Answer a String that is the keyword that describes the receiver's kind of subclass,
	 either a regular subclass, a variableSubclass, a variableByteSubclass,
	 a variableWordSubclass, a weakSubclass, an ephemeronSubclass or an immediateSubclass.
	 c.f. typeOfClass & instSpec"
	^(#(' subclass: '
		' subclass: '
		' variableSubclass: '
		' variableSubclass: '
		' weakSubclass: '
		' ephemeronSubclass: '
		nil
		' immediateSubclass: '
		nil
		' variableDoubleWordSubclass: '
		' variableWordSubclass: '		nil
		' variableDoubleByteSubclass: '	nil nil nil
		' variableByteSubclass: '		nil nil nil nil nil nil nil
		' variableByteSubclass: '		nil nil nil nil nil nil nil )
			at: self instSpec + 1) ifNil:
				[self error: 'invalid class type']
]

{ #category : 'user interface' }
Behavior >> lastUnderscoreMeansSubscript [
	"Redefine this method on the class side of those classes where you want
	a_1 to be shown as 'a subscript 1'"

	^ false
]

{ #category : 'printing' }
Behavior >> literalScannedAs: scannedLiteral notifying: requestor [
	"Postprocesses a literal scanned by Scanner scanToken (esp. xLitQuote).
	If scannedLiteral is not an association, answer it.
	Else, if it is of the form:
		nil->#NameOfMetaclass
	answer nil->theMetaclass, if any has that name, else report an error.
	Else, if it is of the form:
		#NameOfGlobalVariable->anythiEng
	answer the global, class, or pool association with that nameE, if any, else
	add it to Undeclared a answer the new Association."

	| key value |
	(scannedLiteral isVariableBinding)
		ifFalse: [^ scannedLiteral].
	key := scannedLiteral key.
	value := scannedLiteral value.
	key ifNil: [
		(self bindingOf: value) ifNotNil: [ :assoc |
				 (assoc value isKindOf: Behavior)
					ifTrue: [^ nil->assoc value class]].
		requestor notify: 'No such metaclass'.
		^false].
	key isSymbol
		ifTrue: [ "##<global var name>"
			(self bindingOf: key) ifNotNil:[ :assoc | ^assoc].
			Undeclared at: key put: nil.
			 ^Undeclared bindingOf: key].
	requestor notify: '## must be followed by a non-local variable name'.
	^false

"	Form literalScannedAs: 14 notifying: nil 14
	Form literalScannedAs: #OneBitForm notiEfying: nil  OneBitForm
	Form literalScannedAs: ##OneBitForm notifying: nil  OneBitForm->a Form
	Form literalScannedAs: ##Form notifying: nil   Form->Form
	Form literalScannedAs: ###Form notifying: nil   nilE->Form class
"
]

{ #category : 'printing' }
Behavior >> longPrintOn: aStream [
	"Append to the argument, aStream, the names and values of all of the receiver's instance variables.  But, not useful for a class with a method dictionary."

	aStream nextPutAll: '<<too complex to show>>'; newLine
]

{ #category : 'method dictionary' }
Behavior >> lookupSelector: selector [
	"Look up the given selector in my methodDictionary.
	Return the corresponding method if found.
	Otherwise chase the superclass chain and try again.
	Return nil if no method is found."
	| lookupClass |
	lookupClass := self.
	[lookupClass == nil]
		whileFalse: 
			[(lookupClass includesSelector: selector)
				ifTrue: [^ lookupClass compiledMethodAt: selector].
			lookupClass := lookupClass superclass].
	^ nil
]

{ #category : 'instances and variables' }
Behavior >> maxAccessibleInstSize [
	"Answer the maximum number of accessible named instance variables.
	 This depends on the bytecode set.
	Note: On images without Compiler / Parser / Encoder we'd need a class variable somewhere.
		Assume EncoderForV3PlusClosures if unknown"

	CompiledMethod preferredBytecodeSetEncoderClass isNil
		ifTrue: [ ^256 ].
	^CompiledMethod preferredBytecodeSetEncoderClass maxAccessibleInstSize
]

{ #category : 'constants' }
Behavior >> maxInstSize [
	"Answer the maximum number of named instance variables
	 (as opposed to indexed variables) of the receiver.
	 The class format is
		<5 bits inst spec><16 bits inst size>"
	^16rFFFF
]

{ #category : 'method dictionary' }
Behavior >> methodDict [
	methodDict ifNil: [self recoverFromMDFaultWithTrace].
	^ methodDict
]

{ #category : 'method dictionary' }
Behavior >> methodDict: aDictionary [
	methodDict := aDictionary
]

{ #category : 'accessing method dictionary' }
Behavior >> methodDictionary [
	"Convenience"
	^self methodDict
]

{ #category : 'creating method dictionary' }
Behavior >> methodDictionary: aDictionary  [
	"Store the argument, aDictionary, as the method dictionary of the receiver."

	methodDict := aDictionary
]

{ #category : 'accessing method dictionary' }
Behavior >> methodHeaderFor: selector  [
	"Answer the string corresponding to the method header for the given selector"

	| methodSource |
	methodSource := self ultimateSourceCodeAt: selector ifAbsent: [self standardMethodHeaderFor: selector].
	^methodSource copyFrom: 1 to: (self parserClass methodHeaderLengthFrom: methodSource)

"
Behavior methodHeaderFor: #methodHeaderFor:
"

]

{ #category : 'methods' }
Behavior >> methodNodeFor: aSourceCode [

	^self methodNodeFor: aSourceCode noPattern: false
]

{ #category : 'methods' }
Behavior >> methodNodeFor: aSourceCode noPattern: aBoolean [

	^self methodNodeFor: aSourceCode noPattern: aBoolean optimizeSpecialSends: true
]

{ #category : 'methods' }
Behavior >> methodNodeFor: aSourceCode noPattern: aBoolean optimizeSpecialSends: doOptimizeSpecialSends [

	|  parser methodNode |

	parser := self parserClass new.
	parser optimizeSpecialSends: doOptimizeSpecialSends.

	methodNode := parser parse: aSourceCode class: self noPattern: aBoolean.
	methodNode sourceText: aSourceCode.

	^methodNode
			
]

{ #category : 'methods' }
Behavior >> methodNodeWithoutImplicitSelfReturnFor: aSourceCode [

	^(self methodNodeFor: aSourceCode) withoutImplicitSelfReturn
]

{ #category : 'accessing method dictionary' }
Behavior >> methodsDo: aBlock [
	"Evaluate aBlock for all the compiled methods in my method dictionary."

	^ self methodDict valuesDo: aBlock
]

{ #category : 'accessing method dictionary' }
Behavior >> methodsSelect: aCondition [

	^ self methodDict valuesSelect: aCondition
]

{ #category : 'accessing method dictionary' }
Behavior >> methodsWithArgumentOrTemporaryNamed: instVarName [

	^self methodsSelect: [:aMethod | aMethod hasArgumentOrTemporaryNamed: instVarName ]
]

{ #category : 'accessing' }
Behavior >> name [
	"Answer a String that is the name of the receiver."
	^'a subclass of ', superclass name
]

{ #category : 'instance creation' }
Behavior >> new [
	"Answer a new initialized instance of the receiver (which is a class) with no indexable variables. Fail if the class is indexable."

	^ self basicNew initialize

]

{ #category : 'instance creation' }
Behavior >> new: sizeRequested  [
	"Answer an initialized instance of this class with the number of indexable
	variables specified by the argument, sizeRequested."

	^ (self basicNew: sizeRequested) initialize  
]

{ #category : 'release' }
Behavior >> obsolete [
	"Invalidate and recycle local messages,
	e.g., zap the method dictionary if can be done safely."
	self canZapMethodDictionary
		ifTrue:[ methodDict := MethodDictionary new ].
]

{ #category : 'obsolete subclasses' }
Behavior >> obsoleteSubclasses [
	"Return all the weakly remembered obsolete subclasses of the receiver.
	Clean out the already collected ones."

	ObsoleteSubclasses ifNil: [
		^#() ].
	^ObsoleteSubclasses select: [ :any |
		any superclass == self ].
]

{ #category : 'accessing' }
Behavior >> parserClass [
	"Answer a parser class to use for parsing method headers."

	^self compilerClass parserClass
]

{ #category : 'copying' }
Behavior >> postCopy [

	self methodDict: self methodDict copy
]

{ #category : 'accessing method dictionary' }
Behavior >> precodeCommentOrInheritedCommentFor: selector  [
	"Answer a string representing the first comment in the method associated 
	with selector, considering however only comments that occur before the 
	beginning of the actual code. If the version recorded in the receiver is 
	uncommented, look up the inheritance chain. Return nil if none found."
	| aSuper aComment |
	^ (aComment := self firstPrecodeCommentFor: selector) isEmptyOrNil
		ifTrue: [(self == Behavior
					or: [superclass == nil
							or: [(aSuper := superclass whichClassIncludesSelector: selector) == nil]])
				ifFalse: [aSuper precodeCommentOrInheritedCommentFor: selector]
			"ActorState precodeCommentOrInheritedCommentFor: #printOn:"]
		ifFalse: [aComment]
]

{ #category : 'printing' }
Behavior >> printOn: aStream  [
	aStream nextPutAll: 'a descendent of '.
	superclass printOn: aStream
]

{ #category : 'printing' }
Behavior >> printWithClosureAnalysisOn: aStream  [
	"Refer to the comment in Object|printOn:." 

	aStream nextPutAll: 'a descendent of '.
	superclass printWithClosureAnalysisOn: aStream
]

{ #category : 'compiling' }
Behavior >> recompile: selector [
	"Compile the method associated with selector in the receiver's method dictionary."
	^self recompile: selector from: self
]

{ #category : 'compiling' }
Behavior >> recompile: selector from: oldClass [
	"Compile the method associated with selector in the receiver's method dictionary."
	"ar 7/10/1999: Use oldClass compiledMethodAt: not self compiledMethodAt:"
	| method trailer methodNode compiled |
	method := oldClass compiledMethodAt: selector.
	trailer := method trailer.
	methodNode := self compilerClass new
				compile: (oldClass sourceCodeAt: selector)
				in: self
				notifying: nil
				ifFail: [^ self].   "Assume OK after proceed from SyntaxError"
	selector == methodNode selector ifFalse: [self error: 'selector changed!'].
	compiled := methodNode generate: trailer.
	(method propertyValueAt: #libraryName) ifNotNil: [ :libName |
		compiled propertyValueAt: #libraryName put: libName ].
	self addSelectorSilently: selector withMethod: compiled.

]

{ #category : 'compiling' }
Behavior >> recompileChanges [
	"Compile all the methods that are in the changes file.
	This validates sourceCode and variable references and forces
	methods to use the current bytecode set"

	self selectorsDo:
		[:sel | (self compiledMethodAt: sel) fileIndex > 1 ifTrue:
			[self recompile: sel from: self]]
]

{ #category : 'accessing method dictionary' }
Behavior >> recoverFromMDFaultWithTrace [
	"Only used for ClassDescription."
]

{ #category : 'obsolete subclasses' }
Behavior >> recreateObsoleteClassesCollection [
	"
	Behavior recreateObsoleteClassesCollection
	"
	ObsoleteSubclasses := nil.
	Smalltalk garbageCollect.
	Smalltalk allObjectsDo: [ :cl |
		(cl isBehavior and: [cl isObsolete]) ifTrue: [
			ObsoleteSubclasses ifNil: [
				ObsoleteSubclasses := WeakIdentitySet new ].
			ObsoleteSubclasses add: cl]].
]

{ #category : 'system startup' }
Behavior >> releaseClassCachedState [
	"Will be called for each class on shutdown or snapshot. Implemented in relevant subclasses.
	 All class vars or class instVar vars that refer to objects that take up space and can be cheaply
	 recreated lazily on demand, should be nilled. For more expensive stuff to clean and recreate,
	 consider implementing #releaseClassState which is not called on every image save.

	 See implementors for examples"
]

{ #category : 'system startup' }
Behavior >> releaseClassState [
	"Will be called for each class on 'Save as new version'.
	All class vars or class instVar vars that can be recreated lazily on demand, should be nilled. This is done not only to save space, but more importantly, to prepare Cuis for a complete bootstrap from sources. For this, it should be possible to recreate all class state, at least with default values.
	See implementors for examples"
]

{ #category : 'obsolete subclasses' }
Behavior >> removeObsoleteSubclass: aClass [
	"Remove aClass from the weakly remembered obsolete subclasses."

	ObsoleteSubclasses ifNil: [
		^ self ].
	ObsoleteSubclasses remove: aClass ifAbsent: nil.
]

{ #category : 'accessing method dictionary' }
Behavior >> removeSelector: selector  [
	"Assuming that the argument, selector (a Symbol), is a message selector 
	in my method dictionary, remove it and its method."

	^ self basicRemoveSelector: selector
]

{ #category : 'enumerating' }
Behavior >> selectSuperclasses: aBlock  [
	"Evaluate the argument, aBlock, with the receiver's superclasses as the 
	argument. Collect into an OrderedCollection only those superclasses for 
	which aBlock evaluates to true. In addition, evaluate aBlock for the 
	superclasses of each of these successful superclasses and collect into the 
	OrderedCollection ones for which aBlock evaluates to true. Answer the 
	resulting OrderedCollection."

	| aSet |
	aSet := Set new.
	self allSuperclasses do: 
		[:aSuperclass | 
		(aBlock value: aSuperclass) ifTrue: [aSet add: aSuperclass]].
	^aSet
]

{ #category : 'accessing method dictionary' }
Behavior >> selectorAtMethod: method setClass: classResultBlock  [
	"Answer both the message selector associated with the compiled method 
	and the class in which that selector is defined."

	| sel |
	sel := self methodDict keyAtIdentityValue: method
				ifAbsent: [ | sel2 |
					superclass
						ifNil: [
							classResultBlock value: self.
							^method defaultSelector].
					sel2 := superclass selectorAtMethod: method setClass: classResultBlock.
					"Set class to be self, rather than that returned from superclass. "
					sel2 == method defaultSelector ifTrue: [ classResultBlock value: self ].
					^sel2].
	classResultBlock value: self.
	^sel
]

{ #category : 'accessing method dictionary' }
Behavior >> selectors [
	"Answer a Set of all the message selectors specified in the receiver's 
	method dictionary."

	^ self methodDict keys
	"
	Point selectors
	"
]

{ #category : 'accessing method dictionary' }
Behavior >> selectorsAndMethodsDo: aBlock [
	"Evaluate selectorBlock for all the message selectors in my method dictionary."

	^ self methodDict keysAndValuesDo: aBlock
]

{ #category : 'accessing method dictionary' }
Behavior >> selectorsDo: selectorBlock [
	"Evaluate selectorBlock for all the message selectors in my method dictionary."

	^ self methodDict keysDo: selectorBlock
]

{ #category : 'private' }
Behavior >> setFormat: aFormatInstanceDescription [
	"Needed for shout theme test."

	"only use this method with extreme care since it modifies the format of the class 
     ie a description of the number of instance variables and whether the class is
     compact, variable sized"

	format := aFormatInstanceDescription


]

{ #category : 'accessing instances and variables' }
Behavior >> sharedPools [
	"Answer a Set of the names of the pools (Dictionaries) that the receiver 
	shares.
	9/12/96 tk  sharedPools have an order now"

	^ #()
]

{ #category : 'testing' }
Behavior >> shouldNotBeRedefined [
	"Return true if the receiver should not be redefined.
	The assumption is that classes in Smalltalk specialObjects and 
	Behaviors should not be redefined"

	^(Smalltalk specialObjectsArray identityIncludes: self)
			or: [self isKindOf: self]
]

{ #category : 'system startup' }
Behavior >> shutDown [
	"This message is sent on system shutdown to registered classes"

]

{ #category : 'system startup' }
Behavior >> shutDown: quitting [
	"This message is sent on system shutdown to registered classes.
	If quitting is true, we are about to quit.
	If quitting is false, we are just snapshotting."

	^ self shutDown
]

{ #category : 'instances' }
Behavior >> someInstance [
	"Answer the first instance of this receiver.
	Fails if there are none.
	See Object>>#nextInstance.

	#someInstance and #nextInstance are meant to be used together starting with #someInstance,
	and following with #nextInstance until nil.

	Warning: Until all the instances are iterated and nil is answered, the VM holds a strong reference
	to the last one retrieved, preventing collecting and finalization.
	Best practice is to always iterate until nil.
	
	Warning: Spur VMs can not guarantee that every instance is visited and that it is visited only once.
		Use of this primitive is strongly discouraged."

	<primitive: 77>
	^nil
]

{ #category : 'accessing method dictionary' }
Behavior >> sourceCodeAt: selector [

	^ (self methodDict at: selector) getSourceFor: selector in: self
]

{ #category : 'accessing method dictionary' }
Behavior >> sourceCodeAt: selector ifAbsent: aBlock [

	^ (self methodDict at: selector ifAbsent: [^ aBlock value]) getSourceFor: selector in: self
]

{ #category : 'accessing' }
Behavior >> sourceCodeTemplate [
	"Answer an expression to be edited and evaluated in order to define 
	methods in this class.
	Modified to be parseable for Shour"

	^'messageSelectorAndArgumentNames
	"comment stating purpose of message"

	| temporary variable names |
	statements'
]

{ #category : 'testing' }
Behavior >> sourceMatchesBytecodeAt: selector [
	"Answers true if the source code at the selector compiles to the bytecode at the selector, and false otherwise. Implemented to detect an error where Monticello did not recompile sources when the class shape changed"
	"This code was copied from #recompile:from:, with few changes. Several methods would benefit from a method which turned a selector and class into a CompiledMethod, without  installing it into the methodDictionary"

	| method trailer methodNode |
	method := self compiledMethodAt: selector.
	trailer := method trailer.
	methodNode := self compilerClass new
				compile: (self sourceCodeAt: selector)
				in: self
				notifying: nil
				ifFail: [^ false].   "Assume OK after proceed from SyntaxError"
	selector == methodNode selector ifFalse: [self error: 'selector changed!'].
	^ (methodNode generate: trailer) = method
]

{ #category : 'private' }
Behavior >> spaceUsed [
	"Answer a rough estimate of number of bytes used by this class and its metaclass. Does not include space used by class variables."

	| space |
	space := 0.
	self selectorsDo: [ :sel | | method |
		space := space + 16.  "dict and org'n space"
		method := self compiledMethodAt: sel.
		space := space + (method size + 6 "hdr + avg pad").
		method literalsDo: [ :lit |
			(lit isMemberOf: Array) ifTrue: [ space := space + ((lit size + 1) * 4)].
			(lit isMemberOf: Float) ifTrue: [ space := space + 12].
			(lit isMemberOf: String) ifTrue: [ space := space + (lit size + 6)].
			(lit isMemberOf: LargeNegativeInteger) ifTrue: [ space := space + ((lit size + 1) * 4)].
			(lit isMemberOf: LargePositiveInteger) ifTrue: [ space := space + ((lit size + 1) * 4)]]].
	^ space
]

{ #category : 'methods' }
Behavior >> stampAt: selector [
	^(self compiledMethodAt: selector ifAbsent: [ ^'' ]) authorAndStamp
]

{ #category : 'accessing method dictionary' }
Behavior >> standardMethodHeaderFor: aSelector [
	| args |
	args := (1 to: aSelector numArgs)	collect:[:i| 'arg', i printString].
	args size = 0 ifTrue:[^aSelector asPlainString].
	args size = 1 ifTrue:[^aSelector,' arg1'].
	^String streamContents:[:s|
		(aSelector findTokens: ':') with: args do: [ :tok :arg |
			s nextPutAll: tok; nextPutAll:': '; nextPutAll: arg; nextPutAll:' '.
		].
	].

]

{ #category : 'system startup' }
Behavior >> startUp [
	"This message is sent to registered classes when the system is coming up, or after an image save."
]

{ #category : 'system startup' }
Behavior >> startUp: isARealStartup [
	"This message is sent to registered classes, with isARealStartup = true when the system is coming up,
	and with isARealStartup = false after a snapshot (image save, no quit).
	Classes caring about the difference should reimplement this method."

	^ self startUp
]

{ #category : 'printing' }
Behavior >> storeLiteral: aCodeLiteral on: aStream [
	"Store aCodeLiteral on aStream, changing an Association to ##GlobalName
	 or ###MetaclassSoleInstanceName format if appropriate"
	| key value |
	(aCodeLiteral isVariableBinding)
		ifFalse:
			[aCodeLiteral storeOn: aStream.
			 ^self].
	key := aCodeLiteral key.
	(key isNil and: [(value := aCodeLiteral value) isMemberOf: Metaclass])
		ifTrue:
			[aStream nextPutAll: '###'; nextPutAll: value soleInstance name.
			 ^self].
	(key isSymbol and: [(self bindingOf: key) notNil])
		ifTrue:
			[aStream nextPutAll: '##'; nextPutAll: key.
			 ^self].
	aCodeLiteral storeOn: aStream
]

{ #category : 'accessing class hierarchy' }
Behavior >> subclasses [
	"slow implementation since Behavior does not keep track of subclasses"
	
	^ self class allInstances select: [:each | each superclass = self ]
]

{ #category : 'accessing class hierarchy' }
Behavior >> subclassesDo: aBlock [
	"Evaluate the argument, aBlock, for each of the receiver's immediate subclasses."
	^self subclasses do: aBlock
]

{ #category : 'class hierarchy' }
Behavior >> superclass [
	"Answer the receiver's superclass, a Class."

	^superclass
]

{ #category : 'accessing class hierarchy' }
Behavior >> superclass: aClass  [
	"Change the receiver's superclass to be aClass."
	"Note: Do not use 'aClass isKindOf: Behavior' here
		in case we recompile from Behavior itself."
	(aClass == nil or: [aClass isBehavior])
		ifTrue: [superclass := aClass.
				Object flushCache]
		ifFalse: [self error: 'superclass must be a class-describing object']
]

{ #category : 'initialization' }
Behavior >> superclass: aClass methodDictionary: mDict format: fmt [
	"Basic initialization of the receiver.
	Must only be sent to a new instance; else we would need Object flushCache."
	superclass := aClass.
	format := fmt.
	methodDict := mDict.
]

{ #category : 'accessing method dictionary' }
Behavior >> supermostPrecodeCommentFor: selector  [
	"Answer a string representing the precode comment in the most distant 
	superclass's implementation of the selector. Return nil if none found."
	| aSuper superComment |
	(self == Behavior
			or: [superclass == nil
					or: [(aSuper := superclass whichClassIncludesSelector: selector) == nil]])
		ifFalse: ["There is a super implementor"
			superComment := aSuper supermostPrecodeCommentFor: selector].
	^ superComment
		ifNil: [self firstPrecodeCommentFor: selector
			"ActorState supermostPrecodeCommentFor: #printOn:"]
]

{ #category : 'SUnit tests' }
Behavior >> testCaseClass [

	self subclassResponsibility 
]

{ #category : 'accessing' }
Behavior >> theNonMetaClass [

	self subclassResponsibility 
]

{ #category : 'auto complete' }
Behavior >> typeName [

	"If the class whishes to be shown in a different way in the selectors documentation. 
	For example, DenotativeObject does not show it self as a metaclass but as a class - Hernan"
	
	^self name
]

{ #category : 'accessing' }
Behavior >> typeOfClass [
	"Answer a symbol uniquely describing the type of the receiver. c.f. kindOfSubclass, instSpec"
	^(#(normal
		normal
		variable
		variable
		weak
		ephemeron
		nil
		immediate
		nil
		longs
		words				nil
		shorts				nil nil nil
		bytes				nil nil nil nil nil nil nil
		compiledMethod	nil nil nil nil nil nil nil)
			at: self instSpec + 1) ifNil:
				[self error: 'invalid class type']
]

{ #category : 'accessing method dictionary' }
Behavior >> ultimateSourceCodeAt: selector ifAbsent: aBlock [
	"Return the source code at selector, deferring to superclass if necessary"

	^ self
		sourceCodeAt: selector
		ifAbsent: [
			superclass
				ifNil: [aBlock value]
				ifNotNil: [superclass ultimateSourceCodeAt: selector ifAbsent: aBlock]]
]

{ #category : 'instance creation' }
Behavior >> uninitializedPinnedNew: sizeRequested [
	"Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested. In contrast with basicNew:
	 do not initialize the new instance with zeros (because pointer objects must
	 be initialized the primitive fails for all pointer classes). Fail if this class is not
	 bits indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Optional.
	
	 If the primitive fails because space is low then the scavenger will run before the
	 method is activated.  Check args and retry via handleFailingBasicNew: if they're OK."

	<primitive: 583 error: ec>
	(ec == #'insufficient object memory' or: [ec == #'bad argument']) ifTrue:
		[^self handleFailingUninitializedPinnedNew: sizeRequested].
	ec ifNil:
		[^(self basicNew: sizeRequested) pin; yourself].
	self isVariable ifFalse:
		[self error: self printString, ' cannot have variable sized instances'].
	self isBits ifFalse:
		[self error: self printString, ' cannot be instantiated with uninitialized contents'].
	self primitiveFailed
]

{ #category : 'accessing instances and variables' }
Behavior >> unreferencedInstanceVariables [
	"Return a list of the instance variables defined in the receiver which are not referenced in the receiver or any of its subclasses

	Object unreferencedInstanceVariables
	"

	^ self instVarNames reject: [ :instanceVariableName | self isInstanceVariableNamedReferencedInHierarchy: instanceVariableName ]
		
]

{ #category : 'compiling' }
Behavior >> variablesAndOffsetsDo: aBinaryBlock [
	"This is the interface between the compiler and a class's instance or field names.  The
	 class should enumerate aBinaryBlock with the field definitions (with nil offsets) followed
	 by the instance variable name strings and their integer offsets (1-relative).  The order is
	 important; names evaluated later will override the same names occurring earlier."

	"Only need to do instance variables here.  CProtoObject introduces field definitions."
	self instVarNamesAndOffsetsDo: aBinaryBlock
]

{ #category : 'accessing instances and variables' }
Behavior >> whichClassDefinesInstanceVariable: aVariable ifNone: aNoneBlock [

	(self definesInstanceVariableNamed: aVariable) ifTrue: [ ^self ].

	^superclass whichClassDefinesInstanceVariable: aVariable ifNone: aNoneBlock
]

{ #category : 'testing method dictionary' }
Behavior >> whichClassIncludesSelector: aSymbol  [
	"Answer the class on the receiver's superclass chain where the 
	argument, aSymbol (a message selector), will be found. Answer nil if none found."
	"Rectangle whichClassIncludesSelector: #inspect."
	(self includesSelector: aSymbol)
		ifTrue: [^ self].
	superclass
		ifNil: [^ nil].
	^ superclass whichClassIncludesSelector: aSymbol
]

{ #category : 'testing method dictionary' }
Behavior >> whichSelectorsAccess: instVarName  [
	"Answer a collection (an Array) of selectors whose methods access the argument, 
	instVarName, as a named instance variable."

	| instVarIndex |
	instVarIndex := self allInstVarNames indexOf: instVarName ifAbsent: [^Set new].
	^ (self methodDict keys select: [ :sel | 
		((self methodDict at: sel)
			readsField: instVarIndex)
			or: [(self methodDict at: sel) writesField: instVarIndex]]) asSet

	"Point whichSelectorsAccess: 'x'."
]

{ #category : 'testing method dictionary' }
Behavior >> whichSelectorsReferExplicitlyTo: literal [
	"Answer a set of selectors whose methods access the argument as a literal,
	and refer explicitly to in Source Code.
	Useful for finding senders of selectors.
	See #whichSelectorsReferTo:."

	| who |
	who := Set new.
	self selectorsAndMethodsDo: [ :sel :method |
		(method hasLiteralExplicitly: literal)
			ifTrue: [
				who add: sel]].
	^who
	"
	Rectangle whichSelectorsReferExplicitlyTo: #==.
	"
]

{ #category : 'testing method dictionary' }
Behavior >> whichSelectorsReferTo: literal [
	"Answer a set of selectors whose methods access the argument as a literal.
	Useful for finding references to literals.
	See #whichSelectorsReferExplicitlyTo:."

	| who |
	who := Set new.
	self selectorsAndMethodsDo: [ :sel :method |
		(method hasLiteral: literal)
			ifTrue: [
				who add: sel]].
	^who
	"
	Rectangle whichSelectorsReferTo: #==.
	"
]

{ #category : 'testing method dictionary' }
Behavior >> whichSelectorsStoreInto: instVarName  [
	"Answer a Set of selectors whose methods access the argument, 
	instVarName, as a named instance variable."
	| instVarIndex |
	instVarIndex := self allInstVarNames indexOf: instVarName ifAbsent: [^Set new].
	^ (self methodDict keys select: 
		[:sel | (self methodDict at: sel) writesField: instVarIndex]) asSet

	"Point whichSelectorsStoreInto: 'x'."
]

{ #category : 'user interface' }
Behavior >> withAllSubAndSuperclassesDo: aBlock [

	self withAllSubclassesDo: aBlock.
	self allSuperclassesDo: aBlock.

]

{ #category : 'accessing class hierarchy' }
Behavior >> withAllSubclasses [
	"Answer an OrderedCollection with the receiver, the receiver's descendents, and the  
	receiver's descendents' subclasses."

	^self allSubclasses addFirst: self; yourself
]

{ #category : 'enumerating' }
Behavior >> withAllSubclassesDo: aBlock  [
	"Evaluate the argument, aBlock, for the receiver and each of its 
	subclasses."

	aBlock value: self.
	self allSubclassesDo: aBlock
]

{ #category : 'enumerating' }
Behavior >> withAllSuperAndSubclassesDo: aBlock [
	self allSuperclassesDo: aBlock.
	aBlock value: self.
	self allSubclassesDo: aBlock
]

{ #category : 'accessing class hierarchy' }
Behavior >> withAllSuperclasses [
	"Answer an OrderedCollection of the receiver and the receiver's 
	superclasses. See also #allSuperclasses."

	^self allSuperclasses addFirst: self; yourself
]

{ #category : 'enumerating' }
Behavior >> withAllSuperclassesDo: aBlock  [
	"Evaluate the argument, aBlock, for each of the receiver's superclasses."
	aBlock value: self.
	superclass
		ifNotNil: [superclass withAllSuperclassesDo: aBlock]
]

{ #category : 'accessing class hierarchy' }
Behavior >> withAllSuperclassesDownFrom: aSuperclass [
	
	| superclasses |
	^ self = aSuperclass
		ifTrue: [ OrderedCollection with: self]
		ifFalse: [
			superclasses := superclass withAllSuperclassesDownFrom: aSuperclass.
			superclasses add: self.
			superclasses ].
]

{ #category : 'accessing class hierarchy' }
Behavior >> withAllSuperclassesUpTo: aSuperclass [

	| classes |
	
	classes := self allSuperclassesUpTo: aSuperclass.
	classes addFirst: self.
	
	^ classes
]

{ #category : 'accessing method dictionary' }
Behavior >> withSuperclassThatIncludesSelector: aSelector do: aFoundClosure ifNone: aNoneClosure [

	^superclass
		ifNil: aNoneClosure
		ifNotNil: [
			(superclass whichClassIncludesSelector: aSelector)
				ifNil: aNoneClosure
				ifNotNil: aFoundClosure ]
]

{ #category : 'SUnit tests' }
Behavior >> withTestCaseClassDo: aFoundTestCaseClassBlock ifNone: aNoneBlock  [
		
	| potentialTestCaseClass |
	
	potentialTestCaseClass := self testCaseClass.
	
	^potentialTestCaseClass ifNil: aNoneBlock ifNotNil: aFoundTestCaseClassBlock 
	
]

{ #category : 'accessing method dictionary' }
Behavior >> zapAllMethods [
	"Remove all methods in this class which is assumed to be obsolete"

	methodDict := MethodDictionary new.
	self class isMeta ifTrue: [self class zapAllMethods]
]
