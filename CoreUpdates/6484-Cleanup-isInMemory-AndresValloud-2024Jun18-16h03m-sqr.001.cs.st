'From Cuis7.1 [latest update: #6483] on 18 June 2024 at 4:10:56 pm'!

!Behavior methodsFor: 'enumerating' stamp: 'sqr 6/18/2024 16:06:10'!
withAllSuperAndSubclassesDo: aBlock
	self allSuperclassesDo: aBlock.
	aBlock value: self.
	self allSubclassesDo: aBlock! !


!Object methodsFor: 'tracing' stamp: 'sqr 6/18/2024 16:04:34'!
inboundPointersExcluding: objectsToExclude
"Answer a list of all objects in the system that point to me, excluding those in the collection of objectsToExclude. I do my best to avoid creating any temporary objects that point to myself, especially method and block contexts."

	| allObjectsToExclude |
	Smalltalk garbageCollect.
	"Do this to get rid of just created MethodContext instance."
	Smalltalk primitiveGarbageCollect.

	allObjectsToExclude := { thisContext }, objectsToExclude.
	^Smalltalk allObjects select: [ :object |
		((object statePointsTo: self) or: 			[object class == self])
			and: [(allObjectsToExclude statePointsTo: object) not]
		]! !


!SystemDictionary methodsFor: 'class names' stamp: 'sqr 6/18/2024 16:05:04'!
fillCaches
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
	^{ classNames. nonClassNames }! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'sqr 6/18/2024 16:06:29'!
allImplementorsOf: aSelector  localTo: aClass
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
	^aSet asArray sort! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'sqr 6/18/2024 16:05:15'!
send: startUpOrShutDown toClassesNamedIn: startUpOrShutDownList with: argument
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
	"startUpOrShutDownList removeAll: removals"! !


!DataStream class methodsFor: 'as yet unclassified' stamp: 'sqr 6/18/2024 16:05:27'!
initCachedState
	"TypeMap maps Smalltalk classes to type ID numbers which identify the data stream primitive formats.  nextPut: writes these IDs to the data stream.  NOTE: Changing these type ID numbers will invalidate all extant data stream files.  Adding new ones is OK.  
	Classes named here have special formats in the file.  If such a class has a subclass, it will use type 9 and write correctly.  It will just be slow.  (Later write the class name in the special format, then subclasses can use the type also.)
	 See nextPut:, next, typeIDFor:, & ReferenceStream>>isAReferenceType:"
	"
	DataStream initCachedState
	"

	| refTypes t |
	refTypes := OrderedCollection new.
	t := TypeMap := Dictionary newWithRoomForMoreThan: 80. "sparse for fast hashing"

	t at: UndefinedObject put: 1.   refTypes add: 0.
	t at: True put: 2.   refTypes add: 0.
	t at: False put: 3.   refTypes add: 0.
	t at: SmallInteger put: 4.	 refTypes add: 0.
	t at: String put: 5.   refTypes add: 1.
	t at: Symbol put: 6.   refTypes add: 1.
	t at: ByteArray put: 7.   refTypes add: 1.
	t at: Array put: 8.   refTypes add: 1.
	"(type ID 9 is for arbitrary instances of any class, cf. typeIDFor:)"
		refTypes add: 1.
	"(type ID 10 is for references, cf. ReferenceStream>>tryToPutReference:)"
		refTypes add: 0.
	t at: Bitmap put: 11.   refTypes add: 1.

	t at: Metaclass put: 12.   refTypes add: 0.
	"Type ID 13 is used for HyperSqueak User classes that must be reconstructed."
		refTypes add: 1.

	t at: Float put: 14.  refTypes add: 1.
	t at: BoxedFloat64 put: 14.
	t at: SmallFloat64 put: 14.

	"15: Deprecated compact Rects."
		refTypes add: 1.

	"type ID 16 is an instance with short header.  See beginInstance:size:"
		refTypes add: 1.

	t at: String put: 17.   refTypes add: 1.	"new String format, 1 or 4 bytes of length"
	t at: WordArray put: 18.  refTypes add: 1.	"bitmap-like"
	"t at: WordArrayForSegment put: 19."  refTypes add: 1.		"bitmap-like"
	t at: Float32Array put: 20.  refTypes add: 1.	"And all other word arrays, both 16-bit and 32-bit.  See methods in ArrayedCollection.  Overridden in SoundBuffer."
	"For all classes that are like WordArrays, store them the way ColorArray is stored.  As bits, and able to change endianness."
	Smalltalk do: [:cls |
		cls isBehavior ifTrue: [
			cls isPointers not & cls isVariable & cls isWords ifTrue: [
				(t includesKey: cls) ifFalse: [t at: cls put: 20]]]].

	t at: CompiledMethod put: 21.  refTypes add: 1.	"special creation method"

	t at: Character put: 22. refTypes add: 0.

	t at: UnicodeString put: 23.   refTypes add: 1.
	t at: UnicodeSymbol put: 24.   refTypes add: 1.

	"t at:  put: 25.  refTypes add: 0."
	ReferenceTypes := refTypes.		"save it"! !


!Behavior methodsFor: 'methods' stamp: 'sqr 6/18/2024 16:06:37'!
addTo: aSet methodsThatReferenceInHierarchyTo: aSymbol special: special byte: byte.
	
	self withAllSuperAndSubclassesDo: [ :class |
		class addMethodsTo: aSet thatReferenceTo: aSymbol special: special byte: byte ] 
		! !

!methodRemoval: UndefinedObject #subclassesDoGently: stamp: 'sqr 6/18/2024 16:07:32'!
UndefinedObject removeSelector: #subclassesDoGently:!
!methodRemoval: Metaclass #subclassesDoGently: stamp: 'sqr 6/18/2024 16:07:38'!
Metaclass removeSelector: #subclassesDoGently:!
!methodRemoval: Class #subclassesDoGently: stamp: 'sqr 6/18/2024 16:07:45'!
Class removeSelector: #subclassesDoGently:!
!methodRemoval: Behavior #allSubclassesDoGently: stamp: 'sqr 6/18/2024 16:06:58'!
Behavior removeSelector: #allSubclassesDoGently:!
!methodRemoval: Behavior #subclassesDoGently: stamp: 'sqr 6/18/2024 16:07:51'!
Behavior removeSelector: #subclassesDoGently:!
!methodRemoval: Behavior #withAllSuperAndSubclassesDoGently: stamp: 'sqr 6/18/2024 16:06:47'!
Behavior removeSelector: #withAllSuperAndSubclassesDoGently:!
!methodRemoval: Dictionary #at:ifPresentAndInMemory: stamp: 'sqr 6/18/2024 16:04:50'!
Dictionary removeSelector: #at:ifPresentAndInMemory:!
!methodRemoval: ProtoObject #isInMemory stamp: 'sqr 6/18/2024 16:08:38'!
ProtoObject removeSelector: #isInMemory!
