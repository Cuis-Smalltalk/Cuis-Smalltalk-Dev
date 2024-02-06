'From Cuis6.3 [latest update: #6208] on 6 February 2024 at 9:45:51 am'!

!Set class methodsFor: 'instance creation' stamp: 'jmv 2/6/2024 09:44:16'!
newFrom: aCollection
	"Create a new collection containing all the elements from aCollection"

	^(self newWithRoomForMoreThan: aCollection size)
		addAll: aCollection;
		yourself! !


!WeakRegistry class methodsFor: 'instance creation' stamp: 'jmv 2/6/2024 09:44:01'!
newFrom: aCollection
	"Create a new collection containing all the elements from aCollection"

	^(self newWithRoomForMoreThan: aCollection size)
		addAll: aCollection;
		yourself! !

!WeakRegistry class methodsFor: 'instance creation' stamp: 'jmv 2/6/2024 09:41:03'!
newWithRoomForMoreThan: n
	| registry |
	registry := super new initialize: n.
	WeakArray addWeakDependent: registry.
	^registry! !


!WeakRegistry methodsFor: 'initialization' stamp: 'jmv 2/6/2024 09:31:15'!
initialize: n
	valueDictionary := WeakIdentityKeyDictionary newWithRoomForMoreThan: n.
	accessLock := Semaphore forMutualExclusion! !


!WeakRegistry class methodsFor: 'instance creation' stamp: 'jmv 2/6/2024 09:31:33'!
new
	^self newWithRoomForMoreThan: 5! !


!DataStream class methodsFor: 'as yet unclassified' stamp: 'jmv 2/6/2024 09:31:47'!
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
		cls isInMemory ifTrue: [
			cls isBehavior ifTrue: [
				cls isPointers not & cls isVariable & cls isWords ifTrue: [
					(t includesKey: cls) ifFalse: [t at: cls put: 20]]]]].

	t at: CompiledMethod put: 21.  refTypes add: 1.	"special creation method"

	t at: Character put: 22. refTypes add: 0.

	t at: UnicodeString put: 23.   refTypes add: 1.
	t at: UnicodeSymbol put: 24.   refTypes add: 1.

	"t at:  put: 25.  refTypes add: 0."
	ReferenceTypes := refTypes.		"save it"! !


!ReferenceStream methodsFor: 'writing' stamp: 'jmv 2/6/2024 09:32:04'!
reset
	"PRIVATE -- Reset my internal state.
	   11/15-17/92 jhm: Added transients and fwdRefEnds.
	   7/11/93 sw: Give substantial initial sizes to avoid huge time spent growing.
	   9/3/93 sw: monster version for Sasha"

	super reset.
	references := IdentityDictionary newWithRoomForMoreThan: 4096 * 5.
"	objects := IdentityDictionary newWithRoomForMoreThan: 4096 * 5.
	fwdRefEnds := IdentityDictionary new.
"
	blockers ifNil: [blockers := IdentityDictionary new].
 ! !

!ReferenceStream methodsFor: 'writing' stamp: 'jmv 2/6/2024 09:32:11'!
setStream: aStream
	"PRIVATE -- Initialization method."

	super setStream: aStream.
	references := IdentityDictionary newWithRoomForMoreThan: 4096 * 5.
	objects := IdentityDictionary newWithRoomForMoreThan: 4096 * 5.
	fwdRefEnds := IdentityDictionary new.
	skipping := IdentitySet new.
	blockers ifNil: [blockers := IdentityDictionary new].	"keep blockers we just passed in"
! !

!ReferenceStream methodsFor: 'writing' stamp: 'jmv 2/6/2024 09:32:17'!
setStream: aStream reading: isReading
	"PRIVATE -- Initialization method."

	super setStream: aStream reading: isReading.
	"isReading ifFalse: [  when we are sure"
	references := IdentityDictionary newWithRoomForMoreThan: 4096 * 5.
	isReading ifTrue: [
		objects := IdentityDictionary newWithRoomForMoreThan: 4096 * 5.
		skipping := IdentitySet new.
		fwdRefEnds := IdentityDictionary new].
	blockers ifNil: [blockers := IdentityDictionary new].	"keep blockers we just passed in"
! !

!ReferenceStream methodsFor: 'statistics' stamp: 'jmv 2/6/2024 09:32:24'!
statisticsOfRefs
	"Analyze the information in references, the objects being written out"

	| parents n kids nm ownerBags tallies owners objParent normalReferences |
	normalReferences := self references.	"Exclude unrealized weaks"
	parents := IdentityDictionary newWithRoomForMoreThan: normalReferences size * 2.
	n := 0.
	'Finding Owners...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: normalReferences size
	during: [ :barBlock |
	normalReferences keysDo:
		[ :parent | barBlock value: (n := n+1).
		kids := parent class isFixed
			ifTrue: [(1 to: parent class instSize) collect: [:i | parent instVarAt: i]]
			ifFalse: [parent class isBits ifTrue: [Array new]
					 ifFalse: [(1 to: parent basicSize) collect: [:i | parent basicAt: i]]].
		(kids select: [:x | normalReferences includesKey: x])
			do: [:child | parents at: child put: parent]]].
	ownerBags := Dictionary new.
	tallies := Bag new.
	n := 0.
	'Tallying Owners...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: normalReferences size
	during: [ :barBlock |
	normalReferences keysDo:  "For each class of obj, tally a bag of owner classes"
		[ :obj | barBlock value: (n := n+1).
		nm := obj class name.
		tallies add: nm.
		owners := ownerBags at: nm ifAbsent: [ownerBags at: nm put: Bag new].
		(objParent := parents at: obj ifAbsent: nil) ifNotNil: [
			owners add: objParent class name]]].
	^ String streamContents: [ :strm | 
		tallies sortedCounts do: [ :assn |
			n := assn key.  nm := assn value.
			owners := ownerBags at: nm.
			strm newLine; nextPutAll: nm; space; print: n.
			owners size > 0 ifTrue: [
				strm newLine; tab; print: owners sortedCounts]]]! !


!VariableNode class methodsFor: 'class initialization' stamp: 'jmv 2/6/2024 09:39:13'!
initClassCachedState
	"
	VariableNode initialize. Decompiler initialize
	"
	| encoder |
	encoder := Encoder new.
	StdVariables := Dictionary newWithRoomForMoreThan: 16.
	encoder
		fillDict: StdVariables
		with: VariableNode
		mapping: #('self' 'thisContext' 'super' 'nil' 'false' 'true' )
		to: (Array with: LdSelf with: LdThisContext with: LdSuper)
				, (Array with: LdNil with: LdFalse with: LdTrue).
	StdSelectors := Dictionary newWithRoomForMoreThan: 64.
	encoder
		fillDict: StdSelectors
		with: SpecialSelectorNode
		mapping: ((1 to: Smalltalk specialSelectorSize) collect: 
							[:i | Smalltalk specialSelectorAt: i])
		to: (1 to: Smalltalk specialSelectorSize) asArray.
	StdLiterals := LiteralDictionary newWithRoomForMoreThan: 16.
	encoder
		fillDict: StdLiterals
		with: LiteralNode
		mapping: #(-1 0 1 2 )
		to: (LdMinus1 to: LdMinus1 + 3).
	encoder initScopeAndLiteralTables.

	NodeNil := encoder encodeVariable: 'nil'.
	NodeTrue := encoder encodeVariable: 'true'.
	NodeFalse := encoder encodeVariable: 'false'.
	NodeSelf := encoder encodeVariable: 'self'.
	NodeThisContext := encoder encodeVariable: 'thisContext'.
	NodeSuper := encoder encodeVariable: 'super'! !


!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 2/6/2024 09:32:38'!
removeAllMorphsIn: aCollection
	"greatly speeds up the removal of *lots* of submorphs"
	| set |
	aCollection isEmpty ifTrue: [ ^self ].
	set := IdentitySet newWithRoomForMoreThan: aCollection size * 4 // 3.
	aCollection do: [ :each | each owner == self ifTrue: [ set add: each ]].
	set isEmpty ifTrue: [ ^self ].
	set do: [ :m | m privateOwner: nil ].
	submorphs := submorphs reject: [ :each | set includes: each].
	set do: [ :m | self removedMorph: m ].
	self someSubmorphPositionOrExtentChanged.
	self redrawNeeded.! !


!ClosureScanner methodsFor: 'private - closure scan storage' stamp: 'SqR 7/16/2015 05:54'!
newNodes

	^IdentitySet newWithRoomForMoreThan: self storagePreallocatedCapacity! !

!ClosureScanner methodsFor: 'private - depth first scan queue' stamp: 'sqr 12/23/2013 22:30'!
newScanQueueSegment
	"Segment the scan queue, otherwise the GC will be forced to scan the whole
	queue every time (obviously, the scan queue will be in the remember table)"

	| newScanQueue |
	newScanQueue := OrderedCollection ofSize: self scanQueueSegmentTargetSize.
	newScanQueue resetTo: 1.
	^newScanQueue! !

!methodRemoval: WeakRegistry class #new: stamp: 'jmv 2/6/2024 09:43:08'!
WeakRegistry class removeSelector: #new:!
