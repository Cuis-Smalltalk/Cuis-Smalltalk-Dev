'From Cuis 4.2 of 25 July 2013 [latest update: #2169] on 26 July 2015 at 1:41:24.998448 pm'!
!classDefinition: #WeightExplorer category: #'Tools-ReferenceFinder'!
ReferencesExplorer subclass: #WeightExplorer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-ReferenceFinder'!

!WeightExplorer commentStamp: '<historical>' prior: 0!
See comments in ClosureScanner.!

Smalltalk renameClassNamed: #ReferenceFinder as: #ClosureScanner!

!ClosureScanner commentStamp: '<historical>' prior: 0!
Closure scanners for Cuis.

This package implements a Reference Finder and a Weight Tracer.  The Reference Finder scans object memory to determine the locations from where an object is referenced.  The Weight Tracer determines the closure of objects held solely from the given object.


Reference Finder.

Introduction.

This tool traces the object reference graph recording how a given object is connected to a set of well known roots.  This information can be useful to examine object relationships.  In addition, the tool's results show why an object is not considered garbage by the system.

Generally speaking, the only public use class is ReferenceFinder.  The trace node classes are considered private implementation details.  The reference explorer classes are provided to integrate the tool with the system's retrospection capabilities (e.g. the inspector).  For the sake of convenience, you may also use the reference finder programmatically by evaluating expressions such as

	ReferenceFinder openExplorerOn: ReferenceFinder

The result is a window with a tree that shows how the ReferenceFinder class is (eventually) referenced by the system's special object array.


Advanced settings and usage.

The reference finder implementation provides a few configuration switches available to the user.  By default, the reference finder will scan using a breadth first strategy.  This is the recommended mode of operation because of breadth first's usefulness compared to depth first.  Moreover, the breadth first scan implementation has been optimized significantly, so in most cases it can go through an image in 1 second or less.  However, if depth first is preferred, one can also write expressions such as

	(ReferenceFinder target: ReferenceFinder)
		useDepthFirst: true;
		scanClosure;
		openExplorerWindow

In addition, the reference finder will scan through weak objects by default.  If this it not desired, then one can also write expressions such as

	(ReferenceFinder target: ReferenceFinder)
		skipWeakObjects: true;
		scanClosure;
		openExplorerWindow

Note that skipping weak objects will also skip scanning the instance variables of weak objects.  The instance variables of weak objects are strong, as opposed to weak objects' indexed slots.

Moreover, the reference finder can optionally skip references from certain objects.  This is useful in situations such as when the reference finder's UI is open.  At that point, invoking a rescan should not pick up references that exist solely because the UI is open.  The functionality can be invoked programmatically with expressions such as

	(ReferenceFinder target: ReferenceFinder)
		scanClosureSkipping: (Array with: Smalltalk);
		openExplorerWindow


Weight Tracer.

The Weight Tracer works similarly to the Reference Finder.  However, the answer is the subset of the given object's closure that is solely reachable from the given object.  For example, in the following code, only 'inner' is reachable solely from the inner array (please run the code in a workspace).

	outer := Array with: nil with: 'outer' copy.
	inner := Array with: outer with: 'inner' copy.
	WeightTracer openExplorerOn: inner


Implementation details.

The reference finder generally traces through objects trying to find references to a given target.  Currently, the reference finder does not scan references to classes via the object header.  In the rare case that you trace a class object and find no references, try finding references to the class' instances.

Starting with a root node, generally the reference finder dives through referents by creating additional nodes.  Other than the root node, all nodes know their parent node.  When a reference to the target node is found, a new node for the target backtracks through the parent chain recording the new path.  Initially, paths are held in sets to avoid path information duplication.  At the end of the trace, a clean up phase converts those sets into sorted arrays for faster, deterministic access.

For the sake of efficiency, the implementation has several observable characteristics.  Special care is taken to avoid repeatedly calling the mirror primitives on the same object.  Nodes are created with the basicNew primitive directly to avoid megamorphic message sends (with the current implementation of new, that's two megamorphic message sends: basicNew, and initialize).  The implementation has a special trace node for CompiledMethod so that class checks for CompiledMethod class are performed only once.  The special instance of UndefinedObject, nil, is treated as a special case and is assumed not to refer to anything else.  Similarly, instances of SmallInteger are treated as a special case.  In some cases, the code tends to choose faster variants over simpler expressions.  An example choice is the use of to:do: instead of do: and timesRepeat:.

The breadth first scanning is optimized to avoid having a large scan queue around while references are being traced.  This approach avoids having a very large object in the remember table during every incremental GC.  Moreover, the queue segments can be recycled in some cases which leads to reduced garbage creation.  This approach trades off some complexity in exchange for a significant performance improvement.

The depth first scanning is implemented in a reasonably straightforward manner only.  It is assumed that breadth first scanning is typically preferable over depth first scanning.

Weight tracers work analogously.!

!classDefinition: #ReferenceFinder category: #'Tools-ReferenceFinder'!
ClosureScanner subclass: #ReferenceFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-ReferenceFinder'!

!ReferenceFinder commentStamp: '<historical>' prior: 0!
See comments in ClosureScanner.!

!classDefinition: #WeightTracer category: #'Tools-ReferenceFinder'!
ClosureScanner subclass: #WeightTracer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-ReferenceFinder'!

!WeightTracer commentStamp: '<historical>' prior: 0!
See comments in ClosureScanner.!

Smalltalk renameClassNamed: #ReferenceTraceNode as: #ClosureTraceNode!

!ClosureTraceNode commentStamp: '<historical>' prior: 0!
See comments in ClosureScanner.!

!classDefinition: #ReferenceTraceNode category: #'Tools-ReferenceFinder'!
ClosureTraceNode subclass: #ReferenceTraceNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-ReferenceFinder'!

!ReferenceTraceNode commentStamp: '<historical>' prior: 0!
See comments in ClosureScanner.!

!classDefinition: #ReferenceTraceCompiledMethodNode category: #'Tools-ReferenceFinder'!
ReferenceTraceNode subclass: #ReferenceTraceCompiledMethodNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-ReferenceFinder'!
!classDefinition: #WeightTraceNode category: #'Tools-ReferenceFinder'!
ClosureTraceNode subclass: #WeightTraceNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-ReferenceFinder'!

!WeightTraceNode commentStamp: '<historical>' prior: 0!
See comments in ClosureScanner.!

!classDefinition: #WeightTraceCompiledMethodNode category: #'Tools-ReferenceFinder'!
WeightTraceNode subclass: #WeightTraceCompiledMethodNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-ReferenceFinder'!

!WeightTraceCompiledMethodNode commentStamp: '<historical>' prior: 0!
See comments in ClosureScanner.!

!classDefinition: #WeightExplorerWrapper category: #'Tools-ReferenceFinder'!
ReferencesExplorerWrapper subclass: #WeightExplorerWrapper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-ReferenceFinder'!

!WeightExplorerWrapper commentStamp: '<historical>' prior: 0!
See comments in ClosureScanner.!


!WeightExplorer methodsFor: 'references' stamp: 'SqR 7/16/2015 05:41'!
getList

	| wrapper |
	wrapper _ WeightExplorerWrapper
		with: self finder rootNode object
		name: self rootObject identityHash asString
		model: self finder rootNode.
	wrapper finderSkipsClasses: self finder skipClasses.
	^Array with: wrapper! !


!ClosureScanner methodsFor: 'closure' stamp: 'SqR 7/16/2015 05:25'!
scanClosure

	self scanClosureSkipping: Array new! !

!ClosureScanner methodsFor: 'closure' stamp: 'SqR 7/16/2015 05:52'!
scanClosureSkipping: aCollection

	self prepareToScanClosure.
	self skipInternalNodesAnd: aCollection.
	self basicScanClosure.
	self cleanUpAfterScan! !

!ClosureScanner methodsFor: 'explorer window' stamp: 'SqR 7/16/2015 05:20'!
openExplorerWindow
	"Assumes closure scanning already done"

	| explorer window |
	explorer _ self explorerClass finder: self.
	window _ self explorerWindowClass
		open: explorer
		label: self explorerWindowPrefix, self target printString.
	window expandAll! !

!ClosureScanner methodsFor: 'private - closure' stamp: 'SqR 7/16/2015 06:04'!
breadthFirstScanClosure
	"Using to:do: is ~10% faster than timesRepeat:"

	self scanQueueAdd: self rootNode.
	[self scanQueue first isEmpty] whileFalse:
		[
			| segment |
			segment _ self scanQueueRemoveFirstSegment.
			1 to: segment size do:
				[:some |
					| next |
					next _ segment at: some.
					next breadthFirstCloseOver: self target for: self.
				].
			self scanQueueRecycledSegment: segment
		]! !

!ClosureScanner methodsFor: 'private - closure' stamp: 'SqR 7/16/2015 06:03'!
depthFirstScanClosure

	self rootNode depthFirstCloseOver: self target for: self! !

!ClosureScanner methodsFor: 'private - closure' stamp: 'SqR 7/16/2015 06:04'!
shouldCloseOver: anObject instanceOf: aClass
	"Tracing assumes UndefinedObjects do not have referents.  Also, note
	that sending isBits is faster than selecting the bits classes and doing linear
	search by a factor of about 3x.  The SmallInteger is specialized because
	overall it achieves 5% performance gain.  Finally, assume aClass is not
	CompiledMethod"

	nil == anObject ifTrue: [^false].
	(SmallInteger == aClass or: [aClass isBits]) ifTrue: [^false].
	((ReferenceFinderMirror objectSize: anObject) = 0
		and: [aClass instSize = 0])
			ifTrue: [^false].
	(self skipWeakObjects and: [aClass isWeak]) ifTrue: [^false].
	(self nodes includes: anObject) ifTrue: [^false].
	self nodes add: anObject.
	^true! !

!ClosureScanner methodsFor: 'private - closure' stamp: 'SqR 7/16/2015 05:35'!
shouldCloseOverCompiledMethod: anObject
	"Assume anObject is an instance of CompiledMethod, and
	further that compiled methods almost always have literals"

	(self nodes includes: anObject) ifTrue: [^false].
	self nodes add: anObject.
	^true! !

!ClosureScanner methodsFor: 'private - closure scan' stamp: 'SqR 7/16/2015 05:58'!
basicScanClosure

	self useDepthFirst
		ifTrue: [self depthFirstScanClosure]
		ifFalse: [self breadthFirstScanClosure]! !

!ClosureScanner methodsFor: 'private - closure scan' stamp: 'SqR 7/16/2015 05:48'!
cleanUpAfterScan

	self nodes: nil.
	self scanQueue: nil.
	self scanQueueLastSegment: nil.
	self scanQueueRecycledSegment: nil.
	self rootNode cleanUpAfterScan! !

!ClosureScanner methodsFor: 'private - closure scan' stamp: 'SqR 7/16/2015 05:40'!
defaultRootObject

	^Smalltalk specialObjectsArray! !

!ClosureScanner methodsFor: 'private - closure scan' stamp: 'SqR 7/16/2015 05:44'!
prepareToScanClosure

	| mustCleanRootNode |
	mustCleanRootNode _ self rootNode notNil.
	self rootNode: self newRootNode.
	self nodes: self newNodes.
	self useDepthFirst ifFalse: [self initializeScanQueue].
	mustCleanRootNode ifTrue: [Smalltalk garbageCollect]! !

!ClosureScanner methodsFor: 'private - closure scan' stamp: 'SqR 7/16/2015 07:52'!
skipInternalNodesAnd: aCollection
	"Assumes no bitNodes required"

	self nodes
		add: self;
		add: self rootNode object;
		addAll: aCollection! !

!ClosureScanner methodsFor: 'private - closure scan storage' stamp: 'SqR 7/16/2015 06:46'!
closureTraceCompiledMethodNodeClass

	^ReferenceTraceCompiledMethodNode! !

!ClosureScanner methodsFor: 'private - closure scan storage' stamp: 'SqR 7/16/2015 06:14'!
closureTraceNodeClass

	^ReferenceTraceNode! !

!ClosureScanner methodsFor: 'private - closure scan storage' stamp: 'SqR 7/16/2015 06:46'!
newRootNodeFor: rootObject

	^self
		newRootNodeFor: rootObject
		defaultNodeClass: self closureTraceNodeClass
		compiledMethodNodeClass: self closureTraceCompiledMethodNodeClass! !

!ClosureScanner methodsFor: 'private - closure scan storage' stamp: 'SqR 7/16/2015 06:43'!
newRootNodeFor: rootObject
defaultNodeClass: traceNodeClass
compiledMethodNodeClass: compiledMethodNodeClass

	| rootObjectClass node |
	rootObjectClass _ ReferenceFinderMirror objectClass: rootObject.
	node _ CompiledMethod == rootObjectClass
		ifTrue: [compiledMethodNodeClass object: rootObject]
		ifFalse: [traceNodeClass object: rootObject].
	node classOfObject: rootObjectClass.
	^node! !

!ClosureScanner methodsFor: 'private - explorer window' stamp: 'SqR 7/16/2015 05:19'!
explorerClass

	self subclassResponsibility! !

!ClosureScanner methodsFor: 'private - explorer window' stamp: 'SqR 7/16/2015 05:19'!
explorerWindowClass

	^ReferencesExplorerWindow! !

!ClosureScanner methodsFor: 'private - explorer window' stamp: 'SqR 7/16/2015 05:20'!
explorerWindowPrefix

	^'Exploring '! !


!ClosureScanner class methodsFor: 'convenience' stamp: 'SqR 7/16/2015 05:30'!
openExplorerOn: anObject

	| finder |
	finder _ self target: anObject.
	finder scanClosure; openExplorerWindow! !

!ClosureScanner class methodsFor: 'instance creation' stamp: 'SqR 7/16/2015 05:30'!
scanClosureWithTarget: anObject

	| finder |
	finder _ self target: anObject.
	finder scanClosure.
	^finder! !


!ReferenceFinder methodsFor: 'private - explorer window' stamp: 'SqR 7/16/2015 05:21'!
explorerClass

	^ReferencesExplorer! !

!ReferenceFinder methodsFor: 'private - explorer window' stamp: 'SqR 7/16/2015 05:21'!
explorerWindowPrefix

	^'References to '! !


!WeightTracer methodsFor: 'weight' stamp: 'SqR 7/26/2015 12:45'!
scanClosureSkipping: anArray

	self prepareToScanClosure.
	self skipInternalNodesAnd: Array new.
	self basicScanClosure.
	self prepareToWeighClosure.
	self skipInternalNodesAnd: anArray.
	self basicScanClosure.
	self cleanUpAfterScan! !

!WeightTracer methodsFor: 'private - closure' stamp: 'SqR 7/26/2015 13:25'!
shouldCloseOver: anObject instanceOf: aClass
	"Bit objects must be counted, but not traced into"

	nil == anObject ifTrue: [^false].
	SmallInteger == aClass ifTrue: [^false].
	(self nodes includes: anObject) ifTrue: [^false].
	self nodes add: anObject.
	^true! !

!WeightTracer methodsFor: 'private - closure scan' stamp: 'SqR 7/26/2015 13:21'!
prepareToWeighClosure

	| mustCleanRootNode |
	mustCleanRootNode _ self rootNode notNil.
	self rootNode: (self newRootNodeFor: self target).
	self useDepthFirst ifFalse: [self initializeScanQueue].
	mustCleanRootNode ifFalse: [Smalltalk garbageCollect]! !

!WeightTracer methodsFor: 'private - closure scan storage' stamp: 'SqR 7/26/2015 13:20'!
closureTraceCompiledMethodNodeClass

	^WeightTraceCompiledMethodNode! !

!WeightTracer methodsFor: 'private - closure scan storage' stamp: 'SqR 7/26/2015 13:19'!
closureTraceNodeClass

	^WeightTraceNode! !

!WeightTracer methodsFor: 'private - explorer window' stamp: 'SqR 7/16/2015 05:24'!
explorerClass

	^WeightExplorer! !

!WeightTracer methodsFor: 'private - explorer window' stamp: 'SqR 7/16/2015 05:24'!
explorerWindowPrefix

	^'Objects held solely from '! !


!ClosureTraceNode methodsFor: 'closure' stamp: 'SqR 7/16/2015 06:22'!
breadthFirstCloseOver: anObject for: aFinder

	1 to: self classOfObject instSize do:
		[:each |
			self
				breadthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object instVarAt: each)
				for: aFinder
		].
	1 to: (ReferenceFinderMirror objectSize: self object) do:
		[:each |
			self
				breadthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object basicAt: each)
				for: aFinder
		]! !

!ClosureTraceNode methodsFor: 'closure' stamp: 'SqR 7/16/2015 06:22'!
depthFirstCloseOver: anObject for: aFinder

	1 to: self classOfObject instSize do:
		[:each |
			self
				depthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object instVarAt: each)
				for: aFinder
		].
	1 to: (ReferenceFinderMirror objectSize: self object) do:
		[:each |
			self
				depthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object basicAt: each)
				for: aFinder
		]! !

!ClosureTraceNode methodsFor: 'private - paths' stamp: 'SqR 7/16/2015 05:33'!
cleanUpAfterScan

	self paths: self paths asArray.
	self paths sort: [:x :y | self path: x comesBefore: y].
	self paths do: [:each | each cleanUpAfterScan]! !


!ReferenceTraceNode methodsFor: 'closure' stamp: 'SqR 7/16/2015 06:24'!
breadthFirstCloseOver: aTarget into: anObject for: aFinder

	| node referentClass |
	anObject == aTarget ifTrue: [^self foundTargetAtReferent: anObject].
	referentClass _ ReferenceFinderMirror objectClass: anObject.
	CompiledMethod == referentClass
		ifTrue:
			[
				(aFinder shouldCloseOverCompiledMethod: anObject) ifFalse: [^self].
				node _ ReferenceTraceCompiledMethodNode object: anObject
			]
		ifFalse:
			[
				(aFinder shouldCloseOver: anObject instanceOf: referentClass) ifFalse: [^self].
				node _ ReferenceTraceNode object: anObject
			].
	node classOfObject: referentClass.
	node parent: self.
	aFinder scanQueueAdd: node! !

!ReferenceTraceNode methodsFor: 'closure' stamp: 'SqR 7/16/2015 06:25'!
depthFirstCloseOver: aTarget into: anObject for: aFinder

	| node referentClass |
	anObject == aTarget ifTrue: [^self foundTargetAtReferent: anObject].
	referentClass _ ReferenceFinderMirror objectClass: anObject.
	CompiledMethod == referentClass
		ifTrue:
			[
				(aFinder shouldCloseOverCompiledMethod: anObject) ifFalse: [^self].
				node _ ReferenceTraceCompiledMethodNode object: anObject
			]
		ifFalse:
			[
				(aFinder shouldCloseOver: anObject instanceOf: referentClass) ifFalse: [^self].
				node _ ReferenceTraceNode object: anObject
			].
	node classOfObject: referentClass.
	node parent: self.
	node depthFirstCloseOver: aTarget for: aFinder! !


!ReferenceTraceCompiledMethodNode methodsFor: 'closure' stamp: 'SqR 7/16/2015 06:31'!
breadthFirstCloseOver: anObject for: aFinder

	self object literalsDo:
		[:each |
			self
				breadthFirstCloseOver: anObject
				into: each
				for: aFinder
		]! !

!ReferenceTraceCompiledMethodNode methodsFor: 'closure' stamp: 'SqR 7/16/2015 06:31'!
depthFirstCloseOver: anObject for: aFinder

	self object literalsDo:
		[:each |
			self
				depthFirstCloseOver: anObject
				into: each
				for: aFinder
		]! !


!WeightTraceNode methodsFor: 'closure' stamp: 'SqR 7/26/2015 13:15'!
breadthFirstCloseOver: anObject for: aFinder
	"Bit objects must be counted, but not traced into"

	1 to: self classOfObject instSize do:
		[:each |
			self
				breadthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object instVarAt: each)
				for: aFinder
		].
	self classOfObject isBits ifTrue: [^self].
	1 to: (ReferenceFinderMirror objectSize: self object) do:
		[:each |
			self
				breadthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object basicAt: each)
				for: aFinder
		]! !

!WeightTraceNode methodsFor: 'closure' stamp: 'SqR 7/26/2015 12:44'!
breadthFirstCloseOver: aTarget into: anObject for: aFinder

	| node referentClass |
	anObject == aTarget ifTrue: [^self foundTargetAtReferent: anObject].
	referentClass _ ReferenceFinderMirror objectClass: anObject.
	CompiledMethod == referentClass
		ifTrue:
			[
				(aFinder shouldCloseOverCompiledMethod: anObject) ifFalse: [^self].
				node _ WeightTraceCompiledMethodNode object: anObject
			]
		ifFalse:
			[
				(aFinder shouldCloseOver: anObject instanceOf: referentClass) ifFalse: [^self].
				node _ WeightTraceNode object: anObject
			].
	node classOfObject: referentClass.
	node parent: self.
	self paths add: node.
	aFinder scanQueueAdd: node! !

!WeightTraceNode methodsFor: 'closure' stamp: 'SqR 7/26/2015 13:16'!
depthFirstCloseOver: anObject for: aFinder
	"Bit objects must be counted, but not traced into"

	1 to: self classOfObject instSize do:
		[:each |
			self
				depthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object instVarAt: each)
				for: aFinder
		].
	self classOfObject isBits ifTrue: [^self].
	1 to: (ReferenceFinderMirror objectSize: self object) do:
		[:each |
			self
				depthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object basicAt: each)
				for: aFinder
		]! !

!WeightTraceNode methodsFor: 'closure' stamp: 'SqR 7/26/2015 12:44'!
depthFirstCloseOver: aTarget into: anObject for: aFinder

	| node referentClass |
	anObject == aTarget ifTrue: [^self foundTargetAtReferent: anObject].
	referentClass _ ReferenceFinderMirror objectClass: anObject.
	CompiledMethod == referentClass
		ifTrue:
			[
				(aFinder shouldCloseOverCompiledMethod: anObject) ifFalse: [^self].
				node _ WeightTraceCompiledMethodNode object: anObject
			]
		ifFalse:
			[
				(aFinder shouldCloseOver: anObject instanceOf: referentClass) ifFalse: [^self].
				node _ WeightTraceNode object: anObject
			].
	node classOfObject: referentClass.
	node parent: self.
	self paths add: node.
	node depthFirstCloseOver: aTarget for: aFinder! !


!WeightTraceCompiledMethodNode methodsFor: 'closure' stamp: 'SqR 7/16/2015 06:31'!
breadthFirstCloseOver: anObject for: aFinder

	self object literalsDo:
		[:each |
			self
				breadthFirstCloseOver: anObject
				into: each
				for: aFinder
		]! !

!WeightTraceCompiledMethodNode methodsFor: 'closure' stamp: 'SqR 7/16/2015 06:31'!
depthFirstCloseOver: anObject for: aFinder

	self object literalsDo:
		[:each |
			self
				depthFirstCloseOver: anObject
				into: each
				for: aFinder
		]! !


!WeightExplorerWrapper methodsFor: 'reference chain' stamp: 'SqR 7/16/2015 05:05'!
contents

	| answer |
	answer _ Array new: model paths size.
	model paths doWithIndex:
		[:each :eachIndex |
			| name newWrapper |
			name _ self weightStringFrom: model object to: each object count: eachIndex.
			newWrapper _ self class with: each object name: name model: each parent: self.
			newWrapper finderSkipsClasses: self finderSkipsClasses.
			answer at: eachIndex put: newWrapper.
		].
	^answer! !

!WeightExplorerWrapper methodsFor: 'printing' stamp: 'SqR 7/16/2015 05:06'!
weightStringFrom: parentObject to: childObject count: anInteger

	| answer |
	answer _ (String new: 32) writeStream.
	self print: parentObject on: answer.
	^answer contents! !


!ReferencesExplorer methodsFor: 'references' stamp: 'SqR 7/16/2015 05:41'!
getList

	| wrapper |
	wrapper _ ReferencesExplorerWrapper
		with: self finder rootNode object
		name: self rootObject identityHash asString
		model: self finder rootNode.
	wrapper finderSkipsClasses: self finder skipClasses.
	^Array with: wrapper! !

!ReferencesExplorer methodsFor: 'references' stamp: 'SqR 7/16/2015 05:26'!
rescanSkipping: aCollection

	self finder scanClosureSkipping: aCollection.
	self changed: #getList! !


!ClosureScanner methodsFor: 'private - closure scan storage' stamp: 'SqR 7/16/2015 05:44'!
initializeScanQueue

	self scanQueueLastSegment: self newScanQueueSegment.
	self scanQueue: self newScanQueueSegment.
	self scanQueue add: self scanQueueLastSegment! !

!ClosureScanner methodsFor: 'private - closure scan storage' stamp: 'SqR 7/16/2015 05:54'!
newNodes

	^IdentitySet new: self storagePreallocatedCapacity! !

!ClosureScanner methodsFor: 'private - closure scan storage' stamp: 'SqR 7/16/2015 05:54'!
newRootNode

	^self newRootNodeFor: self defaultRootObject! !

!ClosureScanner methodsFor: 'private - closure scan storage' stamp: 'SqR 7/16/2015 05:55'!
storagePreallocatedCapacity

	^262144! !


!ReferencesExplorerWrapper methodsFor: 'printing' stamp: 'SqR 7/16/2015 05:06'!
pathStringFrom: parentObject to: childObject count: anInteger

	| answer referenceDescription |
	answer _ (String new: 32) writeStream.
	referenceDescription _ self referenceDescriptionFrom: parentObject to: childObject count: anInteger.
	self print: parentObject on: answer.
	answer
		nextPutAll: ' ';
		nextPutAll: referenceDescription;
		nextPutAll: ' --> '.
	self print: childObject on: answer.
	^answer contents! !

!ReferencesExplorerWrapper methodsFor: 'printing' stamp: 'SqR 7/15/2015 06:45'!
referenceDescriptionFrom: parentObject to: childObject count: anInteger

	| parentClass referencesSoFar |
	referencesSoFar _ 0.
	parentClass _ ReferenceFinderMirror objectClass: parentObject.
	(self finderSkipsClasses not and: [parentClass == childObject]) ifTrue:
		[
			referencesSoFar _ referencesSoFar + 1.
			referencesSoFar = anInteger ifTrue: [^'class']
		].
	parentClass == CompiledMethod
		ifTrue:
			[
				1 to: parentObject numLiterals do:
					[:eachIndex |
						| eachLiteral |
						eachLiteral _ parentObject literalAt: eachIndex.
						eachLiteral == childObject ifTrue:
							[
								referencesSoFar _ referencesSoFar + 1.
								referencesSoFar = anInteger ifTrue: [^'literalAt: ', eachIndex printString]
							]
					]
			]
		ifFalse:
			[
				1 to: parentClass instSize do:
					[:eachIndex |
						| eachVariable |
						eachVariable _ ReferenceFinderMirror object: parentObject instVarAt: eachIndex.
						eachVariable == childObject ifTrue:
							[
								referencesSoFar _ referencesSoFar + 1.
								referencesSoFar = anInteger ifTrue: [^parentClass allInstVarNames at: eachIndex]
							]
					].
				1 to: (ReferenceFinderMirror objectSize: parentObject) do:
					[:eachIndex |
						| eachSlot |
						eachSlot _ ReferenceFinderMirror object: parentObject basicAt: eachIndex.
						eachSlot == childObject ifTrue:
							[
								referencesSoFar _ referencesSoFar + 1.
								referencesSoFar = anInteger ifTrue: [^'at: ', eachIndex printString]
							]
					]
			].
	"self halt."
	^'unknown (maybe the reference changed)'! !

!ReferencesExplorerWrapper methodsFor: 'reference chain' stamp: 'SqR 7/16/2015 05:05'!
contents

	| answer |
	answer _ Array new: model paths size.
	model paths doWithIndex:
		[:each :eachIndex |
			| name newWrapper |
			name _ self pathStringFrom: model object to: each object count: eachIndex.
			newWrapper _ self class with: each object name: name model: each parent: self.
			newWrapper finderSkipsClasses: self finderSkipsClasses.
			answer at: eachIndex put: newWrapper.
		].
	^answer! !


!InspectorWindow methodsFor: 'menu commands' stamp: 'SqR 7/16/2015 05:30'!
openReferencesFinder
	"Create and schedule a References Finder on the receiver's model's currently selected object."

	^ReferenceFinder openExplorerOn: model selection! !


!ObjectExplorerWindow methodsFor: 'menu commands - reference finder' stamp: 'SqR 7/16/2015 05:30'!
openReferencesFinder
	"Create and schedule a References Finder on the receiver's model's currently selected object."
	
	ReferenceFinder openExplorerOn: model object! !


!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'SqR 7/16/2015 05:30'!
openReferencesFinder

	"Create and schedule a References Finder on the receiver's model's currently selected object."
	model selectedProcess ifNotNil: [ :pr | ReferenceFinder openExplorerOn: pr]! !


!WeightExplorerWrapper reorganize!
('reference chain' contents)
('printing' weightStringFrom:to:count:)
!


!WeightTraceCompiledMethodNode reorganize!
('closure' breadthFirstCloseOver:for: depthFirstCloseOver:for:)
!


!WeightTraceNode reorganize!
('closure' breadthFirstCloseOver:for: breadthFirstCloseOver:into:for: depthFirstCloseOver:for: depthFirstCloseOver:into:for:)
!

!methodRemoval: ReferenceTraceCompiledMethodNode #breadthFirstClosureOf:for:!
ReferenceTraceCompiledMethodNode removeSelector: #breadthFirstClosureOf:for:!
!methodRemoval: ReferenceTraceCompiledMethodNode #breadthFirstFindReferencesTo:for:!
ReferenceTraceCompiledMethodNode removeSelector: #breadthFirstFindReferencesTo:for:!
!methodRemoval: ReferenceTraceCompiledMethodNode #breadthFirstWeigh:for:!
ReferenceTraceCompiledMethodNode removeSelector: #breadthFirstWeigh:for:!
!methodRemoval: ReferenceTraceCompiledMethodNode #depthFirstClosureOf:for:!
ReferenceTraceCompiledMethodNode removeSelector: #depthFirstClosureOf:for:!
!methodRemoval: ReferenceTraceCompiledMethodNode #depthFirstFindReferencesTo:for:!
ReferenceTraceCompiledMethodNode removeSelector: #depthFirstFindReferencesTo:for:!

!ReferenceTraceCompiledMethodNode reorganize!
('closure' breadthFirstCloseOver:for: depthFirstCloseOver:for:)
!


!ReferenceTraceNode reorganize!
('closure' breadthFirstCloseOver:into:for: depthFirstCloseOver:into:for:)
!

!methodRemoval: ClosureTraceNode #breadthFirstClosureOf:for:!
ClosureTraceNode removeSelector: #breadthFirstClosureOf:for:!
!methodRemoval: ClosureTraceNode #breadthFirstClosureOf:traceReferent:for:!
ClosureTraceNode removeSelector: #breadthFirstClosureOf:traceReferent:for:!
!methodRemoval: ClosureTraceNode #breadthFirstFindReferencesTo:for:!
ClosureTraceNode removeSelector: #breadthFirstFindReferencesTo:for:!
!methodRemoval: ClosureTraceNode #breadthFirstReferencesTo:traceReferent:for:!
ClosureTraceNode removeSelector: #breadthFirstReferencesTo:traceReferent:for:!
!methodRemoval: ClosureTraceNode #breadthFirstWeigh:for:!
ClosureTraceNode removeSelector: #breadthFirstWeigh:for:!
!methodRemoval: ClosureTraceNode #breadthFirstWeigh:traceReferent:for:!
ClosureTraceNode removeSelector: #breadthFirstWeigh:traceReferent:for:!
!methodRemoval: ClosureTraceNode #cleanUpAfterSearch!
ClosureTraceNode removeSelector: #cleanUpAfterSearch!
!methodRemoval: ClosureTraceNode #depthFirstClosureOf:for:!
ClosureTraceNode removeSelector: #depthFirstClosureOf:for:!
!methodRemoval: ClosureTraceNode #depthFirstClosureOf:traceReferent:for:!
ClosureTraceNode removeSelector: #depthFirstClosureOf:traceReferent:for:!
!methodRemoval: ClosureTraceNode #depthFirstFindReferencesTo:for:!
ClosureTraceNode removeSelector: #depthFirstFindReferencesTo:for:!
!methodRemoval: ClosureTraceNode #depthFirstReferencesTo:traceReferent:for:!
ClosureTraceNode removeSelector: #depthFirstReferencesTo:traceReferent:for:!
!methodRemoval: ClosureTraceNode #depthFirstWeigh:for:!
ClosureTraceNode removeSelector: #depthFirstWeigh:for:!
!methodRemoval: ClosureTraceNode #depthFirstWeigh:traceReferent:for:!
ClosureTraceNode removeSelector: #depthFirstWeigh:traceReferent:for:!

!ClosureTraceNode reorganize!
('accessing' classOfObject object parent paths)
('closure' breadthFirstCloseOver:for: depthFirstCloseOver:for:)
('paths' recordPath)
('printing' printOn:)
('private - accessing' classOfObject: object: parent: paths:)
('private - paths' cleanUpAfterScan foundTargetAtReferent: path:comesBefore:)
!

!methodRemoval: WeightTracer #basicWeighClosure!
WeightTracer removeSelector: #basicWeighClosure!
!methodRemoval: WeightTracer #bitNodes!
WeightTracer removeSelector: #bitNodes!
!methodRemoval: WeightTracer #bitNodes:!
WeightTracer removeSelector: #bitNodes:!
!methodRemoval: WeightTracer #breadthFirstClosure!
WeightTracer removeSelector: #breadthFirstClosure!
!methodRemoval: WeightTracer #cleanUpAfterScan!
WeightTracer removeSelector: #cleanUpAfterScan!
!methodRemoval: WeightTracer #depthFirstClosure!
WeightTracer removeSelector: #depthFirstClosure!
!methodRemoval: WeightTracer #newBitNodes!
WeightTracer removeSelector: #newBitNodes!
!methodRemoval: WeightTracer #newWeighRootNodeFor:!
WeightTracer removeSelector: #newWeighRootNodeFor:!
!methodRemoval: WeightTracer #newWeightRootNodeFor:!
WeightTracer removeSelector: #newWeightRootNodeFor:!
!methodRemoval: WeightTracer #prepareToScanClosure!
WeightTracer removeSelector: #prepareToScanClosure!
!methodRemoval: WeightTracer #shouldCloseOverCompiledMethod:!
WeightTracer removeSelector: #shouldCloseOverCompiledMethod:!
!methodRemoval: WeightTracer #shouldWeigh:instanceOf:!
WeightTracer removeSelector: #shouldWeigh:instanceOf:!
!methodRemoval: WeightTracer #shouldWeighCompiledMethod:!
WeightTracer removeSelector: #shouldWeighCompiledMethod:!
!methodRemoval: WeightTracer #weightTraceCompiledMethodNodeClass!
WeightTracer removeSelector: #weightTraceCompiledMethodNodeClass!
!methodRemoval: WeightTracer #weightTraceNodeClass!
WeightTracer removeSelector: #weightTraceNodeClass!

!WeightTracer reorganize!
('weight' scanClosureSkipping:)
('private - closure' shouldCloseOver:instanceOf:)
('private - closure scan' prepareToWeighClosure)
('private - closure scan storage' closureTraceCompiledMethodNodeClass closureTraceNodeClass)
('private - explorer window' explorerClass explorerWindowPrefix)
!

!methodRemoval: ReferenceFinder #scanClosureSkipping:!
ReferenceFinder removeSelector: #scanClosureSkipping:!

!ReferenceFinder reorganize!
('private - explorer window' explorerClass explorerWindowPrefix)
!

!methodRemoval: ClosureScanner class #openOn:!
ClosureScanner class removeSelector: #openOn:!
!methodRemoval: ClosureScanner class #openTraceOn:!
ClosureScanner class removeSelector: #openTraceOn:!
!methodRemoval: ClosureScanner class #openWeighOn:!
ClosureScanner class removeSelector: #openWeighOn:!
!methodRemoval: ClosureScanner class #traceTarget:!
ClosureScanner class removeSelector: #traceTarget:!
!methodRemoval: ClosureScanner class #weighTarget:!
ClosureScanner class removeSelector: #weighTarget:!
!methodRemoval: ClosureScanner #breadthFirstClosure!
ClosureScanner removeSelector: #breadthFirstClosure!
!methodRemoval: ClosureScanner #breadthFirstTrace!
ClosureScanner removeSelector: #breadthFirstTrace!
!methodRemoval: ClosureScanner #breadthFirstWeigh!
ClosureScanner removeSelector: #breadthFirstWeigh!
!methodRemoval: ClosureScanner #cleanUpAfterSearch!
ClosureScanner removeSelector: #cleanUpAfterSearch!
!methodRemoval: ClosureScanner #closureCompiledMethodTraceNodeClass!
ClosureScanner removeSelector: #closureCompiledMethodTraceNodeClass!
!methodRemoval: ClosureScanner #depthFirstClosure!
ClosureScanner removeSelector: #depthFirstClosure!
!methodRemoval: ClosureScanner #depthFirstTrace!
ClosureScanner removeSelector: #depthFirstTrace!
!methodRemoval: ClosureScanner #depthFirstWeigh!
ClosureScanner removeSelector: #depthFirstWeigh!
!methodRemoval: ClosureScanner #explorerWindowRootObject!
ClosureScanner removeSelector: #explorerWindowRootObject!
!methodRemoval: ClosureScanner #initializeSearch!
ClosureScanner removeSelector: #initializeSearch!
!methodRemoval: ClosureScanner #initializeWeigh!
ClosureScanner removeSelector: #initializeWeigh!
!methodRemoval: ClosureScanner #initializeWeighFor:!
ClosureScanner removeSelector: #initializeWeighFor:!
!methodRemoval: ClosureScanner #newExplorer!
ClosureScanner removeSelector: #newExplorer!
!methodRemoval: ClosureScanner #newExplorerWindow!
ClosureScanner removeSelector: #newExplorerWindow!
!methodRemoval: ClosureScanner #newRootNodeFor:defaultNodeClass::compiledMethodNodeClass:!
ClosureScanner removeSelector: #newRootNodeFor:defaultNodeClass::compiledMethodNodeClass:!
!methodRemoval: ClosureScanner #open!
ClosureScanner removeSelector: #open!
!methodRemoval: ClosureScanner #openReferences!
ClosureScanner removeSelector: #openReferences!
!methodRemoval: ClosureScanner #openWeight!
ClosureScanner removeSelector: #openWeight!
!methodRemoval: ClosureScanner #rootObject!
ClosureScanner removeSelector: #rootObject!
!methodRemoval: ClosureScanner #shouldClose:instanceOf:!
ClosureScanner removeSelector: #shouldClose:instanceOf:!
!methodRemoval: ClosureScanner #shouldCloseCompiledMethod:!
ClosureScanner removeSelector: #shouldCloseCompiledMethod:!
!methodRemoval: ClosureScanner #shouldTraceInto:instanceOf:!
ClosureScanner removeSelector: #shouldTraceInto:instanceOf:!
!methodRemoval: ClosureScanner #shouldTraceIntoCompiledMethod:!
ClosureScanner removeSelector: #shouldTraceIntoCompiledMethod:!
!methodRemoval: ClosureScanner #shouldWeigh:instanceOf:!
ClosureScanner removeSelector: #shouldWeigh:instanceOf:!
!methodRemoval: ClosureScanner #shouldWeighCompiledMethod:!
ClosureScanner removeSelector: #shouldWeighCompiledMethod:!
!methodRemoval: ClosureScanner #trace!
ClosureScanner removeSelector: #trace!
!methodRemoval: ClosureScanner #traceSkipping:!
ClosureScanner removeSelector: #traceSkipping:!
!methodRemoval: ClosureScanner #weigh!
ClosureScanner removeSelector: #weigh!

!ClosureScanner reorganize!
('accessing' rootNode skipClasses skipWeakObjects skipWeakObjects: target useDepthFirst useDepthFirst:)
('closure' scanClosure scanClosureSkipping:)
('explorer window' openExplorerWindow)
('private' initialize)
('private - accessing' nodes nodes: rootNode: scanQueue scanQueue: scanQueueLastSegment scanQueueLastSegment: scanQueueRecycledSegment scanQueueRecycledSegment: target:)
('private - closure' breadthFirstScanClosure depthFirstScanClosure shouldCloseOver:instanceOf: shouldCloseOverCompiledMethod:)
('private - closure scan' basicScanClosure cleanUpAfterScan defaultRootObject prepareToScanClosure skipInternalNodesAnd:)
('private - closure scan storage' closureTraceCompiledMethodNodeClass closureTraceNodeClass initializeScanQueue newNodes newRootNode newRootNodeFor: newRootNodeFor:defaultNodeClass:compiledMethodNodeClass: storagePreallocatedCapacity)
('private - depth first scan queue' newScanQueueSegment scanQueueAdd: scanQueueAddNewSegment scanQueueRemoveFirstSegment scanQueueSegmentTargetSize)
('private - explorer window' explorerClass explorerWindowClass explorerWindowPrefix)
!


!WeightExplorer reorganize!
('references' getList)
!

!classRemoval: #Cooler!
Smalltalk removeClassNamed: #Cooler!
