"
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

Weight tracers work analogously.
"
Class {
	#name : 'ClosureScanner',
	#superclass : 'Object',
	#instVars : [
		'target',
		'rootNode',
		'nodes',
		'scanQueue',
		'scanQueueLastSegment',
		'scanQueueRecycledSegment',
		'useDepthFirst',
		'skipWeakObjects'
	],
	#category : 'Tools-ReferenceFinder',
	#package : 'Tools',
	#tag : 'ReferenceFinder'
}

{ #category : 'convenience' }
ClosureScanner class >> openExplorerOn: anObject [

	| finder |
	finder := self target: anObject.
	finder scanClosure; openExplorerWindow
]

{ #category : 'instance creation' }
ClosureScanner class >> scanClosureWithTarget: anObject [

	| finder |
	finder := self target: anObject.
	finder scanClosure.
	^finder
]

{ #category : 'instance creation' }
ClosureScanner class >> target: anObject [

	^self new
		target: anObject;
		yourself
]

{ #category : 'private - closure scan' }
ClosureScanner >> basicScanClosure [

	self useDepthFirst
		ifTrue: [self depthFirstScanClosure]
		ifFalse: [self breadthFirstScanClosure]
]

{ #category : 'private - closure' }
ClosureScanner >> breadthFirstScanClosure [
	"Using to:do: is ~10% faster than timesRepeat:"

	self scanQueueAdd: self rootNode.
	[self scanQueue first isEmpty] whileFalse:
		[
			| segment |
			segment := self scanQueueRemoveFirstSegment.
			1 to: segment size do:
				[:some |
					| next |
					next := segment at: some.
					next breadthFirstCloseOver: self target for: self.
				].
			self scanQueueRecycledSegment: segment
		]
]

{ #category : 'private - closure scan' }
ClosureScanner >> cleanUpAfterScan [

	self nodes: nil.
	self scanQueue: nil.
	self scanQueueLastSegment: nil.
	self scanQueueRecycledSegment: nil.
	self rootNode cleanUpAfterScan
]

{ #category : 'private - closure scan storage' }
ClosureScanner >> closureTraceCompiledMethodNodeClass [

	^ReferenceTraceCompiledMethodNode
]

{ #category : 'private - closure scan storage' }
ClosureScanner >> closureTraceNodeClass [

	^ReferenceTraceNode
]

{ #category : 'private - closure scan' }
ClosureScanner >> defaultRootObject [

	^Smalltalk specialObjectsArray
]

{ #category : 'private - closure' }
ClosureScanner >> depthFirstScanClosure [

	self rootNode depthFirstCloseOver: self target for: self
]

{ #category : 'private - explorer window' }
ClosureScanner >> explorerClass [

	self subclassResponsibility
]

{ #category : 'private - explorer window' }
ClosureScanner >> explorerWindowClass [

	^ReferencesExplorerWindow
]

{ #category : 'private - explorer window' }
ClosureScanner >> explorerWindowPrefix [

	^'Exploring '
]

{ #category : 'private' }
ClosureScanner >> initialize [

	super initialize.
	self useDepthFirst: false.
	self skipWeakObjects: false
]

{ #category : 'private - closure scan storage' }
ClosureScanner >> initializeScanQueue [

	self scanQueueLastSegment: self newScanQueueSegment.
	self scanQueue: self newScanQueueSegment.
	self scanQueue add: self scanQueueLastSegment
]

{ #category : 'private - closure scan storage' }
ClosureScanner >> newNodes [

	^IdentitySet newWithRoomForMoreThan: self storagePreallocatedCapacity
]

{ #category : 'private - closure scan storage' }
ClosureScanner >> newRootNode [

	^self newRootNodeFor: self defaultRootObject
]

{ #category : 'private - closure scan storage' }
ClosureScanner >> newRootNodeFor: rootObject [

	^self
		newRootNodeFor: rootObject
		defaultNodeClass: self closureTraceNodeClass
		compiledMethodNodeClass: self closureTraceCompiledMethodNodeClass
]

{ #category : 'private - closure scan storage' }
ClosureScanner >> newRootNodeFor: rootObject [
defaultNodeClass: traceNodeClass
compiledMethodNodeClass: compiledMethodNodeClass

	| rootObjectClass node |
	rootObjectClass := ReferenceFinderMirror objectClass: rootObject.
	node := CompiledMethod == rootObjectClass
		ifTrue: [compiledMethodNodeClass object: rootObject]
		ifFalse: [traceNodeClass object: rootObject].
	node classOfObject: rootObjectClass.
	^node
]

{ #category : 'private - depth first scan queue' }
ClosureScanner >> newScanQueueSegment [
	"Segment the scan queue, otherwise the GC will be forced to scan the whole
	queue every time (obviously, the scan queue will be in the remember table)"

	| newScanQueue |
	newScanQueue := OrderedCollection basicNew.
	newScanQueue setContents: (Array new: self scanQueueSegmentTargetSize).
	newScanQueue resetTo: 1.
	^newScanQueue
]

{ #category : 'private - accessing' }
ClosureScanner >> nodes [

	^nodes
]

{ #category : 'private - accessing' }
ClosureScanner >> nodes: aDictionary [

	nodes := aDictionary
]

{ #category : 'explorer window' }
ClosureScanner >> openExplorerWindow [
	"Assumes closure scanning already done"

	| explorer window |
	explorer := self explorerClass finder: self.
	window := self explorerWindowClass
		open: explorer
		label: self explorerWindowPrefix, self target printString, '(', self target identityHash printString,  ')'.
	window expandAll
]

{ #category : 'private - closure scan' }
ClosureScanner >> prepareToScanClosure [

	| mustCleanRootNode |
	mustCleanRootNode := self rootNode notNil.
	self rootNode: self newRootNode.
	self nodes: self newNodes.
	self useDepthFirst ifFalse: [self initializeScanQueue].
	mustCleanRootNode ifTrue: [Smalltalk garbageCollect]
]

{ #category : 'accessing' }
ClosureScanner >> rootNode [

	^rootNode
]

{ #category : 'private - accessing' }
ClosureScanner >> rootNode: aNode [

	rootNode := aNode
]

{ #category : 'closure' }
ClosureScanner >> scanClosure [

	self scanClosureSkipping: Array new
]

{ #category : 'closure' }
ClosureScanner >> scanClosureSkipping: aCollection [

	self prepareToScanClosure.
	self skipInternalNodesAnd: aCollection.
	self basicScanClosure.
	self cleanUpAfterScan
]

{ #category : 'private - accessing' }
ClosureScanner >> scanQueue [

	^scanQueue
]

{ #category : 'private - accessing' }
ClosureScanner >> scanQueue: anOrderedCollection [

	scanQueue := anOrderedCollection
]

{ #category : 'private - depth first scan queue' }
ClosureScanner >> scanQueueAdd: aNode [

	self scanQueueLastSegment add: aNode.
	self scanQueueLastSegment size >= self scanQueueSegmentTargetSize
		ifTrue: [self scanQueueAddNewSegment]
]

{ #category : 'private - depth first scan queue' }
ClosureScanner >> scanQueueAddNewSegment [

	| newSegment |
	self scanQueueRecycledSegment isNil
		ifTrue: [newSegment := self newScanQueueSegment]
		ifFalse:
			[
				newSegment := self scanQueueRecycledSegment.
				newSegment removeAll.
				self scanQueueRecycledSegment: nil
			].
	self scanQueue add: newSegment.
	self scanQueueLastSegment: newSegment
]

{ #category : 'private - accessing' }
ClosureScanner >> scanQueueLastSegment [

	^scanQueueLastSegment
]

{ #category : 'private - accessing' }
ClosureScanner >> scanQueueLastSegment: anOrderedCollection [

	scanQueueLastSegment := anOrderedCollection
]

{ #category : 'private - accessing' }
ClosureScanner >> scanQueueRecycledSegment [

	^scanQueueRecycledSegment
]

{ #category : 'private - accessing' }
ClosureScanner >> scanQueueRecycledSegment: anOrderedCollection [

	scanQueueRecycledSegment := anOrderedCollection
]

{ #category : 'private - depth first scan queue' }
ClosureScanner >> scanQueueRemoveFirstSegment [

	| answer |
	answer := self scanQueue removeFirst.
	self scanQueue isEmpty ifTrue: [self scanQueueAddNewSegment].
	^answer
]

{ #category : 'private - depth first scan queue' }
ClosureScanner >> scanQueueSegmentTargetSize [

	^1024
]

{ #category : 'private - closure' }
ClosureScanner >> shouldCloseOver: anObject instanceOf: aClass [
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
	^true
]

{ #category : 'private - closure' }
ClosureScanner >> shouldCloseOverCompiledMethod: anObject [
	"Assume anObject is an instance of CompiledMethod, and
	further that compiled methods almost always have literals"

	(self nodes includes: anObject) ifTrue: [^false].
	self nodes add: anObject.
	^true
]

{ #category : 'accessing' }
ClosureScanner >> skipClasses [
	"For the time being"

	^true
]

{ #category : 'private - closure scan' }
ClosureScanner >> skipInternalNodesAnd: aCollection [
	"Assumes no bitNodes required"

	self nodes
		add: self;
		add: self rootNode object;
		addAll: aCollection
]

{ #category : 'accessing' }
ClosureScanner >> skipWeakObjects [
	"Skipping weak objects will also skip objects referenced
	from weak objects' instance variables (which are strong)"

	^skipWeakObjects
]

{ #category : 'accessing' }
ClosureScanner >> skipWeakObjects: aBoolean [

	skipWeakObjects := aBoolean
]

{ #category : 'private - closure scan storage' }
ClosureScanner >> storagePreallocatedCapacity [

	^262144
]

{ #category : 'accessing' }
ClosureScanner >> target [

	^target
]

{ #category : 'private - accessing' }
ClosureScanner >> target: anObject [

	target := anObject
]

{ #category : 'accessing' }
ClosureScanner >> useDepthFirst [

	^useDepthFirst
]

{ #category : 'accessing' }
ClosureScanner >> useDepthFirst: aBoolean [

	useDepthFirst := aBoolean
]
