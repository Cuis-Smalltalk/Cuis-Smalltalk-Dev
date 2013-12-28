'From Cuis 4.2 of 25 July 2013 [latest update: #1898] on 26 December 2013 at 6:05:59.031531 pm'!
!classDefinition: #ReferencesExplorer category: #'Tools-ReferenceFinder'!
ObjectExplorer subclass: #ReferencesExplorer
	instanceVariableNames: 'finder'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-ReferenceFinder'!

!ReferencesExplorer commentStamp: '<historical>' prior: 0!
See comments in ReferenceFinder.!

!classDefinition: #ReferencesExplorerWrapper category: #'Tools-ReferenceFinder'!
ObjectExplorerWrapper subclass: #ReferencesExplorerWrapper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-ReferenceFinder'!

!ReferencesExplorerWrapper commentStamp: '<historical>' prior: 0!
See comments in ReferenceFinder.!

!classDefinition: #HierarchicalListMorph category: #'Morphic-Views for Models'!
PluggableScrollPane subclass: #HierarchicalListMorph
	instanceVariableNames: 'selectedMorph getListSelector keystrokeActionSelector autoDeselect sortingSelector getSelectionSelector setSelectionSelector menuGetter mainView highlightedMorph autoExpand '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
Smalltalk renameClassNamed: #ObjectReferencesExplorer as: #ReferencesExplorerWindow!
!classDefinition: #ReferencesExplorerWindow category: #'Tools-ReferenceFinder'!
ObjectExplorerWindow subclass: #ReferencesExplorerWindow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-ReferenceFinder'!

!ReferencesExplorerWindow commentStamp: '<historical>' prior: 0!
See comments in ReferenceFinder.!

!classDefinition: #ReferenceFinder category: #'Tools-ReferenceFinder'!
Object subclass: #ReferenceFinder
	instanceVariableNames: 'target rootNode nodes scanQueue scanQueueLastSegment scanQueueRecycledSegment useDepthFirst skipWeakObjects'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-ReferenceFinder'!

!ReferenceFinder commentStamp: '<historical>' prior: 0!
Reference Finder for Cuis.

Introduction.

This tool traces the object reference graph recording how a given object is connected to a set of well known roots.  This information can be useful to examine object relationships.  In addition, the tool's results show why an object is not considered garbage by the system.

Generally speaking, the only public use class is ReferenceFinder.  The trace node classes are considered private implementation details.  The reference explorer classes are provided to integrate the tool with the system's retrospection capabilities (e.g. the inspector).  For the sake of convenience, you may also use the reference finder programmatically by evaluating expressions such as

	ReferenceFinder openOn: ReferenceFinder

The result is a window with a tree that shows how the ReferenceFinder class is (eventually) referenced by the system's special object array.


Advanced settings and usage.

The reference finder implementation provides a few configuration switches available to the user.  By default, the reference finder will scan using a breadth first strategy.  This is the recommended mode of operation because of breadth first's usefulness compared to depth first.  Moreover, the breadth first scan implementation has been optimized significantly, so in most cases it can go through an image in 1 second or less.  However, if depth first is preferred, one can also write expressions such as

	(ReferenceFinder target: ReferenceFinder)
		useDepthFirst: true;
		open

In addition, the reference finder will scan through weak objects by default.  If this it not desired, then one can also write expressions such as

	(ReferenceFinder target: ReferenceFinder)
		skipWeakObjects: true;
		open

Note that skipping weak objects will also skip scanning the instance variables of weak objects.  The instance variables of weak objects are strong, as opposed to weak objects' indexed slots.

Moreover, the reference finder can optionally skip references from certain objects.  This is useful in situations such as when the reference finder's UI is open.  At that point, invoking a rescan should not pick up references that exist solely because the UI is open.  The functionality can be invoked programmatically with expressions such as

	(ReferenceFinder target: ReferenceFinder)
		traceSkipping: (Array with: Smalltalk);
		open


Implementation details.

The reference finder generally traces through objects trying to find references to a given target.  Currently, the reference finder does not scan references to classes via the object header.  In the rare case that you trace a class object and find no references, try finding references to the class' instances.

Starting with a root node, generally the reference finder dives through referents by creating additional nodes.  Other than the root node, all nodes know their parent node.  When a reference to the target node is found, a new node for the target backtracks through the parent chain recording the new path.  Initially, paths are held in sets to avoid path information duplication.  At the end of the trace, a clean up phase converts those sets into sorted arrays for faster, deterministic access.

For the sake of efficiency, the implementation has several observable characteristics.  Special care is taken to avoid repeatedly calling the mirror primitives on the same object.  Nodes are created with the basicNew primitive directly to avoid megamorphic message sends (with the current implementation of new, that's two megamorphic message sends: basicNew, and initialize).  The implementation has a special trace node for CompiledMethod so that class checks for CompiledMethod class are performed only once.  The special instance of UndefinedObject, nil, is treated as a special case and is assumed not to refer to anything else.  Similarly, instances of SmallInteger are treated as a special case.  In some cases, the code tends to choose faster variants over simpler expressions.  An example choice is the use of to:do: instead of do: and timesRepeat:.

The breadth first scanning is optimized to avoid having a large scan queue around while references are being traced.  This approach avoids having a very large object in the remember table during every incremental GC.  Moreover, the queue segments can be recycled in some cases which leads to reduced garbage creation.  This approach trades off some complexity in exchange for a significant performance improvement.

The depth first scanning is implemented in a reasonably straightforward manner only.  It is assumed that breadth first scanning is typically preferable over depth first scanning.!


!ReferenceFinder commentStamp: '<historical>' prior: 0!
Reference Finder for Cuis.

Introduction.

This tool traces the object reference graph recording how a given object is connected to a set of well known roots.  This information can be useful to examine object relationships.  In addition, the tool's results show why an object is not considered garbage by the system.

Generally speaking, the only public use class is ReferenceFinder.  The trace node classes are considered private implementation details.  The reference explorer classes are provided to integrate the tool with the system's retrospection capabilities (e.g. the inspector).  For the sake of convenience, you may also use the reference finder programmatically by evaluating expressions such as

	ReferenceFinder openOn: ReferenceFinder

The result is a window with a tree that shows how the ReferenceFinder class is (eventually) referenced by the system's special object array.


Advanced settings and usage.

The reference finder implementation provides a few configuration switches available to the user.  By default, the reference finder will scan using a breadth first strategy.  This is the recommended mode of operation because of breadth first's usefulness compared to depth first.  Moreover, the breadth first scan implementation has been optimized significantly, so in most cases it can go through an image in 1 second or less.  However, if depth first is preferred, one can also write expressions such as

	(ReferenceFinder target: ReferenceFinder)
		useDepthFirst: true;
		open

In addition, the reference finder will scan through weak objects by default.  If this it not desired, then one can also write expressions such as

	(ReferenceFinder target: ReferenceFinder)
		skipWeakObjects: true;
		open

Note that skipping weak objects will also skip scanning the instance variables of weak objects.  The instance variables of weak objects are strong, as opposed to weak objects' indexed slots.

Moreover, the reference finder can optionally skip references from certain objects.  This is useful in situations such as when the reference finder's UI is open.  At that point, invoking a rescan should not pick up references that exist solely because the UI is open.  The functionality can be invoked programmatically with expressions such as

	(ReferenceFinder target: ReferenceFinder)
		traceSkipping: (Array with: Smalltalk);
		open


Implementation details.

The reference finder generally traces through objects trying to find references to a given target.  Currently, the reference finder does not scan references to classes via the object header.  In the rare case that you trace a class object and find no references, try finding references to the class' instances.

Starting with a root node, generally the reference finder dives through referents by creating additional nodes.  Other than the root node, all nodes know their parent node.  When a reference to the target node is found, a new node for the target backtracks through the parent chain recording the new path.  Initially, paths are held in sets to avoid path information duplication.  At the end of the trace, a clean up phase converts those sets into sorted arrays for faster, deterministic access.

For the sake of efficiency, the implementation has several observable characteristics.  Special care is taken to avoid repeatedly calling the mirror primitives on the same object.  Nodes are created with the basicNew primitive directly to avoid megamorphic message sends (with the current implementation of new, that's two megamorphic message sends: basicNew, and initialize).  The implementation has a special trace node for CompiledMethod so that class checks for CompiledMethod class are performed only once.  The special instance of UndefinedObject, nil, is treated as a special case and is assumed not to refer to anything else.  Similarly, instances of SmallInteger are treated as a special case.  In some cases, the code tends to choose faster variants over simpler expressions.  An example choice is the use of to:do: instead of do: and timesRepeat:.

The breadth first scanning is optimized to avoid having a large scan queue around while references are being traced.  This approach avoids having a very large object in the remember table during every incremental GC.  Moreover, the queue segments can be recycled in some cases which leads to reduced garbage creation.  This approach trades off some complexity in exchange for a significant performance improvement.

The depth first scanning is implemented in a reasonably straightforward manner only.  It is assumed that breadth first scanning is typically preferable over depth first scanning.!

!classDefinition: #ReferenceTraceNode category: #'Tools-ReferenceFinder'!
Object subclass: #ReferenceTraceNode
	instanceVariableNames: 'parent object classOfObject paths'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-ReferenceFinder'!

!ReferenceTraceNode commentStamp: '<historical>' prior: 0!
See comments in ReferenceFinder.!

!classDefinition: #ReferenceTraceCompiledMethodNode category: #'Tools-ReferenceFinder'!
ReferenceTraceNode subclass: #ReferenceTraceCompiledMethodNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-ReferenceFinder'!

!ReferenceTraceCompiledMethodNode commentStamp: '<historical>' prior: 0!
See comments in ReferenceFinder.!


!ReferencesExplorer methodsFor: 'accessing' stamp: 'sqr 12/26/2013 14:58'!
finder

	finder isNil ifTrue: [self finder: ReferenceFinder new].
	^finder! !

!ReferencesExplorer methodsFor: 'references' stamp: 'sqr 12/26/2013 18:04'!
getList

	| wrapper |
	wrapper _ ReferencesExplorerWrapper
		with: self finder rootObject
		name: self rootObject identityHash asString
		model: self finder rootNode.
	^Array with: wrapper! !

!ReferencesExplorer methodsFor: 'references' stamp: 'sqr 12/26/2013 15:58'!
rescan

	self rescanSkipping: Array new! !

!ReferencesExplorer methodsFor: 'references' stamp: 'sqr 12/26/2013 15:47'!
rescanSkipping: aCollection

	self finder traceSkipping: aCollection.
	self changed: #getList! !

!ReferencesExplorer methodsFor: 'private - accessing' stamp: 'sqr 12/26/2013 15:00'!
finder: aReferenceFinder

	finder _ aReferenceFinder! !

!ReferencesExplorer methodsFor: 'private - accessing' stamp: 'sqr 12/26/2013 18:03'!
rootObject: anObject
	"Assume a preloaded finder if the target matches"

	super rootObject: anObject.
	self finder target == anObject ifFalse:
		[
			self finder target: anObject.
			self rescan
		]! !


!ReferencesExplorer class methodsFor: 'instance creation' stamp: 'sqr 12/26/2013 15:00'!
finder: aReferenceFinder

	^self new
		finder: aReferenceFinder;
		rootObject: aReferenceFinder target;
		yourself! !


!ReferencesExplorerWrapper methodsFor: 'printing' stamp: 'sqr 12/26/2013 14:24'!
pathStringTo: anObject

	| localObject answer |
	localObject _ model object.
	answer := (String new: 32) writeStream.
	self print: localObject on: answer.
	answer
		nextPutAll: ' ';
		nextPutAll: (localObject referenceDescriptionTo: anObject);
		nextPutAll: ' --> '.
	self print: anObject on: answer.
	^answer contents! !

!ReferencesExplorerWrapper methodsFor: 'printing' stamp: 'sqr 12/26/2013 14:24'!
print: anObject on: aStream

	aStream
		nextPut: $(;
		print: anObject class;
		nextPut: $:;
		print: anObject identityHash;
		nextPut: $)! !

!ReferencesExplorerWrapper methodsFor: 'reference chain' stamp: 'sqr 12/26/2013 14:22'!
contents

	^model paths collect:
		[:each |
			| name |
			name _ self pathStringTo: each object.
			self class with: each object name: name model: each parent: self
		]! !

!ReferencesExplorerWrapper methodsFor: 'reference chain' stamp: 'sqr 12/16/2013 00:20'!
hasContents

	^model paths notEmpty! !


!HierarchicalListMorph methodsFor: 'accessing' stamp: 'sqr 12/26/2013 16:57'!
autoDeselect

	^autoDeselect ifNil: [true]! !

!HierarchicalListMorph methodsFor: 'accessing' stamp: 'sqr 12/26/2013 16:58'!
autoExpand

	^autoExpand ifNil: [false]! !

!HierarchicalListMorph methodsFor: 'initialization' stamp: 'sqr 12/26/2013 17:00'!
autoExpand: trueOrFalse

	autoExpand _ trueOrFalse! !


!InspectorWindow methodsFor: 'menu commands' stamp: 'sqr 12/26/2013 14:47'!
openReferencesFinder
	"Create and schedule a References Finder on the receiver's model's currently selected object."

	^ReferenceFinder openOn: model selection! !


!ObjectExplorerWindow methodsFor: 'GUI building' stamp: 'sqr 12/26/2013 16:18'!
expandAll

	| firstSelection |
	firstSelection _ model getList isEmpty
		ifTrue: [nil]
		ifFalse: [model getList first].
	model noteNewSelection: firstSelection.
	listMorph expandAll! !

!ObjectExplorerWindow methodsFor: 'menu commands - reference finder' stamp: 'sqr 12/26/2013 17:06'!
objectsToSkipDuringRescan
	"Avoid observing side effects of the reference finder itself"

	| answer |
	answer := OrderedCollection
		with: self
		with: self model
		with: self model getCurrentSelection.
	self allMorphsDo: [:each | answer add: each].
	^answer! !

!ObjectExplorerWindow methodsFor: 'menu commands - reference finder' stamp: 'sqr 12/26/2013 14:53'!
openReferencesFinder
	"Create and schedule a References Finder on the receiver's model's currently selected object."
	
	ReferenceFinder openOn: model object! !

!ObjectExplorerWindow methodsFor: 'menu commands - reference finder' stamp: 'sqr 12/26/2013 16:16'!
rescan

	| objectsToSkip |
	objectsToSkip := self objectsToSkipDuringRescan.
	model noteNewSelection: nil.
	self model rescanSkipping: objectsToSkip.
	self expandAll! !


!ReferencesExplorerWindow methodsFor: 'GUI building' stamp: 'sqr 12/26/2013 17:03'!
buildMorphicWindow

	super buildMorphicWindow.
	listMorph autoExpand: true! !


!IndentingListItemMorph methodsFor: 'accessing' stamp: 'sqr 12/26/2013 16:51'!
beFullyExpanded

	| allChildren |
	allChildren _ OrderedCollection new: 10.
	self recursiveAddTo: allChildren.
	allChildren do:
		[:each |
			(each isExpanded not and: [each canExpand]) ifTrue:
				[
					each toggleExpandedState.
					each beFullyExpanded
				]
		]! !


!ReferenceFinder methodsFor: 'accessing' stamp: 'sqr 12/23/2013 22:28'!
rootNode

	^rootNode! !

!ReferenceFinder methodsFor: 'accessing' stamp: 'sqr 12/16/2013 14:02'!
skipWeakObjects
	"Skipping weak objects will also skip objects referenced
	from weak objects' instance variables (which are strong)"

	^skipWeakObjects! !

!ReferenceFinder methodsFor: 'accessing' stamp: 'sqr 12/16/2013 14:03'!
skipWeakObjects: aBoolean

	skipWeakObjects _ aBoolean! !

!ReferenceFinder methodsFor: 'accessing' stamp: 'sqr 12/15/2013 12:08'!
target

	^target! !

!ReferenceFinder methodsFor: 'accessing' stamp: 'sqr 12/15/2013 23:50'!
useDepthFirst

	^useDepthFirst! !

!ReferenceFinder methodsFor: 'accessing' stamp: 'sqr 12/15/2013 23:50'!
useDepthFirst: aBoolean

	useDepthFirst _ aBoolean! !

!ReferenceFinder methodsFor: 'references' stamp: 'sqr 12/26/2013 18:00'!
open

	| explorer window |
	self trace.
	explorer _ ReferencesExplorer finder: self.
	window _ ReferencesExplorerWindow
		open: explorer
		label: 'References to ', self target printString.
	window expandAll! !

!ReferenceFinder methodsFor: 'references' stamp: 'sqr 12/26/2013 15:49'!
trace

	self traceSkipping: Array new! !

!ReferenceFinder methodsFor: 'references' stamp: 'sqr 12/26/2013 15:49'!
traceSkipping: aCollection

	self initializeSearch.
	self nodes addAll: aCollection.
	self useDepthFirst
		ifTrue: [self depthFirstTrace]
		ifFalse: [self breadthFirstTrace].
	self cleanUpAfterSearch! !

!ReferenceFinder methodsFor: 'private' stamp: 'sqr 12/23/2013 22:27'!
cleanUpAfterSearch

	self nodes: nil.
	self scanQueue: nil.
	self scanQueueLastSegment: nil.
	self scanQueueRecycledSegment: nil.
	self rootNode cleanUpAfterSearch! !

!ReferenceFinder methodsFor: 'private' stamp: 'sqr 12/23/2013 22:40'!
initialize

	super initialize.
	self useDepthFirst: false.
	self skipWeakObjects: false! !

!ReferenceFinder methodsFor: 'private' stamp: 'sqr 12/23/2013 21:03'!
initializeScanQueue

	self scanQueueLastSegment: self newScanQueueSegment.
	self scanQueue: self newScanQueueSegment.
	self scanQueue add: self scanQueueLastSegment! !

!ReferenceFinder methodsFor: 'private' stamp: 'sqr 12/26/2013 15:33'!
initializeSearch

	| mustCleanRootNode |
	mustCleanRootNode _ self rootNode notNil.
	self rootNode: self newRootNode.
	self nodes: self newNodes.
	self useDepthFirst ifFalse: [self initializeScanQueue].
	mustCleanRootNode ifFalse: [Smalltalk garbageCollect]! !

!ReferenceFinder methodsFor: 'private' stamp: 'sqr 12/23/2013 22:27'!
newNodes

	| newNodes |
	newNodes _ IdentitySet new: self storagePreallocatedCapacity.
	newNodes add: self rootNode object.
	newNodes add: self.
	^newNodes! !

!ReferenceFinder methodsFor: 'private' stamp: 'sqr 12/23/2013 22:23'!
newRootNode

	| rootObject rootObjectClass node |
	rootObject _ self rootObject.
	rootObjectClass _ self objectClass: rootObject.
	node _ CompiledMethod == rootObjectClass
		ifTrue: [ReferenceTraceCompiledMethodNode object: rootObject]
		ifFalse: [ReferenceTraceNode object: rootObject].
	node classOfObject: rootObjectClass.
	^node! !

!ReferenceFinder methodsFor: 'private' stamp: 'sqr 12/23/2013 18:25'!
rootObject

	^Smalltalk specialObjectsArray! !

!ReferenceFinder methodsFor: 'private' stamp: 'sqr 12/23/2013 21:04'!
storagePreallocatedCapacity

	^262144! !

!ReferenceFinder methodsFor: 'private - accessing' stamp: 'sqr 12/15/2013 23:44'!
nodes

	^nodes! !

!ReferenceFinder methodsFor: 'private - accessing' stamp: 'sqr 12/15/2013 23:43'!
nodes: aDictionary

	nodes _ aDictionary! !

!ReferenceFinder methodsFor: 'private - accessing' stamp: 'sqr 12/23/2013 22:28'!
rootNode: aNode

	rootNode _ aNode! !

!ReferenceFinder methodsFor: 'private - accessing' stamp: 'sqr 12/15/2013 23:52'!
scanQueue

	^scanQueue! !

!ReferenceFinder methodsFor: 'private - accessing' stamp: 'sqr 12/15/2013 23:53'!
scanQueue: anOrderedCollection

	scanQueue _ anOrderedCollection! !

!ReferenceFinder methodsFor: 'private - accessing' stamp: 'sqr 12/23/2013 20:58'!
scanQueueLastSegment

	^scanQueueLastSegment! !

!ReferenceFinder methodsFor: 'private - accessing' stamp: 'sqr 12/23/2013 20:58'!
scanQueueLastSegment: anOrderedCollection

	scanQueueLastSegment _ anOrderedCollection! !

!ReferenceFinder methodsFor: 'private - accessing' stamp: 'sqr 12/23/2013 22:00'!
scanQueueRecycledSegment

	^scanQueueRecycledSegment! !

!ReferenceFinder methodsFor: 'private - accessing' stamp: 'sqr 12/23/2013 22:01'!
scanQueueRecycledSegment: anOrderedCollection

	scanQueueRecycledSegment _ anOrderedCollection! !

!ReferenceFinder methodsFor: 'private - accessing' stamp: 'sqr 12/15/2013 12:39'!
target: anObject

	target _ anObject! !

!ReferenceFinder methodsFor: 'private - references' stamp: 'sqr 12/23/2013 22:35'!
breadthFirstTrace
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
					next breadthFirstFindReferencesTo: self target for: self.
				].
			self scanQueueRecycledSegment: segment
		]! !

!ReferenceFinder methodsFor: 'private - references' stamp: 'sqr 12/23/2013 22:27'!
depthFirstTrace

	self rootNode depthFirstFindReferencesTo: self target for: self! !

!ReferenceFinder methodsFor: 'private - references' stamp: 'sqr 12/16/2013 00:35'!
objectClass: anObject

	<primitive: 111>
	self primitiveFailed! !

!ReferenceFinder methodsFor: 'private - references' stamp: 'sqr 12/16/2013 00:36'!
objectSize: anObject
	"Answer the number of indexable variables in the argument anObject without sending
	 it a message. This mimics the action of the VM when it fetches an object's variable size.
	 Used to simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0! !

!ReferenceFinder methodsFor: 'private - references' stamp: 'sqr 12/26/2013 17:48'!
shouldTraceInto: anObject instanceOf: aClass
	"Tracing assumes UndefinedObjects do not have referents.  Also, note
	that sending isBits is faster than selecting the bits classes and doing linear
	search by a factor of about 3x.  The SmallInteger is specialized because
	overall it achieves 5% performance gain.  Finally, assume aClass is not
	CompiledMethod"

	nil == anObject ifTrue: [^false].
	(SmallInteger == aClass or: [aClass isBits]) ifTrue: [^false].
	((self objectSize: anObject) = 0 and: [aClass instSize = 0]) ifTrue: [^false].
	(self skipWeakObjects and: [aClass isWeak]) ifTrue: [^false].
	(self nodes includes: anObject) ifTrue: [^false].
	self nodes add: anObject.
	^true! !

!ReferenceFinder methodsFor: 'private - references' stamp: 'sqr 12/23/2013 18:50'!
shouldTraceIntoCompiledMethod: anObject
	"Assume anObject is an instance of CompiledMethod, and
	further that compiled methods almost always have literals"

	(self nodes includes: anObject) ifTrue: [^false].
	self nodes add: anObject.
	^true! !

!ReferenceFinder methodsFor: 'private - depth first scan queue' stamp: 'sqr 12/23/2013 22:30'!
newScanQueueSegment
	"Segment the scan queue, otherwise the GC will be forced to scan the whole
	queue every time (obviously, the scan queue will be in the remember table)"

	| newScanQueue |
	newScanQueue _ OrderedCollection new: self scanQueueSegmentTargetSize.
	newScanQueue resetTo: 1.
	^newScanQueue! !

!ReferenceFinder methodsFor: 'private - depth first scan queue' stamp: 'sqr 12/23/2013 21:27'!
scanQueueAdd: aNode

	self scanQueueLastSegment add: aNode.
	self scanQueueLastSegment size >= self scanQueueSegmentTargetSize
		ifTrue: [self scanQueueAddNewSegment]! !

!ReferenceFinder methodsFor: 'private - depth first scan queue' stamp: 'sqr 12/23/2013 22:03'!
scanQueueAddNewSegment

	| newSegment |
	self scanQueueRecycledSegment isNil
		ifTrue: [newSegment _ self newScanQueueSegment]
		ifFalse:
			[
				newSegment _ self scanQueueRecycledSegment.
				newSegment removeAll.
				self scanQueueRecycledSegment: nil
			].
	self scanQueue add: newSegment.
	self scanQueueLastSegment: newSegment! !

!ReferenceFinder methodsFor: 'private - depth first scan queue' stamp: 'sqr 12/23/2013 21:09'!
scanQueueRemoveFirstSegment

	| answer |
	answer _ self scanQueue removeFirst.
	self scanQueue isEmpty ifTrue: [self scanQueueAddNewSegment].
	^answer! !

!ReferenceFinder methodsFor: 'private - depth first scan queue' stamp: 'sqr 12/23/2013 21:54'!
scanQueueSegmentTargetSize

	^1024! !


!ReferenceFinder class methodsFor: 'convenience' stamp: 'sqr 12/26/2013 14:48'!
openOn: anObject

	| finder |
	finder _ self target: anObject.
	finder open! !

!ReferenceFinder class methodsFor: 'instance creation' stamp: 'sqr 12/15/2013 12:42'!
target: anObject

	^self new
		target: anObject;
		yourself! !

!ReferenceFinder class methodsFor: 'instance creation' stamp: 'sqr 12/16/2013 16:09'!
traceTarget: anObject

	| finder |
	finder _ self target: anObject.
	finder trace.
	^finder! !


!ReferenceTraceNode methodsFor: 'accessing' stamp: 'sqr 12/23/2013 18:46'!
classOfObject

	^classOfObject! !

!ReferenceTraceNode methodsFor: 'accessing' stamp: 'sqr 12/15/2013 13:00'!
object

	^object! !

!ReferenceTraceNode methodsFor: 'accessing' stamp: 'sqr 12/15/2013 13:00'!
parent

	^parent! !

!ReferenceTraceNode methodsFor: 'accessing' stamp: 'sqr 12/15/2013 23:21'!
paths

	paths isNil ifTrue: [self paths: Set new].
	^paths! !

!ReferenceTraceNode methodsFor: 'printing' stamp: 'sqr 12/16/2013 00:10'!
printOn: aStream

	aStream
		nextPutAll: '> ';
		print: self object! !

!ReferenceTraceNode methodsFor: 'references' stamp: 'sqr 12/23/2013 18:46'!
breadthFirstFindReferencesTo: anObject for: aFinder

	1 to: self classOfObject instSize do:
		[:each |
			self
				breadthFirstReferencesTo: anObject
				traceReferent: (self object: self object instVarAt: each)
				for: aFinder
		].
	1 to: (self objectSize: self object) do:
		[:each |
			self
				breadthFirstReferencesTo: anObject
				traceReferent: (self object: self object basicAt: each)
				for: aFinder
		]! !

!ReferenceTraceNode methodsFor: 'references' stamp: 'sqr 12/23/2013 18:46'!
depthFirstFindReferencesTo: anObject for: aFinder

	1 to: self classOfObject instSize do:
		[:each |
			self
				depthFirstReferencesTo: anObject
				traceReferent: (self object: self object instVarAt: each)
				for: aFinder
		].
	1 to: (self objectSize: self object) do:
		[:each |
			self
				depthFirstReferencesTo: anObject
				traceReferent: (self object: self object basicAt: each)
				for: aFinder
		]! !

!ReferenceTraceNode methodsFor: 'references' stamp: 'sqr 12/26/2013 17:38'!
recordPath

	self parent isNil ifTrue: [^self].
	self parent paths add: self.
	self parent recordPath! !

!ReferenceTraceNode methodsFor: 'private - accessing' stamp: 'sqr 12/23/2013 18:47'!
classOfObject: anObject

	classOfObject _ anObject! !

!ReferenceTraceNode methodsFor: 'private - accessing' stamp: 'sqr 12/26/2013 17:40'!
object: anObject

	object _ anObject! !

!ReferenceTraceNode methodsFor: 'private - accessing' stamp: 'sqr 12/15/2013 23:23'!
parent: aTraceNode

	parent _ aTraceNode! !

!ReferenceTraceNode methodsFor: 'private - accessing' stamp: 'sqr 12/15/2013 23:20'!
paths: aSet

	paths _ aSet! !

!ReferenceTraceNode methodsFor: 'private - references' stamp: 'sqr 12/23/2013 20:57'!
breadthFirstReferencesTo: aTarget traceReferent: anObject for: aFinder

	| node referentClass |
	anObject == aTarget ifTrue: [^self foundTargetAtReferent: anObject].
	referentClass _ self objectClass: anObject.
	CompiledMethod == referentClass
		ifTrue:
			[
				(aFinder shouldTraceIntoCompiledMethod: anObject) ifFalse: [^self].
				node _ ReferenceTraceCompiledMethodNode object: anObject
			]
		ifFalse:
			[
				(aFinder shouldTraceInto: anObject instanceOf: referentClass) ifFalse: [^self].
				node _ ReferenceTraceNode object: anObject
			].
	node classOfObject: referentClass.
	node parent: self.
	aFinder scanQueueAdd: node! !

!ReferenceTraceNode methodsFor: 'private - references' stamp: 'sqr 12/26/2013 16:11'!
cleanUpAfterSearch

	self paths: self paths asArray.
	self paths sort: [:x :y | self path: x comesBefore: y].
	self paths do: [:each | each cleanUpAfterSearch]! !

!ReferenceTraceNode methodsFor: 'private - references' stamp: 'sqr 12/23/2013 18:47'!
depthFirstReferencesTo: aTarget traceReferent: anObject for: aFinder

	| node referentClass |
	anObject == aTarget ifTrue: [^self foundTargetAtReferent: anObject].
	referentClass _ self objectClass: anObject.
	CompiledMethod == referentClass
		ifTrue:
			[
				(aFinder shouldTraceIntoCompiledMethod: anObject) ifFalse: [^self].
				node _ ReferenceTraceCompiledMethodNode object: anObject
			]
		ifFalse:
			[
				(aFinder shouldTraceInto: anObject instanceOf: referentClass) ifFalse: [^self].
				node _ ReferenceTraceNode object: anObject
			].
	node classOfObject: referentClass.
	node parent: self.
	node depthFirstFindReferencesTo: aTarget for: aFinder! !

!ReferenceTraceNode methodsFor: 'private - references' stamp: 'sqr 12/16/2013 16:10'!
foundTargetAtReferent: aReferent

	| node |
	node _ self class object: aReferent.
	node parent: self.
	node recordPath! !

!ReferenceTraceNode methodsFor: 'private - references' stamp: 'sqr 12/15/2013 23:40'!
object: anObject basicAt: index 
	"Answer the value of an indexable element in the argument anObject without sending
	 it a message. Fail if the argument index is not an Integer or is out of bounds, or if
	 anObject is not indexable. This mimics the action of the VM when it indexes an object.
	 Used to simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 60>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self object: anObject basicAt: index asInteger]
		ifFalse: [self errorNonIntegerIndex]! !

!ReferenceTraceNode methodsFor: 'private - references' stamp: 'sqr 12/15/2013 23:40'!
object: anObject instVarAt: anIndex
	"Primitive. Answer a fixed variable in an object. The numbering of the 
	 variables corresponds to the named instance variables. Fail if the index 
	 is not an Integer or is not the index of a fixed variable. Essential for the
	 debugger. See  Object documentation whatIsAPrimitive."

	<primitive: 73>
	"Access beyond fixed variables."
	^self object: anObject basicAt: anIndex - (self objectClass: anObject) instSize! !

!ReferenceTraceNode methodsFor: 'private - references' stamp: 'sqr 12/23/2013 18:46'!
objectClass: anObject

	<primitive: 111>
	self primitiveFailed! !

!ReferenceTraceNode methodsFor: 'private - references' stamp: 'sqr 12/15/2013 23:39'!
objectSize: anObject
	"Answer the number of indexable variables in the argument anObject without sending
	 it a message. This mimics the action of the VM when it fetches an object's variable size.
	 Used to simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0! !

!ReferenceTraceNode methodsFor: 'private - references' stamp: 'sqr 12/26/2013 16:11'!
path: x comesBefore: y

	x object identityHash < y object identityHash ifTrue: [^true].
	x object identityHash > y object identityHash ifTrue: [^false].
	^(self objectClass: x) name <= (self objectClass: y) name! !


!ReferenceTraceNode class methodsFor: 'instance creation' stamp: 'sqr 12/23/2013 22:12'!
new
	"Copied here to avoid megamorphic message sends.
	Also, these objects do not need to be sent initialize"

	<primitive: 70>
	OutOfMemory signal.
	^self new! !

!ReferenceTraceNode class methodsFor: 'instance creation' stamp: 'sqr 12/15/2013 23:22'!
object: anObject

	^self new
		object: anObject;
		yourself! !


!ReferenceTraceCompiledMethodNode methodsFor: 'references' stamp: 'sqr 12/23/2013 18:28'!
breadthFirstFindReferencesTo: anObject for: aFinder

	self object literalsDo:
		[:each |
			self
				breadthFirstReferencesTo: anObject
				traceReferent: each
				for: aFinder
		]! !

!ReferenceTraceCompiledMethodNode methodsFor: 'references' stamp: 'sqr 12/23/2013 18:31'!
depthFirstFindReferencesTo: anObject for: aFinder

	self object literalsDo:
		[:each |
			self
				depthFirstReferencesTo: anObject
				traceReferent: each
				for: aFinder
		]! !


!ProtoObject methodsFor: 'tracing' stamp: 'sqr 12/15/2013 12:22'!
referenceDescriptionTo: anObject
	"Answer a string that describes how I reference anObject"

	self class == anObject ifTrue: [ ^ 'class' ].
	1 to: self class instSize do: [ :i |
		(self instVarAt: i) == anObject ifTrue: [ ^ self class allInstVarNames at: i ]].
	1 to: self basicSize do: [ :i |
		(self basicAt: i) == anObject ifTrue: [ ^ 'at: ', i printString ]].
	^ 'unknown (maybe the reference changed)'! !


!HierarchicalListMorph methodsFor: 'commands' stamp: 'sqr 12/26/2013 16:51'!
expandAll

	scroller submorphs isEmpty ifTrue: [^self].
	scroller submorphs first beFullyExpanded.
	scroller adjustExtent.
	self setScrollDeltas! !

!HierarchicalListMorph methodsFor: 'commands' stamp: 'sqr 12/26/2013 17:01'!
toggleExpandedState: aMorph event: event

	"self setSelectedMorph: aMorph."
	(self autoExpand and: [aMorph isExpanded not])
		ifTrue: [aMorph beFullyExpanded]
		ifFalse: [aMorph toggleExpandedState].
	scroller adjustExtent.
	self setScrollDeltas! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'sqr 12/26/2013 16:58'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	| itemMorph |
	itemMorph _ self itemFromPoint: localEventPosition.
	itemMorph ifNil: [ ^self ].
	"No change if model is locked"
	self owningWindow ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	(self autoDeselect and: [ itemMorph == selectedMorph ])
		ifTrue:  [self setSelectedMorph: nil ]
		ifFalse: [ self setSelectedMorph: itemMorph ]! !

!HierarchicalListMorph methodsFor: 'initialization' stamp: 'sqr 12/26/2013 17:00'!
autoDeselect: trueOrFalse

	autoDeselect _ trueOrFalse! !

!HierarchicalListMorph methodsFor: 'initialization' stamp: 'sqr 12/26/2013 16:58'!
model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel

	self model: anObject.
	getListSelector _ getListSel.
	getSelectionSelector _ getSelectionSel.
	setSelectionSelector _ setSelectionSel.
	mainView _ aMainView.
	menuGetter _ getMenuSel.
	keystrokeActionSelector _ keyActionSel.
	self autoDeselect: true.
	self list: self getList.! !


!SystemWindow class methodsFor: 'instance creation' stamp: 'sqr 12/16/2013 14:29'!
open: model label: aString

	|  window |
	window _ self new.
	window
		model: model;
		buildMorphicWindow.
	aString ifNotNil: [ window setLabel: aString ].
	window openInWorld.
	^window! !


!InspectorWindow methodsFor: 'menu building' stamp: 'sqr 12/15/2013 12:16'!
fieldListMenu
	"Arm the supplied menu with items for the field-list of the receiver"

	| aMenu object |
	aMenu _ MenuMorph new defaultTarget: self.

	aMenu addList: #(
		('inspect (i)'							inspectSelection)
		('explore (I)'						exploreSelection)
		('basic inspect'						inspectBasic)
		('references finder'					openReferencesFinder)
		('explore pointers'					exploreObjectPointers)
		('explore all pointers (incl.weak)'	exploreAllObjectPointers)).

	object _ model object.
	(object isKindOf: Dictionary) ifTrue: [ aMenu addList: #(
		-
		('senders of this key'				sendersOfSelectedKey)
		('add key'							addEntry)
		('rename key'						renameEntry)
		('remove'							removeSelection			''		model)) ]
	
	ifFalse: [ (object isKindOf: Set) ifTrue: [ aMenu addList: #(
		-
		('remove'							removeSelection			''		model))]].

	aMenu addList: #(
		-
		('browse full (b)'					browseMethodFull)
		('browse hierarchy (h)'				browseHierarchy)
		('browse protocol (p)'				browseFullProtocol)).
	^ aMenu! !


!ObjectExplorerWindow methodsFor: 'building menus' stamp: 'sqr 12/26/2013 15:21'!
genericMenu
	"Borrow a menu from my inspector"
	
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	model getCurrentSelection
		ifNil: [
			aMenu
				add: '*nothing selected*'
				target: self
				selector: #yourself]
		ifNotNil: [
			aMenu addList: #(
				('inspect (i)'							inspectSelection)
				('explore (I)'						exploreSelection)
				('basic inspect'						inspectBasic)
				('references finder'					openReferencesFinder)
				('explore pointers'					exploreObjectPointers)
				('explore all pointers (incl.weak)'	exploreAllObjectPointers)
				-
				('browse full (b)'					browseMethodFull)
				('browse hierarchy (h)'				browseHierarchy)
				('browse protocol (p)'				browseFullProtocol)).
			aMenu addLine;
				add: 'monitor changes'
				target: model			"Model!!"
				selector: #monitor:
				argument: model getCurrentSelection.
			model class = PointerExplorer ifTrue: [
				aMenu addLine;
					add: 'shortest ref. path from globals (slow!!)'
					target: self
					selector: #expandPathFromRoot ].
			model class = ReferencesExplorer ifTrue: [
				aMenu addLine;
					add: 'rescan'
					target: self
					selector: #rescan ]].
	model basicMonitorList isEmptyOrNil
		ifFalse: [
			aMenu addLine;
				add: 'stop monitoring all'
				target: model			"Model!!"
				selector: #stopMonitoring].
	^ aMenu! !


!ReferenceTraceCompiledMethodNode reorganize!
('references' breadthFirstFindReferencesTo:for: depthFirstFindReferencesTo:for:)
!


!ReferenceTraceNode reorganize!
('accessing' classOfObject object parent paths)
('printing' printOn:)
('references' breadthFirstFindReferencesTo:for: depthFirstFindReferencesTo:for: recordPath)
('private - accessing' classOfObject: object: parent: paths:)
('private - references' breadthFirstReferencesTo:traceReferent:for: cleanUpAfterSearch depthFirstReferencesTo:traceReferent:for: foundTargetAtReferent: object:basicAt: object:instVarAt: objectClass: objectSize: path:comesBefore:)
!


!ReferenceFinder class reorganize!
('convenience' openOn:)
('instance creation' target: traceTarget:)
!


!ReferenceFinder reorganize!
('accessing' rootNode skipWeakObjects skipWeakObjects: target useDepthFirst useDepthFirst:)
('references' open trace traceSkipping:)
('private' cleanUpAfterSearch initialize initializeScanQueue initializeSearch newNodes newRootNode rootObject storagePreallocatedCapacity)
('private - accessing' nodes nodes: rootNode: scanQueue scanQueue: scanQueueLastSegment scanQueueLastSegment: scanQueueRecycledSegment scanQueueRecycledSegment: target:)
('private - references' breadthFirstTrace depthFirstTrace objectClass: objectSize: shouldTraceInto:instanceOf: shouldTraceIntoCompiledMethod:)
('private - depth first scan queue' newScanQueueSegment scanQueueAdd: scanQueueAddNewSegment scanQueueRemoveFirstSegment scanQueueSegmentTargetSize)
!


!IndentingListItemMorph reorganize!
('accessing' balloonText beExpanded beFullyExpanded canExpand firstChild indentLevel isExpanded isFirstItem isHighlighted: isSelected: isSoleItem nextSibling nextSibling:)
('converting' withoutListWrapper)
('drawing' drawOn:)
('geometry' desiredWidth)
('initialization' initWithContents:prior:forList:indentLevel: initialize)
('mouse events' inToggleArea:)
('private' hasToggle toggleRectangle withSiblingsDo:)
('private-container protocol' addChildrenForList:addingTo:withExpandedItems: complexContents openPath:adaptor:compare: recursiveAddTo: toggleExpandedState)
!


!ReferencesExplorerWindow reorganize!
('GUI building' buildMorphicWindow)
!


!ObjectExplorerWindow reorganize!
('GUI building' buildMorphicWindow expandAll initialExtent)
('building menus' genericMenu)
('keyboard shortcuts' explorerKey:from:)
('menu commands' browseClassRefs browseFullProtocol browseHierarchy browseMethodFull expandPathFromRoot exploreAllObjectPointers exploreObjectPointers exploreSelection inspectBasic inspectSelection)
('menu commands - reference finder' objectsToSkipDuringRescan openReferencesFinder rescan)
!

!methodRemoval: HierarchicalListMorph #expandAll:!
HierarchicalListMorph removeSelector: #expandAll:!
!classDefinition: #HierarchicalListMorph category: #'Morphic-Views for Models'!
PluggableScrollPane subclass: #HierarchicalListMorph
	instanceVariableNames: 'selectedMorph getListSelector keystrokeActionSelector autoDeselect autoExpand sortingSelector getSelectionSelector setSelectionSelector menuGetter mainView highlightedMorph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!

!HierarchicalListMorph reorganize!
('accessing' autoDeselect autoExpand selectedMorph)
('commands' expandAll toggleExpandedState:event:)
('drawing' drawOn: expandedForm notExpandedForm)
('event handling' itemFromPoint: keyboardFocusChange: mouseButton2Activity)
('event handling testing' handlesKeyboard)
('events' keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseEnter:)
('events-processing' processMouseMove:localPosition:)
('geometry' scrollDeltaHeight scrollDeltaWidth)
('initialization' autoDeselect: autoExpand: currentlyExpanded indentingItemClass innerMorphClass list: listItemHeight model:listGetter:indexGetter:indexSetter:mainView:menuGetter:keystrokeAction:)
('keyboard navigation' arrowKey: getSelectionIndex setSelectionIndex: toggleExpandedState:)
('menu' getMenu)
('model access' getList)
('selection' getCurrentSelectionItem highlightedMorph: listMorphAt: maximumSelection minimumSelection numSelectionsInView scrollSelectionIntoView selectedMorph: selection: selectionIndex: setSelectedMorph:)
('updating' update:)
('private' addMorphsTo:from:allowSorting:withExpandedItems:atLevel: addSubmorphsAfter:fromCollection:allowSorting: insertNewMorphs: noteRemovalOfAll:)
!


!ReferencesExplorerWrapper reorganize!
('printing' pathStringTo: print:on:)
('reference chain' contents hasContents)
!


!ReferencesExplorer class reorganize!
('instance creation' finder:)
!


!ReferencesExplorer reorganize!
('accessing' finder)
('references' getList rescan rescanSkipping:)
('private - accessing' finder: rootObject:)
!

