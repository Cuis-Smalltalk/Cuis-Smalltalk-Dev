"
I am a place-holder for information needed by the Debugger to inspect method activations.  I insulate the debugger from details of code generation such as exact bytecode offsets and temporary variable locations.  I have two concreate subclasses, one for methods compiled using BlueBook blocks and one for methods compiled using Closures.  These classes deal with temporary variable access. My function is to abstract the source map away from actual bytecode pcs to abstract bytecode pcs.

To reduce compilation time I try and defer as much computation to access time as possible as instances of me will be created after each compilation.

I maintain a WeakIdentityDictionary of method to DebuggerMethodMap to cache maps.  I refer to my method through a WeakArray to keep the map cache functional. If the reference from a DebuggerMethodMap to its method were strong then the method would never be dropped from the cache because the reference from its map would keep it alive.

Instance variables
	blockExtentsToTempsRefs <Dictionary of: Interval -> Array of: (Array with: String with: (Integer | (Array with: Integer with: Integer)))>
		maps a block extent to an Array of temp references for that block/method.
		Each reference is a pair of temp name and index, where the index can itself be a pair for a remote temp.
	startpcsToTempRefs <Dictionary of: Integer -> Array of: (Array with: String with: temp reference)> where
		temp reference ::= Integer
						| (Array with: Integer with: Integer)
						| (Array with: #outer with: temp reference)
"
Class {
	#name : 'DebuggerMethodMap',
	#superclass : 'Object',
	#instVars : [
		'timestamp',
		'methodReference',
		'methodNode',
		'abstractSourceRanges',
		'sortedSourceMap',
		'blockExtentsToTempRefs',
		'startpcsToTempRefs'
	],
	#classVars : [
		'AccessLock',
		'MapCache',
		'MapCacheEntries'
	],
	#category : 'Tools-Debugger',
	#package : 'Tools',
	#tag : 'Debugger'
}

{ #category : 'debugger support' }
DebuggerMethodMap class >> cacheDebugMap: aDebuggerMethodMap forMethod: aCompiledMethod [
	
	MapCache size >= MapCacheEntries ifTrue: [
		MapCache slowSize >= MapCacheEntries 
			ifFalse: [ MapCache rehash ]
			ifTrue: [
				| mapsByAge |
				mapsByAge := MapCache keys sort: [ :m1 :m2 |
					"We are holding strongly on the keys, so #at: is suitable."
					(MapCache at: m1) timestamp < (MapCache at: m2) timestamp].
				mapsByAge from: 1 to: mapsByAge size - MapCacheEntries do: [ :each |
					MapCache removeKey: each ] ] ].
	^MapCache
		at: aCompiledMethod
		put: aDebuggerMethodMap
]

{ #category : 'instance creation' }
DebuggerMethodMap class >> forMethod: aMethod "<CompiledMethod>" [
	"Answer a DebuggerMethodMap suitable for debugging activations of aMethod.
	 Answer an existing instance from the cache if it exists, cacheing a new one if required.
	Try not to block. If would block, just avoid caching: Some performance loss is better than possible system freeze."
	
	^self
		protected: [ 
			MapCache at: aMethod ifAbsent: [
				self 
					cacheDebugMap: (self uncachedForMethod: aMethod)
					forMethod: aMethod ]]
		ifWouldBlock: [ self uncachedForMethod: aMethod ]
]

{ #category : 'instance creation' }
DebuggerMethodMap class >> forMethod: aMethod "<CompiledMethod>" methodNode: methodNode "<MethodNode>" [
	"Uncached instance creation method for private use or for tests.
	 Please consider using forMethod: instead."
	^self new
		forMethod: aMethod
		methodNode: methodNode
]

{ #category : 'class initialization' }
DebuggerMethodMap class >> initialize [
	"DebuggerMethodMap initialize"

	self voidMapCache
]

{ #category : 'synchronization' }
DebuggerMethodMap class >> protected: aBlock [

	AccessLock ifNil: [ AccessLock := Mutex new ].
	^AccessLock critical: aBlock
]

{ #category : 'synchronization' }
DebuggerMethodMap class >> protected: aBlock ifWouldBlock: unprotectedBlock [
	"Try not to block.
	If our protected state is busy with someone else"

	AccessLock ifNil: [ AccessLock := Mutex new ].
	^AccessLock critical: aBlock ifWouldBlock: unprotectedBlock
]

{ #category : 'instance creation' }
DebuggerMethodMap class >> uncachedForMethod: aMethod "<CompiledMethod>" [
	"Answer a DebuggerMethodMap suitable for debugging activations of aMethod."
	
	| methodNode |
	[ methodNode := aMethod methodNode ]
		on: UndeclaredVariableWarning
		do: [ :ex | ex resume ].
	^self forMethod: aMethod methodNode: methodNode.
]

{ #category : 'class initialization' }
DebuggerMethodMap class >> voidMapCache [

	self protected: [ 
		MapCache := EphemeronIdentityDictionary new.
		MapCacheEntries := 16 ]
]

{ #category : 'source mapping' }
DebuggerMethodMap >> abstractSourceMap [
	"Answer with a Dictionary of abstractPC <Integer> to sourceRange <Interval>."

	| theMethodToScan rawSourceRanges concreteSourceRanges abstractPC scanner client |

	abstractSourceRanges ifNotNil: [ ^abstractSourceRanges].

	methodNode encoder hasGeneratedMethod
		ifTrue: [
			rawSourceRanges := methodNode encoder rawSourceRanges.
		 	theMethodToScan := self method ]
		ifFalse: [
			"If the methodNode hasn't had a method generated it doesn't have pcs set in its
			 nodes so we must generate a new method and might as well use it for scanning."
			 [methodNode rawSourceRangesAndMethodDo: [ :ranges :method |
				rawSourceRanges := ranges.
			 	theMethodToScan := method ]]
			on: UndeclaredVariableWarning
			do: [ :ex | ex resume ]].

	concreteSourceRanges := Dictionary new.
	rawSourceRanges keysAndValuesDo: [ :node :range |
		node pc ~= 0 ifTrue: [ | realRange |
			realRange := (methodNode isMultipleRanges: range) ifTrue: [ range last ] ifFalse: [ range ].
			concreteSourceRanges at: node pc put: realRange ]].

	abstractPC := 1.
	abstractSourceRanges := Dictionary new.
	scanner := InstructionStream on: theMethodToScan.
	client := InstructionClient new.
	[
		(concreteSourceRanges includesKey: scanner pc) ifTrue: [
			abstractSourceRanges at: abstractPC put: (concreteSourceRanges at: scanner pc)].
		 abstractPC := abstractPC + 1.
		 scanner interpretNextInstructionFor: client.
		 scanner atEnd ] whileFalse.
	
	^abstractSourceRanges
]

{ #category : 'initialization' }
DebuggerMethodMap >> forMethod: aMethod "<CompiledMethod>" methodNode: theMethodNode "<MethodNode>" [
	methodReference := WeakArray with: aMethod.
	methodNode := theMethodNode.
	self markRecentlyUsed
]

{ #category : 'testing' }
DebuggerMethodMap >> mapsPcTo: aPosition [

	^self abstractSourceMap anySatisfy: [:aRange | aRange includes: aPosition ] 
]

{ #category : 'accessing' }
DebuggerMethodMap >> markRecentlyUsed [
	timestamp := Time localSecondClock
]

{ #category : 'accessing' }
DebuggerMethodMap >> method [
	^methodReference at: 1
]

{ #category : 'accessing' }
DebuggerMethodMap >> namedTempAt: index in: aContext [
	"Answer the value of the temp at index in aContext where index is relative
	 to the array of temp names answered by tempNamesForContext:"
	^self
		privateTempAt: index
		in: aContext
		startpcsToBlockExtents: aContext method startpcsToBlockExtents
]

{ #category : 'accessing' }
DebuggerMethodMap >> namedTempAt: index put: aValue in: aContext [
	"Assign the value of the temp at index in aContext where index is relative
	 to the array of temp names answered by tempNamesForContext:.
	 If the value is a copied value we also need to set it along the lexical chain."
	^self
		privateTempAt: index
		in: aContext
		put: aValue
		startpcsToBlockExtents: aContext method startpcsToBlockExtents
]

{ #category : 'private' }
DebuggerMethodMap >> privateDereference: tempReference in: aContext [
	"Fetch the temporary with reference tempReference in aContext.
	 tempReference can be
		integer - direct temp reference
		#( indirectionVectorIndex tempIndex ) - remote temp in indirectionVector at index
		#( outer. temp reference ) - a temp reference in an outer context."
	^tempReference isInteger
		ifTrue:
			[aContext tempAt: tempReference]
		ifFalse:
			[tempReference first == #outer
				ifTrue:
					[self privateDereference: tempReference last
						in: aContext outerContext]
				ifFalse: "If stopped before indirection vectors are created they will be nil. Simply answer nil"
					[(aContext tempAt: tempReference first) ifNotNil:
						[:indirectionVector|
						indirectionVector at: tempReference second]]]
]

{ #category : 'private' }
DebuggerMethodMap >> privateDereference: tempReference in: aContext put: aValue [
	"Assign the temporary with reference tempReference in aContext.
	 tempReference can be
		integer - direct temp reference
		#( indirectionVectorIndex tempIndex ) - remote temp in indirectionVector at index
		#( outer. temp reference ) - a temp reference in an outer context."
	^tempReference isInteger
		ifTrue:
			[aContext tempAt: tempReference put: aValue]
		ifFalse:
			[tempReference first == #outer
				ifTrue:
					[self privateDereference: tempReference last
						in: aContext outerContext
						put: aValue]
				ifFalse: "If stopped before indirection vectors are created they will be nil."
					[(aContext tempAt: tempReference first)
						ifNil: [ self inform: 'Cannot assign remote temp because indirection vector is nil.\Too early in method execution?' withNewLines.
							nil]
						ifNotNil:
							[:indirectionVector|
							indirectionVector
								at: tempReference second
								put: aValue]]]
]

{ #category : 'private' }
DebuggerMethodMap >> privateTempAt: index in: aContext put: aValue startpcsToBlockExtents: theContextsStartpcsToBlockExtents [
	
	^self 
		privateTempAt: index 
		in: aContext 
		startpcsToBlockExtents: theContextsStartpcsToBlockExtents 
		ifXtraBindings: [ :xtraBinding | xtraBinding value: aValue ]
		ifNot: [ :ref | self privateDereference: ref in: aContext put: aValue ]
		
]

{ #category : 'private' }
DebuggerMethodMap >> privateTempAt: index in: aContext startpcsToBlockExtents: theContextsStartpcsToBlockExtents [

	^self 
		privateTempAt: index 
		in: aContext 
		startpcsToBlockExtents: theContextsStartpcsToBlockExtents 
		ifXtraBindings: [ :xtraBinding | xtraBinding value ] 
		ifNot: [ :ref | self privateDereference: ref in: aContext ]
]

{ #category : 'private' }
DebuggerMethodMap >> privateTempAt: index in: aContext startpcsToBlockExtents: theContextsStartpcsToBlockExtents ifXtraBindings: xtraBindingsBlock ifNot: notXtraBindingBlock [

	| nameRefPair namesAndRefs |
	
	namesAndRefs := self 
		privateTempRefsForContext: aContext
		startpcsToBlockExtents: theContextsStartpcsToBlockExtents.
		
	^index <= namesAndRefs size
		ifTrue: [
			nameRefPair := namesAndRefs
				at: index
				ifAbsent: [ aContext errorSubscriptBounds: index ].
			notXtraBindingBlock value: nameRefPair last ]
		ifFalse: [ xtraBindingsBlock value: (aContext method xtraBindings at: index - namesAndRefs size) ]
]

{ #category : 'private' }
DebuggerMethodMap >> privateTempRefsForContext: aContext startpcsToBlockExtents: theContextsStartpcsToBlockExtents [
	"Answer the sequence of temps in scope in aContext in the natural order,
	 outermost arguments and temporaries first, innermost last.  Each temp is
	 a pair of the temp's name followed by a reference.  The reference can be
		integer - index of temp in aContext
		#( indirectionVectorIndex tempIndex ) - remote temp in indirectionVector at index in aContext
		#( outer. temp reference ) - a temp reference in an outer context."
	blockExtentsToTempRefs ifNil:
		[blockExtentsToTempRefs := methodNode blockExtentsToTempsMap.
		 blockExtentsToTempRefs ifNil:
			["an empty method.  shouldn't be able to step into here but it
			  can happen in weird circumstances (i.e. with MethodWrapper)."
			blockExtentsToTempRefs := Dictionary new.
			blockExtentsToTempRefs
				at: (theContextsStartpcsToBlockExtents at: aContext startpc)
				put: {}].
		 startpcsToTempRefs := Dictionary new].
	^startpcsToTempRefs
		at: aContext startpc
		ifAbsentPut:
			[| localRefs outerTemps |
			 localRefs := blockExtentsToTempRefs at: (theContextsStartpcsToBlockExtents at: aContext startpc).
			 aContext outerContext
				ifNil: [localRefs]
				ifNotNil:
					[:outer|
					"Present temps in the order outermost to innermost left-to-right, but replace
					 copied outermost temps with their innermost copies"
					 outerTemps := (self
										privateTempRefsForContext: outer
										startpcsToBlockExtents: theContextsStartpcsToBlockExtents) collect:
						[:outerPair|
						localRefs
							detect: [:localPair| outerPair first = localPair first]
							ifNone: [{ outerPair first. { #outer. outerPair last } }]].
					outerTemps,
					 (localRefs reject: [:localPair| outerTemps anySatisfy: [:outerPair| localPair first = outerPair first]])]]
]

{ #category : 'source mapping' }
DebuggerMethodMap >> rangeForPC: contextsConcretePC contextIsActiveContext: contextIsActiveContext [
	"Answer the indices in the source code for the supplied pc.
	 If the context is the actve context (is at the hot end of the stack)
	 then its pc is the current pc.  But if the context isn't, because it is
	 suspended sending a message, then its current pc is the previous pc."

	| pc end |
	pc := self method abstractPCForConcretePC:
		(contextIsActiveContext
			ifTrue: [ contextsConcretePC ]
			ifFalse: [
				(self method pcPreviousTo: contextsConcretePC) ifNil: [ contextsConcretePC ]]).
	(self abstractSourceMap includesKey: pc) ifTrue: [
		^self abstractSourceMap at: pc].
	sortedSourceMap ifNil: [
		sortedSourceMap := self abstractSourceMap.
		sortedSourceMap := (sortedSourceMap keys
			collect: [ :key | key -> (sortedSourceMap at: key)])
				asSortedCollection ].
	sortedSourceMap
		findBinaryIndex: [ :assoc | pc - assoc key ]
		do: [ :i | ^(sortedSourceMap at: i) value ]
		ifNone: [ :lower :upper |
			lower = 0 ifTrue: [ ^1 to: 0 ].
			upper <= sortedSourceMap size ifTrue: [
				^(sortedSourceMap at: upper) value ].    "No match, but  a nearby element."
			end := sortedSourceMap detectMax: [ :each |
				each value last ].
			end := end value last.
			^end + 1 to: end ].

	"| method source scanner map |
	 method := DebuggerMethodMap compiledMethodAt: #rangeForPC:contextIsActiveContext:.
	method := Debugger >> #pcRange.
	source := method getSourceFromFile asString.
	scanner := InstructionStream on: method.
	map := method debuggerMap.
	Array streamContents:
		[:ranges|
		[scanner atEnd] whileFalse:
			[| range |
			 range := map rangeForPC: scanner pc contextIsActiveContext: true.
			 ((map abstractSourceMap includesKey: scanner abstractPC)
			  and: [range first ~= 0]) ifTrue:
				[ranges nextPut: (source copyFrom: range first to: range last)].
			scanner interpretNextInstructionFor: InstructionClient new]]"
]

{ #category : 'source mapping' }
DebuggerMethodMap >> sourceText [

	^methodNode sourceText
]

{ #category : 'accessing' }
DebuggerMethodMap >> tempNamesForContext: aContext [
	"Answer an Array of all the temp names in scope in aContext starting with
	 the home's first local (the first argument or first temporary if no arguments)."
	^((self
		privateTempRefsForContext: aContext
		startpcsToBlockExtents: aContext method startpcsToBlockExtents) collect: [ :pair |
			pair first]),
		(aContext method xtraBindings collect: [ :assoc | assoc key ])
]

{ #category : 'accessing' }
DebuggerMethodMap >> tempsAndValuesForContext: aContext [
	"Return a string of the temporary variabls and their current values"

	^ Text streamContents: [ :strm |
		(self tempNamesForContext: aContext) withIndexDo: [ :title :index |
			strm nextPutAll: title; nextPut: $:; space; tab.
			aContext print: (self namedTempAt: index in: aContext) on: strm.
			strm newLine]].
]

{ #category : 'accessing' }
DebuggerMethodMap >> timestamp [
	^timestamp
]
