'From Cuis 4.2 of 25 July 2013 [latest update: #2943] on 20 September 2016 at 7:19:08 pm'!
!classDefinition: #DebuggerMethodMapForClosureCompiledMethods category: #'Tools-Debugger'!
Object subclass: #DebuggerMethodMapForClosureCompiledMethods
	instanceVariableNames: 'blockExtentsToTempRefs startpcsToTempRefs timestamp methodReference abstractSourceRanges sortedSourceMap methodNode '
	classVariableNames: 'MapCache MapCacheEntries AccessLock '
	poolDictionaries: ''
	category: 'Tools-Debugger'!

!DebuggerMethodMapForClosureCompiledMethods commentStamp: 'jmv 9/20/2016 19:12:17' prior: 0!
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
						| (Array with: #outer with: temp reference)!


!DebuggerMethodMapForClosureCompiledMethods commentStamp: 'jmv 9/20/2016 19:12:17' prior: 0!
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
						| (Array with: #outer with: temp reference)!


!DebuggerMethodMapForClosureCompiledMethods methodsFor: 'accessing' stamp: 'jmv 4/17/2013 11:56'!
markRecentlyUsed
	timestamp := Time localSecondClock! !

!DebuggerMethodMapForClosureCompiledMethods methodsFor: 'accessing' stamp: 'eem 6/5/2008 09:21'!
method
	^methodReference at: 1! !

!DebuggerMethodMapForClosureCompiledMethods methodsFor: 'accessing' stamp: 'jmv 4/2/2016 23:05'!
tempsAndValuesForContext: aContext
	"Return a string of the temporary variabls and their current values"
	| aStream |
	aStream := WriteStream on: (String new: 100).
	(self tempNamesForContext: aContext) withIndexDo: [ :title :index |
		aStream nextPutAll: title; nextPut: $:; space; tab.
		aContext print: (self namedTempAt: index in: aContext) on: aStream.
		aStream newLine].
	^aStream contents! !

!DebuggerMethodMapForClosureCompiledMethods methodsFor: 'accessing' stamp: 'eem 6/2/2008 18:32'!
timestamp
	^timestamp! !

!DebuggerMethodMapForClosureCompiledMethods methodsFor: 'source mapping' stamp: 'jmv 7/3/2015 19:09'!
abstractSourceMap
	"Answer with a Dictionary of abstractPC <Integer> to sourceRange <Interval>."
	| theMethodToScan rawSourceRanges concreteSourceRanges abstractPC scanner client |
	abstractSourceRanges ifNotNil: [
		^abstractSourceRanges].
	"If the methodNode hasn't had a method generated it doesn't have pcs set in its
	 nodes so we must generate a new method and might as well use it for scanning."
	[
		methodNode rawSourceRangesAndMethodDo: [ :ranges :method |
			rawSourceRanges := ranges.
		 	theMethodToScan := method ]]
		on: UndeclaredVariableWarning
		do: [ :ex | ex resume ].
	concreteSourceRanges := Dictionary new.
	rawSourceRanges keysAndValuesDo: [ :node :range |
		node pc ~= 0 ifTrue: [
			concreteSourceRanges at: node pc put: range ]].
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
	^abstractSourceRanges! !

!DebuggerMethodMapForClosureCompiledMethods methodsFor: 'source mapping' stamp: 'jmv 10/2/2011 21:51'!
rangeForPC: contextsConcretePC contextIsActiveContext: contextIsActiveContext
	"Answer the indices in the source code for the supplied pc.
	 If the context is the actve context (is at the hot end of the stack)
	 then its pc is the current pc.  But if the context isn't, because it is
	 suspended sending a message, then its current pc is the previous pc."

	| pc end |
	pc _ self method abstractPCForConcretePC:
		(contextIsActiveContext
			ifTrue: [ contextsConcretePC ]
			ifFalse: [
				(self method pcPreviousTo: contextsConcretePC) ifNil: [ contextsConcretePC ]]).
	(self abstractSourceMap includesKey: pc) ifTrue: [
		^self abstractSourceMap at: pc].
	sortedSourceMap ifNil: [
		sortedSourceMap _ self abstractSourceMap.
		sortedSourceMap _ (sortedSourceMap keys
			collect: [ :key | key -> (sortedSourceMap at: key)])
				asSortedCollection ].
	sortedSourceMap
		findBinaryIndex: [ :assoc | pc - assoc key ]
		do: [ :i | ^(sortedSourceMap at: i) value ]
		ifNone: [ :lower :upper |
			lower = 0 ifTrue: [ ^1 to: 0 ].
			upper <= sortedSourceMap size ifTrue: [
				^(sortedSourceMap at: upper) value ].    "No match, but  a nearby element."
			end _ sortedSourceMap detectMax: [ :each |
				each value last ].
			end _ end value last.
			^end + 1 to: end ].

	"| method source scanner map |
	 method := DebuggerMethodMapForClosureCompiledMethods compiledMethodAt: #rangeForPC:contextIsActiveContext:.
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
			scanner interpretNextInstructionFor: InstructionClient new]]"! !

!DebuggerMethodMapForClosureCompiledMethods methodsFor: 'source mapping' stamp: 'jmv 7/31/2016 15:44:00'!
sourceText
	^methodNode sourceText! !

!DebuggerMethodMapForClosureCompiledMethods methodsFor: 'initialization' stamp: 'eem 6/5/2008 09:21'!
forMethod: aMethod "<CompiledMethod>" methodNode: theMethodNode "<MethodNode>"
	methodReference := WeakArray with: aMethod.
	methodNode := theMethodNode.
	self markRecentlyUsed! !


!DebuggerMethodMapForClosureCompiledMethods class methodsFor: 'instance creation' stamp: 'jmv 7/3/2015 19:08'!
forMethod: aMethod "<CompiledMethod>"
	"Answer a DebuggerMethodMap suitable for debugging activations of aMethod.
	 Answer an existing instance from the cache if it exists, cacheing a new one if required."
	
	| n |
	^self protected: [ 
		MapCache
			at: aMethod
			ifAbsent: [
				[ n _ aMethod methodNode ] 
					on: UndeclaredVariableWarning
					do: [ :ex | ex resume ].
				self
						cacheDebugMap:
							(self
								forMethod: aMethod
								methodNode: n)
						forMethod: aMethod] ]! !

!DebuggerMethodMapForClosureCompiledMethods class methodsFor: 'instance creation' stamp: 'jmv 5/11/2016 09:27'!
forMethod: aMethod "<CompiledMethod>" methodNode: methodNode "<MethodNode>"
	"Uncached instance creation method for private use or for tests.
	 Please consider using forMethod: instead."
	^DebuggerMethodMapForClosureCompiledMethods new
		forMethod: aMethod
		methodNode: methodNode! !

!DebuggerMethodMapForClosureCompiledMethods class methodsFor: 'class initialization' stamp: 'eem 6/5/2008 09:14'!
initialize
	"DebuggerMethodMapForClosureCompiledMethods initialize"

	self voidMapCache! !

!DebuggerMethodMapForClosureCompiledMethods class methodsFor: 'class initialization' stamp: 'ul 11/15/2010 08:38'!
voidMapCache

	self protected: [ 
		MapCache := WeakIdentityKeyDictionary new.
		MapCacheEntries := 16 ]! !

!DebuggerMethodMapForClosureCompiledMethods class methodsFor: 'debugger support' stamp: 'ul 11/15/2010 08:44'!
cacheDebugMap: aDebuggerMethodMap forMethod: aCompiledMethod
	
	^self protected: [ 
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
		MapCache
			at: aCompiledMethod
			put: aDebuggerMethodMap ]! !

!DebuggerMethodMapForClosureCompiledMethods class methodsFor: 'synchronization' stamp: 'ul 11/15/2010 08:38'!
protected: aBlock

	^(AccessLock ifNil: [ AccessLock := Mutex new ]) critical: aBlock! !


!CompiledMethod methodsFor: 'debugger support' stamp: 'jmv 9/20/2016 19:17:17'!
debuggerMap
	^DebuggerMethodMapForClosureCompiledMethods forMethod: self! !

!methodRemoval: DebuggerMethodMapForClosureCompiledMethods #ensureExtentsMapsInitialized!
DebuggerMethodMapForClosureCompiledMethods removeSelector: #ensureExtentsMapsInitialized!
!classDefinition: #DebuggerMethodMapForClosureCompiledMethods category: #'Tools-Debugger'!
Object subclass: #DebuggerMethodMapForClosureCompiledMethods
	instanceVariableNames: 'timestamp methodReference methodNode abstractSourceRanges sortedSourceMap blockExtentsToTempRefs startpcsToTempRefs'
	classVariableNames: 'AccessLock MapCache MapCacheEntries'
	poolDictionaries: ''
	category: 'Tools-Debugger'!
DebuggerMethodMapForClosureCompiledMethods initialize!
!classRemoval: #DebuggerMethodMap!
Smalltalk removeClassNamed: #DebuggerMethodMap!
