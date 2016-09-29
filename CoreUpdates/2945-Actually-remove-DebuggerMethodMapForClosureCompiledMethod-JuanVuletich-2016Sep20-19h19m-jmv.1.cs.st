'From Cuis 4.2 of 25 July 2013 [latest update: #2943] on 20 September 2016 at 7:25:33 pm'!
!classDefinition: #DebuggerMethodMap category: #'Tools-Debugger'!
Object subclass: #DebuggerMethodMap
	instanceVariableNames: 'timestamp methodReference methodNode abstractSourceRanges sortedSourceMap blockExtentsToTempRefs startpcsToTempRefs'
	classVariableNames: 'AccessLock MapCache MapCacheEntries'
	poolDictionaries: ''
	category: 'Tools-Debugger'!

!DebuggerMethodMap commentStamp: 'jmv 9/20/2016 19:12:17' prior: 0!
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


!DebuggerMethodMap methodsFor: 'accessing' stamp: 'jmv 4/17/2013 11:56'!
markRecentlyUsed
	timestamp := Time localSecondClock! !

!DebuggerMethodMap methodsFor: 'accessing' stamp: 'eem 6/5/2008 09:21'!
method
	^methodReference at: 1! !

!DebuggerMethodMap methodsFor: 'accessing' stamp: 'eem 7/29/2008 19:28'!
namedTempAt: index in: aContext
	"Answer the value of the temp at index in aContext where index is relative
	 to the array of temp names answered by tempNamesForContext:"
	^self
		privateTempAt: index
		in: aContext
		startpcsToBlockExtents: aContext method startpcsToBlockExtents! !

!DebuggerMethodMap methodsFor: 'accessing' stamp: 'eem 7/29/2008 19:33'!
namedTempAt: index put: aValue in: aContext
	"Assign the value of the temp at index in aContext where index is relative
	 to the array of temp names answered by tempNamesForContext:.
	 If the value is a copied value we also need to set it along the lexical chain."
	^self
		privateTempAt: index
		in: aContext
		put: aValue
		startpcsToBlockExtents: aContext method startpcsToBlockExtents! !

!DebuggerMethodMap methodsFor: 'accessing' stamp: 'jmv 5/31/2013 18:30'!
tempNamesForContext: aContext
	"Answer an Array of all the temp names in scope in aContext starting with
	 the home's first local (the first argument or first temporary if no arguments)."
	^((self
		privateTempRefsForContext: aContext
		startpcsToBlockExtents: aContext method startpcsToBlockExtents) collect: [ :pair |
			pair first]),
		(aContext method xtraBindings collect: [ :assoc | assoc key ])! !

!DebuggerMethodMap methodsFor: 'accessing' stamp: 'jmv 4/2/2016 23:05'!
tempsAndValuesForContext: aContext
	"Return a string of the temporary variabls and their current values"
	| aStream |
	aStream := WriteStream on: (String new: 100).
	(self tempNamesForContext: aContext) withIndexDo: [ :title :index |
		aStream nextPutAll: title; nextPut: $:; space; tab.
		aContext print: (self namedTempAt: index in: aContext) on: aStream.
		aStream newLine].
	^aStream contents! !

!DebuggerMethodMap methodsFor: 'accessing' stamp: 'eem 6/2/2008 18:32'!
timestamp
	^timestamp! !

!DebuggerMethodMap methodsFor: 'private' stamp: 'eem 10/7/2011 10:19'!
privateDereference: tempReference in: aContext
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
						indirectionVector at: tempReference second]]]! !

!DebuggerMethodMap methodsFor: 'private' stamp: 'jmv 3/13/2012 23:13'!
privateDereference: tempReference in: aContext put: aValue
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
								put: aValue]]]! !

!DebuggerMethodMap methodsFor: 'private' stamp: 'eem 7/29/2008 20:03'!
privateTempAt: index in: aContext put: aValue startpcsToBlockExtents: theContextsStartpcsToBlockExtents
	| nameRefPair |
	nameRefPair := (self privateTempRefsForContext: aContext
						 startpcsToBlockExtents: theContextsStartpcsToBlockExtents)
						at: index
						ifAbsent: [aContext errorSubscriptBounds: index].
	^self privateDereference: nameRefPair last in: aContext put: aValue! !

!DebuggerMethodMap methodsFor: 'private' stamp: 'jmv 5/31/2013 18:29'!
privateTempAt: index in: aContext startpcsToBlockExtents: theContextsStartpcsToBlockExtents
	| nameRefPair namesAndRefs |
	namesAndRefs _ self
		privateTempRefsForContext: aContext
		startpcsToBlockExtents: theContextsStartpcsToBlockExtents.
	^index <= namesAndRefs size
		ifTrue: [
			nameRefPair _ namesAndRefs
				at: index
				ifAbsent: [ aContext errorSubscriptBounds: index ].
			self privateDereference: nameRefPair last in: aContext ]
		ifFalse: [
			(aContext method xtraBindings at: index - namesAndRefs size) value ]! !

!DebuggerMethodMap methodsFor: 'private' stamp: 'jmv 7/31/2016 15:44:35'!
privateTempRefsForContext: aContext startpcsToBlockExtents: theContextsStartpcsToBlockExtents
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
					 (localRefs reject: [:localPair| outerTemps anySatisfy: [:outerPair| localPair first = outerPair first]])]]! !

!DebuggerMethodMap methodsFor: 'source mapping' stamp: 'jmv 7/3/2015 19:09'!
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

!DebuggerMethodMap methodsFor: 'source mapping' stamp: 'jmv 10/2/2011 21:51'!
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
			scanner interpretNextInstructionFor: InstructionClient new]]"! !

!DebuggerMethodMap methodsFor: 'source mapping' stamp: 'jmv 7/31/2016 15:44:00'!
sourceText
	^methodNode sourceText! !

!DebuggerMethodMap methodsFor: 'initialization' stamp: 'eem 6/5/2008 09:21'!
forMethod: aMethod "<CompiledMethod>" methodNode: theMethodNode "<MethodNode>"
	methodReference := WeakArray with: aMethod.
	methodNode := theMethodNode.
	self markRecentlyUsed! !


!DebuggerMethodMap class methodsFor: 'instance creation' stamp: 'jmv 7/3/2015 19:08'!
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

!DebuggerMethodMap class methodsFor: 'instance creation' stamp: 'jmv 5/11/2016 09:27'!
forMethod: aMethod "<CompiledMethod>" methodNode: methodNode "<MethodNode>"
	"Uncached instance creation method for private use or for tests.
	 Please consider using forMethod: instead."
	^DebuggerMethodMap new
		forMethod: aMethod
		methodNode: methodNode! !

!DebuggerMethodMap class methodsFor: 'class initialization' stamp: 'eem 6/5/2008 09:14'!
initialize
	"DebuggerMethodMap initialize"

	self voidMapCache! !

!DebuggerMethodMap class methodsFor: 'class initialization' stamp: 'ul 11/15/2010 08:38'!
voidMapCache

	self protected: [ 
		MapCache := WeakIdentityKeyDictionary new.
		MapCacheEntries := 16 ]! !

!DebuggerMethodMap class methodsFor: 'debugger support' stamp: 'ul 11/15/2010 08:44'!
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

!DebuggerMethodMap class methodsFor: 'synchronization' stamp: 'ul 11/15/2010 08:38'!
protected: aBlock

	^(AccessLock ifNil: [ AccessLock := Mutex new ]) critical: aBlock! !


!CompiledMethod methodsFor: 'debugger support' stamp: 'jmv 9/20/2016 19:25:07'!
debuggerMap
	^DebuggerMethodMap forMethod: self! !

DebuggerMethodMap initialize!
!classRemoval: #DebuggerMethodMapForClosureCompiledMethods!
Smalltalk removeClassNamed: #DebuggerMethodMapForClosureCompiledMethods!
