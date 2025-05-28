'From Cuis7.3 [latest update: #7253] on 28 May 2025 at 4:47:57 pm'!

!Mutex methodsFor: 'mutual exclusion' stamp: 'jmv 5/28/2025 16:33:28'!
critical: aBlock ifWouldBlock: unprotectedBlock

	self wouldBlock ifTrue: [ ^unprotectedBlock value ].
	^self critical: aBlock! !

!Mutex methodsFor: 'testing' stamp: 'jmv 5/28/2025 16:27:21'!
wouldBlock
	
	owner isNil ifTrue: [ ^false ].
	owner == Processor activeProcess ifTrue: [ ^false ].
	^true! !


!DebuggerMethodMap class methodsFor: 'instance creation' stamp: 'jmv 5/28/2025 16:18:09'!
uncachedForMethod: aMethod "<CompiledMethod>"
	"Answer a DebuggerMethodMap suitable for debugging activations of aMethod."
	
	| methodNode |
	[ methodNode := aMethod methodNode ]
		on: UndeclaredVariableWarning
		do: [ :ex | ex resume ].
	^self forMethod: aMethod methodNode: methodNode.! !

!DebuggerMethodMap class methodsFor: 'synchronization' stamp: 'jmv 5/28/2025 16:33:53'!
protected: aBlock ifWouldBlock: unprotectedBlock
	"Try not to block.
	If our protected state is busy with someone else"

	AccessLock ifNil: [ AccessLock := Mutex new ].
	^AccessLock critical: aBlock ifWouldBlock: unprotectedBlock! !


!DebuggerMethodMap class methodsFor: 'instance creation' stamp: 'jmv 5/28/2025 16:30:12'!
forMethod: aMethod "<CompiledMethod>"
	"Answer a DebuggerMethodMap suitable for debugging activations of aMethod.
	 Answer an existing instance from the cache if it exists, cacheing a new one if required.
	Try not to block. If would block, just avoid caching: Some performance loss is better than possible system freeze."
	
	^self
		protected: [ 
			MapCache at: aMethod ifAbsent: [
				self 
					cacheDebugMap: (self uncachedForMethod: aMethod)
					forMethod: aMethod ]]
		ifWouldBlock: [ self uncachedForMethod: aMethod ]! !

!DebuggerMethodMap class methodsFor: 'debugger support' stamp: 'jmv 5/28/2025 16:15:41'!
cacheDebugMap: aDebuggerMethodMap forMethod: aCompiledMethod
	
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
		put: aDebuggerMethodMap! !

!DebuggerMethodMap class methodsFor: 'synchronization' stamp: 'jmv 5/28/2025 16:23:15'!
protected: aBlock

	AccessLock ifNil: [ AccessLock := Mutex new ].
	^AccessLock critical: aBlock! !

