'From Cuis 6.0 [latest update: #5032] on 4 January 2022 at 5:06:18 pm'!

!DebuggerMethodMap methodsFor: 'private' stamp: 'HAW 1/4/2022 17:04:37'!
privateTempAt: index in: aContext startpcsToBlockExtents: theContextsStartpcsToBlockExtents ifXtraBindings: xtraBindingsBlock ifNot: notXtraBindingBlock

	| nameRefPair namesAndRefs |
	
	namesAndRefs _ self 
		privateTempRefsForContext: aContext
		startpcsToBlockExtents: theContextsStartpcsToBlockExtents.
		
	^index <= namesAndRefs size
		ifTrue: [
			nameRefPair _ namesAndRefs
				at: index
				ifAbsent: [ aContext errorSubscriptBounds: index ].
			notXtraBindingBlock value: nameRefPair last ]
		ifFalse: [ xtraBindingsBlock value: (aContext method xtraBindings at: index - namesAndRefs size) ]! !


!DebuggerMethodMap methodsFor: 'private' stamp: 'HAW 1/4/2022 17:04:45'!
privateTempAt: index in: aContext put: aValue startpcsToBlockExtents: theContextsStartpcsToBlockExtents
	
	^self 
		privateTempAt: index 
		in: aContext 
		startpcsToBlockExtents: theContextsStartpcsToBlockExtents 
		ifXtraBindings: [ :xtraBinding | xtraBinding value: aValue ]
		ifNot: [ :ref | self privateDereference: ref in: aContext put: aValue ]
		! !

!DebuggerMethodMap methodsFor: 'private' stamp: 'HAW 1/4/2022 17:05:12'!
privateTempAt: index in: aContext startpcsToBlockExtents: theContextsStartpcsToBlockExtents

	^self 
		privateTempAt: index 
		in: aContext 
		startpcsToBlockExtents: theContextsStartpcsToBlockExtents 
		ifXtraBindings: [ :xtraBinding | xtraBinding value ] 
		ifNot: [ :ref | self privateDereference: ref in: aContext ]! !

