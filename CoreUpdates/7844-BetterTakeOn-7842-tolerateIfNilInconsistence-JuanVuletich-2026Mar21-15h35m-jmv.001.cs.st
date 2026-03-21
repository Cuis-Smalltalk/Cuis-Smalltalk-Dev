'From Cuis7.7 [latest update: #7843] on 21 March 2026 at 4:37:49 pm'!

!DebuggerMethodMap methodsFor: 'private' stamp: 'jmv 21/Mar/2026 16:37:35'!
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
			(blockExtentsToTempRefs includesKey: (theContextsStartpcsToBlockExtents at: aContext startpc))
				ifTrue: [
					 localRefs := blockExtentsToTempRefs at: (theContextsStartpcsToBlockExtents at: aContext startpc)]
				ifFalse: [
					"This could happen, for instance, if preference #avoidIfNilOptimization has changed, but the method has not been recompiled.
					Go ahead without names. At lease values will make sense."
					localRefs := (1 to: aContext size) collect: [ :i | { 'canNotRetrieveTempNames', i printString. aContext size - i + 1 } ].
					 ].
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

