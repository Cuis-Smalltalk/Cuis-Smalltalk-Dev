'From Cuis 4.2 of 25 July 2013 [latest update: #2401] on 3 July 2015 at 8:48:51.835596 pm'!

!Debugger methodsFor: 'shout styling' stamp: 'jmv 7/3/2015 20:48'!
hasBindingOf: aString
	^self selectedContext tempNames includes: aString! !


!Debugger methodsFor: 'shout styling' stamp: 'jmv 7/3/2015 20:47'!
shoutAboutToStyle: aSHTextStyler
	"This is a notification that aSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in aSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	
	self isModeStyleable ifFalse: [^false].
	aSHTextStyler 
		classOrMetaClass: self selectedClassOrMetaClass;
		disableFormatAndConvert;
		workspace: self.
	^true! !


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

