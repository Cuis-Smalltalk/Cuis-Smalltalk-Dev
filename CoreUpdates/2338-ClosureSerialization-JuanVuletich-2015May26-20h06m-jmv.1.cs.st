'From Cuis 4.2 of 25 July 2013 [latest update: #2337] on 26 May 2015 at 8:07:11.033475 pm'!

!ProcessBrowser methodsFor: 'process list' stamp: 'jmv 5/26/2015 17:15'!
findContext: aString
	| initialProcess initialContext found |
	searchString _ aString.
	searchString isEmpty
		ifTrue: [^ false].
	initialProcess _ selectedProcess.
	initialContext _ selectedContext.
	self processListIndex: 1.
	self stackListIndex: 1.
	found _ self nextContext.
	found ifFalse: [
		self selectedProcess: initialProcess.
		self context: initialContext].
	^ found! !


!BlockClosure methodsFor: 'objects from disk' stamp: 'jmv 5/26/2015 16:53'!
objectForDataStream: refStrm
	"I am about to be written on an object file.  Write a textual reference instead."
	self isTrivialClosure ifFalse: [
		self error: 'Can only serialize trivial (context free) closures.' ].
	^ DiskProxy
		global: #Compiler
		selector: #evaluate:
		args: (Array with: self decompile decompileString)! !


!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 5/26/2015 17:15'!
sqrt
	| d n |
	n _ numerator sqrt.
	d _ denominator sqrt.
	"The #sqrt method in integer will only answer a Float if there's no exact square root.
	So, we need a float anyway."
	(n isInfinite or: [ d isInfinite ]) ifTrue: [
		^self asFloat sqrt ].
	^n / d! !

