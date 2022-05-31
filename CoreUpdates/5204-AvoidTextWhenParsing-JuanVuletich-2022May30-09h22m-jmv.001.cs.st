'From Cuis 6.0 [latest update: #5202] on 30 May 2022 at 9:24:28 am'!

!Inspector methodsFor: 'selecting' stamp: 'jmv 5/30/2022 09:19:36'!
accept: aString

	| result |
	result _ self doItReceiver class compilerClass new
				evaluate: aString asPlainString readStream
				in: self doItContext
				to: self doItReceiver
				notifying: nil	"fix this"
				ifFail:  [^ false].
	acceptedContentsCache _ result printString.
	self replaceSelectionValue: result.	"may put contents back"
	self acceptedContentsChanged.
	^ true! !


!Scanner methodsFor: 'public access' stamp: 'jmv 5/30/2022 08:55:18'!
scanFieldNames: stringOrArray
	"Answer an Array of Strings that are the identifiers in the input string, 
	stringOrArray. If passed an Array, just answer with that Array, i.e., 
	assume it has already been scanned."

	| strm |
	(stringOrArray isMemberOf: Array)
		ifTrue: [^stringOrArray].
	self scan: stringOrArray asString asPlainString readStream.
	strm := (Array new: 10) writeStream.
	[tokenType == #doIt]
		whileFalse: [
			tokenType == #word ifTrue: [strm nextPut: token].
			self scanToken ].
	^strm contents

	"Scanner new scanFieldNames: 'abc  def ghi' ('abc' 'def' 'ghi' )"! !


!Parser methodsFor: 'private' stamp: 'jmv 5/30/2022 08:50:24'!
privateReadSelectorFrom: aMethodSource
	"Answer the message selector for the argument, aMethodSource, which should 
	 parse successfully up to the temporary declaration or the end of the 
	 method header."
	"Note: only intended to read the selector. Parser instance might be inconsistent afterwards.
	Optimized these kind of methods (compare the following):
	[ 100000 timesRepeat: [Parser new parseSelector: 'a ^#[123 123 123 123 123 123 123 123 987 987 987 987 987 987 987 987]'] ] timeToRun 4824
	[ 100000 timesRepeat: [Parser new privateReadSelectorFrom: 'a ^#[123 123 123 123 123 123 123 123 987 987 987 987 987 987 987 987]'] ] timeToRun  342
	"

	| result |
	self initScannerForTokenization.
	self
		init: aMethodSource asString asPlainString readStream
		notifying: nil
		failBlock: [ ^nil ].
	encoder _ self.
	result _ self privateReadSelector.
	encoder _ failBlock _ nil.  "break cycles"
	^result! !

!Parser methodsFor: 'public access' stamp: 'jmv 5/30/2022 08:59:04'!
parse: sourceStream class: class category: aCategory noPattern: noPattern doIt: doIt context: aContext notifying: aRequestor ifFail: aBlock

	"Answer a MethodNode for the argument, sourceStream, that is the root of
	 a parse tree. Parsing is done with respect to the argument, class, to find
	 instance, class, and pool variables; and with respect to the argument,
	 ctxt, to find temporary variables. Errors in parsing are reported to the
	 argument, req, if not nil; otherwise aBlock is evaluated. The argument
	 noPattern is a Boolean that is true if the the sourceStream does not
	 contain a method header (i.e., for DoIts)."
	
	| methNode repeatNeeded myStream sourceCode startPosition |
	
	category _ aCategory.
	myStream _ sourceStream.
	[
		repeatNeeded _ false.
		startPosition _ myStream position.
		sourceCode _ myStream upToEnd.
		myStream position: startPosition.
		self encoder init: class context: aContext notifying: self.
		"Protect against possible parsing failure"
		doIt ifTrue: [
			(sourceCode beginsWith: Scanner doItInSelector)
				ifTrue: [encoder selector: Scanner doItInSelector]
				ifFalse: [
					(sourceCode beginsWith: Scanner doItSelector)
						ifTrue: [encoder selector: Scanner doItSelector]]].
		self init: myStream notifying: aRequestor failBlock: [^ aBlock value ].
		doitFlag _ noPattern.
		failBlock _ aBlock.
		[ methNode _ self method: noPattern doIt: doIt context: aContext ]
			on: ReparseAfterSourceEditing
			do: [ :ex |
				repeatNeeded _ true.
				myStream _ requestor text string asPlainString readStream ].
		repeatNeeded
	] whileTrue: [ encoder _ self encoder class new ].

	methNode sourceText: sourceCode.
	"See #xBacktick"
	sentInLiterals do: [ :sym | encoder noteOptimizedSelector: sym ].

	^ methNode! !

!Parser methodsFor: 'public access' stamp: 'jmv 5/30/2022 09:17:34'!
parse: sourceStreamOrString class: behavior noPattern: aBoolean
	| stream |
	stream _ (sourceStreamOrString is: #Stream)
		ifTrue: [sourceStreamOrString]
		ifFalse: [sourceStreamOrString asPlainString readStream].
	^ self
		parse: stream
		class: behavior
		noPattern: aBoolean
		context: nil
		notifying: nil
		ifFail: [^nil]! !


!MethodNode methodsFor: 'source mapping' stamp: 'jmv 5/30/2022 09:04:09'!
rawSourceRangesAndMethodDo: aBinaryBlock
	"Evaluate aBinaryBlock with the rawSourceRanges and method generated from the receiver."

	| methNode method |
	methNode := encoder classEncoding parserClass new
					encoderClass: encoder class;
					parse: (sourceText "If no source, use decompile string as source to map from"
							ifNil: [self decompileString]
							ifNotNil: [sourceText asPlainString])
					class: self methodClass.
	method := methNode generate.  "set bytecodes to map to"
	^aBinaryBlock
		value: methNode encoder rawSourceRanges
		value: method! !

!MethodNode methodsFor: 'debugger support' stamp: 'jmv 5/30/2022 09:04:20'!
blockExtentsToTempsMap
	"Answer a Dictionary of blockExtent to temp locations for the current method.
	 This is used by the debugger to locate temp vars in contexts.  A temp map
	 entry is a pair of the temp's name and its index, where an index is either an
	 integer for a normal temp or a pair of the index of the indirect temp vector
	 containing  the temp and the index of the temp in its indirect temp vector."

	^encoder blockExtentsToTempsMap ifNil:
		[| methNode |
		methNode := encoder classEncoding parserClass new
						encoderClass: encoder class;
						parse: (sourceText
							ifNil: [self decompileString]
							ifNotNil: [sourceText asPlainString])
						class: self methodClass.
		"As a side effect generate: creates data needed for the map."
		methNode generate.
		methNode encoder blockExtentsToTempsMap]! !

