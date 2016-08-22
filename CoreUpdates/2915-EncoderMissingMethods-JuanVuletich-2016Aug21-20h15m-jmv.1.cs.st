'From Cuis 4.2 of 25 July 2013 [latest update: #2914] on 21 August 2016 at 8:29:50 pm'!

!EncoderForV3PlusClosures commentStamp: 'eem 2/29/2016 00:07' prior: 0!
An encoder for the V3 bytecode set augmented with the following bytecodes that are part of the full closure implementation.
	138   10001010 jkkkkkkk		Push (Array new: kkkkkkk) (j = 0)
								or	Pop kkkkkkk elements into: (Array new: kkkkkkk) (j = 1)
	139   10001011 kkkkkkkk jjjjjjjj 	Invoke primitive number jjjjjjjjkkkkkkkk
	140   10001100 kkkkkkkk jjjjjjjj 	Push Temp At kkkkkkkk In Temp Vector At: jjjjjjjj
	141   10001101 kkkkkkkk jjjjjjjj 	Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj
	142   10001110 kkkkkkkk jjjjjjjj 	Pop and Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj
	143   10001111 llllkkkk jjjjjjjj iiiiiiii	Push Closure Num Copied llll Num Args kkkk BlockSize jjjjjjjjiiiiiiii
This is an exact duplicate of EncoderForLongFormV3PlusClosures.
Could be a trait (or in Newspeak, a Mixin).
For now we impose upon you to synchronise any and all changes between these two classes.!


!InstructionStream methodsFor: 'testing' stamp: 'eem 5/12/2016 16:33'!
willReallyStore
	"Answer whether the bytecode at pc is a store or store-pop into an explicit variable.
	 This eliminates stores into indirect temp vectors, which implement mutable closed-over
	 variables in the the closure implementation, and hence stores into temp vectors are not real stores."
	| method |
	method := self method.
	^method encoderClass isNonSyntheticStoreAt: pc in: method for: self! !

!InstructionStream methodsFor: 'testing' stamp: 'eem 9/2/2014 14:11'!
willReturnTopFromMethod
	"Answer whether the next bytecode is a return stack top from method."
	| method |
	method := self method.
	^method encoderClass isReturnTopFromMethodAt: pc in: method! !


!CompiledMethod class methodsFor: 'constants' stamp: 'jmv 8/21/2016 20:18:32'!
maxNumLiterals
	"The current header format and the VM's interpretation of it allows for a maximum of 32767 literals.
	(jmv) Is this also true for Pre-Spur?"
	^32767! !


!Encoder methodsFor: 'accessing' stamp: 'eem 8/25/2015 15:39'!
maxIndexableLiterals
	"Answer the maximum number of literals supported by the receiver's
	 bytecode set. This is a nominal value based on the Blue Book bytecode
	 set; subclasses answer a more accurate value."
	^63! !

!Encoder methodsFor: 'accessing' stamp: 'eem 8/25/2015 15:39'!
maxNumLiterals
	^CompiledMethod maxNumLiterals min: self maxIndexableLiterals! !


!BytecodeEncoder class methodsFor: 'bytecode decoding' stamp: 'eem 5/13/2016 10:03'!
createClosureCode
	"Answer the create closure bytecode, if it exists in the encoder's byetcode set, or nil if not."
	^nil! !

!BytecodeEncoder class methodsFor: 'bytecode decoding' stamp: 'eem 5/13/2016 10:12'!
pcFollowingBlockAt: pc in: method
	"Assuming the pc is that of a block creation bytecode, answer the pc immediately following the block,
	 i.e. the next pc after the block creation."
	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'bytecode decoding' stamp: 'eem 5/13/2016 11:29'!
pcPreviousTo: thePC in: method for: anInstructionStreamOrContext
	"Answer the pc of the bytecode before the bytecode at thePC.
	 Unlike CompiledMethod>>pcPreviousTo:, this version answers nil for
	 the first bytecode of an embedded block, and answers the pc of the
	 block creation bytecode for a bytecode following an embedded block."
	| pc nextPc prevPc byte createClosureCode |
	thePC > method endPC ifTrue:
		[^method endPC].
	pc := method initialPC.
	"We could save time by scanning from the block creation bytecode of an embedded block,
	 using the following, but it saves less time than it loses in additional tests."
	"(anInstructionStreamOrContext isContext
	 and: [anInstructionStreamOrContext isClosureContext
	 and: [(nextPc := anInstructionStreamOrContext startpc) > pc]]) ifTrue:
		[pc := self pcOfBlockCreationBytecodeForBlockStartingAt: nextPc in: method]."
	createClosureCode := self createClosureCode.
	[pc < thePC] whileTrue:
		[byte := method at: (prevPc := pc).
		 [pc := createClosureCode == byte
					ifTrue:
						[nextPc := self pcFollowingBlockAt: pc in: method.
						 nextPc = thePC ifTrue: "first bytecode following block"
							[^prevPc].
						 nextPc > thePC
							ifTrue:
								[pc + (self bytecodeSize: byte) = thePC ifTrue: "first bytecode of block"
									[^nil].
								 pc + (self bytecodeSize: byte)]
						 	ifFalse: [nextPc]]
					ifFalse: [pc + (self bytecodeSize: byte)].
		  self isExtension: byte] whileTrue:
			[byte := method at: pc]].
	^prevPc

"Here's code to measure the effect of short-cutting scanning for blocks by starting at the startpc.
 It measures how much time is used to scan for the pcs from the last block to the end of all mwetods containing blocks.  Uncomment out the short-cut above to compare time with the optimization and time without.  I see approximately 290ms for all such methods with the optimization and 292 ms without, so given that this slows down the substantial majority of methods without blocks, we KISS."
"| candidates |
candidates := Dictionary new.
self systemNavigation allSelect:
	[:m| | ebc |
	(m isQuick or: [(ebc := m embeddedBlockClosures) isEmpty]) ifFalse:
		[candidates at: m put: { ebc last.
								Array streamContents:
									[:s| | is |
									(is:= InstructionStream on: m)
										pc: ebc last startpc;
										scanFor:
											[:b|
											s nextPut: is pc.
											false]] }].
	 false].
(1 to: 10) collect:
	[:ign|
	{ [candidates keysAndValuesDo:
		[:m :tuple|
		[:ebc :pcs| | c |
		c := ebc outerContext.
		pcs do:
			[:pc| m encoderClass pcPreviousTo: pc in: m for: c]] valueWithArguments: tuple]] timeToRun.
	  [candidates keysAndValuesDo:
		[:m :tuple|
		[:ebc :pcs| | c |
		c := ebc outerContext.
		pcs do:
			[:pc| m encoderClass pcPreviousTo: pc in: m for: nil]] valueWithArguments: tuple]] timeToRun. }]"! !

!BytecodeEncoder class methodsFor: 'bytecode decoding' stamp: 'eem 5/13/2016 09:57'!
pushNewArrayCode
	"Answer the pushNewArray bytecode, if it exists in the encoder's byetcode set, or nil if not."
	^nil! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/13/2016 09:24'!
isNonSyntheticStoreAt: pc in: method for: anInstructionStream
	"Answer whether the bytecode at pc is a store or store-pop into an explicit variable.
	 This eliminates stores into indirect temp vectors, which implement mutable closed-over
	 variables in the the closure implementation, and hence stores into temp vectors are not real stores."

	^(self isStoreAt: pc in: method)
	  and: [(self isSyntheticStoreAt: pc in: method for: anInstructionStream) not]! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/13/2016 11:32'!
isSyntheticStoreAt: pc in: method for: anInstructionStream
	"Answer whether the bytecode at pc is a store or store-pop of an indirect temp vector,
	 which implement mutable closed-over variables in the the closure implementation.
	 Stores into temp vectors are not real stores."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 10/10/2014 09:24'!
nonExtensionPcAt: pc in: method
	"Answer the pc of the actual bytecode at pc in method, skipping past any preceeding extensions."
	| thePC bytecode |
	thePC := pc.
	[self isExtension: (bytecode := method at: thePC)] whileTrue:
		[thePC := thePC + (self bytecodeSize: bytecode)].
	^thePC! !


!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 8/25/2015 15:38'!
maxIndexableLiterals
	"This bytecode set can index up to 256 literals."
	^256! !


!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/13/2016 09:26'!
isSyntheticStoreAt: pc in: method for: anInstructionStream
	"Answer whether the bytecode at pc is a store or store-pop of an indirect temp vector,
	 which implement mutable closed-over variables in the the closure implementation.
	 Stores into temp vectors are not real stores."

	^false! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/13/2016 11:43'!
isTempStoreAt: pc in: method
	"Answer whether the bytecode at pc is a store or store-pop into a temporary variable.
	 104-111 	01101iii 	Pop and Store Temporary Location #iii
	 129 		10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk
	 130 		10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"

	| byte |
	byte := method at: pc.
	^byte >= 104
	  and: [byte <= 111
			or: [byte <= 130 and: [byte >= 129 and: [(method at: pc + 1) >> 6 = 1]]]]! !


!EncoderForV3PlusClosures class methodsFor: 'bytecode decoding' stamp: 'eem 5/13/2016 10:23'!
createClosureCode
	"Answer the create closure bytecode, if it exists in the encoder's byetcode set, or nil if not."
	^143! !

!EncoderForV3PlusClosures class methodsFor: 'bytecode decoding' stamp: 'eem 5/13/2016 10:24'!
pcFollowingBlockAt: pc in: method
	"Assuming the pc is that of a block creation bytecode, answer the pc immediately following the block,
	 i.e. the next pc after the block creation."
	self assert: (method at: pc) = self createClosureCode.
	^(method at: pc + 2) * 256 + (method at: pc + 3) + pc + 4! !

!EncoderForV3PlusClosures class methodsFor: 'bytecode decoding' stamp: 'eem 5/13/2016 09:57'!
pushNewArrayCode
	"138   10001010 jkkkkkkk		Push (Array new: kkkkkkk) (j = 0)
								or	Pop kkkkkkk elements into: (Array new: kkkkkkk) (j = 1)"
	^138! !

!EncoderForV3PlusClosures class methodsFor: 'instruction stream support' stamp: 'eem 5/13/2016 11:30'!
isSyntheticStoreAt: pc in: method for: anInstructionStream
	"Answer whether the bytecode at pc is a store or store-pop of an indirect temp vector,
	 which implement mutable closed-over variables in the the closure implementation.
	 Stores into temp vectors are not real stores.  N.B. pcPreviousTo:in:for: is slow, so filter
	 out any preceding bytecodes other than what looks like a pushNewArrayCode.  But the
	 pcPreviousTo:in:for: is still necessary, since the presence of a pcPreviousTo:in:for: in the
	 right place is potentially ambiguous, possibly part of a different bytecode seqence."

	^(self isTempStoreAt: pc in: method)
	  and: [pc - 2 >= method initialPC
	  and: [(method at: pc - 2) = self pushNewArrayCode
	  and: [(method at: pc - 1) <= 127
	  and: [pc - 2 = (self pcPreviousTo: pc in: method for: anInstructionStream)]]]]! !


!LiteralDictionary methodsFor: 'as yet unclassified' stamp: 'jmv 8/21/2016 19:51:18'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or raise an error if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."

	| index start |
	index := start := anObject hash \\ array size + 1.
	[ 
		| element |
		((element := array at: index) == nil or: [
			element key literalEqual: anObject ])
				ifTrue: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
	self error: 'There is no free space in this collection!!'! !


!Encoder methodsFor: 'results' stamp: 'eem 8/25/2015 15:42'!
allLiterals
	addedSelectorAndMethodClassLiterals ifFalse:
		[addedSelectorAndMethodClassLiterals := true.
		"Put the optimized selectors in literals so as to browse senders more easily"
		optimizedSelectors := optimizedSelectors reject: [:e| literalStream originalContents hasLiteral: e].
		optimizedSelectors isEmpty ifFalse: [
			"Use one entry per literal if enough room, else make anArray"
			literalStream position + optimizedSelectors size + 2 >= self maxNumLiterals
				ifTrue: [self litIndex: optimizedSelectors asArray]
				ifFalse: [optimizedSelectors do: [:e | self litIndex: e]]].
		"Add a slot for selector or MethodProperties"
		self litIndex: nil.
		self litIndex: self associationForClass].
	^literalStream contents! !

!Encoder methodsFor: 'temps' stamp: 'eem 7/8/2015 09:09'!
bindTemp: name 
	"Declare a temporary; error not if a field or class variable."
	scopeTable at: name ifPresent:[:node|
		"When non-interactive raise the error only if its a duplicate"
		node isTemp
			ifTrue:[^self notify:'Name already used in this method']
			ifFalse:[self warnAboutShadowed: name]].
	^self reallyBind: name! !

!Encoder methodsFor: 'temps' stamp: 'jmv 8/21/2016 19:45:58'!
bindTemp: name in: methodSelector
	"Declare a temporary; error not if a field or class variable."
	scopeTable at: name ifPresent:[:node|
		"When non-interactive raise the error only if its a duplicate"
		(node isTemp or:[requestor interactive])
			ifTrue:[^self notify:'Name already used in this method']
			ifFalse:[Transcript 
				show: '(', name, ' is shadowed in "' , class printString , '>>' , methodSelector printString , '")']].
	^self reallyBind: name! !

!Encoder methodsFor: 'encoding' stamp: 'jmv 8/21/2016 19:47:57'!
litIndex: literal
	| p |
	p := literalStream position.
	p = self maxNumLiterals ifTrue:
		[self notify: 'More than ', self maxNumLiterals printString, ' literals referenced. 
You must split or otherwise simplify this method.
The ', (self maxNumLiterals + 1) printString, 'th literal is: ', literal printString. ^nil].
	"Would like to show where it is in the source code, 
	 but that info is hard to get."
	literalStream nextPut: literal.
	^ p! !

!Encoder methodsFor: 'encoding' stamp: 'ul 10/24/2010 06:34'!
sharableLitIndex: literal
	"Special access prevents multiple entries for post-allocated super send special selectors"
	1 to: literalStream position do:
		[:index|
		(literal literalEqual: (literalStream originalContents at: index)) ifTrue:
			[^index - 1]].
	^self litIndex: literal! !


!BytecodeEncoder methodsFor: 'temps' stamp: 'eem 7/8/2015 09:08'!
bindTemp: name
	"Declare a temporary; error not if a field or class variable or out-of-scope temp.
	 Read the comment in Encoder>>bindBlockArg:within: and subclass implementations."
	self supportsClosureOpcodes ifFalse:
		[^super bindTemp: name].
	scopeTable at: name ifPresent:
		[:node|
		"When non-interactive raise the error only if it is a duplicate"
		node isTemp
			ifTrue:[node scope >= 0 ifTrue:
						[^self notify: 'Name already used in this method']]
			ifFalse:[self warnAboutShadowed: name]].
	^self reallyBind: name! !


!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 10/10/2014 09:32'!
extensionsFor: pc in: aCompiledMethod into: trinaryBlock
	"If the bytecode at pc is an extension, or if the bytecode at pc is preceeded by extensions,
	 then evaluate aTrinaryBlock with the values of extA and extB and number of extension *bytes*.
	 If the bytecode at pc is neither an extension or extended then evaluate with 0, 0, 0."
 
	| prevPC |
	"If there is what appears to be an extension bytecode before this bytecode
	 then scan for the previous pc to confirm."
	(pc - 2 >= aCompiledMethod initialPC
	 and: [self isExtension: (aCompiledMethod at: pc - 2)]) ifTrue:
		[prevPC := aCompiledMethod pcPreviousTo: pc.
		 (self nonExtensionPcAt: prevPC in: aCompiledMethod) = pc ifTrue:
			[^self extensionsAt: prevPC in: aCompiledMethod into: trinaryBlock]].
	^self extensionsAt: pc in: aCompiledMethod into: trinaryBlock! !


!EncoderForV3 class methodsFor: 'compiled method support' stamp: 'nice 10/31/2015 17:59'!
markerOrNilFor: aMethod
	"If aMethod is a marker method, answer the symbol used to mark it.  Otherwise
	 answer nil.  What is a marker method?  It is method with body like 
		'self subclassResponsibility' or '^ self subclassResponsibility' 
	 used to indicate ('mark') a special property.

	Marker methods compile to two bytecode forms, this:
		self
		send: <literal 1>
		pop
		returnSelf
	or this:
		self
		send: <literal 1>
		returnTop"
	| expectedHeaderPlusLliteralSize e |
	expectedHeaderPlusLliteralSize := Smalltalk wordSize * 4.
	^(((e := aMethod endPC - expectedHeaderPlusLliteralSize) = 3 or: [e = 4]) 
	  and: [aMethod numLiterals = 3
	  and: [(aMethod at:  expectedHeaderPlusLliteralSize + 1) = 16r70	"push self"
	  and: [(aMethod at: expectedHeaderPlusLliteralSize + 2) = 16rD0]]])	"send <literal 1>"
		ifTrue: [aMethod literalAt: 1]! !

!methodRemoval: LiteralDictionary #literalEquality:and:!
LiteralDictionary removeSelector: #literalEquality:and:!
