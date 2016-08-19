'From Cuis 4.2 of 25 July 2013 [latest update: #2902] on 18 August 2016 at 4:39:39 pm'!

!InstructionStream class methodsFor: 'cached class state' stamp: 'jmv 8/18/2016 16:38:17'!
specialConstants
	SpecialConstants ifNil: [ self initSpecialConstants ].
	^SpecialConstants! !


!InstructionStream methodsFor: 'decoding - private - v3 plus closures' stamp: 'jmv 8/18/2016 16:38:49'!
interpretNextV3ClosuresInstructionFor: client 
	"Send to the argument, client, a message that specifies the type of the 
	 next instruction."

	| byte type offset method |
	method := self method.  
	byte := method at: pc.
	type := byte // 16.  
	offset := byte \\ 16.  
	pc := pc+1.
	"We do an inline binary search on each of the possible 16 values of type."
	type < 8 ifTrue:
		[type < 4 ifTrue:
			[type < 2 ifTrue:
				[type = 0 ifTrue:
					[^ client pushReceiverVariable: offset].
				^ client pushTemporaryVariable: offset].				"type = 1"
			type = 2 ifTrue: 
				[^ client pushConstant: (method literalAt: offset + 1)].
			^ client pushConstant: (method literalAt: offset + 17)].		"type = 3"
		type < 6 ifTrue:
			[type = 4 ifTrue:
				[^ client pushLiteralVariable: (method literalAt: offset + 1)].
			^ client pushLiteralVariable: (method literalAt: offset + 17)]."type = 5"
		type = 6 ifTrue:
			[offset < 8 ifTrue:
				[^ client popIntoReceiverVariable: offset].
			^ client popIntoTemporaryVariable: offset - 8].
		"type = 7"
		offset = 0 ifTrue: [^ client pushReceiver].
		offset < 8 ifTrue: [^ client pushConstant: (self class specialConstants at: offset)].
		offset = 8 ifTrue: [^ client methodReturnReceiver].
		offset < 12 ifTrue: [^ client methodReturnConstant: (self class specialConstants at: offset - 8)].
		offset = 12 ifTrue: [^ client methodReturnTop].
		offset = 13 ifTrue: [^ client blockReturnTop].
		^ self unusedBytecode: client at: pc - 1]. "offset = 14 & offset = 15, 126 & 127"
	type < 12 ifTrue:
		[type < 10 ifTrue:
			[type = 8 ifTrue:
				[^ self
					interpretV3ClosuresExtension: offset
					in: method
					for: client].
			"type = 9 (short jumps)"
			offset < 8 ifTrue: [^ client jump: offset + 1].
			^ client jump: offset - 8 + 1 if: false].
		type = 10 ifTrue: "(long jumps)"
			[byte := method at: pc.
			pc := pc + 1.
			offset < 8 ifTrue: [^ client jump: offset - 4 * 256 + byte].
			^ client jump: (offset bitAnd: 3) * 256 + byte if: offset < 12].
		"type = 11; arithmetic special selector sends"
		^ client
			send: (Smalltalk specialSelectorAt: offset + 1)
			super: false
			numArgs: (Smalltalk specialNargsAt: offset + 1)].
		type = 12 ifTrue: "non-arithmetic special selector sends"
			[^ client
				send: (Smalltalk specialSelectorAt: offset + 17)
				super: false
				numArgs: (Smalltalk specialNargsAt: offset + 17)].
	"type = 13, 14 or 15"
	^ client
		send: (method literalAt: offset + 1)
		super: false
		numArgs: type - 13 "0, 1 & 2"! !

!InstructionStream methodsFor: 'decoding - private - v3 plus closures' stamp: 'jmv 8/18/2016 16:38:57'!
interpretNextV3InstructionFor: client 
	"Send to the argument, client, a message that specifies the type of the 
	 next instruction."

	| byte type offset method |
	method := self method.  
	byte := method at: pc.
	type := byte // 16.  
	offset := byte \\ 16.  
	pc := pc+1.
	"We do an inline binary search on each of the possible 16 values of type."
	type < 8 ifTrue:
		[type < 4 ifTrue:
			[type < 2 ifTrue:
				[type = 0 ifTrue:
					[^ client pushReceiverVariable: offset].
				^ client pushTemporaryVariable: offset].				"type = 1"
			type = 2 ifTrue: 
				[^ client pushConstant: (method literalAt: offset + 1)].
			^ client pushConstant: (method literalAt: offset + 17)].		"type = 3"
		type < 6 ifTrue:
			[type = 4 ifTrue:
				[^ client pushLiteralVariable: (method literalAt: offset + 1)].
			^ client pushLiteralVariable: (method literalAt: offset + 17)]."type = 5"
		type = 6 ifTrue:
			[offset < 8 ifTrue:
				[^ client popIntoReceiverVariable: offset].
			^ client popIntoTemporaryVariable: offset - 8].
		"type = 7"
		offset = 0 ifTrue: [^ client pushReceiver].
		offset < 8 ifTrue: [^ client pushConstant: (self class specialConstants at: offset)].
		offset = 8 ifTrue: [^ client methodReturnReceiver].
		offset < 12 ifTrue: [^ client methodReturnConstant: (self class specialConstants at: offset - 8)].
		offset = 12 ifTrue: [^ client methodReturnTop].
		offset = 13 ifTrue: [^ client blockReturnTop].
		^ self unusedBytecode: client at: pc - 1]. "offset = 14 & offset = 15, 126 & 127"
	type < 12 ifTrue:
		[type < 10 ifTrue:
			[type = 8 ifTrue:
				[^ self
					interpretV3Extension: offset
					in: method
					for: client].
			"type = 9 (short jumps)"
			offset < 8 ifTrue: [^ client jump: offset + 1].
			^ client jump: offset - 8 + 1 if: false].
		type = 10 ifTrue: "(long jumps)"
			[byte := method at: pc.
			pc := pc + 1.
			offset < 8 ifTrue: [^ client jump: offset - 4 * 256 + byte].
			^ client jump: (offset bitAnd: 3) * 256 + byte if: offset < 12].
		"type = 11; arithmetic special selector sends"
		^ client
			send: (Smalltalk specialSelectorAt: offset + 1)
			super: false
			numArgs: (Smalltalk specialNargsAt: offset + 1)].
		type = 12 ifTrue: "non-arithmetic special selector sends"
			[^ client
				send: (Smalltalk specialSelectorAt: offset + 17)
				super: false
				numArgs: (Smalltalk specialNargsAt: offset + 17)].
	"type = 13, 14 or 15"
	^ client
		send: (method literalAt: offset + 1)
		super: false
		numArgs: type - 13 "0, 1 & 2"! !

