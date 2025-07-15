'From Cuis7.5 [latest update: #7351] on 14 July 2025 at 4:46:31 pm'!

!BytecodeEncoder class methodsFor: 'compiled method support' stamp: 'jmv 7/14/2025 15:50:34'!
instVarReadScanBlockUsing: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of any inst var."
	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'compiled method support' stamp: 'jmv 7/14/2025 16:13:10'!
instVarWriteScanBlockUsing: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of any inst var."
	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'compiled method support' stamp: 'jmv 7/14/2025 16:33:01'!
pushReceiverScanBlock
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for push receiver bytecode."
	self subclassResponsibility! !


!EncoderForSistaV1 class methodsFor: 'compiled method support' stamp: 'jmv 7/14/2025 16:11:58'!
instVarReadScanBlockUsing: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of any inst var.
	See #genPushInstVar:
	See #genPushInstVarLong:"

	"	0-15	0000 i i i i 				Push Receiver Variable #iiii
	*	224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	*	226		11100010	i i i i i i i i	Push Receiver Variable #iiiiiiii (+ Extend A * 256)"

	^[:b|
		(b < 16)
		or: [b = 226 ]]! !

!EncoderForSistaV1 class methodsFor: 'compiled method support' stamp: 'jmv 7/14/2025 16:27:00'!
instVarWriteScanBlockUsing: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of any inst var.
	See #genStoreInstVar:
	See #genStoreInstVarLong:
	See #genStorePopInstVar:
	See #genStorePopInstVarLong:"

	"	200-207	11001 iii					Pop and Store Receiver Variable #iii
	*	224			11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	*	240			11110000	iiiiiiii		Pop and Store Receiver Variable #iiiiiii (+ Extend A * 256)
	*	243			11110011	iiiiiiii		Store Receiver Variable #iiiiiii (+ Extend A * 256)"
	| extension |
	extension := 0.
	^[:b|
		(b >= 200 and: [b < 208])
		or: [b = 240 or: [b = 243]]]! !

!EncoderForSistaV1 class methodsFor: 'compiled method support' stamp: 'jmv 7/14/2025 16:32:57'!
pushReceiverScanBlock
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for push receiver bytecode."

	^[ :bc | bc = 76]! !


!EncoderForV3PlusClosures class methodsFor: 'compiled method support' stamp: 'jmv 7/14/2025 16:11:13'!
instVarReadScanBlockUsing: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of any inst var.
	See #genPushInstVar:
	See #genPushInstVarLong:"

	^[:b|
		(b < 16)
		or: [(b = 16r80 and: [scanner followingByte <= 16r3F])
		or:  [(b = 16r84 and: [scanner followingByte = 16r40])]]]! !

!EncoderForV3PlusClosures class methodsFor: 'compiled method support' stamp: 'jmv 7/14/2025 16:24:18'!
instVarWriteScanBlockUsing: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of any inst var.
	See #genStoreInstVar:
	See #genStoreInstVarLong:
	See #genStorePopInstVar:
	See #genStorePopInstVarLong:"

	^[:b|
		(b between: 16r60 and: 16r60 + 7)
		or: [(b = 16r81 and: [scanner followingByte <= 16r3F])
		or: [(b = 16r82 and: [scanner followingByte <= 16r3F])
		or: [(b = 16r84 and: [scanner followingByte = 160])
		or: [(b = 16r84 and: [scanner followingByte = 192])]]]]]! !

!EncoderForV3PlusClosures class methodsFor: 'compiled method support' stamp: 'jmv 7/14/2025 16:32:34'!
pushReceiverScanBlock
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for push receiver bytecode."

	^[ :bc | bc = 112]! !


!BlockClosure methodsFor: 'testing' stamp: 'jmv 7/14/2025 16:35:05'!
pushesSelf
	"Answer whether the receiver pushes self to send a message or pass as argument."
	| myMethod scanner end |
	myMethod := self method.
	scanner := InstructionStream new method: myMethod pc: startpc.
	end := self endpc.
	scanner scanFor: [ :byte |
		((myMethod encoderClass pushReceiverScanBlock)
			value: byte)
				or: [scanner pc > end]].
	^scanner pc <= end! !

!BlockClosure methodsFor: 'testing' stamp: 'jmv 7/14/2025 16:29:06'!
readsReceiverIvar
	"Answer whether the receiver reads an instance variable of 'self'."
	| myMethod scanner end |
	myMethod := self method.
	scanner := InstructionStream new method: myMethod pc: startpc.
	end := self endpc.
	scanner scanFor: [ :byte |
		((myMethod encoderClass instVarReadScanBlockUsing: scanner)
			value: byte)
				or: [scanner pc > end]].
	^scanner pc <= end! !

!BlockClosure methodsFor: 'testing' stamp: 'jmv 7/14/2025 16:29:23'!
writesReceiverIvar
	"Answer whether the receiver writes an instance variable of 'self'."
	| myMethod scanner end |
	myMethod := self method.
	scanner := InstructionStream new method: myMethod pc: startpc.
	end := self endpc.
	scanner scanFor: [ :byte |
		((myMethod encoderClass instVarWriteScanBlockUsing: scanner)
			value: byte)
				or: [scanner pc > end]].
	^scanner pc <= end! !


!BlockClosure methodsFor: 'testing' stamp: 'jmv 7/14/2025 16:44:44'!
isCleanClosure
	"A clean closure is one that doesn't really need the home context because:
		- It doesn't send messages to self or super
		- It doesn't use self in any way
		- It doesn't access any instance variable
		- It doesn't access any outer temp
		- It doesn't do ^ return (Non local return, return from method to caller)
	Therefore it doesn't close over a lexical scope, and in this sense they are trivial.
	They can also be called 'context free' or 'simple' block.
	"

	| recreated source |
	source := self decompile decompileString.

	"This catches any acess to outer context!!"
	recreated := [ Compiler evaluate: source. ] on: UndeclaredVariableWarning do: [ :ex | ex return].
	recreated isNil ifTrue: [^false].
	
	recreated hasNonLocalReturn ifTrue: [ ^false ].
	recreated sendsToSuper ifTrue: [ ^false ].
	recreated pushesSelf ifTrue: [ ^false ].
	recreated readsReceiverIvar ifTrue: [ ^false ].
	recreated writesReceiverIvar ifTrue: [ ^false ].

	"Ok."
	^true! !

