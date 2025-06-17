'From Cuis7.5 [latest update: #7267] on 17 June 2025 at 5:18:17 pm'!

!Object methodsFor: 'user interface' stamp: 'len 5/20/2020 04:00:44'!
request: queryString do: acceptBlock
	"Non-modal. Method returns immediately.
	Uses acceptBlock to process user's answer later, when provided by them."
	^ self request: queryString initialAnswer: '' verifying: [:aString| true] do: acceptBlock orCancel: [nil]! !


!EncoderForSistaV1 class methodsFor: 'compiled method support' stamp: 'eem 5/18/2014 18:57'!
instVarReadScanBlockFor: varIndexCode using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."

	"	0-15	0000 i i i i 				Push Receiver Variable #iiii
	*	224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	*	226		11100010	i i i i i i i i	Push Receiver Variable #iiiiiiii (+ Extend A * 256)"
	| extension |
	extension := 0.
	^[:b| | prevext |
	   prevext := extension.
	   extension := b = 224 ifTrue: [scanner followingByte bitShift: 8] ifFalse: [0].
	   (b < 16 and: [b = varIndexCode])
	    or: [b = 226
			and: [scanner followingByte + prevext = varIndexCode]]]! !

!EncoderForSistaV1 class methodsFor: 'compiled method support' stamp: 'eem 5/18/2014 18:59'!
instVarWriteScanBlockFor: varIndexCode using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."

	"	200-207	11001 iii					Pop and Store Receiver Variable #iii
	*	224			11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	*	240			11110000	iiiiiiii		Pop and Store Receiver Variable #iiiiiii (+ Extend A * 256)
	*	243			11110011	iiiiiiii		Store Receiver Variable #iiiiiii (+ Extend A * 256)"
	| extension |
	extension := 0.
	^[:b| | prevext |
	   prevext := extension.
	   extension := b = 224 ifTrue: [scanner followingByte bitShift: 8] ifFalse: [0].
	   (b >= 200
	    and: [b < 208
	    and: [b - 200 = varIndexCode]])
	   or: [(b = 240 or: [b = 243])
		  and: [scanner followingByte + prevext = varIndexCode]]]! !

