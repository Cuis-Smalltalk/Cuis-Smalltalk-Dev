'From Cuis7.5 [latest update: #7359] on 17 July 2025 at 9:29:54 am'!

!InstructionClient methodsFor: 'instruction decoding' stamp: 'jmv 7/16/2025 11:36:04'!
doNop
	"Do Nothing Operation bytecode."
! !


!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'jmv 7/16/2025 11:36:44'!
doNop
	"Print the Do Nothing Operation bytecode."
	self print: 'nop'! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'jmv 7/16/2025 16:38:06'!
doNop
	"No action needed"! !


!ContextPart methodsFor: 'instruction decoding' stamp: 'jmv 7/16/2025 16:43:49'!
doNop
	"Simulates the action of a 'No Operation' bytecode."! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'jmv 7/16/2025 11:46:14'!
genPushInstVar: instVarIndex
	"0-15 		0000iiii 					Push Receiver Variable #iiii
	 226		11100010	i i i i i i i i	Push Receiver Variable #iiiiiiii (+ Extend A * 256)"

	(instVarIndex < 0 or: [instVarIndex > 15]) ifTrue:
		[^self genPushInstVarLong: instVarIndex].

	 stream nextPut: 0 + instVarIndex.
	"Cuis Specific.
	In order of allowing the quick remapping of instance variables, leave some 'slack'.
	See
		#displaceInstVarAccessesIn:starting:by:doIt:
		#remapInstVarAccessesIn:asPer:doIt:
	The strategy for the Sista V1 Bytecode Set is to add a NOP (do nothing)
	byte to use two bytes in any case.
	This is good up to 255 instance variables: Use of #genUnsignedSingleExtendA: requires
	a more expensive approach."
	self genNop.! !

!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'jmv 7/16/2025 10:21:12'!
genStoreInstVar: instVarIndex
	"243		11110011	iiiiiiii		Store Receiver Variable #iiiiiii (+ Extend A * 256)"

	self genStoreInstVarLong: instVarIndex.! !

!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'jmv 7/16/2025 11:46:25'!
genStorePopInstVar: instVarIndex
	"200-207	11001 iii				Pop and Store Receiver Variable #iii
	 240		11110000	iiiiiiii	Pop and Store Receiver Variable #iiiiiii (+ Extend A * 256)"

	(instVarIndex < 0 or: [instVarIndex > 7]) ifTrue:
		[^self genStorePopInstVarLong: instVarIndex].

	stream nextPut: 200 + instVarIndex.
	"Cuis Specific.
	In order of allowing the quick remapping of instance variables, leave some 'slack'.
	See
		#displaceInstVarAccessesIn:starting:by:doIt:
		#remapInstVarAccessesIn:asPer:doIt:
	The strategy for the Sista V1 Bytecode Set is to add a NOP (do nothing)
	byte to use two bytes in any case.
	This is good up to 255 instance variables: Use of #genUnsignedSingleExtendA: requires
	a more expensive approach."
	self genNop.! !


!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'jmv 7/16/2025 11:38:57'!
genPushInstVar: instVarIndex
	"Cuis Specific.
	In order of allowing the quick remapping of instance variables, leave some 'slack'.
	See
		#displaceInstVarAccessesIn:starting:by:doIt:
		#remapInstVarAccessesIn:asPer:doIt:
	"
	true ifTrue: [
		^self genPushInstVarAllowForIvarsRemapping: instVarIndex ].

	"See BlueBook page 596"
	instVarIndex >= 0 ifTrue:
		[instVarIndex < 16 ifTrue:
			["0-15 	0000iiii 	Push Receiver Variable #iiii"
			 stream nextPut: 0 + instVarIndex.
			 ^self].
		instVarIndex < 64 ifTrue:
			["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
			 stream
				nextPut: 128;
				nextPut: instVarIndex.
			 ^self]].
	self genPushInstVarLong: instVarIndex.! !

!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'jmv 7/16/2025 11:40:51'!
genPushInstVarAllowForIvarsRemapping: instVarIndex
	"Cuis Specific.
	In order of allowing the quick remapping of instance variables, leave some 'slack'.
	See
		#displaceInstVarAccessesIn:starting:by:doIt:
		#remapInstVarAccessesIn:asPer:doIt:
	The strategy for the V3 Bytecode Set is as follows:
		- never use the single byte bytecodes in [0..15] and [96..103]
		- Use 2 byte bytecodes 128, 129, 130 for ivar accesses up to 9 (even if they can handle up to 63)
		- Use 3 byte bytecode 132 for all ivar accesses, if ivar index >= 10
	This means that the first 10 ivars can be shifted by adding up to 54 ivars in superclasses, and
	other ivars can be shifted by adding any number of ivars in superclasses (up to the 255 ivars limit).
	This is needed for binding class shape modifying DynamicCuisLibraries, or 
	DynamicCuisLibraries built with a different class shape than the image loading them.
	The reason for using 128, 129 and 130 bytecodes for the first ivars is because the VM uses bytecode 132
	with small ivar index to indicate an access to ivars of MethodContext or ContextPart that require a more
	expensive processing. Using 128, 129 or 130 avoids this extra cost.
	This applies both to Spur Interpreter and Cog jitter.
	In the VMMaker package see #isReadMediatedContextInstVarIndex: and #isWriteMediatedContextInstVarIndex:
	The space cost of the use of these longer bytecodes is very small: about 34kb in the official Cuis image, or
	2 bytes per CompiledMethod in average.
	"

	"See BlueBook page 596"
	(instVarIndex >= 0 and: [instVarIndex < 10]) ifTrue: [
		"128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: instVarIndex.
		 ^self ].
	self genPushInstVarLong: instVarIndex.! !

!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'jmv 7/16/2025 11:39:16'!
genStoreInstVar: instVarIndex
	"Cuis Specific.
	In order of allowing the quick remapping of instance variables, leave some 'slack'.
	See
		#displaceInstVarAccessesIn:starting:by:doIt:
		#remapInstVarAccessesIn:asPer:doIt:
	"
	true ifTrue: [
		^self genStoreInstVarAllowForIvarsRemapping: instVarIndex ].

	"See BlueBook page 596"
	(instVarIndex >= 0 and: [instVarIndex < 64]) ifTrue: 
		["129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 129;
			nextPut: instVarIndex.
		 ^self].
	self genStoreInstVarLong: instVarIndex! !

!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'jmv 7/16/2025 11:40:57'!
genStoreInstVarAllowForIvarsRemapping: instVarIndex
	"Cuis Specific.
	In order of allowing the quick remapping of instance variables, leave some 'slack'.
	See
		#displaceInstVarAccessesIn:starting:by:doIt:
		#remapInstVarAccessesIn:asPer:doIt:
	The strategy for the V3 Bytecode Set is as follows:
		- never use the single byte bytecodes in [0..15] and [96..103]
		- Use 2 byte bytecodes 128, 129, 130 for ivar accesses up to 9 (even if they can handle up to 63)
		- Use 3 byte bytecode 132 for all ivar accesses, if ivar index >= 10
	This means that the first 10 ivars can be shifted by adding up to 54 ivars in superclasses, and
	other ivars can be shifted by adding any number of ivars in superclasses (up to the 255 ivars limit).
	This is needed for binding class shape modifying DynamicCuisLibraries, or 
	DynamicCuisLibraries built with a different class shape than the image loading them.
	The reason for using 128, 129 and 130 bytecodes for the first ivars is because the VM uses bytecode 132
	with small ivar index to indicate an access to ivars of MethodContext or ContextPart that require a more
	expensive processing. Using 128, 129 or 130 avoids this extra cost.
	This applies both to Spur Interpreter and Cog jitter.
	In the VMMaker package see #isReadMediatedContextInstVarIndex: and #isWriteMediatedContextInstVarIndex:
	The space cost of the use of these longer bytecodes is very small: about 34kb in the official Cuis image, or
	2 bytes per CompiledMethod in average.
	"

	"See BlueBook page 596"
	(instVarIndex >= 0 and: [instVarIndex < 10]) ifTrue: [
		"129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 129;
			nextPut: instVarIndex.
		 ^self ].
	self genStoreInstVarLong: instVarIndex.! !

!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'jmv 7/16/2025 11:39:22'!
genStorePopInstVar: instVarIndex
	"Cuis Specific.
	In order of allowing the quick remapping of instance variables, leave some 'slack'.
	See
		#displaceInstVarAccessesIn:starting:by:doIt:
		#remapInstVarAccessesIn:asPer:doIt:
	"
	true ifTrue: [
		^self genStorePopInstVarAllowForIvarsRemapping: instVarIndex ].

	"See BlueBook page 596"
	instVarIndex >= 0 ifTrue:
		[instVarIndex < 8 ifTrue:
			["96-103 	01100iii 	Pop and Store Receiver Variable #iii"
			 stream nextPut: 96 + instVarIndex.
			 ^self].
		instVarIndex < 64 ifTrue:
			["130 	10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
			 stream
				nextPut: 130;
				nextPut: instVarIndex.
			 ^self]].
	self genStorePopInstVarLong: instVarIndex! !

!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'jmv 7/16/2025 11:41:01'!
genStorePopInstVarAllowForIvarsRemapping: instVarIndex
	"Cuis Specific.
	In order of allowing the quick remapping of instance variables, leave some 'slack'.
	See
		#displaceInstVarAccessesIn:starting:by:doIt:
		#remapInstVarAccessesIn:asPer:doIt:
	The strategy for the V3 Bytecode Set is as follows:
		- never use the single byte bytecodes in [0..15] and [96..103]
		- Use 2 byte bytecodes 128, 129, 130 for ivar accesses up to 9 (even if they can handle up to 63)
		- Use 3 byte bytecode 132 for all ivar accesses, if ivar index >= 10
	This means that the first 10 ivars can be shifted by adding up to 54 ivars in superclasses, and
	other ivars can be shifted by adding any number of ivars in superclasses (up to the 255 ivars limit).
	This is needed for binding class shape modifying DynamicCuisLibraries, or 
	DynamicCuisLibraries built with a different class shape than the image loading them.
	The reason for using 128, 129 and 130 bytecodes for the first ivars is because the VM uses bytecode 132
	with small ivar index to indicate an access to ivars of MethodContext or ContextPart that require a more
	expensive processing. Using 128, 129 or 130 avoids this extra cost.
	This applies both to Spur Interpreter and Cog jitter.
	In the VMMaker package see #isReadMediatedContextInstVarIndex: and #isWriteMediatedContextInstVarIndex:
	The space cost of the use of these longer bytecodes is very small: about 34kb in the official Cuis image, or
	2 bytes per CompiledMethod in average.
	"

	"See BlueBook page 596"
	(instVarIndex >= 0 and: [instVarIndex < 10]) ifTrue: [
		"130 	10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 130;
			nextPut: instVarIndex.
		 ^self ].
	self genStorePopInstVarLong: instVarIndex.! !

