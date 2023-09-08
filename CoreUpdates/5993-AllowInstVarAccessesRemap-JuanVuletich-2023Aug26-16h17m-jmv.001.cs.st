'From Cuis 6.0 [latest update: #5983] on 26 August 2023 at 4:18:32 pm'!

!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'jmv 8/24/2023 15:49:06'!
genPushInstVarAllowForIvarsRemapping: instVarIndex
	"Cuis Specific.
	In order of allowing the remapping of methods without the need for recompilation, leave some 'slack'.
	The strategy is:
		- never use the single byte bytecodes in [0..15] and [96..103]
		- Use 2 byte bytecodes 128, 129, 130 for ivar accesses up to 9 (even if they can handle up to 63)
		- Use 3 byte bytecode 132 for all ivar accesses, if ivar index >= 10
	This means that the first 10 ivars can be shifted by adding up to 54 ivars in superclasses, and
	other ivars can be shifted by adding any number of ivars in superclasses (up to the 256 ivars limit).
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

!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'jmv 8/25/2023 15:51:21'!
genStoreInstVarAllowForIvarsRemapping: instVarIndex
	"Cuis Specific.
	In order of allowing the remapping of methods without the need for recompilation, leave some 'slack'.
	The strategy is:
		- never use the single byte bytecodes in [0..15] and [96..103]
		- Use 2 byte bytecodes 128, 129, 130 for ivar accesses up to 9 (even if they can handle up to 63)
		- Use 3 byte bytecode 132 for all ivar accesses, if ivar index >= 10
	This means that the first 10 ivars can be shifted by adding up to 54 ivars in superclasses, and
	other ivars can be shifted by adding any number of ivars in superclasses (up to the 256 ivars limit).
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

!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'jmv 8/25/2023 15:51:27'!
genStorePopInstVarAllowForIvarsRemapping: instVarIndex
	"Cuis Specific.
	In order of allowing the remapping of methods without the need for recompilation, leave some 'slack'.
	The strategy is:
		- never use the single byte bytecodes in [0..15] and [96..103]
		- Use 2 byte bytecodes 128, 129, 130 for ivar accesses up to 9 (even if they can handle up to 63)
		- Use 3 byte bytecode 132 for all ivar accesses, if ivar index >= 10
	This means that the first 10 ivars can be shifted by adding up to 54 ivars in superclasses, and
	other ivars can be shifted by adding any number of ivars in superclasses (up to the 256 ivars limit).
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


!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'jmv 8/24/2023 15:51:39'!
genPushInstVar: instVarIndex
	"Cuis Specific.
	In order of allowing the remapping of methods without the need for recompilation, leave some 'slack'."
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
	self genPushInstVarLong: instVarIndex! !

!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'jmv 8/25/2023 15:51:51'!
genStoreInstVar: instVarIndex
	"Cuis Specific.
	In order of allowing the remapping of methods without the need for recompilation, leave some 'slack'."
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

!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'jmv 8/25/2023 15:52:10'!
genStorePopInstVar: instVarIndex
	"Cuis Specific.
	In order of allowing the remapping of methods without the need for recompilation, leave some 'slack'."
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

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Compiler recompileAll!

