'From Cuis 5.0 [latest update: #4384] on 29 September 2020 at 10:52:57 am'!

!SmartRefStream methodsFor: 'read write' stamp: 'jmv 9/29/2020 10:52:42'!
readWordLike
	| refPosn className newName newClass anObject |
	"Can be used by any class that is bits and not bytes (WordArray, Bitmap, SoundBuffer, etc)."

	refPosn _ self getCurrentReference.
	className _ self next asSymbol.
	newName _ renamed at: className ifAbsent: [className].
	newClass _ Smalltalk at: newName.
	anObject _ newClass newFromStream: byteStream.
	"Size is number of long words."
	self setCurrentReference: refPosn.  "before returning to next"
	^ anObject
! !


!SmartRefStream methodsFor: 'read write' stamp: 'jmv 9/29/2020 10:39:04'!
initKnownRenames
	"Stuff like"
	"
	renamed
		at: #FlasherMorph put: #Flasher;
		yourself
	"
	renamed
		at: #FloatArray put: #Float32Array;
		yourself! !

