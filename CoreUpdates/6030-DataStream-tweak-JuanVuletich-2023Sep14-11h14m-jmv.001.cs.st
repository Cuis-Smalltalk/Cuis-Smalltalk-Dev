'From Cuis 6.0 [latest update: #6028] on 14 September 2023 at 11:14:46 am'!

!DataStream methodsFor: 'write and read' stamp: 'jmv 9/14/2023 10:24:50'!
readInstance
	"PRIVATE -- Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
	Let the instance, not the class read the data.  "
	| instSize aSymbol refPosn anObject newClass |

	instSize := (byteStream nextUint32BigEndian: true) - 1.
	refPosn := self getCurrentReference.
	aSymbol := self next.
	newClass := Smalltalk at: aSymbol asSymbol ifAbsent: [
		self error: 'Can not deserialize an instance of ', aSymbol, '. Class is missing.'.
		^nil ].
	anObject := newClass isVariable 	"Create object here"
			ifFalse: [newClass basicNew]
			ifTrue: [newClass basicNew: instSize - (newClass instSize)].
	self setCurrentReference: refPosn.  "before readDataFrom:size:"
	anObject := anObject readDataFrom: self size: instSize.
	self setCurrentReference: refPosn.  "before returning to next"
	^ anObject! !

