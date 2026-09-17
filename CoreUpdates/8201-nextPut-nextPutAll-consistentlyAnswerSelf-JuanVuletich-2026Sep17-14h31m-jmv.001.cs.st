'From Cuis7.9 [latest update: #8200] on 17 September 2026 at 2:41:23 pm'!

!ImageReadWriter methodsFor: 'stream access' stamp: 'jmv 16/Sep/2026, 8:49:08 pm (UTC)'!
nextPut: aByte

	stream nextPut: aByte! !

!ImageReadWriter methodsFor: 'stream access' stamp: 'jmv 16/Sep/2026, 8:50:39 pm (UTC)'!
nextPutAll: aByteArray

	stream nextPutAll: aByteArray! !


!SharedQueue methodsFor: 'accessing' stamp: 'jmv 16/Sep/2026, 8:50:02 pm (UTC)'!
nextPut: value 
	"Send value through the receiver.  If a Process has been suspended waiting to 
	receive a value through the receiver, allow it to proceed."

	accessProtect
		critical: [writePosition > contentsArray size
						ifTrue: [self makeRoomAtEnd].
				 contentsArray at: writePosition put: value.
				 writePosition := writePosition + 1].
	readSynch signal.! !


!Stream methodsFor: 'writing' stamp: 'jmv 16/Sep/2026, 8:50:45 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
nextPutAll: aCollection 
	"Append the elements of aCollection to the sequence of objects accessible 
	by the receiver. Answer aCollection."

	aCollection do: [:v | self nextPut: v].
! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 16/Sep/2026, 8:48:26 pm (UTC)'!
nextPut: anObject
	"Write anObject to the receiver stream. Answer anObject."
	| typeID selector objectToStore |

	typeID := self typeIDFor: anObject.
	(self tryToPutReference: anObject typeID: typeID)
		ifTrue: [^ self].

	objectToStore := (self objectIfBlocked: anObject) objectForDataStream: self.
	objectToStore == anObject ifFalse: [typeID := self typeIDFor: objectToStore].

	byteStream nextPut: typeID.
	selector := #(
		writeNil: writeTrue: writeFalse: writeInteger32Bits: 
		errorWriteReference: writeSymbol: writeByteArray: writeArray:
		writeInstance: errorWriteReference: writeBitmap: writeClass:
		errorWriteReference: writeFloat: errorWriteReference: errorWriteReference:
		writeString: writeBitmap: errorWriteReference: writeWordLike: 
		writeInstance: "CompiledMethod"
		writeCharacter: writeUnicodeString: writeUnicodeSymbol:
		writeInteger64Bits:) at: typeID.
	self perform: selector with: objectToStore.


"NOTE: If anObject is a reference type (one that we write cross-references to) but its externalized form (result of objectForDataStream:) isn't (e.g. CompiledMethod), then we should remember its externalized form
 but not add to 'references'. Putting that object again should just put its
 external form again. That's more compact and avoids seeks when reading.
 But we just do the simple thing here, allowing backward-references for
 non-reference types like nil. So objectAt: has to compensate. Objects that
 externalize nicely won't contain the likes of these, so this shouldn't
 hurt much.
	 writeReference: -> errorWriteReference:."! !


!StdIOWriteStream methodsFor: 'streaming' stamp: 'jmv 16/Sep/2026, 8:48:10 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
nextPut: aCharacter
	"Write the given character or byte to this file.
		 StdIOWriteStream stdout nextPut: $a; flush.
	"
	self nextPutAll: (String with: aCharacter).! !

!StdIOWriteStream methodsFor: 'streaming' stamp: 'jmv 16/Sep/2026, 8:51:19 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
nextPutAll: aStringOrUTF8Bytes
	"Write the given String  or ByteArray (with utf-8 contents)
		 StdIOWriteStream stdout nextPutAll: '--- &#16r2713; ==='; flush.
		 StdIOWriteStream stdout nextPutAll: '--- &#10003; ==='; flush.
		 StdIOWriteStream stdout nextPutAll: #[226 156 147]; flush.
		StdIOWriteStream stdout nextPutAll: '¿El Ñandú toma agüita?', String newLineString; flush.
		StdIOWriteStream stdout nextPutAll: ('¿El Ñandú toma agüita?', String newLineString) asUtf8 ; flush.
	See at the end of this method for a larger example with NCRs for arbitrary Unicode
	"
	| utf8Bytes |
	utf8Bytes := aStringOrUTF8Bytes isString
		ifTrue: [ aStringOrUTF8Bytes asUtf8Bytes ]
		ifFalse: [ aStringOrUTF8Bytes ].
	self primWrite: fileID from: utf8Bytes startingAt: 1 count: utf8Bytes size.
"
StdIOWriteStream stdout nextPutAll: 'Лорем ипсум долор сит амет, дебет салутанди меи еи. Ан хис граеце утамур долорум, лорем принципес молестиае дуо ин. Еи мовет ностро оптион нам, еос епицури омиттантур ут. Ут мунере лаборе интеллегебат усу, еа сед дебет луптатум, видит иллуд иудицо яуо ут.'; flush
"! !

