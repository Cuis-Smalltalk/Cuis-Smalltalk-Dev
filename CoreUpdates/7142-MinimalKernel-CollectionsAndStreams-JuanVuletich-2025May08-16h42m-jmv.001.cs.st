'From Cuis7.3 [latest update: #7130] on 8 May 2025 at 5:09:52 pm'!
!classDefinition: #IdentitySet category: #'MinimalKernel-Collections'!
Set subclass: #IdentitySet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MinimalKernel-Collections'!

!Set reorganize!
('[mkCollections] private' findElementOrNil: init: scanFor:)
('accessing' atRandom: like: size)
('adding' add: add:withOccurrences:)
('lookup' includes: lookup:)
('enumerating' collect: do: union:)
('comparing' emptyCollectionHash)
('copying' postCopy)
('objects from disk' comeFullyUpOnReload:)
('private' array atNewIndex:put: fixCollisionsFrom: fullCheck grow growSize keyAt: noCheckAdd: privateSwapArrayElement:with: rehash)
('removing' copyWithout: remove:ifAbsent: removeAll)
('testing' = is: occurrencesOf:)
('inspecting' inspectorClass)
!


!Set class reorganize!
('[mkCollections] instance creation' new newWithRoomForMoreThan: sizeFor:)
('instance creation' newFrom:)
('initialization' quickRehashAllSets rehashAllSets)
!


!IdentitySet reorganize!
('[mkCollections] private' scanFor:)
('accessing' elementForIdentityHash:)
!


!Stream reorganize!
('[mk] accessing' binary nextPutAll:)
('accessing' basicNext contents flush localName next next: next:put: nextMatchAll: nextPut: nextPut:when: nextPutAll:asCommaSeparated: nextPutAll:when: openReadOnly printOn: readOnly)
('testing' atEnd closed is: isReadOnly isWriteOnly nextUint32WordsPutAll:)
('enumerating' do:)
('printing' print: print:fractionDigits: print:when:)
('FileStream compatibility' close)
('normalized access' nextFloat32BigEndian: nextFloat32Put:bigEndian: nextFloat64BigEndian: nextFloat64Put:bigEndian: nextInt16BigEndian: nextInt16Put:bigEndian: nextInt32BigEndian: nextInt32Put:bigEndian: nextInt64BigEndian: nextInt64Put:bigEndian: nextUint16BigEndian: nextUint16Put:bigEndian: nextUint32BigEndian: nextUint32Put:bigEndian: nextUint64BigEndian: nextUint64Put:bigEndian:)
('nonhomogeneous accessing' nextNumber nextString nextStringPut: nextUnicodeString nextUnicodeStringPut:)
!


!ReadStream reorganize!
('[mk] accessing' next)
('accessing' next: next:into:startingAt: next:putAll:startingAt: nextPut: nextPutAll: readInto:startingAt:count: readStream size upTo:delimiterIsTerminator: upToEnd)
('private' on:from:to:)
('file stream compatibility' localName)
('testing' isReadOnly)
!


!DataStream reorganize!
('[mkObjectStorage] reading' next noteCurrentReference:)
('[mkObjectStorage] writing' setCurrentReference: setStream:)
('write and read' beginInstance:size: beginReference: getCurrentReference maybeBeginReference: nextPut: objectAt: objectIfBlocked: outputReference: readArray readBitmap readByteArray readCharacter readClass readFalse readFloat readInstance readInteger readMethod readNil readReference readShortInst readShortRef readString readSymbol readTrue readUnicodeString readUnicodeSymbol readUser readWordArray readWordArrayForSegment readWordLike replace:with: tryToPutReference:typeID: typeIDFor: writeArray: writeBitmap: writeByteArray: writeCharacter: writeClass: writeFalse: writeFloat: writeInstance: writeInteger: writeNil: writeString: writeStringOld: writeSymbol: writeTrue: writeUnicodeString: writeUnicodeSymbol: writeUser: writeWordLike:)
('other' atEnd byteStream contents errorRead errorWriteReference: flush next: reset rootObject rootObject: setStream:reading: size vacantRef)
!


!DataStream class reorganize!
('[mkObjectStorage] instance creation' on:)
('[mkObjectStorage] cached state access' initCachedState referenceTypes releaseClassCachedState typeMap)
('instance creation' new)
('services' streamedRepresentationOf: unStream:)
('examples' example exampleWithPictures testWith:)
!


!ReferenceStream reorganize!
('[mkObjectStorage] reading' next noteCurrentReference:)
('[mkObjectStorage] writing' isAReferenceType: setCurrentReference: setStream:)
('reading' beginReference: getCurrentReference maybeBeginReference: objectAt:)
('writing' addSpecialReference: beginInstance:size: blockers blockers: nextPutWeak: objectIfBlocked: references replace:with: reset setStream:reading: tryToPutReference:typeID:)
('statistics' statisticsOfRefs)
!

