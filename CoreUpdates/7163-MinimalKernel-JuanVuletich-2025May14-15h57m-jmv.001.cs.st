'From Cuis7.3 [latest update: #7160] on 14 May 2025 at 4:10:32 pm'!

!WeakSet methodsFor: '[mkSymbol] accessing' stamp: 'jmv 5/14/2025 16:10:12'!
size
	| answer |
	answer := 0.
	buckets do: [ :bucket |
		bucket notNil ifTrue: [
			bucket do: [ :elem |
				elem notNil ifTrue: [ answer := answer + 1 ]]]].
	^answer.! !


!CharacterSequence class reorganize!
('[mk] instance creation' new numElements)
('[mkSymbol] testing' representsSymbols)
('instance creation' fromUtf8Bytes: new:withAll: readFrom:)
('fileman-constants' pathSeparators)
!


!WeakSet reorganize!
('[mkSymbol] accessing' estimatedSize size)
('[mkSymbol] adding and finding' add: includes: lookup: lookup:ifAbsentAddFrom:)
('[mkSymbol] initialization' init:)
('[mkSymbol] testing' = is:)
('[mkSymbol] comparing' emptyCollectionHash)
('[mkSymbol] private' basicAdd:withHash: basicLookup:withHash: bucketIndexFor: grow indexInBucketFor:)
('[mkSymbol] private-equality or identity' areEqual:and: basicLook:inIterablePartOf: hashFor:)
('[mkSymbol] tuning constants' bucketDesiredMeanOccupation bucketDirectAccessMask bucketDirectAccessSize bucketIterablePartGrow bucketIterablePartStart bucketsHashShift)
('adding and finding' add:withOccurrences: occurrencesOf:)
('enumerating' collect: do: union:)
('copying' postCopy)
('objects from disk' comeFullyUpOnReload:)
('private' rehash rehash:)
('removing' copyWithout: remove:ifAbsent: removeAll)
!

