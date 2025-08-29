'From Cuis7.5 [latest update: #7462] on 28 August 2025 at 3:50:22 pm'!

!CharacterSequence methodsFor: 'converting' library: #MinimalKernel02Symbol stamp: 'ST-80 5/31/1983 9:10:35'!
asSymbol
	"Answer the unique symbol whose characters are the characters of the string."
	^Symbol intern: self! !


!CharacterSequence class methodsFor: 'testing' library: #MinimalKernel02Symbol stamp: 'jmv 5/6/2025 15:02:25'!
representsSymbols
	^false! !


!String class methodsFor: 'system' library: #MinimalKernel02Symbol stamp: 'jmv 6/3/2022 10:29:45'!
symbolClassToUse
	"Answer the class to create symbols from us"
	^Symbol! !


!Symbol methodsFor: 'converting' library: #MinimalKernel02Symbol stamp: 'ST-80 5/31/1983 9:10:35'!
asSymbol! !

!Symbol methodsFor: 'private' library: #MinimalKernel02Symbol stamp: 'ST-80 5/31/1983 9:10:35'!
errorNoModification
	self error:  'symbols can not be modified.'! !

!Symbol methodsFor: 'private' library: #MinimalKernel02Symbol stamp: 'di 1/2/1999 17:00'!
flushCache
	"Tell the interpreter to remove all entries with this symbol as a selector from its method lookup cache, if it has one.  This primitive must be called whenever a method is defined or removed.
	NOTE:  Only one of the two selective flush methods needs to be used.
	Squeak 2.3 and later uses 116 (See CompiledMethod flushCache)."

	<primitive: 119>
! !

!Symbol methodsFor: 'private' library: #MinimalKernel02Symbol stamp: 'ST-80 5/31/1983 9:10:35'!
string: aString 
	1 to: aString size do: [:j | super at: j put: (aString at: j)].
	^self! !


!Symbol class methodsFor: 'instance creation' library: #MinimalKernel02Symbol stamp: 'jmv 1/10/2025 11:53:15'!
intern: aStringOrSymbol

	^self accessProtect critical:
		[
			Symbols lookup: aStringOrSymbol ifAbsentAddFrom:
				[
					| preferByteStringForAscii |
					aStringOrSymbol isSymbol
						ifTrue: [ aStringOrSymbol ]
						ifFalse: [
							preferByteStringForAscii := aStringOrSymbol.
							(aStringOrSymbol isUnicodeString and: [ aStringOrSymbol isAscii ])
								ifTrue: [preferByteStringForAscii := aStringOrSymbol asByteString].
							preferByteStringForAscii class symbolClassToUse basicFromString: preferByteStringForAscii ].
				]
			].! !

!Symbol class methodsFor: 'instance creation' library: #MinimalKernel02Symbol stamp: 'jmv 12/1/2024 15:49:35'!
lookup: aStringOrSymbol

	^self accessProtect critical: [
		Symbols lookup: aStringOrSymbol ].! !

!Symbol class methodsFor: 'private' library: #MinimalKernel02Symbol stamp: 'jmv 12/1/2024 15:47:52'!
accessProtect
	AccessProtect ifNil: [
		AccessProtect := Semaphore forMutualExclusion ].
	^AccessProtect! !

!Symbol class methodsFor: 'private' library: #MinimalKernel02Symbol stamp: 'jmv 6/3/2022 10:28:45'!
basicFromString: aString
	"Private use. Answers an instance that is not yet interned.
	Can break the Symbol invariant: forAllSymbols, (s1 = s2) => (s1 == s2)
	Please don't call lightly."

	^(self basicNew: aString size) string: aString! !

!Symbol class methodsFor: 'services' library: #MinimalKernel02Symbol stamp: 'jmv 5/6/2025 14:51:13'!
estimatedCount
	^Symbols estimatedSize! !

!Symbol class methodsFor: 'class initialization' library: #MinimalKernel02Symbol stamp: 'jmv 5/6/2025 15:06:49'!
initialize
	"Rebuild the hash table"
	"
	Symbol initialize.
	"
	self accessProtect critical:
		[
			Symbols := WeakSet new.
			CharacterSequence allSubclassesDo: [ :cls | | sym |
				cls representsSymbols ifTrue: [
					sym := cls someInstance.
					[sym == nil] whileFalse: [
						Symbols add: sym.
						sym := sym nextInstance ]]]]! !

!Symbol class methodsFor: 'testing' library: #MinimalKernel02Symbol stamp: 'jmv 5/6/2025 15:02:37'!
representsSymbols
	^true! !


!WeakSet methodsFor: 'adding and finding' library: #MinimalKernel02Symbol stamp: 'jmv 1/13/2025 13:11:00'!
add: newObject
	"Include newObject as one of the receiver's elements, but only if
	not already present.
	Answer newObject (even if another, but equal, object is already included)."

	| hash |
	hash := self hashFor: newObject.
	(self basicLookup: newObject withHash: hash) ifNil: [
		self basicAdd: newObject withHash: hash ].
	^newObject! !

!WeakSet methodsFor: 'adding and finding' library: #MinimalKernel02Symbol stamp: 'jmv 1/10/2025 10:55:11'!
includes: anObject 
	^ (self lookup: anObject) ~~ nil! !

!WeakSet methodsFor: 'adding and finding' library: #MinimalKernel02Symbol stamp: 'jmv 1/11/2025 10:14:21'!
lookup: anObject
	"Look for an element equal to anObject.
	If found, answer it. Answer nil otherwise.
	Note: The hash will be used several times, and it may be expensive. Ask for it just once."

	^self basicLookup: anObject withHash: (self hashFor: anObject)! !

!WeakSet methodsFor: 'adding and finding' library: #MinimalKernel02Symbol stamp: 'jmv 1/11/2025 10:22:07'!
lookup: anObject ifAbsentAddFrom: aBlock
	"Look for an element equal to anObject.
	If found, answer it.
	If not found, evaluate aBlock, add the result and answer it.
	Note: Do NOT add anObject to the Set!!"
	
	| newObject |
	^(self lookup: anObject) ifNil: [
		newObject := aBlock value.
		self basicAdd: newObject withHash: (self hashFor: newObject).
		newObject ].! !

!WeakSet methodsFor: 'accessing' library: #MinimalKernel02Symbol stamp: 'jmv 1/13/2025 13:08:52'!
estimatedSize
	"Actual size may be less: as our references to elements are weak, elements could vanish anytime."

	^estimatedSize! !

!WeakSet methodsFor: 'accessing' library: #MinimalKernel02Symbol stamp: 'jmv 5/14/2025 16:10:12'!
size
	| answer |
	answer := 0.
	buckets do: [ :bucket |
		bucket notNil ifTrue: [
			bucket do: [ :elem |
				elem notNil ifTrue: [ answer := answer + 1 ]]]].
	^answer.! !

!WeakSet methodsFor: 'initialization' library: #MinimalKernel02Symbol stamp: 'jmv 1/13/2025 12:44:39'!
init: n
	"Initialize array to an array size of n"
	buckets := Array new: n.
	estimatedSize := 0.! !

!WeakSet methodsFor: 'testing' library: #MinimalKernel02Symbol stamp: 'jmv 1/13/2025 12:29:57'!
= aSet

	self == aSet ifTrue: [^ true].	"Any object is equal to itself"
	(aSet is: #Set) ifFalse: [^ false].
	(aSet is: #Dictionary) ifTrue: [^ false].
	self size = aSet size ifFalse: [^ false].
	self do: [ :each | (aSet includes: each) ifFalse: [^ false]].
	^ true! !

!WeakSet methodsFor: 'testing' library: #MinimalKernel02Symbol stamp: 'jmv 1/10/2025 10:10:13'!
is: aSymbol
	^aSymbol == #Set or: [ super is: aSymbol ]! !

!WeakSet methodsFor: 'comparing' library: #MinimalKernel02Symbol stamp: 'jmv 1/10/2025 10:17:35'!
emptyCollectionHash
	^ Set hash! !

!WeakSet methodsFor: 'private' library: #MinimalKernel02Symbol stamp: 'jmv 1/14/2025 17:51:20'!
basicAdd: anObject withHash: hashValue
	"Add an object.
	Note: It is assumed that the argument is NOT already included. Check senders."

	| bucketIndex bucket indexInBucket bucketSize newBucket |

	"bucket creation is lazy"
	bucketIndex := self bucketIndexFor: hashValue.
	bucket := buckets at: bucketIndex.
	bucket ifNil: [ bucket := buckets at: bucketIndex put: (WeakArray new: self bucketDirectAccessSize)].

	"1. If our direct access spot in the hasheable area is free, just use it and we are done."
	indexInBucket := self indexInBucketFor: hashValue.
	(bucket at: indexInBucket) == nil ifTrue: [
		bucket at: indexInBucket put: anObject.
		estimatedSize := estimatedSize + 1.
		^self ].

	"2. If there is available room in the iterable part of the bucket, add our new element there."
	bucketSize := bucket size.
	self bucketIterablePartStart to: bucketSize do: [ :i |
		(bucket at: i) == nil ifTrue: [
			bucket at: i put: anObject.
			estimatedSize := estimatedSize + 1.
			^self ]].

	"There's no room in this bucket. It may be worth growing the collection.
	I (jmv) could not find a good heuristic on whether to grow the collection based on a single bucket.
	The adopted one was to limit average mean occupation over all buckets."
	estimatedSize > (buckets size * self bucketDesiredMeanOccupation) ifTrue: [
		self grow.
		"Now try again.
		Maybe the new element is added in the first part of the bucket,
		or in already available room in the second part. (1 and 2 above).
		Or maybe growing the bucket as below (3) is needed.
		What is sure is that this won't recurse more than once: buckets size was just doubled by #grow."
		^self basicAdd: anObject withHash: hashValue ].

	"3. We still need to grow the bucket. Grow it and add our new element."
	newBucket := WeakArray new: bucketSize + self bucketIterablePartGrow.
	newBucket replaceFrom: 1 to: bucketSize with: bucket startingAt: 1.
	newBucket at: bucketSize + 1 put: anObject.
	buckets at: bucketIndex put: newBucket.
	estimatedSize := estimatedSize + 1.! !

!WeakSet methodsFor: 'private' library: #MinimalKernel02Symbol stamp: 'jmv 1/13/2025 14:56:03'!
basicLookup: anObject withHash: hashValue
	"Look for anObject. Assume hashValue is anObject hash, passed as argument to avoid possibly expensive repeated hash computation.
	Answer the element found, or nil if not found."

	| bucket indexInBucketForDirectAccess directAccessElelemt |
	bucket := buckets at: (self bucketIndexFor: hashValue).
	bucket ifNil: [ ^nil ].

	indexInBucketForDirectAccess := self indexInBucketFor: hashValue.
	directAccessElelemt := bucket at: indexInBucketForDirectAccess.
	(self areEqual: directAccessElelemt and: anObject)
		ifTrue: [ ^directAccessElelemt ].

	(self basicLook: anObject inIterablePartOf: bucket)
		ifNotNil: [ :index | | found |
			"We found it at the iterable part of the bucket.
			Move to the the direct access slot if possible."
			found := bucket at: index.
			directAccessElelemt isNil ifTrue: 
				[
					bucket at: indexInBucketForDirectAccess put: found.
					bucket at: index put: nil].
			^found].

	^nil! !

!WeakSet methodsFor: 'private' library: #MinimalKernel02Symbol stamp: 'jmv 1/13/2025 14:38:32'!
bucketIndexFor: hashValue

	"The lowest few bits are used to index into each bucket.
	Don't use them for finding the bucket.
	See #indexInBucketFor:"
	^ ((hashValue bitShift: self bucketsHashShift)
			"Take only as much bits as we can use."
			bitAnd: buckets size-1)
				"And remember Smalltalk does 1-based indexing."
				+ 1! !

!WeakSet methodsFor: 'private' library: #MinimalKernel02Symbol stamp: 'jmv 1/14/2025 17:20:05'!
grow
	"Duplicate size of buckets. Keep old buckets.
	For each old bucket:
	- Duplicate it as a new bucket
	- Move elements that belong in the new bucket to it
	- If possible, move elements in the iterable area to the hasheable area. This is important for search performance.
	
	Note: An alternative implementation is simply
		self rehash: buckets size * 2
	But the current implementation is much faster, at the expense of additional memory consumption."

	| newBuckets oldBucketsSize maskForNewUsedHashBit anOldBucket aNewBucket elementHash diirectAccessIndexInBucket keepdOldBucket |
	estimatedSize := 0.
	oldBucketsSize := buckets size.
	maskForNewUsedHashBit := oldBucketsSize * self bucketDirectAccessSize. "We're directly accessing the full hash, including the lower bits."
	newBuckets := buckets grownTo: oldBucketsSize * 2.
	1 to: oldBucketsSize do:
		[ :indexToOldBucket |
			anOldBucket := buckets at: indexToOldBucket.
			anOldBucket ifNotNil:
				[
					keepdOldBucket := false.
					aNewBucket := nil.
					"Move elements in hasheable area to new bucket if appropriate."
					1 to: self bucketDirectAccessSize do:
						[ :indexInBucket |
							(anOldBucket at: indexInBucket) ifNotNil:
								[ :element |
									estimatedSize := estimatedSize + 1.
									((self hashFor: element) bitAnd: maskForNewUsedHashBit) = 0
									ifTrue:
										[ keepdOldBucket := true ]
									ifFalse:
										[
											aNewBucket ifNil: [
												aNewBucket := WeakArray new: anOldBucket size.
												newBuckets at: oldBucketsSize + indexToOldBucket put: aNewBucket ].
											aNewBucket at: indexInBucket put: (anOldBucket at: indexInBucket).
											anOldBucket at: indexInBucket put: nil ]]].
					"For elements is iterable area, try to move them to the hasheable area."
					self bucketIterablePartStart to: anOldBucket size do:
						[ :indexInBucket |
							(anOldBucket at: indexInBucket) ifNotNil:
								[ :element |
									estimatedSize := estimatedSize + 1.
									elementHash := self hashFor: element.
									diirectAccessIndexInBucket := self indexInBucketFor: elementHash.
									(elementHash bitAnd: maskForNewUsedHashBit) = 0
									ifTrue:
										[ "Stays in old bucket. Try to move to hasheable part."
											keepdOldBucket := true.
											(anOldBucket at: diirectAccessIndexInBucket) ifNil:
												[
													anOldBucket at: diirectAccessIndexInBucket put: element.
													anOldBucket at: indexInBucket put: nil ]]
									ifFalse:
										[ "Moves to new bucket. Try to move to hasheable part, or move to iterable part."
											aNewBucket ifNil: [
												aNewBucket := WeakArray new: anOldBucket size.
												newBuckets at: oldBucketsSize + indexToOldBucket put: aNewBucket ].
											(aNewBucket at: diirectAccessIndexInBucket) ifNil:
												[ aNewBucket at: diirectAccessIndexInBucket put: element ]
											ifNotNil:
												[ aNewBucket at: indexInBucket put: element ].
											anOldBucket at: indexInBucket put: nil ]]].
					keepdOldBucket ifFalse:
						[newBuckets at: indexToOldBucket put: nil ]]].
	buckets := newBuckets.! !

!WeakSet methodsFor: 'private' library: #MinimalKernel02Symbol stamp: 'jmv 1/13/2025 14:38:55'!
indexInBucketFor: hashValue
	"see also #bucketIndexFor:"

	^ (hashValue bitAnd: self bucketDirectAccessMask) + 1! !

!WeakSet methodsFor: 'private' library: #MinimalKernel02Symbol stamp: 'jmv 1/14/2025 11:55:48'!
rehash
	self rehash: buckets size! !

!WeakSet methodsFor: 'private' library: #MinimalKernel02Symbol stamp: 'jmv 1/14/2025 11:55:25'!
rehash: newBucketsSize
	| oldBuckets |
	oldBuckets := buckets.
	self init: newBucketsSize.
	oldBuckets do:
		[:eachBucket |
			eachBucket ifNotNil:
				[
					eachBucket do:
						[:element |
							element notNil
								ifTrue: [self add: element]]]]! !

!WeakSet methodsFor: 'private-equality or identity' library: #MinimalKernel02Symbol stamp: 'jmv 1/13/2025 09:43:19'!
areEqual: a and: b
	"To make it easy to implement WeakIdentitySet as a subclass."

	^a = b! !

!WeakSet methodsFor: 'private-equality or identity' library: #MinimalKernel02Symbol stamp: 'jmv 1/13/2025 09:32:18'!
basicLook: anObject inIterablePartOf: aBucket
	"To make it easy to implement WeakIdentitySet as a subclass."
	"Answer the index or nil if not found"

	self bucketIterablePartStart to: aBucket size do: [ :i |
		(aBucket at: i) = anObject ifTrue: [ ^i ]].
	^nil! !

!WeakSet methodsFor: 'private-equality or identity' library: #MinimalKernel02Symbol stamp: 'jmv 1/13/2025 09:08:56'!
hashFor: anObject
	"To make it easy to implement WeakIdentitySet as a subclass."

	^anObject hash! !

!WeakSet methodsFor: 'tuning constants' library: #MinimalKernel02Symbol stamp: 'jmv 1/14/2025 17:56:51'!
bucketDesiredMeanOccupation
	"How much of a bucket to use on average.
	Larger values, avoiding waste of memory on empty slots in buckets mean slower access due to hash conflicts forcing iteration on the iterable part of the unlucky buckets.
	Smaller values, avoiding iteration and faster access mean more wasted memory.
	Some experimentation sugges that using in average half of the buckets gives great performance, with some disregard for memory."

	^5! !

!WeakSet methodsFor: 'tuning constants' library: #MinimalKernel02Symbol stamp: 'jmv 1/13/2025 12:29:06'!
bucketDirectAccessMask
	"The mask to apply to a hash value to index it (+1 due to 1-based indexing).
	Must equal #bucketDirectAccessSize-1.
	See also #bucketDirectAccessSize, #bucketIterablePartStart and #bucketsHashShift"

	^7! !

!WeakSet methodsFor: 'tuning constants' library: #MinimalKernel02Symbol stamp: 'jmv 1/13/2025 12:28:59'!
bucketDirectAccessSize
	"The size of the initial part of each bucket, that is only used for elements with matching hash bits.
	See also #bucketDirectAccessMask, #bucketIterablePartStart and #bucketsHashShift"

	^8! !

!WeakSet methodsFor: 'tuning constants' library: #MinimalKernel02Symbol stamp: 'jmv 1/14/2025 17:53:35'!
bucketIterablePartGrow
	"Number of slots to add at once to the iterable part of Buckets."

	^1! !

!WeakSet methodsFor: 'tuning constants' library: #MinimalKernel02Symbol stamp: 'jmv 1/13/2025 12:29:13'!
bucketIterablePartStart
	"The first index of the iterable area of each Bucket.
	Must equal #bucketDirectAccessSize+1
	See also #bucketDirectAccessSize, #bucketDirectAccessMask and #bucketsHashShift"

	^9! !

!WeakSet methodsFor: 'tuning constants' library: #MinimalKernel02Symbol stamp: 'jmv 1/13/2025 12:29:22'!
bucketsHashShift
	"The mask to apply to a hash value to index it (+1 due to 1-based indexing).
	Must equal (#bucketDirectAccessMask highBit negated)
	See also #bucketDirectAccessSize, #bucketDirectAccessMask and #bucketIterablePartStart"

	^ -3! !

!WeakSet methodsFor: 'copying' library: #MinimalKernel02Symbol stamp: 'jmv 1/13/2025 13:03:50'!
postCopy

	self rehash! !


!WeakSet class methodsFor: 'instance creation' library: #MinimalKernel02Symbol stamp: 'jmv 1/13/2025 13:01:47'!
new
	^ self newWithRoomForMoreThan: 32! !

!WeakSet class methodsFor: 'instance creation' library: #MinimalKernel02Symbol stamp: 'jmv 6/2/2025 14:38:40'!
newWithRoomForMoreThan: nElements
	"Create a Set large enough to hold nElements without growing
	WeakSet newWithRoomForMoreThan: 31
	WeakSet newWithRoomForMoreThan: 32
	"

	| answer |
	answer := self basicNew.
	answer init: (2 raisedToInteger: (nElements // answer bucketDesiredMeanOccupation max: 1) highBit -1).
	^answer! !


!CharacterSequence reorganize!
('accessing' findAnySubStr:startingAt: findBetweenSubStrs: findDelimiters:startingAt: findLastOccurrenceOfString:startingAt: findString: findString:startingAt:caseSensitive: findTokens: findTokens:keep: includesSubString: includesSubstring:caseSensitive: indexOf: indexOfSubCollection:startingAt:ifAbsent: lineCount lineNumber: lines linesDo: skipAnySubStr:startingAt: skipDelimiters:startingAt: string subStrings: substringsSeparatedBy: utf8BytesSize)
('adding' add:)
('copying' append: appendToText: copyReplaceTokens:with: copyReplacing: copyReplacing:into: copyReplacing:with: copyReplacing:with:into: copyReplacing:withAll: copyReplacing:withAll:into: flattenTo: join: newFromAfterAppending:into:keepingFrom:skipping: padded:to:with:)
('comparing' < <= = > >= alike: beginsWith: commonPartWith:startAt:stopAt:applying: commonPrefixWith: endsWith: hash is:substringAt: match: startingAt:match:startingAt:)
('converting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend: asByteString asCamelCase asDate asFileName asIdentifier: asLowercase asMonth asNumber asPlainString asSmalltalkComment asString asSymbol asText asUnHtml asUnaccented asUnicodeString asUnicodeStringOrText asUppercase asUtf8Bytes asUtf8BytesOrByteString asUtf8BytesOrByteStringOrByteArray asValidInstanceVariableName asValidSelector asWeek asYear base64Decoded capitalized contractTo: correctAgainst: correctAgainst:continuedFrom: correctAgainstDictionary:continuedFrom: displayStringOrText findPositiveInteger findSelector firstNonSeparator fixLineEndings initialIntegerOrNil integerOrNilAt: keywords lastNonSeparator prefixAndSuffix: romanNumber squeezedTo: translated truncateTo: truncateWithElipsisTo: uncapitalized withBlanksCondensed withBlanksTrimmed withCuisLineEndings withDescriptiveLineEndings withDoubleColonsDoubled withDoubleColonsUndoubled withNewLines withoutEnclosing: withoutLeadingBlanks withoutPossiblePrefix: withoutPossibleSuffix: withoutPrefix: withoutSeparators withoutSuffix: withoutTrailingBlanks)
('enumerating' collect: collectInArray:)
('printing' print printAsLiteralOn: printOn: storeOn:)
('testing' isAlphaNumeric isLineEndAt: isRemote isSeparators isString)
('user interface' edit editLabel:)
('fileman-converting' asDirectoryEntry asDriveName asFileEntry asFullDirectoryEntry asFullFileEntry asPathComponents asPathTokens withoutWindowsDriveName)
('fileman-testing' beginsWithPathSeparator beginsWithWindowsDriveName isAbsolutePathName isDriveName isPathSeparator isRelativeMark isRelativePathName)
('fileman-accessing' indexOfFirstPathSeparator indexOfLastPathSeparator pathAndLocalName upToFirstPathSeparator upToLastPathSeparator)
('arithmetic' * + - / // \\)
('text conversion emphasis' bold italic pointSize: struck sub super under)
('text conversion attributes' centered justified leftFlush rightFlush)
('text conversion color' black blue cyan gray green magenta red yellow)
('displaying' displayAt: displayOn: displayOn:at: displayOn:at:textColor: displayProgressAt:from:to:during:)
('private' correctAgainstEnumerator:continuedFrom: evaluateExpression:parameters: getEnclosedExpressionFrom:)
('formatting' format:)
('uCompletion' separateKeywords)
('paragraph support' endOfParagraphBefore: indentationIfBlank:)
('grammar' article asPlural exceptionalPlural isUninflictedNoun lowercasePlural pluralize: withArticle)
('fileman-actions' fileContents fileContents:)
('Unicode normalization' asNFC asNFD canonicalComposition canonicalDecomposition)
!


!CharacterSequence class reorganize!
('instance creation' fromUtf8Bytes: new new:withAll: numElements readFrom:)
('fileman-constants' pathSeparators)
('testing' representsSymbols)
!


!String class reorganize!
('instance creation' crString crlfString fromUtf8Bytes: fromUtf8Bytes:trimLastNull: lfString new: new:withAll: newFrom: newLineString percentEscapingNonAscii: percentEscapingNonAsciiAndThoseIn:bytes: tab with: with:with: with:with:with: withAll:)
('primitives' translate:from:to:table:)
('character collation' does:caseInsensitiveCollateBefore: does:collateAfter: does:collateBefore:)
('stream creation' writeStream)
('services' compare:with: compare:with:collated: compareIgnoreCase:with: findStringIgnoreCase:in:startingAt: findSubstring:in:startingAt:matchTable: is:equalTo: isAscii: string:lineIndicesDo: string:withLineEndings: substringsIn: translate:table: translateToLowercase: translateToUppercase:)
('accessing' findString:in:startingAt:)
('static state' basicCaseInsensitiveOrder basicCaseSensitiveOrder caseInsensitiveOrder caseSensitiveOrder lowercasingTable tokenishTable uppercasingTable)
('system' symbolClassToUse)
!


!Symbol reorganize!
('accessing' at:put: precedence replaceFrom:to:with:startingAt:)
('converting' asPlainString asString asSymbol capitalized numArgs: uncapitalized)
('printing' print storeOn:)
('testing' isInfix isInitializePvtSelector isKeyword isLiteral isPvtSelector isSymbol isUnary isValidBinarySelector isValidGlobalName isValidKeywordSelector isValidSelector isValidUnarySelector)
('uCompletion' separateKeywords)
('evaluating' value: value:value: value:value:value: value:value:value:value: valueWithArguments:)
('comparing' =)
('copying' shallowCopy)
('private' errorNoModification flushCache species string:)
!

Symbol initialize!

!Symbol class reorganize!
('access' allSymbolTablesDo: allSymbols allSymbolsSelect: findInterned: selectorsContaining: selectorsMatching: symbolCount thatStartsCaseSensitive:)
('instance creation' intern: lookup: newFrom: readFrom:)
('private' accessProtect basicFromString: rehash)
('services' estimatedCount hasInterned:and: hasInterned:ifTrue: hasInternedAndImplemented: hasInternedAndImplementedOrReferenced: isLiteralSymbol: possibleSelectorsFor:)
('class initialization' initialize)
('testing' representsSymbols)
!


!WeakSet reorganize!
('[mkObjectSerialization] object serialization' comeFullyUpOnReload:)
('adding and finding' add: add:withOccurrences: includes: lookup: lookup:ifAbsentAddFrom: occurrencesOf:)
('enumerating' collect: do: union:)
('removing' copyWithout: remove:ifAbsent: removeAll)
('accessing' estimatedSize size)
('initialization' init:)
('testing' = is:)
('comparing' emptyCollectionHash)
('private' basicAdd:withHash: basicLookup:withHash: bucketIndexFor: grow indexInBucketFor: rehash rehash:)
('private-equality or identity' areEqual:and: basicLook:inIterablePartOf: hashFor:)
('tuning constants' bucketDesiredMeanOccupation bucketDirectAccessMask bucketDirectAccessSize bucketIterablePartGrow bucketIterablePartStart bucketsHashShift)
('copying' postCopy)
!


!WeakSet class reorganize!
('instance creation' new newFrom: newWithRoomForMoreThan:)
('experiments - tuning' experiment01)
!

