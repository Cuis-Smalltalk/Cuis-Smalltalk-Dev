'From Cuis7.5 [latest update: #7456] on 28 August 2025 at 3:25:56 pm'!

!Collection methodsFor: 'enumerating' library: #MinimalKernel04HashedCollections stamp: 'Squeak1.0 9/20/1996 10:50:05'!
associationsDo: aBlock
	"Evaluate aBlock for each of the receiver's elements (key/value 
	associations).  If any non-association is within, the error is not caught now,
	but later, when a key or value message is sent to it."

	self do: aBlock! !


!Set methodsFor: 'accessing' library: #MinimalKernel04HashedCollections stamp: 'ST-80 5/31/1983 9:10:35'!
size
	^tally! !

!Set methodsFor: 'enumerating' library: #MinimalKernel04HashedCollections stamp: 'jmv 5/27/2015 13:49'!
do: aBlock 
	| each |
	tally = 0 ifTrue: [^ self].
	1 to: array size do: [ :index |
		(each := array at: index) ifNotNil: [aBlock value: each]]! !

!Set methodsFor: 'private' library: #MinimalKernel04HashedCollections stamp: 'len 2/26/2016 21:29'!
atNewIndex: index put: anObject
	array at: index put: anObject.
	tally := tally + 1.
	self fullCheck.
	^ anObject! !

!Set methodsFor: 'private' library: #MinimalKernel04HashedCollections stamp: 'Squeak1.2 6/29/1997 10:40:55'!
findElementOrNil: anObject
	"Answer the index of a first slot containing either a nil (indicating an empty slot) or an element that matches the given object. Answer the index of that slot or zero. Fail if neither a match nor an empty slot is found."
	| index |
	index := self scanFor: anObject.
	index > 0 ifTrue: [ ^ index ].

	"Bad scene.  Neither have we found a matching element
	nor even an empty slot.  No hashed set is ever supposed to get
	completely full."
	self error: 'There is no free space in this set!!'.! !

!Set methodsFor: 'private' library: #MinimalKernel04HashedCollections stamp: 'di 11/4/97 20:11'!
fullCheck
	"Keep array at least 1/4 free for decent hash behavior"
	array size - tally < (array size // 4 max: 1)
		ifTrue: [self grow]! !

!Set methodsFor: 'private' library: #MinimalKernel04HashedCollections stamp: 'jmv 3/1/2010 13:19'!
grow
	"Grow the elements array and reinsert the old elements"
	| oldElements |
	oldElements := array.
	array := Array new: array size + self growSize.
	tally := 0.
	oldElements do: [ :each |
		each ifNotNil: [self noCheckAdd: each]]! !

!Set methodsFor: 'private' library: #MinimalKernel04HashedCollections stamp: 'Squeak1.0 9/20/1996 10:50:05'!
growSize
	^ array size max: 2! !

!Set methodsFor: 'private' library: #MinimalKernel04HashedCollections stamp: 'Squeak1.0 9/20/1996 10:50:05'!
init: n
	"Initialize array to an array size of n"
	array := Array new: n.
	tally := 0! !

!Set methodsFor: 'private' library: #MinimalKernel04HashedCollections stamp: 'Squeak1.0 9/20/1996 10:50:05'!
noCheckAdd: anObject
	array at: (self findElementOrNil: anObject) put: anObject.
	tally := tally + 1! !

!Set methodsFor: 'private' library: #MinimalKernel04HashedCollections stamp: 'Squeak1.2 6/29/1997 10:40:55'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	start := (anObject hash \\ array size) + 1.
	finish := array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [element = anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [element = anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !

!Set methodsFor: 'testing' library: #MinimalKernel04HashedCollections stamp: 'jmv 1/10/2025 10:49:13'!
includes: anObject 
	^ (self lookup: anObject) ~~ nil! !

!Set methodsFor: 'testing' library: #MinimalKernel04HashedCollections stamp: 'jmv 7/4/2016 22:16'!
is: aSymbol
	^aSymbol == #Set or: [ super is: aSymbol ]! !

!Set methodsFor: 'comparing' library: #MinimalKernel04HashedCollections stamp: 'jmv 6/12/2019 18:05:55'!
= aSet

	self == aSet ifTrue: [^ true].	"Any object is equal to itself"
	(aSet is: #Set) ifFalse: [^ false].
	(aSet is: #Dictionary) ifTrue: [^ false].
	self size = aSet size ifFalse: [^ false].
	self do: [ :each | (aSet includes: each) ifFalse: [^ false]].
	^ true! !

!Set methodsFor: 'comparing' library: #MinimalKernel04HashedCollections stamp: 'jmv 6/12/2019 18:09:50'!
emptyCollectionHash
	^ Set hash! !

!Set methodsFor: 'lookup' library: #MinimalKernel04HashedCollections stamp: 'jmv 1/10/2025 10:48:11'!
lookup: anObject
	"Look for an element equal to anObject.
	If found, answer it. Answer nil otherwise."

	^ array at: (self findElementOrNil: anObject).! !

!Set methodsFor: 'copying' library: #MinimalKernel04HashedCollections stamp: 'jmv 12/5/2014 11:02'!
postCopy

	array := array copy! !


!Set class methodsFor: 'instance creation' library: #MinimalKernel04HashedCollections stamp: 'Squeak1.0 9/20/1996 10:50:05'!
new
	^ self newWithRoomForMoreThan: 4! !

!Set class methodsFor: 'instance creation' library: #MinimalKernel04HashedCollections stamp: 'Squeak1.0 9/20/1996 10:50:05'!
newWithRoomForMoreThan: nElements
	"Create a Set large enough to hold nElements without growing"
	^ self basicNew init: (self sizeFor: nElements)! !

!Set class methodsFor: 'instance creation' library: #MinimalKernel04HashedCollections stamp: 'Squeak1.0 9/20/1996 10:50:05'!
sizeFor: nElements
	"Large enough size to hold nElements with some slop (see fullCheck)"
	nElements <= 0 ifTrue: [^ 1].
	^ nElements+1*4//3! !


!Dictionary methodsFor: 'accessing' library: #MinimalKernel04HashedCollections stamp: 'Squeak1.0 9/20/1996 10:50:05'!
associationAt: key ifAbsent: aBlock 
	"Answer the association with the given key.
	If key is not found, return the result of evaluating aBlock."

	| index assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	nil == assoc ifTrue: [ ^ aBlock value ].
	^ assoc! !

!Dictionary methodsFor: 'accessing' library: #MinimalKernel04HashedCollections stamp: 'JMM 12/12/2024 09:19:05'!
associationClass

	^Association! !

!Dictionary methodsFor: 'accessing' library: #MinimalKernel04HashedCollections stamp: 'jmv 9/20/2023 12:05:53'!
at: key 
	"Answer the value at key.  If key is not found, create an error message."

	^self at: key ifAbsent: [ self errorKeyNotFound: key ]! !

!Dictionary methodsFor: 'accessing' library: #MinimalKernel04HashedCollections stamp: 'jmv 5/25/2014 15:51'!
at: key ifAbsent: absentBlock
	"Answer the value associated with the key or, if key isn't found,
	answer the result of evaluating aBlock."

	| assoc |
	assoc := array at: (self findElementOrNil: key).
	assoc ifNil: [ ^ absentBlock value ].
	^ assoc value! !

!Dictionary methodsFor: 'accessing' library: #MinimalKernel04HashedCollections stamp: 'jmv 5/25/2014 15:51'!
at: key ifPresent: presentBlock
	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v := self at: key ifAbsent: [ ^ nil ].
	^ presentBlock value: v
! !

!Dictionary methodsFor: 'accessing' library: #MinimalKernel04HashedCollections stamp: 'JMM 12/2/2024 09:54:17'!
at: key put: anObject
	"Set the value at key to be anObject. 
	If key is not found, create a new entry for key and set is value to anObject.
	If key is found, update the existing association.
	Answer anObject."

	| index assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	assoc
		ifNil: [ self atNewIndex: index put: (self associationClass key: key value: anObject) ]
		ifNotNil: [ assoc value: anObject ].
	^ anObject! !

!Dictionary methodsFor: 'accessing' library: #MinimalKernel04HashedCollections stamp: 'jmv 11/1/2011 23:09'!
bindingOf: varName

	^self associationAt: varName ifAbsent: nil! !

!Dictionary methodsFor: 'testing' library: #MinimalKernel04HashedCollections stamp: 'RAA 8/23/2001 12:56'!
includesKey: key 
	"Answer whether the receiver has a key equal to the argument, key."
	
	self at: key ifAbsent: [^false].
	^true! !

!Dictionary methodsFor: 'testing' library: #MinimalKernel04HashedCollections stamp: 'jmv 7/4/2016 22:13'!
is: aSymbol
	^aSymbol == #Dictionary or: [ super is: aSymbol ]! !

!Dictionary methodsFor: 'enumerating' library: #MinimalKernel04HashedCollections stamp: 'ST-80 5/31/1983 9:10:35'!
associationsDo: aBlock 
	"Evaluate aBlock for each of the receiver's key/value associations."

	super do: aBlock! !

!Dictionary methodsFor: 'enumerating' library: #MinimalKernel04HashedCollections stamp: 'ST-80 5/31/1983 9:10:35'!
do: aBlock 
	super do: [:assoc | aBlock value: assoc value]! !

!Dictionary methodsFor: 'enumerating' library: #MinimalKernel04HashedCollections stamp: 'ST-80 5/31/1983 9:10:35'!
select: aBlock 
	"Evaluate aBlock with each of my values as the argument.  Collect into a new 
	dictionary, only those associations for which aBlock evaluates to true."

	| newCollection |
	newCollection := self species new.
	self associationsDo: 
		[:each | 
		(aBlock value: each value) ifTrue: [newCollection add: each]].
	^newCollection! !

!Dictionary methodsFor: 'private' library: #MinimalKernel04HashedCollections stamp: 'Squeak1.0 9/20/1996 10:50:05'!
noCheckAdd: anObject
	"Must be defined separately for Dictionary because (self findElementOrNil:) expects a key, not an association.  9/7/96 tk"

	array at: (self findElementOrNil: anObject key) put: anObject.
	tally := tally + 1! !

!Dictionary methodsFor: 'private' library: #MinimalKernel04HashedCollections stamp: 'SqR 2/20/2015 21:06'!
postCopy
	"Must copy the associations, or later store will affect both the
original and the copy"

	array := array collect: [:each | each copy]! !

!Dictionary methodsFor: 'private' library: #MinimalKernel04HashedCollections stamp: 'Squeak1.2 6/29/1997 10:40:55'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	start := (anObject hash \\ array size) + 1.
	finish := array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [element key = anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [element key = anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !

!Dictionary methodsFor: 'lookup' library: #MinimalKernel04HashedCollections stamp: 'jmv 1/10/2025 10:48:44'!
lookup: anObject
	"Look for an element equal to anObject.
	If found, answer it. Answer nil otherwise."

	self do: [:each | anObject = each ifTrue: [^each]].
	^nil! !

!Dictionary methodsFor: 'comparing' library: #MinimalKernel04HashedCollections stamp: 'jmv 7/5/2016 09:20'!
= aDictionary
	"Two dictionaries are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same set of keys.
	 (c) for each (common) key, they have the same value".

	self == aDictionary ifTrue: [^ true].	"Any object is equal to itself"
	(aDictionary is: #Dictionary) ifFalse: [^false].
	self size = aDictionary size ifFalse: [^false].
	self associationsDo: [:assoc|
		(aDictionary at: assoc key ifAbsent: [^false]) = assoc value
			ifFalse: [^false]].
	^true

! !

!Dictionary methodsFor: 'comparing' library: #MinimalKernel04HashedCollections stamp: 'jmv 6/12/2019 18:10:04'!
emptyCollectionHash
	^ Dictionary hash! !

!Dictionary methodsFor: 'comparing' library: #MinimalKernel04HashedCollections stamp: 'jmv 6/12/2019 18:11:13'!
hash
	"Note that this method is insensitive to contents when the size is greater than 10, so critical applications that compare many large collections of the same length will want to refine this behavior."

	| hash |
	hash := self emptyCollectionHash.
	self size <= 10 ifTrue: [
		self associationsDo: [ :association | hash := hash bitXor: association hash ]].
	^ hash bitXor: self size hash! !

!Dictionary methodsFor: 'variable bindings' library: #MinimalKernel04HashedCollections stamp: 'jmv 9/14/2023 10:06:58'!
declare: key from: aDictionary
	"Add key to the receiver. If key already exists, do nothing. If aDictionary 
	includes key, then remove it from aDictionary and use its association as 
	the element of the receiver. Answer it.
	If the key wasn't in the receiver or aDictionary, use nil as the value.
	Note: We allow aDictionary to be nil, although it normally is not."

	| association |
	(self includesKey: key) ifTrue: [^ nil].
	(aDictionary notNil and: [aDictionary includesKey: key])
		ifTrue: [
			association := aDictionary associationAt: key.
			self add: association.
			aDictionary removeKey: key.
			^ association ]
		ifFalse: [
			self at: key put: nil.
			^ nil ]! !


!IdentityDictionary methodsFor: 'private' library: #MinimalKernel04HashedCollections stamp: 'jmv 2/21/2020 12:33:35'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| finish scale start element |
	finish := array size.
	scale := finish // (Smalltalk maxIdentityHash + 1).
	scale = 0 ifTrue: [scale := 1].
	start := anObject identityHash * scale \\ finish + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [element key == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [element key == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !


!SystemDictionary methodsFor: 'globals' library: #MinimalKernel04HashedCollections stamp: 'jmv 1/3/2016 00:22'!
at: aKey put: anObject 
	"Override from Dictionary to check Undeclared and fix up
	references to undeclared variables."

	| association |
	(self includesKey: aKey) ifFalse: [
		self flushClassNameCache.

		"Update existing association if there is one."
		(Undeclared includesKey: aKey) ifTrue: [
			association := self declare: aKey from: Undeclared.
			association value: anObject.
			^ anObject ]].

	"Update existing association if there is one."
	^super at: aKey put: anObject! !

!SystemDictionary methodsFor: 'globals' library: #MinimalKernel04HashedCollections stamp: 'jmv 12/28/2010 10:53'!
flushClassNameCache
	"Smalltalk flushClassNameCache"
	"Forse recomputation of the cached list of class names."

	cachedClassNames := nil.
	cachedNonClassNames := nil! !

!SystemDictionary methodsFor: 'system attributes' library: #MinimalKernel04HashedCollections stamp: 'jmv 1/4/2024 15:39:42'!
maxIdentityHash
	"Answer the maximum identityHash value supported by the VM.
	Usually the VM is able to answer. It seems that SqueakJS (as of October 5, 2020) isn't.
	On primitive failure, still give a correct answer."

	<primitive: 176>	
	^ 16r3FFFFF							"22 bits in Spur"! !


!MethodDictionary methodsFor: 'accessing' library: #MinimalKernel04HashedCollections stamp: 'ul 6/18/2010 21:38'!
associationAt: key ifAbsent: aBlock 
	"Answer the association with the given key.
	If key is not found, return the result of evaluating aBlock."

	^(array at: (self scanFor: key)) 
		ifNil: [ aBlock value ]
		ifNotNil: [ :value | key -> value ]! !

!MethodDictionary methodsFor: 'accessing' library: #MinimalKernel04HashedCollections stamp: 'jmv 3/1/2010 11:41'!
at: key ifAbsent: aBlock

	| index |
	index := self findElementOrNil: key.
	(self basicAt: index) ifNil: [ ^ aBlock value ].
	^ array at: index! !

!MethodDictionary methodsFor: 'accessing' library: #MinimalKernel04HashedCollections stamp: 'jmv 3/1/2010 11:41'!
at: key put: value
	"Set the value at key to be value."
	| index |
	index := self findElementOrNil: key.
	(self basicAt: index)
		ifNil: [ 
			tally := tally + 1.
			self basicAt: index put: key]
		ifNotNil: [
			(array at: index) flushCache].
	array at: index put: value.
	self fullCheck.
	^ value! !

!MethodDictionary methodsFor: 'enumeration' library: #MinimalKernel04HashedCollections stamp: 'jmv 3/1/2010 11:45'!
associationsDo: aBlock 

	tally = 0 ifTrue: [^ self].
	1 to: self basicSize do: [ :i | 
		(self basicAt: i) ifNotNil: [ :key |
			aBlock value: (
				Association key: key value: (array at: i))]]! !

!MethodDictionary methodsFor: 'private' library: #MinimalKernel04HashedCollections stamp: 'jmv 2/5/2024 18:26:41'!
grow 
	| newSelf |
	newSelf := self species newWithRoomForMoreThan: self basicSize.  "This will double the size"
	1 to: self basicSize do: [ :i |
		(self basicAt: i)
			ifNotNil: [ :key | newSelf at: key put: (array at: i)]].
	self become: newSelf! !

!MethodDictionary methodsFor: 'private' library: #MinimalKernel04HashedCollections stamp: 'jmv 12/5/2014 11:02'!
postCopy

	array := array copy! !

!MethodDictionary methodsFor: 'private' library: #MinimalKernel04HashedCollections stamp: 'Squeak1.2 6/29/1997 10:40:55'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	start := (anObject identityHash \\ array size) + 1.
	finish := array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := self basicAt: index) == nil or: [element == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := self basicAt: index) == nil or: [element == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !


!IdentitySet methodsFor: 'private' library: #MinimalKernel04HashedCollections stamp: 'jmv 2/21/2020 12:33:27'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| finish scale start element |
	finish := array size.
	scale := finish // (Smalltalk maxIdentityHash + 1).
	scale = 0 ifTrue: [scale := 1].
	start := anObject identityHash * scale \\ finish + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [element == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [element == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !


!Collection reorganize!
('accessing' anyOne atRandom atRandom: size)
('adapting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend:)
('adding' add: add:withOccurrences: addAll:)
('arithmetic' * + - / // \\ raisedTo:)
('comparing' = emptyCollectionHash hash)
('converting' as:on:delimiter:last: asArray asBag asByteArray asCommaSeparated:on: asCommaStringAnd asDictionary asFloat32Array asFloat64Array asIdentitySet asIntegerArray asNewArray asOrderedCollection asPointArray asSet asSortedCollection asSortedCollection: asStringOn:delimiter:last: asWordArray)
('copying' , copyWith: copyWithout: copyWithoutAll: flatten flattenTo:)
('enumerating' allSatisfy: anySatisfy: associationsDo: associationsDo:separatedBy: collect: collect:andFold: collect:andFold:ifEmpty: collect:thenSelect: count: detect: detect:ifFound:ifNone: detect:ifNone: detectMax: detectMin: difference: do: do:separatedBy: do:without: fold: fold:ifEmpty: groupBy: groupBy:having: inject:into: intersection: noneSatisfy: reduce: reject: reject:thenCollect: reject:thenDo: select: select:thenCollect: select:thenDo: union:)
('filter streaming' contents)
('inspecting' hasContentsInExplorer)
('math functions' abs arcCos arcSin arcTan ceiling cos degreeCos degreeSin exp floor ln log log2 negated reciprocal roundTo: rounded sign sin sqrt squared tan truncateTo: truncated)
('printing' printElementsOn: printNameOn: printOn: storeOn:)
('removing' remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat:)
('sorting' sorted sorted:)
('statistics' average average: average:ifEmpty: max max: mean min min: product product: range sum sum: sum:ifEmpty:)
('testing' canHoldAnyCodePoint identityIncludes: ifEmpty: ifEmpty:ifNotEmpty: ifNotEmpty: ifNotEmpty:ifEmpty: includes: includesAllOf: includesAnyOf: includesSubstringAnywhere: is: isCollection isEmpty isEmptyOrNil isSequenceable notEmpty occurrencesOf:)
('private' emptyCheck errorCollectionTooSmall errorEmptyCollection errorNoMatch errorNotFound: errorNotKeyed species)
!


!Set reorganize!
('[mkObjectSerialization] object serialization' comeFullyUpOnReload:)
('accessing' atRandom: like: size)
('adding' add: add:withOccurrences:)
('enumerating' collect: do: union:)
('private' array atNewIndex:put: findElementOrNil: fixCollisionsFrom: fullCheck grow growSize init: keyAt: noCheckAdd: privateSwapArrayElement:with: rehash scanFor:)
('removing' copyWithout: remove:ifAbsent: removeAll)
('testing' includes: is: occurrencesOf:)
('inspecting' inspectorClass)
('comparing' = emptyCollectionHash)
('lookup' lookup:)
('copying' postCopy)
!


!Set class reorganize!
('instance creation' new newFrom: newWithRoomForMoreThan: sizeFor:)
('initialization' quickRehashAllSets rehashAllSets)
!


!Dictionary reorganize!
('accessing' associationAt: associationAt:ifAbsent: associationClass associations at: at:ifAbsent: at:ifAbsentOrNilPut: at:ifAbsentPut: at:ifPresent: at:ifPresent:ifAbsent: at:put: bindingOf: keyAtIdentityValue: keyAtIdentityValue:ifAbsent: keyAtValue: keyAtValue:ifAbsent: keys keysSortedSafely values)
('testing' hasBindingThatBeginsWith: includesKey: is: keyForIdentity: occurrencesOf:)
('adding' add: addAll:)
('removing' keysAndValuesRemove: remove: remove:ifAbsent: removeKey: removeKey:ifAbsent: removeUnreferencedKeys unreferencedKeys)
('enumerating' associationsDo: collect: do: keysAndValuesDo: keysDo: select: valuesDo:)
('printing' printElementsOn: storeOn:)
('private' errorKeyNotFound: errorValueNotFound keyAt: noCheckAdd: postCopy rehash scanFor:)
('inspecting' customizeExplorerContents explorerContents explorerContentsWithIndexCollect: inspectorClass)
('compiling' bindingsDo:)
('lookup' lookup:)
('comparing' = emptyCollectionHash hash)
('variable bindings' declare:from:)
!


!IdentityDictionary reorganize!
('accessing' keyAtValue:ifAbsent:)
('private' scanFor:)
!


!SystemDictionary reorganize!
('[mkVMnImage] primitives' primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primImageName primVmFileName primVmPath)
('[mkVMnImage] miscelaneous' cachedSystemAttribute: getSystemAttribute: listBuiltinModule: listLoadedModule: platformName primGetSystemAttribute:)
('[mkVMnImage] special objects' specialObjectsArray)
('accessing' classes description name organization summary)
('class names' classNamed: fillCaches forgetClass:logged: hasClassNamed: prepareToRenameClass:as: prepareToRenameClass:from:to: removeClassNamed: removeClassNamedIfInBaseSystem: renameClassNamed:as: renamedClass:from:)
('globals' associationOrUndeclaredAt: at:put: declare: flushClassNameCache)
('housekeeping' browseObsoleteMethodReferences browseUndeclaredReferences cleanOutUndeclared condenseChanges condenseSources condenseSources: macroBenchmark1 macroBenchmark3 obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 undeclaredReferences verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName fullNameForChangesNamed: fullNameForImageNamed: imageName imagePath primImageName: userChangesFileName vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost growMemoryByAtLeast: installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' canBeInterrupted: cogitClass detectBuiltInModule: extraVMMemory extraVMMemory: getCurrentWorkingDirectory getVMParameters handleUserInterrupt interpreterClass isDevelopmentEnvironmentPresent isHeadless isModuleAvailable: listBuiltinModules listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformSubtype unloadModule: vmFileName vmOptionsDo: vmVersion voidCogVMState)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn: printOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allImplementorsOf:subTo: allImplementorsOf:superTo: allMethodsInCategory: allMethodsSourceStringMatching: allMethodsWithSourceString:matchCase: allMethodsWithSourceString:matchCase:scopeClass:includeSubclasses:includeSuperclasses: allMethodsWithString: allObjects allObjectsDo: allObjectsOrNil allPrimitiveMessages allPrimitiveMethodsInCategories: allReferencesToLiteral: allSelect: allSelect:localTo: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: hierachySortedAllClassesDo: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: poolUsers rootClasses unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: askConfirmationOnQuit filenameForUpdatedImage: lastQuitLogPosition logSnapshot:andQuit: nopTag printStuffToCleanOnImageSave processShutDownList: processStartUpList: quit quitNoSaveTag quitPrimitive: quitTag readAndApplyUserPrefs removeFromShutDownList: removeFromStartUpList: saveAndQuit saveAndQuitAs:clearAllClassState: saveAndStayAs:clearAllClassState: saveAs saveAs: saveAs:clearAllClassState: saveImage saveRollingReleaseAndQuit saveStableRelease send:toClassesNamedIn:with: setDefaultGCParameters setGCParameters setPlatformPreferences snapshot:andQuit:embedded:clearAllClassState: snapshotEmbeddedPrimitive snapshotMessageFor:andQuit: snapshotPrimitive snapshotTag snapshotTagFor:andQuit: storeOn: tagHeader tagTail unbindExternalPrimitives)
('sources, change log' aboutThisSystem assureStartupStampLogged calcEndianness classRemoved:fromCategory: closeSourceFiles endianness evaluated:context: externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources isBigEndian isLittleEndian lastUpdateString logChange: logChange:preamble: logToUserChanges:preamble: methodRemoved:selector:inProtocol:class: openSourceFiles openSourcesAndChanges systemInformationString timeStamp: versionString)
('special objects' clearExternalObjects externalObjects primitiveErrorTable recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialSelectorAt: specialSelectorSize specialSelectors unregisterExternalObject:)
('ui' beep confirmRemovalOf:on: confirmRemovalOf:on:andSubclasses: globals inspectGlobals primitiveBeep systemCategoryFromUserWithPrompt:)
('browsing - main' browse browseFull: browseFull:selector: browseHierarchy: browseHierarchy:selector: browserWindowClass findAndBrowseClass findInSourceCode)
('browsing' browseAllAccessesTo:from: browseAllCallsOn: browseAllCallsOn:and: browseAllCallsOn:localTo: browseAllCallsOnClass: browseAllClasses browseAllImplementorsOf: browseAllImplementorsOf:localTo: browseAllImplementorsOf:subTo: browseAllImplementorsOf:superTo: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllPrimitives browseAllReferencesToLiteral: browseAllSelect: browseAllSelect:localTo: browseAllSelect:name:autoHighlight: browseAllUnSentMessages browseAllUnimplementedCalls browseClassCommentsWithString: browseClassesWithNamesContaining:caseSensitive: browseEqEqSentToCharacterConstants browseEqEqSentToNumericConstants browseEqEqSentToSmallIntegerConstants browseEqSentToCharacterConstants browseEqSentToNumericConstants browseEqSentToSmallIntegerConstants browseInstVarRefs: browseLikelyUnneededRedefinitions browseMessageList:name: browseMessageList:name:autoHighlight:allOccurrences: browseMessageListUnsorted:name:autoHighlight: browseMethodsWithSourceString: browseMethodsWithSourceString:in: browseMethodsWithSourceString:scopeClass:includeSubclasses:includeSuperclasses: browseMethodsWithString: browseMethodsWithString:matchCase: browseMyChanges browseObsoleteReferences browseSendersOf:notTo: browseViewReferencesFromNonViews showMenuOf:withFirstItem:ifChosenDo: showMenuOf:withFirstItem:ifChosenDo:withCaption:)
('private' allSymbolsIn:do: baseLabel exitToDebugger quitPrimitive)
('Closure measurements' browseMethodsWithEmptyClosures)
('removing' removeKey:ifAbsent:)
('system attributes' flagInterpretedMethods: isRunningCog isRunningCogit maxIdentityHash processHasThreadIdInstVar: processPreemptionYields processPreemptionYields: processSuspensionUnblocks)
('query' classesWithExtensionCategories hierarchySorted:do: methodsWithUnboundGlobals)
('testing' hasBindingThatBeginsWith: isLiveSmalltalkImage isLiveTypingInstalled)
('classes and traits' classNames nonClassNames)
('image format' imageFormatVersion imageFormatVersionFromFile imageFormatVersionFromFileAsIs)
('AndreasProfiler-profiling' interruptChecksPerMSec longRunningPrimitive longRunningPrimitiveSemaphore: profilePrimitive profileSample profileSemaphore: profileStart:)
('startup' checkIfAlreadyRunningOrStoppedNoExit displayCommandLineUsageOn: displaySystemInformationStringOn: doStartUp: lastCommandLineArguments logStartupDebugAid: logStartupDebugAids logStartupError: logStartupNotice: processCommandLineArguments: processFinalCommandLineOption:optionArgument:raiseExceptions: processInitialCommandLineOption:optionArgument: readCommandLineArguments setStartupStamp startUpArguments startUpScriptAllArguments startUpScriptArguments)
('startup - recover user changes' warnAndOfferLastUserChanges)
('startup - restore lost changes' isQuitNoSaveRecord: isQuitRecord: isSnapshotRecord: lostChangesDetectedCaption restoreLostChanges restoreLostChangesAutomatically restoreLostChangesAutomaticallyFrom: restoreLostChangesManually restoreLostChangesOptions restoringChangesHasErrorsCaption shouldShowFileInErrors withChangesFileDo:)
('vm parameters' doMixedArithmetic doMixedArithmetic: sendMouseWheelEvents sendMouseWheelEvents: vmParameterAt: vmParameterAt:put: wordSize)
!


!MethodDictionary reorganize!
('accessing' add: associationAt:ifAbsent: at:ifAbsent: at:put: keyAtIdentityValue:ifAbsent: keyAtValue:ifAbsent:)
('removing' removeAll removeKey:ifAbsent:)
('enumeration' associationsDo: keysAndValuesDo: keysDo: valuesDo: valuesSelect:)
('private' fixCollisionsFrom: grow keyAt: methodArray postCopy privateSwapArrayElement:with: rehash rehashWithoutBecome removeDangerouslyKey:ifAbsent: scanFor:)
!


!IdentitySet reorganize!
('accessing' elementForIdentityHash:)
('private' scanFor:)
!

