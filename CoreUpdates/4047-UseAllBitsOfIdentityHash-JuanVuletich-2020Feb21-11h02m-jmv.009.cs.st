'From Cuis 5.0 [latest update: #4046] on 21 February 2020 at 1:29:11 pm'!

!IdentitySet methodsFor: 'accessing' stamp: 'jmv 2/21/2020 12:47:37'!
elementForIdentityHash: aNumber
	"Answer any element matching argument.
	Answer nil if none found"
	| finish scale index element |
	finish _ array size.
	scale _ finish // (Smalltalk maxIdentityHash + 1).
	scale = 0 ifTrue: [scale _ 1].
	index _ aNumber * scale \\ finish + 1.

	element _ array at: index.
	^element identityHash = aNumber ifTrue: [ element ]! !


!WeakIdentitySet methodsFor: 'accessing' stamp: 'jmv 2/21/2020 12:50:06'!
elementForIdentityHash: aNumber
	"Answer any element matching argument.
	Answer nil if none found"
	| finish scale index element |
	finish _ array size.
	scale _ finish // (Smalltalk maxIdentityHash + 1).
	scale = 0 ifTrue: [scale _ 1].
	index _ aNumber * scale \\ finish + 1.

	element _ array at: index.
	element == flag ifTrue: [ ^ nil ].
	^element identityHash = aNumber ifTrue: [ element ]! !


!ProtoObject methodsFor: 'comparing' stamp: 'jmv 2/21/2020 11:02:26'!
identityHash
	"Answer a SmallInteger whose value is related to the receiver's identity.
	This method must not be overridden, except by immediate classes such as SmallInteger,
	and in Spur systems, Character and SmallFloat64.
	Primitive. Fails if the receiver is a SmallInteger. Essential.
	See Object documentation whatIsAPrimitive.

	Do not override."

	<primitive: 75>
	self primitiveFailed! !


!Set methodsFor: 'private' stamp: 'jmv 2/21/2020 12:39:07'!
keyAt: index
	"May be overridden by subclasses so that fixCollisionsFrom: will work"
	^ array at: index! !

!Set methodsFor: 'private' stamp: 'jmv 2/21/2020 12:39:37'!
swap: oneIndex with: otherIndex
	"May be overridden by subclasses so that fixCollisionsFrom: will work"

	array swap: oneIndex with: otherIndex
! !


!Dictionary methodsFor: 'private' stamp: 'jmv 2/21/2020 12:39:24'!
keyAt: index
	"May be overridden by subclasses so that fixCollisionsFrom: will work"
	| assn |
	assn _ array at: index.
	assn ifNil: [^ nil].
	^ assn key! !


!IdentityDictionary methodsFor: 'private' stamp: 'jmv 2/21/2020 12:33:35'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| finish scale start element |
	finish _ array size.
	scale _ finish // (Smalltalk maxIdentityHash + 1).
	scale = 0 ifTrue: [scale _ 1].
	start _ anObject identityHash * scale \\ finish + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element _ array at: index) == nil or: [element key == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element _ array at: index) == nil or: [element key == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !


!WeakIdentityKeyDictionary methodsFor: 'private' stamp: 'jmv 2/21/2020 12:33:40'!
scanFor: anObject
	"ar 10/21/2000: The method has been copied to this location to indicate that whenever #scanFor: changes #scanForNil: must be changed in the receiver as well."
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| finish scale start element |
	finish _ array size.
	scale _ finish // (Smalltalk maxIdentityHash + 1).
	scale = 0 ifTrue: [scale _ 1].
	start _ anObject identityHash * scale \\ finish + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element _ array at: index) == nil or: [element key == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element _ array at: index) == nil or: [element key == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !

!WeakIdentityKeyDictionary methodsFor: 'private' stamp: 'jmv 2/21/2020 12:33:43'!
scanForNil: anObject
	"Private. Scan the key array for the first slot containing nil (indicating an empty slot). Answer the index of that slot."

	| finish scale start |
	finish _ array size.
	scale _ finish // (Smalltalk maxIdentityHash + 1).
	scale = 0 ifTrue: [scale _ 1].
	start _ anObject identityHash * scale \\ finish + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | (array at: index) == nil ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | (array at: index) == nil ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !


!IdentitySet methodsFor: 'private' stamp: 'jmv 2/21/2020 12:33:27'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| finish scale start element |
	finish _ array size.
	scale _ finish // (Smalltalk maxIdentityHash + 1).
	scale = 0 ifTrue: [scale _ 1].
	start _ anObject identityHash * scale \\ finish + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element _ array at: index) == nil or: [element == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element _ array at: index) == nil or: [element == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !


!WeakIdentitySet methodsFor: 'private' stamp: 'jmv 2/21/2020 12:33:47'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements"

	| finish scale start element |
	finish _ array size.
	scale _ finish // (Smalltalk maxIdentityHash + 1).
	scale = 0 ifTrue: [scale _ 1].
	start _ anObject identityHash * scale \\ finish + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element _ array at: index) == flag or: [element == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element _ array at: index) == flag or: [element == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !


!IdentitySet reorganize!
('accessing' elementForIdentityHash:)
('private' scanFor:)
!


!WeakIdentitySet reorganize!
('accessing' elementForIdentityHash:)
('private' scanFor:)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
IdentityDictionary allInstancesDo: [ :d | d rehash ].
IdentitySet allInstancesDo: [ :d | d rehash ].
WeakIdentityKeyDictionary allInstancesDo: [ :d | d rehash ].
WeakIdentitySet allInstancesDo: [ :d | d rehash ].!

