'From Cuis7.5 [latest update: #7593] on 19 September 2025 at 4:53:06 pm'!
'Description '!
!provides: 'WeakDictionaries' 1 1!
SystemOrganization addCategory: #WeakDictionaries!


!classDefinition: #WeakKeyDictionary category: #WeakDictionaries!
Dictionary subclass: #WeakKeyDictionary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WeakDictionaries'!
!classDefinition: 'WeakKeyDictionary class' category: #WeakDictionaries!
WeakKeyDictionary class
	instanceVariableNames: ''!

!classDefinition: #WeakIdentityKeyDictionary category: #WeakDictionaries!
WeakKeyDictionary subclass: #WeakIdentityKeyDictionary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WeakDictionaries'!
!classDefinition: 'WeakIdentityKeyDictionary class' category: #WeakDictionaries!
WeakIdentityKeyDictionary class
	instanceVariableNames: ''!

!classDefinition: #WeakValueDictionary category: #WeakDictionaries!
Dictionary subclass: #WeakValueDictionary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WeakDictionaries'!
!classDefinition: 'WeakValueDictionary class' category: #WeakDictionaries!
WeakValueDictionary class
	instanceVariableNames: ''!

!classDefinition: #WeakKeyAssociation category: #WeakDictionaries!
Association subclass: #WeakKeyAssociation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WeakDictionaries'!
!classDefinition: 'WeakKeyAssociation class' category: #WeakDictionaries!
WeakKeyAssociation class
	instanceVariableNames: ''!

!classDefinition: #WeakValueAssociation category: #WeakDictionaries!
LookupKey weakSubclass: #WeakValueAssociation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WeakDictionaries'!
!classDefinition: 'WeakValueAssociation class' category: #WeakDictionaries!
WeakValueAssociation class
	instanceVariableNames: ''!


!WeakKeyDictionary commentStamp: '<historical>' prior: 0!
I am a dictionary holding only weakly on my keys. This is a bit dangerous since at any time my keys can go away. Clients are responsible to register my instances by WeakArray such that the appropriate actions can be taken upon loss of any keys.
!

!WeakIdentityKeyDictionary commentStamp: '<historical>' prior: 0!
This class represents an identity dictionary with weak keys.!

!WeakValueDictionary commentStamp: '<historical>' prior: 0!
I am a dictionary holding only weakly on my values. Clients may expect to get a nil value for any object they request.!

!WeakKeyAssociation commentStamp: '<historical>' prior: 0!
I am an association holding only weakly on my key.!

!WeakValueAssociation commentStamp: '<historical>' prior: 0!
I am a lookup key (acting like an association but) holding only weakly on my value.!

!WeakKeyDictionary methodsFor: 'finalization' stamp: 'ar 10/21/2000 20:00'!
finalizeValues
	"remove all nil keys and rehash the receiver afterwards"
	| assoc |
	1 to: array size do:[:i|
		assoc := array at: i.
		(assoc notNil and:[assoc key == nil]) ifTrue:[array at: i put: nil].
	].
	self rehash.! !

!WeakKeyDictionary methodsFor: 'finalization' stamp: 'ar 10/21/2000 20:01'!
finalizeValues: finiObjects
	"Remove all associations with key == nil and value is in finiObjects.
	This method is folded with #rehash for efficiency."
	| oldArray assoc newIndex |
	oldArray := array.
	array := Array new: oldArray size.
	tally := 0.
	1 to: array size do:[:i|
		assoc := oldArray at: i.
		assoc ifNotNil:[
			(assoc key == nil and:[finiObjects includes: assoc value]) ifFalse:[
				newIndex := self scanForNil: assoc key.
				self atNewIndex: newIndex put: assoc].
		].
	].! !

!WeakKeyDictionary methodsFor: 'accessing' stamp: 'JMM 12/12/2024 09:19:24'!
associationClass

	^WeakKeyAssociation! !

!WeakKeyDictionary methodsFor: 'accessing' stamp: 'JMM 12/12/2024 09:08:53'!
at: key put: anObject 
	"Set the value at key to be anObject.  If key is not found, create a new
	entry for key and set is value to anObject. Answer anObject."
	
	key ifNil: [ ^anObject ].
	^super at: key put: anObject! !

!WeakKeyDictionary methodsFor: 'accessing' stamp: 'ar 2/11/2001 02:21'!
keysDo: aBlock 
	"Evaluate aBlock for each of the receiver's keys."
	self associationsDo: [:association | 
		association key ifNotNil:[aBlock value: association key]].! !

!WeakKeyDictionary methodsFor: 'private' stamp: 'ar 10/21/2000 19:58'!
fixCollisionsFrom: oldIndex
	"The element at index has been removed and replaced by nil."
	self rehash. "Do it the hard way - we may have any number of nil keys and #rehash deals with them"! !

!WeakKeyDictionary methodsFor: 'private' stamp: 'ar 10/21/2000 19:56'!
rehash
	"Rehash the receiver. Reimplemented to allow for multiple nil keys"
	| oldArray assoc newIndex |
	oldArray := array.
	array := Array new: oldArray size.
	tally := 0.
	1 to: array size do:[:i|
		assoc := oldArray at: i.
		assoc ifNotNil:[
			newIndex := self scanForNil: assoc key.
			self atNewIndex: newIndex put: assoc.
		].
	].! !

!WeakKeyDictionary methodsFor: 'private' stamp: 'ar 10/21/2000 19:46'!
scanFor: anObject
	"ar 10/21/2000: The method has been copied to this location to indicate that whenever #scanFor: changes #scanForNil: must be changed in the receiver as well."
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

!WeakKeyDictionary methodsFor: 'private' stamp: 'ar 10/21/2000 19:46'!
scanForNil: anObject
	"Private. Scan the key array for the first slot containing nil (indicating an empty slot). Answer the index of that slot."
	| start finish |
	start := (anObject hash \\ array size) + 1.
	finish := array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | (array at: index) == nil ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | (array at: index) == nil ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !

!WeakKeyDictionary methodsFor: 'adding' stamp: 'ar 3/21/98 16:00'!
add: anAssociation
	self at: anAssociation key put: anAssociation value.
	^ anAssociation! !

!WeakKeyDictionary methodsFor: 'public' stamp: 'ul 2/4/2010 00:59'!
slowSize
	"Careful!! Answer the maximum amount
	of elements in the receiver, not the
	exact amount"

	| count |
	count := 0.
	1 to: array size do: [ :index |
		(array at: index) ifNotNil: [ :object |
			object key ifNotNil: [
				count := count + 1 ] ] ].
	^count! !

!WeakIdentityKeyDictionary methodsFor: 'private' stamp: 'jmv 2/21/2020 12:33:40'!
scanFor: anObject
	"ar 10/21/2000: The method has been copied to this location to indicate that whenever #scanFor: changes #scanForNil: must be changed in the receiver as well."
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

!WeakIdentityKeyDictionary methodsFor: 'private' stamp: 'jmv 2/21/2020 12:33:43'!
scanForNil: anObject
	"Private. Scan the key array for the first slot containing nil (indicating an empty slot). Answer the index of that slot."

	| finish scale start |
	finish := array size.
	scale := finish // (Smalltalk maxIdentityHash + 1).
	scale = 0 ifTrue: [scale := 1].
	start := anObject identityHash * scale \\ finish + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | (array at: index) == nil ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | (array at: index) == nil ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !

!WeakValueDictionary methodsFor: 'adding' stamp: 'ar 3/21/98 16:02'!
add: anAssociation
	self at: anAssociation key put: anAssociation value.
	^ anAssociation! !

!WeakValueDictionary methodsFor: 'accessing' stamp: 'JMM 12/12/2024 09:19:43'!
associationClass

	^WeakValueAssociation! !

!WeakValueDictionary methodsFor: 'accessing' stamp: 'jmv 7/23/2009 15:27'!
at: key ifAbsentOrNil: aBlock
	"In a WeakValueDictionary, normally nil values (i.e. collected values) are considered the same as if the key/value was never added. Hence, this convenience method."
	
	| assoc |
	assoc := array at: (self findElementOrNil: key).
	assoc ifNil: [ ^ aBlock value ].
	^ assoc value ifNil: [ aBlock value ]! !

!WeakValueDictionary methodsFor: 'accessing' stamp: 'jmv 7/23/2009 15:28'!
at: key ifAbsentOrNilPut: aBlock 
	"Return the value at the given key.
	If key is not included in the receiver store the result
	of evaluating aBlock as new value."
	^self at: key ifAbsentOrNil: [self at: key put: aBlock value]! !

!WeakKeyAssociation methodsFor: 'comparing' stamp: 'ar 3/21/98 15:45'!
< aLookupKey 
	"Refer to the comment in Magnitude|<."

	^self key < aLookupKey key! !

!WeakKeyAssociation methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:01:54'!
= aLookupKey
	self == aLookupKey ifTrue: [ ^ true ].

	self species == aLookupKey species
		ifFalse: [ ^ false ].

	^self key = aLookupKey key! !

!WeakKeyAssociation methodsFor: 'comparing' stamp: 'ar 3/21/98 15:46'!
hash
	"Hash is reimplemented because = is implemented."

	^self key hash! !

!WeakKeyAssociation methodsFor: 'printing' stamp: 'ar 3/21/98 15:53'!
storeOn: aStream
	aStream 
		nextPut: $(;
		nextPutAll: self class name;
		nextPutAll:' key: '.
	self key storeOn: aStream.
	aStream nextPutAll: ' value: '.
	self value storeOn: aStream.
	aStream nextPut: $)! !

!WeakKeyAssociation methodsFor: 'accessing' stamp: 'jmv 3/2/2010 16:43'!
key
	^key ifNotNil: [ key at: 1]! !

!WeakKeyAssociation methodsFor: 'accessing' stamp: 'ar 3/21/98 15:45'!
key: aKey
	key := WeakArray with: aKey! !

!WeakKeyAssociation methodsFor: 'accessing' stamp: 'raok 11/29/2002 14:49'!
key: aKey value: anObject
	key := WeakArray with: aKey.
	value := anObject.! !

!WeakValueAssociation methodsFor: 'accessing' stamp: 'r++ 5/27/2000 18:11'!
key: aKey value: anObject 
	"Store the arguments as the variables of the receiver."

	key := aKey.
	self value: anObject! !

!WeakValueAssociation methodsFor: 'accessing' stamp: 'r++ 5/27/2000 18:08'!
value
	^self at: 1! !

!WeakValueAssociation methodsFor: 'accessing' stamp: 'r++ 5/27/2000 18:08'!
value: anObject 
	"Store the argument, anObject, as the value of the receiver."

	self at: 1 put: anObject! !

!WeakValueAssociation class methodsFor: 'instance creation' stamp: 'r++ 5/27/2000 18:07'!
key: anObject value: bObject
	^ self new key: anObject value: bObject! !

!WeakValueAssociation class methodsFor: 'as yet unclassified' stamp: 'r++ 5/27/2000 18:12'!
new
	^ self new: 1! !
