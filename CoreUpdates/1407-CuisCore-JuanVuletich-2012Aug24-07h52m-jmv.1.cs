'From Cuis 4.0 of 21 April 2012 [latest update: #1406] on 24 August 2012 at 8:25:58 am'!
!classDefinition: #WeakIdentitySet category: #'Collections-Weak'!
WeakSet subclass: #WeakIdentitySet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Weak'!
!classDefinition: #WeakIdentitySetTest category: #CollectionTests!
TestCase subclass: #WeakIdentitySetTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CollectionTests'!

!WeakIdentitySet methodsFor: 'private' stamp: 'jmv 8/24/2012 08:02'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements"

	| element hash start finish |
	finish _ array size.
	finish > 4096
		ifTrue: [ hash _ anObject identityHash * (finish // 4096) ]
		ifFalse: [ hash _ anObject identityHash ].
	start _ (hash \\ array size) + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element _ array at: index) == flag or: [element == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element _ array at: index) == flag or: [element == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !


!WeakIdentitySetTest methodsFor: 'testing' stamp: 'jmv 8/24/2012 08:24'!
test
	"
	(also tests WeakSet a bit)
	WeakIdentitySetTest new test
	"
	| ary1 ary2 count w wi |
	ary1 _ { 3@4 . 4@5 }.
	ary2 _ { 13@4 . 14@5 }.
	w _ WeakSet new.
	w addAll: ary1.
	wi _ WeakIdentitySet new.
	wi addAll: ary2.
	
	self assert: w size = 2.
	count _ 0.
	w do: [ :each |
		count _ count + 1.
		self assert: each class == Point ].
	self assert: count = 2.
	self assert: (w includes: ary1 first).
	self assert: (w includes: ary1 second).
	self assert: (w includes: 3@4).
	self assert: (w includes: 4@5).

	self assert: wi size = 2.
	count _ 0.
	wi do: [ :each |
		count _ count + 1.
		self assert: each class == Point ].
	self assert: count = 2.
	self assert: (wi includes: ary2 first).
	self assert: (wi includes: ary2 second).
	self deny: (wi includes: 13@4).
	self deny: (wi includes: 14@5).

	"Now make one element in each disappear"
	ary1 at: 1 put: 9.
	ary2 at: 1 put: 99.
	Smalltalk garbageCollect.

	"A little weird, but yes, elements that disappeared are still counted, but NOT iterated!!"
	self assert: w size = 2.
	count _ 0.
	w do: [ :each |
		count _ count + 1 ].
	self assert: count = 1.
	self deny: (w includes: ary1 first).
	self assert: (w includes: ary1 second).

	self assert: wi size = 2. 	"A little weird, but yes, elements that disappeared are still counted"
	count _ 0.
	wi do: [ :each |
		count _ count + 1 ].
	self assert: count = 1.
	self deny: (wi includes: ary2 first).
	self assert: (wi includes: ary2 second).! !


!WorldState methodsFor: 'initialization' stamp: 'jmv 8/24/2012 08:24'!
initialize

	hands _ #().
	damageRecorder _ DamageRecorder new.
	stepList _ Heap sortBlock: self stepListSortBlock.
	lastStepTime _ 0.
	lastAlarmTime _ 0.
	drawingFailingMorphs _ WeakIdentitySet new.
	pause _ 20.
	lastCycleTime _ Time millisecondClockValue.
	lastCycleHadAnyEvent _ false! !

!WorldState methodsFor: 'errors on draw' stamp: 'jmv 8/24/2012 08:25'!
removeAllKnownFailing
	drawingFailingMorphs _ WeakIdentitySet new.! !


!WeakIdentitySetTest reorganize!
('testing' test)
!


!WeakIdentitySet reorganize!
('private' scanFor:)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
World removeAllKnownFailing!

