'From Cuis 5.0 [latest update: #4779] on 23 August 2021 at 7:37:18 pm'!
!classDefinition: #DamageRecorder category: #'Morphic-Support'!
Object subclass: #DamageRecorder
	instanceVariableNames: 'drSemaphore damageByRoot otherDamage '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!DamageRecorder methodsFor: 'initialization' stamp: 'jmv 8/23/2021 18:44:22'!
initialize
	damageByRoot _ IdentityDictionary new.
	otherDamage _ OrderedCollection new.! !

!DamageRecorder methodsFor: 'recording' stamp: 'jmv 8/23/2021 18:51:09'!
damageReportedOther
	"Answer damage reported for no specific morph, for morphs that are not visible, and for morphs that are carried by the Hand."
	| answer |
	answer _ OrderedCollection new.
	self pvtAccessProtect critical: [
		damageByRoot keysAndValuesDo: [ :m :r |
			(m visible not or: [m owner isNil or: [m owner is: #HandMorph]]) ifTrue: [
				answer add: r]].
		otherDamage do: [ :r | answer add: r ].
	].
	^answer! !

!DamageRecorder methodsFor: 'recording' stamp: 'jmv 8/23/2021 19:01:20'!
reset
	"Clear the damage list."
	self pvtAccessProtect critical: [
		damageByRoot removeAll.
		otherDamage removeAll. ].! !

!DamageRecorder methodsFor: 'testing' stamp: 'jmv 8/23/2021 18:48:47'!
updateIsNeeded
	"Return true if the display needs to be updated."
	^ self pvtAccessProtect critical: [damageByRoot notEmpty or: [otherDamage notEmpty]]! !

!DamageRecorder methodsFor: 'private' stamp: 'jmv 8/23/2021 18:49:49'!
pvtInnerRecordInvalidRect: requestedRect for: aRootMorph
	"Record the given rectangle in my damage list, a list of rectangular areas of the display that should be redraw on the next display cycle."
	| newRect |
	newRect _ requestedRect encompassingIntegerRectangle.
	aRootMorph ifNotNil: [
		damageByRoot at: aRootMorph put: 
			(damageByRoot at: aRootMorph
				ifPresent: [:r | r quickMerge: newRect]
				ifAbsent: [newRect]) ]
			ifNil: [otherDamage add: newRect].! !

!classDefinition: #DamageRecorder category: #'Morphic-Support'!
Object subclass: #DamageRecorder
	instanceVariableNames: 'drSemaphore damageByRoot otherDamage'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
DamageRecorder allInstancesDo: [ :dr | dr instVarNamed: 'otherDamage' put: OrderedCollection new ]!

