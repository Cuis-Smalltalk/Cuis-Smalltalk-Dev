Class {
	#name : 'DamageRecorder',
	#superclass : 'Object',
	#instVars : [
		'drSemaphore',
		'damageByRoot',
		'otherDamage'
	],
	#category : 'Morphic-Support',
	#package : 'Morphic',
	#tag : 'Support'
}

{ #category : 'recording' }
DamageRecorder >> damageReportedFor: aMorph [
	^ self pvtAccessProtect critical: [ damageByRoot at: aMorph ifAbsent: nil ]
]

{ #category : 'recording' }
DamageRecorder >> damageReportedOther [
	"Answer damage reported for no specific morph, for morphs that are not visible, and for morphs that are carried by the Hand.
	Answer might include nils. Skip them."
	| answer possiblyMoreRectsToMerge |
	answer := OrderedCollection new.
	self pvtAccessProtect critical: [
		damageByRoot keysAndValuesDo: [ :m :rects |
			rects do: [ :r |
				(m visible not or: [m owner isNil or: [m isOwnedByHand ]]) ifTrue: [
					answer add: r]]].
		otherDamage do: [ :r | answer add: r ].
	].
	possiblyMoreRectsToMerge := true.
	[possiblyMoreRectsToMerge] whileTrue: [
		possiblyMoreRectsToMerge := false.
		answer withIndexDo: [ :r1 :i |
			r1 ifNotNil: [
				i+1 to: answer size do: [ :j | | r2 |
					r2 := answer at: j.
					r2 ifNotNil: [
						(r1 intersects: r2) ifTrue: [
							r1 updateMerging: r2.
							answer at: j put: nil.
							possiblyMoreRectsToMerge := true ]]]]]].
	^answer
]

{ #category : 'initialization' }
DamageRecorder >> initialize [
	damageByRoot := IdentityDictionary new.
	otherDamage := OrderedCollection new.
]

{ #category : 'private' }
DamageRecorder >> pvtAccessProtect [
	^ drSemaphore ifNil: [drSemaphore := Semaphore forMutualExclusion]
]

{ #category : 'private' }
DamageRecorder >> pvtInnerRecordInvalidRect: requestedRect for: aRootMorph [
	"Record the given rectangle in my damage list, a list of rectangular areas of the display that should be redraw on the next display cycle."

	aRootMorph ifNotNil: [
		(damageByRoot at: aRootMorph
			ifPresent: [ :c | |r|
				r := requestedRect copy. "asIntegerRectangle? encompassing?"
				1 to: c size do: [ :i | |old|
					old := c at: i.
					(r intersects: old) ifTrue: [
						r updateMerging: old.
						c at: i put: nil ]].
				c removeAllSuchThat: [:e | e isNil ].
				c add: r ]
			ifAbsent: [ damageByRoot at: aRootMorph put: (OrderedCollection with: requestedRect copy) ]) ]
		ifNil: [otherDamage add: requestedRect copy].
]

{ #category : 'recording' }
DamageRecorder >> recordInvalidRect: requestedRect for: aMorph [
	"aRootMorph should be #root"

	^ self pvtAccessProtect critical: [
		self pvtInnerRecordInvalidRect: requestedRect for: (aMorph ifNotNil: [aMorph root]) ]
]

{ #category : 'recording' }
DamageRecorder >> reset [
	"Clear the damage list."
	self pvtAccessProtect critical: [
		damageByRoot removeAll.
		otherDamage removeAll. ].
]

{ #category : 'testing' }
DamageRecorder >> updateIsNeeded [
	"Return true if the display needs to be updated."
	^ self pvtAccessProtect critical: [damageByRoot notEmpty or: [otherDamage notEmpty]]
]
