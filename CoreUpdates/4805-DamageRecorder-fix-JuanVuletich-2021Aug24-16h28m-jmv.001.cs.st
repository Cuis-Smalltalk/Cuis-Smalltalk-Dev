'From Cuis 5.0 [latest update: #4804] on 24 August 2021 at 4:38:23 pm'!

!Rectangle methodsFor: 'rectangle functions' stamp: 'jmv 8/24/2021 16:36:44'!
updateMerging: aRectangle
	"Modify receiver"

	origin _ origin min: aRectangle origin.
	corner _ corner max: aRectangle corner.! !


!DamageRecorder methodsFor: 'recording' stamp: 'jmv 8/24/2021 16:37:48'!
damageReportedOther
	"Answer damage reported for no specific morph, for morphs that are not visible, and for morphs that are carried by the Hand."
	| answer possiblyMoreRectsToMerge |
	answer _ OrderedCollection new.
	self pvtAccessProtect critical: [
		damageByRoot keysAndValuesDo: [ :m :r |
			(m visible not or: [m owner isNil or: [m owner is: #HandMorph]]) ifTrue: [
				answer add: r]].
		otherDamage do: [ :r | answer add: r ].
	].
	possiblyMoreRectsToMerge _ true.
	[possiblyMoreRectsToMerge] whileTrue: [
		possiblyMoreRectsToMerge _ false.
		answer withIndexDo: [ :r1 :i |
			r1 ifNotNil: [
				i+1 to: answer size do: [ :j | | r2 |
					r2 _ answer at: j.
					r2 ifNotNil: [
						(r1 intersects: r2) ifTrue: [
							r1 updateMerging: r2.
							answer at: j put: nil.
							possiblyMoreRectsToMerge _ true ]]]]]].
	answer _ answer select: [ :r | r notNil ].
	^answer! !

