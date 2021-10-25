'From Cuis 5.0 [latest update: #4942] on 25 October 2021 at 4:18:19 pm'!

!Morph methodsFor: 'accessing' stamp: 'jmv 10/25/2021 14:44:19'!
location
	^ GeometryTransformation identity! !

!Morph methodsFor: 'drawing' stamp: 'jmv 10/25/2021 14:42:45'!
imageForm: extentOrNil depth: depth
	"Scale as needed. Draw unrotated."

	| prevNotVisibleFlag bounds extent location answer auxCanvas |
	"Position and scale us in order to fill required extent, but disregard any rotation.
	Do it without triggering any invalidation at all."
	prevNotVisibleFlag _ self privateFlagAt: 3.
	[
		bounds _ self findFullBoundsInOwner.
		extentOrNil
			ifNotNil: [ | scale |
				extent _ extentOrNil.
				scale _ extent x asFloat / bounds width min: extent y asFloat / bounds height.
				location _ AffineTransformation withScale: scale position: bounds origin negated +1 * scale ]
			ifNil: [
				extent _ bounds extent.
				location _ MorphicTranslation withTranslation: bounds origin negated ].
		answer _ Form extent: extent depth: 32.
		"Ask for a Canvas with subpixels so it can also handle translucent target"
		auxCanvas _ VectorCanvas onFormWithSubPixelAntiAliasing: answer.
		auxCanvas geometryTransformation: location.
		"But disable subpixel anti aliasing, as we are answering a Form, and therefore can't assume a Display geometry."
		auxCanvas engine disableSubPixelSampling.
		depth = 32 ifFalse: [
			"Only 32 bpp can hold translucent anti aliasing over transparent background"
			answer fillColor: Color veryLightGray ].
		self privateFlagAt: 3 put: false.
		auxCanvas fullDraw: self.
	] ensure: [
		self privateFlagAt: 3 put: prevNotVisibleFlag ].
	^answer asFormOfDepth: depth.! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 10/25/2021 14:38:26'!
computeDamage: aWorldMorph repair: aDamageRecorder rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage
	"Iterate front to back while computing damage to actually repair for each morph, and for world background.
	Answer uncoveredDamage, the areas where World background needs to be redrawn."

	| uncoveredDamage additionalUncoveredDamage morph morphDamage morphFullBounds |
	uncoveredDamage _ aDamageRecorder damageReportedOther.
	additionalUncoveredDamage _ OrderedCollection new.

	"Iterate from front to back"
	1 to: rootMorphs size do: [ :i |
		morph _ rootMorphs at: i.
		morphFullBounds _ morph displayFullBounds.
		morphDamage _ nil.
		"Reported damage can be ignored if the area fully covered by another morph above us."
		(aDamageRecorder damageReportedFor: morph) ifNotNil: [ :r | | wasFullyCovered |
			wasFullyCovered _ false.
			1 to: i-1 do: [ :j |
				wasFullyCovered _ wasFullyCovered or: [ (rootMorphs at: j) fullyCovers: r ]].
			wasFullyCovered ifFalse: [
				uncoveredDamage add: r ]].
		"Any uncovered damage this morph may overlap must be redrawn by it."
		morphFullBounds ifNotNil: [
			uncoveredDamage withIndexDo: [ :r :j | | intersection |
				r ifNotNil: [
					intersection _ r intersect: morphFullBounds.
					intersection hasPositiveExtent ifTrue: [
						r = intersection ifTrue: [ uncoveredDamage at: j put: nil ].
						morphDamage
							ifNil: [ morphDamage _ intersection ]
							ifNotNil: [ morphDamage updateMerging: intersection ]]]]].
		rootMorphsDamage at: i put: morphDamage.
		uncoveredDamage add: morphDamage.
		"Whatever we cover completely is no longer uncoveredDamage"
		uncoveredDamage withIndexDo: [ :r :ri |
			r ifNotNil: [
				morph
					on: r
					ifCovered: [uncoveredDamage at: ri put: nil]
						uncoveredPartsDo: [ :r2 | additionalUncoveredDamage add: r2 ]
					else: []]].
		uncoveredDamage addAll: additionalUncoveredDamage.
		additionalUncoveredDamage removeAll.
	].

	"Remove redundant rectangles"
	1 to: uncoveredDamage size do: [ :i1 |
		(uncoveredDamage at: i1) ifNotNil: [ :r1 |
			1 to: uncoveredDamage size do: [ :i2 |
				i1 = i2 ifFalse: [
					(uncoveredDamage at: i2) ifNotNil: [ :r2 |
						(r1 containsRect: r2)
							ifTrue: [ uncoveredDamage at: i2 put: nil ]]]]]].

	"Answer any yet uncovered areas. World background should be drawn on them."
	^ uncoveredDamage select: [ :r | r notNil ].! !

