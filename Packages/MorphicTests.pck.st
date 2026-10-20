'From Cuis7.9 [latest update: #8145] on 24 August 2026 at 11:12:19 am'!
'Description '!
!provides: 'MorphicTests' '1.0.0'!
SystemOrganization addCategory: #MorphicTests!


!classDefinition: #AddMorphTest category: #MorphicTests!
TestCase subclass: #AddMorphTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MorphicTests'!
!classDefinition: 'AddMorphTest class' category: #MorphicTests!
AddMorphTest class
	instanceVariableNames: ''!

!classDefinition: #AffineTransformationTest category: #MorphicTests!
TestCase subclass: #AffineTransformationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MorphicTests'!
!classDefinition: 'AffineTransformationTest class' category: #MorphicTests!
AffineTransformationTest class
	instanceVariableNames: ''!

!classDefinition: #LayoutMorphTest category: #MorphicTests!
TestCase subclass: #LayoutMorphTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MorphicTests'!
!classDefinition: 'LayoutMorphTest class' category: #MorphicTests!
LayoutMorphTest class
	instanceVariableNames: ''!

!classDefinition: #MorphicLocationTest category: #MorphicTests!
TestCase subclass: #MorphicLocationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MorphicTests'!
!classDefinition: 'MorphicLocationTest class' category: #MorphicTests!
MorphicLocationTest class
	instanceVariableNames: ''!

!classDefinition: #MorphicTranslationTest category: #MorphicTests!
TestCase subclass: #MorphicTranslationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MorphicTests'!
!classDefinition: 'MorphicTranslationTest class' category: #MorphicTests!
MorphicTranslationTest class
	instanceVariableNames: ''!

!classDefinition: #WorldTest category: #MorphicTests!
TestCase subclass: #WorldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MorphicTests'!
!classDefinition: 'WorldTest class' category: #MorphicTests!
WorldTest class
	instanceVariableNames: ''!


!AffineTransformationTest commentStamp: '<historical>' prior: 0!
Tests for AffineTransformation!

!MorphicLocationTest commentStamp: 'jmv 1/14/2015 14:32' prior: 0!
Tests for composition of AffineTransformation and MorphicTranslation!

!MorphicTranslationTest commentStamp: '<historical>' prior: 0!
Tests for MorphicTranslation!

!AddMorphTest methodsFor: 'tests - addMorph:' stamp: 'JEC 3/Oct/2025, 6:09:52 pm (UTC)'!
test001_CanAddAMorph

	| owner newSubmorph |
	owner := Morph new.
	newSubmorph := Morph new.
	
	owner addMorph: newSubmorph.
	
	self assert: owner submorphs equals: { newSubmorph }! !

!AddMorphTest methodsFor: 'tests - addMorph:' stamp: 'JEC 3/Oct/2025, 6:09:57 pm (UTC)'!
test002_AddingANewSubmorphInFrontOfTheRest

	| owner existingSubmorph newSubmorph |
	owner := Morph new.
	existingSubmorph := Morph new.
	newSubmorph := Morph new.
	owner addMorph: existingSubmorph.

	owner addMorph: newSubmorph.

	self assert: owner submorphs equals: { newSubmorph . existingSubmorph }. ! !

!AddMorphTest methodsFor: 'tests - addMorph:' stamp: 'JEC 3/Oct/2025, 3:15:21 pm (UTC)'!
test003_MorphCannotBeItsOwnSubmorph

	| owner |
	owner := Morph new.
	
	self
		should: [ owner addMorph: owner ] 
		raise: Error
		withMessageText: 'can not be own submorph'.
	
	self assert: owner submorphs isEmpty! !

!AddMorphTest methodsFor: 'tests - addMorph:' stamp: 'JEC 3/Oct/2025, 6:10:14 pm (UTC)'!
test004_AddingAnExistingSubmorph_RemainsASubmorph

	| owner existingSubmorph |
	owner := Morph new.
	existingSubmorph := Morph new.
	owner addMorph: existingSubmorph.

	owner addMorph: existingSubmorph.
	
	self assert: owner submorphs equals: { existingSubmorph }.! !

!AddMorphTest methodsFor: 'tests - addMorph:' stamp: 'JEC 3/Oct/2025, 6:10:19 pm (UTC)'!
test005_AddingAnExistingSubmorphMoreThanOnce_PlacesItInFront

	| owner submorph1 submorph2 |
	owner := Morph new.
	submorph1 := Morph new.
	submorph2 := Morph new.
	owner addMorph: submorph1.
	owner addMorph: submorph2.
	
	owner addMorph: submorph1.

	self assert: owner submorphs equals: { submorph1 . submorph2 }. ! !

!AddMorphTest methodsFor: 'tests - addMorphBack:' stamp: 'JEC 3/Oct/2025, 6:10:33 pm (UTC)'!
test006_CanAddAMorphBack_WhenNotHavingSubmorphs

	| owner newSubmorph |
	owner := Morph new.
	newSubmorph := Morph new.
	
	owner addMorphBack: newSubmorph.
	
	self assert: owner submorphs equals: { newSubmorph }! !

!AddMorphTest methodsFor: 'tests - addMorphBack:' stamp: 'JEC 3/Oct/2025, 6:10:40 pm (UTC)'!
test007_AddingANewSubmorphBack_PlaceItAtTheEndOfSubmorphs

	| owner existingSubmorph newSubmorph |
	owner := Morph new.
	existingSubmorph := Morph new.
	newSubmorph := Morph new.
	owner addMorphBack: existingSubmorph.
	
	owner addMorphBack: newSubmorph.

	self assert: owner submorphs equals: { existingSubmorph . newSubmorph }! !

!AddMorphTest methodsFor: 'tests - addMorphBack:' stamp: 'JEC 3/Oct/2025, 6:10:47 pm (UTC)'!
test008_AddAnExistingSubmorphToTheBack_PlaceTheSubmorphAtEndOfSubmorphs

	| owner submorphToMove submorph |
	owner := Morph new.
	submorphToMove := Morph new.
	submorph := Morph new.
	owner addMorphBack: submorphToMove.
	owner addMorphBack: submorph.
	
	owner addMorphBack: submorphToMove.

	self assert: owner submorphs equals: { submorph . submorphToMove }! !

!AddMorphTest methodsFor: 'tests - addMorphFront:' stamp: 'JEC 3/Oct/2025, 6:11:06 pm (UTC)'!
test009_CanAddAMorphFront_WhenNotHavingSubmorphs

	| owner newSubmorph |
	owner := Morph new.
	newSubmorph := Morph new.
	
	owner addMorphFront: newSubmorph.
	
	self assert: owner submorphs equals: { newSubmorph }! !

!AddMorphTest methodsFor: 'tests - addMorphFront:' stamp: 'JEC 3/Oct/2025, 6:11:11 pm (UTC)'!
test010_AddingANewSubmorphFront_PlaceItAtTheStartOfSubmorphs

	| owner existingSubmorph newSubmorph |
	owner := Morph new.
	existingSubmorph := Morph new.
	newSubmorph := Morph new.
	owner addMorphFront: existingSubmorph.
	
	owner addMorphFront: newSubmorph.

	self assert: owner submorphs equals: { newSubmorph . existingSubmorph }! !

!AddMorphTest methodsFor: 'tests - addMorphFront:' stamp: 'JEC 3/Oct/2025, 6:11:24 pm (UTC)'!
test011_AddSubmorphFront_PlaceSubmorphsAtBeginningOfSubmorphs

	| owner submorphToMove anotherSubmorph |
	owner := Morph new.
	submorphToMove := Morph new.
	anotherSubmorph := Morph new.
	owner addMorphFront: submorphToMove.
	owner addMorphFront: anotherSubmorph.
	
	owner addMorphFront: submorphToMove.

	self assert: owner submorphs equals: { submorphToMove . anotherSubmorph }. ! !

!AddMorphTest methodsFor: 'tests - addMorph:behind:' stamp: 'JEC 3/Oct/2025, 6:11:41 pm (UTC)'!
test012_CanAddANewSubmorphBehindAnExistingSubmorph

	| owner existingSubmorph newSubmorph |
	owner := Morph new.
	existingSubmorph := Morph new.
	newSubmorph := Morph new.
	owner addMorph: existingSubmorph.
	
	owner addMorph: newSubmorph behind: existingSubmorph.
	
	self assert: owner submorphs equals: { existingSubmorph . newSubmorph }! !

!AddMorphTest methodsFor: 'tests - addMorph:behind:' stamp: 'JEC 3/Oct/2025, 6:12:09 pm (UTC)'!
test013_WhenASubmorphIsBehindAnother_AddingItAgainKeepThemInSamePlaces

	| owner submorph1 submorph2 |
	owner := Morph new.
	submorph1 := Morph new.
	submorph2 := Morph new.
	owner addMorph: submorph1.
	owner addMorph: submorph2 behind: submorph1.
	
	owner addMorph: submorph2 behind: submorph1.

	self assert: owner submorphs equals: { submorph1 . submorph2 }.! !

!AddMorphTest methodsFor: 'tests - addMorph:behind:' stamp: 'JEC 3/Oct/2025, 6:12:17 pm (UTC)'!
test014_CanAddANewSubmorphBetweenExistingsSubmorphs

	| owner submorph1 submorph2 newSubmorph |
	owner := Morph new.
	submorph1 := Morph new.
	submorph2 := Morph new.
	newSubmorph := Morph new.
	owner addMorph: submorph2.
	owner addMorph: submorph1.

	owner addMorph: newSubmorph behind: submorph1.

	self assert: owner submorphs equals: { submorph1 . newSubmorph . submorph2 }.! !

!AddMorphTest methodsFor: 'tests - addMorph:behind:' stamp: 'JEC 3/Oct/2025, 4:56:17 pm (UTC)'!
test015_CannotAddMorphBehindNonSubmorph

	| owner anotherMorph |
	owner := Morph new.
	anotherMorph := Morph new.

	self
		should: [ owner addMorph: anotherMorph behind: Morph new ]
		raise: Error.
		
	self assert: owner submorphs isEmpty! !

!AddMorphTest methodsFor: 'tests - addMorph:behind:' stamp: 'JEC 3/Oct/2025, 6:12:23 pm (UTC)'!
test016_CanMoveASubmorphBehindAnother

	| owner submorph submorphToMove |
	owner := Morph new.
	submorph := Morph new.
	submorphToMove := Morph new.
	owner addMorph: submorph.
	owner addMorph: submorphToMove.

	owner addMorph: submorphToMove behind: submorph.

	self assert: owner submorphs equals: { submorph . submorphToMove }.! !

!AddMorphTest methodsFor: 'tests - addMorph:inFrontOf:' stamp: 'JEC 3/Oct/2025, 6:12:29 pm (UTC)'!
test017_CanAddANewSubmorphInFrontOfAnExistingSubmorph

	| owner existingSubmorph newSubmorph |
	owner := Morph new.
	existingSubmorph := Morph new.
	newSubmorph := Morph new.
	owner addMorph: existingSubmorph.
	
	owner addMorph: newSubmorph inFrontOf: existingSubmorph.
	
	self assert: owner submorphs equals: { newSubmorph . existingSubmorph }.! !

!AddMorphTest methodsFor: 'tests - addMorph:inFrontOf:' stamp: 'JEC 3/Oct/2025, 6:13:09 pm (UTC)'!
test018_WhenASubmorphIsAfterAnother_AddingItAgainKeepThemInSamePlaces

	| owner submorph1 submorph2 |
	owner := Morph new.
	submorph1 := Morph new.
	submorph2 := Morph new.
	owner addMorph: submorph1.
	owner addMorph: submorph2 inFrontOf: submorph1.
	
	owner addMorph: submorph2 inFrontOf: submorph1.
	
	self assert: owner submorphs equals: { submorph2 . submorph1 }! !

!AddMorphTest methodsFor: 'tests - addMorph:inFrontOf:' stamp: 'JEC 3/Oct/2025, 6:13:13 pm (UTC)'!
test019_CanAddANewSubmorphBetweenExistingsSubmorphs

	| owner submorph1 submorph2 newSubmorph |
	owner := Morph new.
	submorph1 := Morph new.
	submorph2 := Morph new.
	newSubmorph := Morph new.
	owner addMorph: submorph1.
	owner addMorph: submorph2.

	owner addMorph: newSubmorph inFrontOf: submorph1.

	self assert: owner submorphs equals: { submorph2 . newSubmorph . submorph1 }! !

!AddMorphTest methodsFor: 'tests - addMorph:inFrontOf:' stamp: 'JEC 3/Oct/2025, 5:00:33 pm (UTC)'!
test020_CannotAddMorphInFrontOfNonSubmorph

	| owner newSubmorph |
	owner := Morph new.
	newSubmorph := Morph new.

	self
		should: [ owner addMorph: newSubmorph inFrontOf: Morph new ]
		raise: Error.
	
	self assert: owner submorphs isEmpty.! !

!AddMorphTest methodsFor: 'tests - addMorph:inFrontOf:' stamp: 'JEC 3/Oct/2025, 6:13:19 pm (UTC)'!
test021_CanMoveASubmorphInFrontOfAnother

	| owner submorphToMove submorph |
	owner := Morph new.
	submorphToMove := Morph new.
	submorph := Morph new.
	owner addMorph: submorphToMove.
	owner addMorph: submorph.

	owner addMorph: submorphToMove inFrontOf: submorph.

	self assert: owner submorphs equals: { submorphToMove  . submorph }! !

!AffineTransformationTest methodsFor: 'testing' stamp: 'jmv 14/Jan/2015, 7:11:00 pm (UTC)'!
testComposition
	"
	AffineTransformationTest new testComposition
	"
	| composition inner outer |

	outer _ AffineTransformation withTranslation: 3@5.
	inner _ AffineTransformation withRadians: 0.3.
	composition _ outer composedWith: inner.
	self assert: composition translation = outer translation.
	self assert: (outer externalizePosition: (inner externalizePosition: 3@4)) = (composition externalizePosition: 3@4).
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: (inner internalizePosition: (outer internalizePosition: 3@4)) = (composition internalizePosition: 3@4).
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).


	outer _ AffineTransformation withRadians: 0.3.
	inner _ AffineTransformation withTranslation: 3@5.
	composition _ outer composedWith: inner.
	self assert: composition radians = outer radians.
	self assert: ((outer externalizePosition: (inner externalizePosition: 3@4)) - (composition externalizePosition: 3@4)) r < 0.0001.
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: ((inner internalizePosition: (outer internalizePosition: 3@4)) - (composition internalizePosition: 3@4)) r < 0.0001.
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).! !

!AffineTransformationTest methodsFor: 'testing' stamp: 'len 9/May/2022, 9:57:24 pm (UTC)'!
testDisplayBounds
	"
	AffineTransformationTest new testDisplayBounds
	"
	self assert: ((AffineTransformation withRadians: 0.3) externalBoundingRectOf: (10@10 extent: 20@30))
			encompassingIntegerRectangle = (-3@12 corner: 26@48).
	self assert: (AffineTransformation new externalBoundingRectOf: (-2@ 2 extent: 10@10))
		encompassingIntegerRectangle = (-2@2 corner: 8@12).
	self assert: (AffineTransformation new externalBoundingRectOf: (-12@ 12 extent: 10@10))
		encompassingIntegerRectangle = (-12@12 corner: -2@22).
	self assert: ((AffineTransformation withTranslation: 2) externalBoundingRectOf: (-4@ 2  extent: 10@10))
		encompassingIntegerRectangle = (-2@4 corner: 8@14).
	self assert: ((AffineTransformation withTranslation: -4) externalBoundingRectOf: (2@ 2  extent: 10@10))
		encompassingIntegerRectangle = (-2@ -2 corner: 8@8).
	self assert: ((AffineTransformation withTranslation: 2) externalBoundingRectOf: (-14@ 2  extent: 10@10))
		encompassingIntegerRectangle = (-12@4 corner: -2@14).
	self assert: ((AffineTransformation withTranslation: 4) externalBoundingRectOf: (-12@ 2  extent: 10@10))
		encompassingIntegerRectangle = (-8@6 corner: 2@16).
	self assert: ((AffineTransformation withTranslation: -4) externalBoundingRectOf: (12@ 2  extent: 10@10))
		encompassingIntegerRectangle = (8 @ -2 corner: 18@8).
	self assert: ((AffineTransformation withTranslation: -2) externalBoundingRectOf: (4@ 2  extent: 10@10))
		encompassingIntegerRectangle = (2@0 corner: 12@10).
	self assert: ((AffineTransformation withTranslation: 4) externalBoundingRectOf: (-2@ 2  extent: 10@10))
		encompassingIntegerRectangle = (2@6 corner: 12@16).! !

!AffineTransformationTest methodsFor: 'testing' stamp: 'len 9/May/2022, 9:56:56 pm (UTC)'!
testFloatInverseTransform
	"
	AffineTransformationTest new testFloatInverseTransform
	"
	self assert: (AffineTransformation new inverseTransform: (-2@ 2)) = (-2@2).
	self assert: ((AffineTransformation withTranslation: 2) inverseTransform: (-4@ 2)) = (-6@0).
	self assert: ((AffineTransformation withTranslation: 4) inverseTransform: (-2@ 2)) = (-6@ -2).
	self assert: ((AffineTransformation withTranslation: -2) inverseTransform: (4@ 2)) = (6@4).
	self assert: ((AffineTransformation withTranslation: -4) inverseTransform: (2@ 2)) = (6@6)! !

!AffineTransformationTest methodsFor: 'testing' stamp: 'len 9/May/2022, 9:57:03 pm (UTC)'!
testFloatTransform
	"
	AffineTransformationTest new testFloatTransform
	"
	self assert: (AffineTransformation new transform: (-2@ 2)) = (-2@2).
	self assert: ((AffineTransformation withTranslation: 2) transform: (-4@ 2)) = (-2@4).
	self assert: ((AffineTransformation withTranslation: 4) transform: (-2@ 2)) = (2@6).
	self assert: ((AffineTransformation withTranslation: -2) transform: (4@ 2)) = (2@0).
	self assert: ((AffineTransformation withTranslation: -4) transform: (2@ 2)) = (-2@ -2)! !

!AffineTransformationTest methodsFor: 'testing' stamp: 'jmv 14/Jan/2015, 7:12:00 pm (UTC)'!
testInverseTransformation
	"
	AffineTransformationTest new testInverseTransformation
	"
	| forward inverse |

	forward _ AffineTransformation withTranslation: 3@5.
	inverse _ forward inverseTransformation.
	
	self assert: inverse translation = forward translation negated.
	self assert: (inverse externalizePosition: 3@4) = (forward internalizePosition: 3@4).
	self assert: (inverse externalizeDelta: 3@4) = (forward internalizeDelta: 3@4).
	self assert: (inverse externalizeScalar: 7) = (forward internalizeScalar: 7).
	self assert: (inverse internalizePosition: 3@4) = (forward externalizePosition: 3@4).
	self assert: (inverse internalizeDelta: 3@4) = (forward externalizeDelta: 3@4).
	self assert: (inverse internalizeScalar: 7) = (forward externalizeScalar: 7).


	forward _ AffineTransformation withRadians: 0.25.
	inverse _ forward inverseTransformation.

	self assert: inverse radians = forward radians negated.
	self assert: ((inverse externalizePosition: 3@4) - (forward internalizePosition: 3@4)) r < 0.0001.
	self assert: ((inverse externalizeDelta: 3@4) - (forward internalizeDelta: 3@4)) r < 0.0001.
	self assert: ((inverse externalizeScalar: 7) - (forward internalizeScalar: 7)) abs < 0.0001.
	self assert: ((inverse internalizePosition: 3@4) - (forward externalizePosition: 3@4)) r < 0.0001.
	self assert: ((inverse internalizeDelta: 3@4) - (forward externalizeDelta: 3@4)) r < 0.0001.
	self assert: ((inverse internalizeScalar: 7) - (forward externalizeScalar: 7)) abs < 0.0001.! !

!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 12/Sep/2024, 4:19:23 pm (UTC)'!
testLayout1
	"
	self new testLayout1
	"
	| pane row1 row2 row3 r1c1 r1c2 r1c3 r1c4 r1c5 r2c1 r2c2 r2c3 r3c1 r3c2 r3c3 |
	pane := LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row1 := LayoutMorph newRow separation: 5.
	row1 color: Color red;
		addMorph: (r1c1 := ColoredBoxMorph new color: (Color h: 60 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 10);
		addMorph: (r1c2 := ColoredBoxMorph new color: Color blue)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8);
		addMorph: (r1c3 := ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.4);
		addMorph: (r1c4 := ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.15);
		addMorph: (r1c5 := ColoredBoxMorph new color: (Color h: 60 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
	pane addMorph: row1 layoutSpec: LayoutSpec useAll.
	row2 := LayoutMorph newRow separation: 5.
	row2 color: Color red;
		addMorph: (r2c1 := ColoredBoxMorph new color: Color blue)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8);
		addMorph: (r2c2 := ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.4);
		addMorph: (r2c3 := ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.2).
	pane addMorph: row2 layoutSpec: LayoutSpec useAll.
	row3 := LayoutMorph newRow separation: 5.
	row3 color: Color red;
		addMorph: (r3c1 := ColoredBoxMorph new color: (Color h: 120 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
		addMorph: (r3c2 := ColoredBoxMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40);
		addMorph: (r3c3 := ColoredBoxMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row3 layoutSpec: (LayoutSpec fixedHeight: 60).
	pane openInWorld: UISupervisor ui; morphExtent: 408@300.
	(UISupervisor animatedUIOf: Processor activeProcess)
		ifNotNil: [ :ui | ui doOneCycleNowOn: ui mainCanvas ]
		ifNil: [ | updated d |
			updated := false.
			d := Delay forMilliseconds: 10.
			UISupervisor whenUIinSafeState: [ UISupervisor whenUIinSafeState: [updated := true ]].
			[updated] whileFalse: [
				d wait ]].

	self assert: row1 morphWidth = (pane morphWidth - 10).
	self assert: r1c1 morphWidth = 10.
	self assert: r1c2 morphWidth rounded = 200.
	self assert: r1c3 morphWidth rounded = (r1c2 morphWidth / 0.8 * 0.4) rounded.
	self assert: r1c4 morphWidth rounded = (r1c2 morphWidth / 0.8 * 0.15) rounded.
	self assert: r1c5 morphWidth = 20.
	self assert: r1c1 morphHeight = (row1 morphHeight - 10).
	self assert: r1c2 morphHeight = (row1 morphHeight - 10).
	self assert: r1c3 morphHeight = (row1 morphHeight - 10).
	self assert: r1c4 morphHeight = (row1 morphHeight - 10).
	self assert: r1c5 morphHeight = 20.

	self assert: row2 morphWidth = (pane morphWidth - 10).
	self assert: r2c1 morphWidth rounded = 216.
	self assert: r2c2 morphWidth rounded = 108.
	self assert: r2c3 morphWidth rounded = 54.
	self assert: r2c1 morphHeight = (row2 morphHeight - 10).
	self assert: r2c2 morphHeight = (row2 morphHeight - 10).
	self assert: r2c3 morphHeight = (row2 morphHeight - 10).

	self assert: row3 morphWidth = (pane morphWidth - 10).
	self assert: r3c1 morphWidth = 20.
	self assert: r3c2 morphWidth = (row3 morphWidth - 10 - 20 - 10 - 30 * 0.5) rounded.
	self assert: r3c3 morphWidth = 30.
	self assert: row3 morphHeight = 60.
	self assert: r3c1 morphHeight = 40.
	self assert: r3c2 morphHeight = 40.
	self assert: r3c3 morphHeight = 50.

	pane delete! !

!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 12/Sep/2024, 4:19:34 pm (UTC)'!
testLayout2
	"
	self new testLayout2
	"
	| pane row c1 c2 c3 |
	pane := LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row := LayoutMorph newRow separation: 5.
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: (c1 := ColoredBoxMorph new color: (Color h: 120 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8 offAxisEdgeWeight: #rowBottom);
		addMorph: (c2 := ColoredBoxMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 40 offAxisEdgeWeight: #rowTop);
		addMorph: (c3 := ColoredBoxMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 0.7 offAxisEdgeWeight: #center).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 1.0).
	pane openInWorld: UISupervisor ui; morphExtent: 400@271.
	(UISupervisor animatedUIOf: Processor activeProcess)
		ifNotNil: [ :ui | ui doOneCycleNowOn: ui mainCanvas ]
		ifNil: [ | updated d |
			updated := false.
			d := Delay forMilliseconds: 10.
			UISupervisor whenUIinSafeState: [ UISupervisor whenUIinSafeState: [updated := true ]].
			[updated] whileFalse: [
				d wait ]].

	self assert: row morphWidth = (pane morphWidth - 10).
	self assert: row morphHeight = 261.
	self assert: c1 displayBounds bottom = (row displayBounds bottom - 5) description: 'Should be at bottom'.
	self assert: c1 morphWidth = 20.
	self assert: c1 morphHeight = 200.8.
	self assert: c2 displayBounds top = (row displayBounds top + 5) description: 'Should be at top'.
	self assert: c2 morphWidth = 256.
	self assert: c2 morphHeight = 40.
	self assert: ((c3 displayBounds top - row displayBounds top) - (row displayBounds bottom - c3 displayBounds bottom)) abs < 2 description: 'Should be centered'.
	self assert: c3 morphWidth = 30.
	self assert: c3 morphHeight rounded = 176.

	pane delete! !

!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 12/Sep/2024, 4:19:43 pm (UTC)'!
testLayout3
	"
	self new testLayout3
	"
	| pane row innerRow i1 i2 i3 c2 c3 |
	pane := LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row := LayoutMorph newRow separation: 5.
	innerRow := LayoutMorph newRow color: Color red;  separation: 5.
	innerRow
		addMorph: (i1 := ColoredBoxMorph new)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (i2 := ColoredBoxMorph new)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (i3 := ColoredBoxMorph new)
			layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 offAxisEdgeWeight: #center);
		addMorph: (c2 := ColoredBoxMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 offAxisEdgeWeight: #rowTop);
		addMorph: (c3 := ColoredBoxMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 200).
	pane openInWorld: UISupervisor ui; morphExtent: 400@300.
	(UISupervisor animatedUIOf: Processor activeProcess)
		ifNotNil: [ :ui | ui doOneCycleNowOn: ui mainCanvas ]
		ifNil: [ | updated d |
			updated := false.
			d := Delay forMilliseconds: 10.
			UISupervisor whenUIinSafeState: [ UISupervisor whenUIinSafeState: [updated := true ]].
			[updated] whileFalse: [
				d wait ]].

	self assert: row displayBounds left = (pane displayBounds left + 5).
	self assert: row morphWidth = (pane morphWidth - 10).
	self assert: row morphHeight = 200.
	self assert: innerRow displayBounds left = (row displayBounds left + 5).
	self assert: (innerRow displayBounds top - row displayBounds top) = (row displayBounds bottom - innerRow displayBounds bottom) description: 'Should be centered'.
	self assert: innerRow morphWidth = 170.
	self assert: innerRow morphHeight = 30.

	self assert: i1 displayBounds left = (innerRow displayBounds left + 5).
	self assert: (i1 displayBounds top - innerRow displayBounds top) = (innerRow displayBounds bottom - i1 displayBounds bottom) description: 'Should be centered'.
	self assert: i1 morphWidth = 10.
	self assert: i1 morphHeight = 10.
	self assert: i2 displayBounds left = (innerRow displayBounds left + 20).
	self assert: (i2 displayBounds top - innerRow displayBounds top) = (innerRow displayBounds bottom - i2 displayBounds bottom) description: 'Should be centered'.
	self assert: i2 morphWidth = 10.
	self assert: i2 morphHeight = 10.
	self assert: i3 displayBounds left = (innerRow displayBounds left + 35).
	self assert: (i3 displayBounds top - innerRow displayBounds top) = (innerRow displayBounds bottom - i3 displayBounds bottom) description: 'Should be centered'.
	self assert: i3 morphWidth = (innerRow morphWidth - 40).
	self assert: i3 morphHeight = 10.

	self assert: c2 displayBounds top = (row displayBounds top + 5) description: 'Should be at top'.
	self assert: c2 morphWidth = 170.
	self assert: c2 morphHeight = 40.
	self assert: (c3 displayBounds top - row displayBounds top) = (row displayBounds bottom - c3 displayBounds bottom) description: 'Should be centered'.
	self assert: c3 morphWidth = 30.
	self assert: c3 morphHeight = (row morphHeight - 10).

	pane delete! !

!MorphicLocationTest methodsFor: 'testing' stamp: 'jmv 14/Jan/2015, 6:32:00 pm (UTC)'!
testComposition
	"
	MorphicLocationTest new testComposition
	"
	| composition inner outer |

	outer _ MorphicTranslation withTranslation: 3@5.
	inner _ AffineTransformation withRadians: 0.3.
	composition _ outer composedWith: inner.
	self assert: composition translation = outer translation.
	self assert: (outer externalizePosition: (inner externalizePosition: 3@4)) = (composition externalizePosition: 3@4).
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: (inner internalizePosition: (outer internalizePosition: 3@4)) = (composition internalizePosition: 3@4).
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).


	outer _ AffineTransformation withRadians: 0.3.
	inner _ MorphicTranslation withTranslation: 3@5.
	composition _ outer composedWith: inner.
	self assert: composition radians = outer radians.
	self assert: ((outer externalizePosition: (inner externalizePosition: 3@4)) - (composition externalizePosition: 3@4)) r < 0.0001.
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: ((inner internalizePosition: (outer internalizePosition: 3@4)) - (composition internalizePosition: 3@4)) r < 0.0001.
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).! !

!MorphicTranslationTest methodsFor: 'testing' stamp: 'jmv 12/Jan/2015, 7:46:00 pm (UTC)'!
testComposition
	"
	MorphicTranslationTest new testComposition
	"
	| composition inner outer |

	outer _ MorphicTranslation withTranslation: 3@5.
	inner _ MorphicTranslation withTranslation: -1@2.
	composition _ outer composedWith: inner.
	self assert: composition translation = (outer translation + inner translation).
	self assert: (outer externalizePosition: (inner externalizePosition: 3@4)) = (composition externalizePosition: 3@4).
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: (inner internalizePosition: (outer internalizePosition: 3@4)) = (composition internalizePosition: 3@4).
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).


	outer _ MorphicTranslation withTranslation: -1@2.
	inner _ MorphicTranslation withTranslation: 3@5.
	composition _ outer composedWith: inner.
	self assert: composition radians = outer radians.
	self assert: (outer externalizePosition: (inner externalizePosition: 3@4)) = (composition externalizePosition: 3@4).
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: (inner internalizePosition: (outer internalizePosition: 3@4)) = (composition internalizePosition: 3@4).
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).! !

!MorphicTranslationTest methodsFor: 'testing' stamp: 'len 9/May/2022, 9:57:28 pm (UTC)'!
testDisplayBounds
	"
	MorphicTranslationTest new testDisplayBounds
	"

	self assert: (MorphicTranslation new externalBoundingRectOf: (-2@ 2 extent: 10@10))
		encompassingIntegerRectangle = (-2@2 corner: 8@12).
	self assert: (MorphicTranslation new externalBoundingRectOf: (-12@ 12 extent: 10@10))
		encompassingIntegerRectangle = (-12@12 corner: -2@22).
	self assert: ((MorphicTranslation withTranslation: 2)  externalBoundingRectOf: (-4@ 2  extent: 10@10))
		encompassingIntegerRectangle = (-2@4 corner: 8@14).
	self assert: ((MorphicTranslation withTranslation: -4)  externalBoundingRectOf: (2@ 2  extent: 10@10))
		encompassingIntegerRectangle = (-2@ -2 corner: 8@8).
	self assert: ((MorphicTranslation withTranslation: 2)  externalBoundingRectOf: (-14@ 2  extent: 10@10))
		encompassingIntegerRectangle = (-12@4 corner: -2@14).
	self assert: ((MorphicTranslation withTranslation: 4)  externalBoundingRectOf: (-12@ 2  extent: 10@10))
		encompassingIntegerRectangle = (-8@6 corner: 2@16).
	self assert: ((MorphicTranslation withTranslation: -4)  externalBoundingRectOf: (12@ 2  extent: 10@10))
		encompassingIntegerRectangle = (8@ -2 corner: 18@8).
	self assert: ((MorphicTranslation withTranslation: -2)  externalBoundingRectOf: (4@ 2  extent: 10@10))
		encompassingIntegerRectangle = (2@0 corner: 12@10).
	self assert: ((MorphicTranslation withTranslation: 4)  externalBoundingRectOf: (-2@ 2  extent: 10@10))
		encompassingIntegerRectangle = (2@6 corner: 12@16).! !

!MorphicTranslationTest methodsFor: 'testing' stamp: 'len 9/May/2022, 9:57:12 pm (UTC)'!
testInverseTransform
	"
	MorphicTranslationTest new testInverseTransform
	"
	self assert: (MorphicTranslation new inverseTransform: (-2@ 2)) = (-2@2).
	self assert: ((MorphicTranslation withTranslation: 2)  inverseTransform: (-4@ 2)) = (-6@0).
	self assert: ((MorphicTranslation withTranslation: 4)  inverseTransform: (-2@ 2)) = (-6@ -2).
	self assert: ((MorphicTranslation withTranslation: -2)  inverseTransform: (4@ 2)) = (6@4).
	self assert: ((MorphicTranslation withTranslation: -4)  inverseTransform: (2@ 2)) = (6@6)! !

!MorphicTranslationTest methodsFor: 'testing' stamp: 'jmv 12/Jan/2015, 7:46:00 pm (UTC)'!
testInverseTransformation
	"
	MorphicTranslationTest new testInverseTransformation
	"
	| forward inverse |

	forward _ MorphicTranslation withTranslation: 3@5.
	inverse _ forward inverseTransformation.
	
	self assert: inverse translation = forward translation negated.
	self assert: (inverse externalizePosition: 3@4) = (forward internalizePosition: 3@4).
	self assert: (inverse externalizeDelta: 3@4) = (forward internalizeDelta: 3@4).
	self assert: (inverse externalizeScalar: 7) = (forward internalizeScalar: 7).
	self assert: (inverse internalizePosition: 3@4) = (forward externalizePosition: 3@4).
	self assert: (inverse internalizeDelta: 3@4) = (forward externalizeDelta: 3@4).
	self assert: (inverse internalizeScalar: 7) = (forward externalizeScalar: 7)! !

!MorphicTranslationTest methodsFor: 'testing' stamp: 'len 9/May/2022, 9:57:17 pm (UTC)'!
testTransform
	"
	MorphicTranslationTest new testTransform
	"
	self assert: (MorphicTranslation new transform: (-2@ 2)) = (-2@2).
	self assert: ((MorphicTranslation withTranslation: 2) transform: (-4@ 2)) = (-2@4).
	self assert: ((MorphicTranslation withTranslation: 4) transform: (-2@ 2)) = (2@6).
	self assert: ((MorphicTranslation withTranslation: -2) transform: (4@ 2)) = (2@0).
	self assert: ((MorphicTranslation withTranslation: -4) transform: (2@ 2)) = (-2@ -2)! !

!WorldTest methodsFor: 'tests' stamp: 'jmv 11/Oct/2023, 5:08:57 pm (UTC)'!
testDoOneCycleWorksWithDeferredQueue
        "Ensure that nested doOneCycles don't break deferred UI messages"
        | finished |
        [
		[
	                UISupervisor whenUIinSafeState: [ UISupervisor ui doOneCycleNowOn: UISupervisor ui mainCanvas ].
	                UISupervisor whenUIinSafeState: nil "whatever".
			UISupervisor ui doOneCycleNowOn: UISupervisor ui mainCanvas.
	                finished := true.
	        ] valueWithin: 1 seconds onTimeout: [finished := false ].
	        self assert: finished
	] forkAt: Processor userInterruptPriority.! !
