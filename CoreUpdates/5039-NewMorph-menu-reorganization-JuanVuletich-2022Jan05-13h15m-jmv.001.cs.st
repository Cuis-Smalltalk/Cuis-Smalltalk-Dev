'From Cuis 6.0 [latest update: #5038] on 5 January 2022 at 1:18:45 pm'!

!PlacedMorph class methodsFor: 'new-morph participation' stamp: 'jmv 1/5/2022 12:20:00'!
categoryInNewMorphMenu
	^ 'Vector Graphics'! !


!ProgressMorph class methodsFor: 'new-morph participation' stamp: 'jmv 1/5/2022 12:46:33'!
initializedInstance
	| progress |
	progress _ ProgressMorph label: 'Test progress' subLabel: 'this is the subheading'.
	[
		100 timesRepeat: [
			(Delay forMilliseconds: 30) wait.
			progress incrDone: 0.01 ].
		progress delete] fork.
	^progress! !


!HaloHandleMorph class methodsFor: 'new-morph participation' stamp: 'jmv 1/5/2022 12:22:20'!
includeInNewMorphMenu
	"Return true for all classes that can be instantiated from the menu"

	^ false! !


!HaloMorph class methodsFor: 'new-morph participation' stamp: 'jmv 1/5/2022 12:22:38'!
includeInNewMorphMenu
	"Return true for all classes that can be instantiated from the menu"

	^ false! !


!DraggingGuideMorph class methodsFor: 'new-morph participation' stamp: 'jmv 1/5/2022 12:23:37'!
includeInNewMorphMenu
	"Return true for all classes that can be instantiated from the menu"

	^ false! !


!TextParagraphMorph class methodsFor: 'instance creation' stamp: 'jmv 1/5/2022 12:30:08'!
initializedInstance

	^self contents:
'I am a read-only text paragraph.
I can hold several lines of text.
Even with ', ' text ' bold, 'styles.' italic, '
Try resizing me with the Halo.'! !


!FillInTheBlankMorph class methodsFor: 'new-morph participation' stamp: 'jmv 1/5/2022 12:37:37'!
includeInNewMorphMenu
	"Return true for all classes that can be instantiated from the menu"

	^ false! !


!ProgressBarMorph class methodsFor: 'new-morph participation' stamp: 'jmv 1/5/2022 12:39:28'!
includeInNewMorphMenu
	"Return true for all classes that can be instantiated from the menu"

	^ false! !


!MenuLineMorph class methodsFor: 'new-morph participation' stamp: 'jmv 1/5/2022 12:25:48'!
includeInNewMorphMenu
	"Return true for all classes that can be instantiated from the menu"

	^ false! !


!MenuMorph class methodsFor: 'new-morph participation' stamp: 'jmv 1/5/2022 12:25:58'!
includeInNewMorphMenu
	"Return true for all classes that can be instantiated from the menu"

	^ false! !


!BoxedMorph class methodsFor: 'new-morph participation' stamp: 'jmv 1/5/2022 12:09:00'!
categoryInNewMorphMenu
	^ 'Basic'! !


!TheWorldMenu methodsFor: 'private' stamp: 'jmv 1/5/2022 12:53:09'!
pvtAlphaSplitListDepth
	"Split factor.  A higher number results in fewer items in each submenu"
	^ 3! !

!methodRemoval: ProgressBarMorph class #categoryInNewMorphMenu stamp: 'jmv 1/5/2022 13:17:16'!
ProgressBarMorph class removeSelector: #categoryInNewMorphMenu!
!methodRemoval: FillInTheBlankMorph class #categoryInNewMorphMenu stamp: 'jmv 1/5/2022 13:17:16'!
FillInTheBlankMorph class removeSelector: #categoryInNewMorphMenu!
!methodRemoval: HaloMorph class #categoryInNewMorphMenu stamp: 'jmv 1/5/2022 13:17:16'!
HaloMorph class removeSelector: #categoryInNewMorphMenu!
!methodRemoval: HaloHandleMorph class #categoryInNewMorphMenu stamp: 'jmv 1/5/2022 13:17:16'!
HaloHandleMorph class removeSelector: #categoryInNewMorphMenu!
