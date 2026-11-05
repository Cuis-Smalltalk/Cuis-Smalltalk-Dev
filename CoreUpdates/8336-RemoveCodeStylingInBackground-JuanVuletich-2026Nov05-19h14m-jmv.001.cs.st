'From Cuis7.9 [latest update: #8335] on 5 November 2026 at 7:24:13 pm'!
!classDefinition: #SHTextStyler category: #'Tools-Syntax Highlighting'!
Object subclass: #SHTextStyler
	instanceVariableNames: 'backgroundProcess formattedText textModel mutex '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!

!SHTextStyler commentStamp: '<historical>'!
I am an Abstract class.
Subclasses of me can create formatted, coloured, and styled copies of Text that is given to them.
	
!


!SHTextStyler methodsFor: 'styling' stamp: 'jmv 5/Nov/2026, 10:18:13 pm (UTC)'!
convertAndStyle: text
	"Do the styling on the model text.
	After finishing, tell model, by triggering #shoutStyled."
	self getReady.
	formattedText := text.
	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].
	formattedText size <= self class maxSizeForStyling
		ifTrue: [
			self parseText.
			self privateAutoHighlight.
			self privateConvertAssignmentsOffset: 0.
			self privateStyle.
			textModel changed: #shoutStyled ].! !


!TextModel methodsFor: 'user interface support' stamp: 'jmv 5/Nov/2026, 10:18:31 pm (UTC)'!
convertAndStyleAllWith: anSHTextStyler
	"Convert and style all contents as a single method or script."

	anSHTextStyler convertAndStyle: self actualContents.! !


!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 5/Nov/2026, 10:19:44 pm (UTC)'!
aboutToEdit
	"NOP in this morph."! !


!SHTextStyler methodsFor: 'accessing' stamp: 'jmv 5/Nov/2026, 10:20:00 pm (UTC)'!
textModel: aTextModel
	textModel := aTextModel.! !


!SHTextStyler class methodsFor: 'constants' stamp: 'jmv 5/Nov/2026, 10:16:12 pm (UTC)'!
maxSizeForStyling
	"Rather arbitrary, but reasonable."
	^40000! !


!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 5/Nov/2026, 10:20:06 pm (UTC)'!
getReady
	"Prepare to parse"

	parser ifNil: [ parser := SHParserST80 new ].
	parser
		workspace: workspace;
		classOrMetaClass: classOrMetaClass.! !

!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 5/Nov/2026, 10:20:11 pm (UTC)'!
workspaceContents: text

	formattedText := text.
	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 5/Nov/2026, 10:23:18 pm (UTC)'!
optimizeForMutationSpeed: rangesSize

	"Optimize for mutation speed unless method is really large but with very few distict elements:
	Source code that includes just big literals is better served by conventional Text+RunArray"
	"Do it only if we are not breaking textModel!! (for instance, StyledTextEditor asks for formatting just sections, not whole #actualContents)."
	| newFormattedText |
	"Not if we are obsoleted because #actualContents changed."
	formattedText == textModel actualContents ifTrue: [
		(formattedText size > 2000 and: [ rangesSize < 50 ]) ifFalse: [
			"What folows could take a little while"
			newFormattedText := formattedText optimizedForMutationSpeed.
			"if already #optimizedForMutationSpeed, do nothing"
			formattedText == newFormattedText ifFalse: [
				formattedText := newFormattedText.
				textModel basicActualContents: newFormattedText ]]].! !

!methodRemoval: SHTextStyler class #minSizeForBackgroundStyling stamp: 'jmv 5/Nov/2026, 10:15:57 pm (UTC)'!
SHTextStyler class removeSelector: #minSizeForBackgroundStyling!
!methodRemoval: SHTextStyler #mutex stamp: 'jmv 5/Nov/2026, 10:20:26 pm (UTC)'!
SHTextStyler removeSelector: #mutex!
!methodRemoval: SHTextStyler #terminateBackgroundStylingProcess stamp: 'jmv 5/Nov/2026, 10:20:15 pm (UTC)'!
SHTextStyler removeSelector: #terminateBackgroundStylingProcess!
!methodRemoval: SHTextStyler #convertAndStyleInBackgroundProcess stamp: 'jmv 5/Nov/2026, 10:17:20 pm (UTC)'!
SHTextStyler removeSelector: #convertAndStyleInBackgroundProcess!
!methodRemoval: SHTextStyler #convertAndStyle:allowBackgroundStyleProcess: stamp: 'jmv 5/Nov/2026, 10:18:41 pm (UTC)'!
SHTextStyler removeSelector: #convertAndStyle:allowBackgroundStyleProcess:!
!classDefinition: #SHTextStyler category: #'Tools-Syntax Highlighting'!
Object subclass: #SHTextStyler
	instanceVariableNames: 'formattedText textModel'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!
