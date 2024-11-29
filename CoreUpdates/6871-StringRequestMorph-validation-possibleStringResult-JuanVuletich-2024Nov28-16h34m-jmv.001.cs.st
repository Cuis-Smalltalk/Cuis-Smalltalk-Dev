'From Cuis7.1 [latest update: #6870] on 28 November 2024 at 4:34:43 pm'!
!classDefinition: #StringRequestMorph category: #'Morphic-Composite Widgets'!
LayoutMorph subclass: #StringRequestMorph
	instanceVariableNames: 'response acceptBlock cancelBlock validationBlock textMorph titleLabelMorph '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!

!StringRequestMorph methodsFor: 'accessing' stamp: 'jmv 11/28/2024 15:40:51'!
response: aText
	"Sent when text pane accepts."
	| validationResult |
	response := aText.

	"ValidationBlock can evaluate to:
		true: validation passed
		false: validation failed
		a String: validation failed, feedback to user."
	validationBlock ifNotNil: [
		validationResult := validationBlock value: aText asPlainString.
		validationResult == true ifFalse: [
			validationResult isString ifTrue: [
				titleLabelMorph contents: validationResult ].
			self flash.
			^ false ]].

	[
		acceptBlock ifNotNil: [acceptBlock value: aText asPlainString]
	] ensure: [ self delete ].
	^ true! !

!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 11/28/2024 15:40:04'!
addTitle: aString
	| titleBarMorph pp w |
	titleBarMorph := ColoredBoxMorph new.
	titleBarMorph color: Theme current menuTitleBar.
	pp := `8@2`.
	aString asPlainString linesDo: [ :line |
		titleLabelMorph := LabelMorph new
			contents: line;
			font: (Preferences at: #standardMenuFont) bold.
		titleBarMorph addMorphBack: titleLabelMorph position: pp.
		pp := pp + (0@(titleLabelMorph morphHeight+2)) ].
	w := titleBarMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleBarMorph morphExtent: (w + 24) @ (pp y).
	self addMorphKeepMorphHeight: titleBarMorph.
	^titleBarMorph morphExtent! !

!classDefinition: #StringRequestMorph category: #'Morphic-Composite Widgets'!
LayoutMorph subclass: #StringRequestMorph
	instanceVariableNames: 'response acceptBlock cancelBlock validationBlock textMorph titleLabelMorph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!
