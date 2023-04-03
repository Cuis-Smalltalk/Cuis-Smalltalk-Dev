'From Cuis 6.0 [latest update: #5741] on 2 April 2023 at 7:01:22 pm'!
!classDefinition: #SHTextStylerST80 category: #'Tools-Syntax Highlighting'!
SHTextStyler subclass: #SHTextStylerST80
	instanceVariableNames: 'classOrMetaClass workspace parser disableFormatAndConvert lengthIncreasePositions lengthIncreaseValues '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!

!PluggableTextModel methodsFor: 'updating' stamp: 'jmv 4/2/2023 18:44:04'!
update: aSymbol
	"We are being notified of a change in our provider.
	Notify our dependents"
	"
	self changed: aSymbol
	"
	aSymbol == textGetter ifTrue: [ ^self changed: #acceptedContents ].
	aSymbol == selectionGetter ifTrue: [ ^self changed: #initialSelection ].
	self changed: aSymbol! !


!InnerTextMorph methodsFor: 'shout' stamp: 'jmv 10/20/2022 18:46:51'!
stylerStyled

	self textComposition composeAll.
	needsFit _ true.
	self editor recomputeSelection.	
	self updateFromTextComposition.
	self editor blinkParen.
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].! !


!TextModelMorph methodsFor: 'updating' stamp: 'jmv 4/2/2023 18:46:22'!
update: aSymbol

	super update: aSymbol.
	aSymbol ifNil: [^self].

	aSymbol == #flash ifTrue: [^self flash].
	aSymbol == #actualContents ifTrue: [ ^self updateActualContents ].
	aSymbol == #acceptedContents ifTrue: [ ^self updateAcceptedContents ].
	aSymbol == #refetched ifTrue: [ ^self updateRefetched ].
	aSymbol == #initialSelection ifTrue: [ ^self updateInitialSelection ].
	aSymbol == #clearUserEdits ifTrue: [ ^self updateClearUserEdits ].
	aSymbol == #shoutStyled ifTrue: [ ^self updateShoutStyled ].
! !

!TextModelMorph methodsFor: 'updating' stamp: 'HAW 4/18/2019 20:36:13'!
updateInitialSelection

	^self 
		setSelection: model getSelection; 
		redrawNeeded! !

!TextModelMorph methodsFor: 'updating' stamp: 'HAW 4/18/2019 20:35:31'!
updateRefetched 

	"#actualContents also signalled when #refetched is signalled.
	No need to repeat what's done there."
	self setSelection: model getSelection.
	self hasUnacceptedEdits: false.
	
	^self ! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 4/2/2023 18:58:21'!
privateFormatAndConvert

	(Preferences at: #showAssignmentAsLeftArrow)
		ifTrue: [
			"Turn := into a left arrow"
			self replaceStringForRangesWithType: #ansiAssignment with: Character smalltalkLeftArrow asString.
			"Turn $_ into a left arrow"
			self replaceStringForRangesWithType: #assignment with: Character smalltalkLeftArrow asString.
			"Turn $^into up arrow"
			self replaceStringForRangesWithType: #return with: Character smalltalkUpArrow asString ].! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 3/15/2023 14:49:59'!
replaceStringForRangesWithType: aSymbol with: aString
	"Answer aText if no replacements, or a copy of aText with 
	each range with a type of aSymbol replaced by aString"

	| toReplace increaseInLength |

	self parseSetWorkspace: false.
	toReplace _ parser ranges select: [ :each |
		each rangeType = aSymbol ].
	toReplace isEmpty ifTrue: [ ^self ].
	increaseInLength := 0.

	(toReplace asArray sort: [ :a :b | a start <= b start ]) 
		do: [ :each | | end start thisIncrease | 
			start := each start + increaseInLength.
			end := each end + increaseInLength.
			formattedText replaceFrom: start to: end with: aString.
			thisIncrease := aString size - each length.
			increaseInLength := increaseInLength + thisIncrease ]! !

!methodRemoval: SHTextStylerST80 #positionInFormattedTextFor: stamp: 'jmv 4/2/2023 18:53:26'!
SHTextStylerST80 removeSelector: #positionInFormattedTextFor:!
!methodRemoval: SHTextStylerST80 #initialize stamp: 'jmv 4/2/2023 18:58:27'!
SHTextStylerST80 removeSelector: #initialize!
!methodRemoval: SHTextStylerST80 #disableFormatAndConvert stamp: 'jmv 4/2/2023 18:56:25'!
SHTextStylerST80 removeSelector: #disableFormatAndConvert!
!methodRemoval: TextModelMorph #updateAutoSelect stamp: 'jmv 4/2/2023 18:46:32'!
TextModelMorph removeSelector: #updateAutoSelect!
!methodRemoval: InnerTextMorph #highlightString stamp: 'jmv 4/2/2023 18:39:47'!
InnerTextMorph removeSelector: #highlightString!
!methodRemoval: InnerTextMorph #highlightMessage stamp: 'jmv 4/2/2023 18:39:54'!
InnerTextMorph removeSelector: #highlightMessage!
!methodRemoval: InnerTextMorph #updateRefetched stamp: 'jmv 4/2/2023 18:52:09'!
InnerTextMorph removeSelector: #updateRefetched!
!methodRemoval: InnerTextMorph #updateInitialSelection stamp: 'jmv 4/2/2023 18:52:54'!
InnerTextMorph removeSelector: #updateInitialSelection!
!methodRemoval: TextEditor class #abandonChangeText stamp: 'jmv 4/2/2023 18:46:48'!
TextEditor class removeSelector: #abandonChangeText!
!methodRemoval: MessageSet #acceptedContentsChanged stamp: 'jmv 4/2/2023 18:45:59'!
MessageSet removeSelector: #acceptedContentsChanged!
!classDefinition: #SHTextStylerST80 category: #'Tools-Syntax Highlighting'!
SHTextStyler subclass: #SHTextStylerST80
	instanceVariableNames: 'classOrMetaClass workspace parser'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!

!SHTextStylerST80 reorganize!
('styling' formatWorkspace: styleWorkspaceFrom:to:allowEmphasis:)
('private - show subscript' formatAsSubscript: hasToShowSubscriptOf: showAsSubscriptIfAppropriate:)
('private' applySmalltalkStylingIn:using: attributesFor: optimizeForMutationSpeed: parseSetWorkspace: privateFormatAndConvert privateStyle replaceStringForRangesWithType:with: setAttributesFromRanges:)
('accessing' classOrMetaClass: workspace:)
('initialization')
!

