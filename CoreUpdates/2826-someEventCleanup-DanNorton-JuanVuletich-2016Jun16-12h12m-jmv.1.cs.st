'From Cuis 4.2 of 25 July 2013 [latest update: #2824] on 16 June 2016 at 12:28:43.52256 pm'!

!CodeProvider methodsFor: 'contents' stamp: 'dhn 6/14/2016 20:59'!
acceptedContentsChanged

	self changed: #acceptedContents.
	self triggerEvent: #decorateButtons.
	self triggerEvent: #annotationChanged! !


!TimeProfileBrowser methodsFor: 'private' stamp: 'jmv 6/16/2016 12:23'!
runBlock: aBlock
"
	TimeProfileBrowser spyOn:  [20 timesRepeat: 
			[Transcript show: 100 factorial printString]]
"
	| result linesStream talliesStream textStream |

	tally := AndreasSystemProfiler new.
	tally observedProcess: Processor activeProcess.
	result := tally spyOn: aBlock.

	textStream _ DummyStream on: nil.
	linesStream _ WriteStream on: Array new.
	talliesStream _ WriteStream on: Array new.
	
	tally reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream.
	self initializeMessageList: linesStream contents talliesList: talliesStream contents.

	self changed: #messageList.
	self changed: #messageListIndex.
	self triggerEvent: #decorateButtons.
	^result! !


!Debugger methodsFor: 'private' stamp: 'jmv 6/16/2016 12:23'!
contextStackIndex: anInteger oldContextWas: oldContext 
	"Change the context stack index to anInteger, perhaps in response to user selection."

	| isNewMethod selectedContextSlotName index |
	contextStackIndex := anInteger.
	anInteger = 0 ifTrue: [
		currentCompiledMethod := nil.
		self changed: #contextStackIndex.
		self acceptedContentsChanged.
		contextVariablesInspector object: nil.
		receiverInspector object: self receiver.
		^self].
	selectedContextSlotName := contextVariablesInspector selectedSlotName.
	isNewMethod := oldContext == nil
					or: [oldContext method ~~ (currentCompiledMethod := self selectedContext method)].
	isNewMethod ifTrue: [
		self acceptedContentsChanged.
		self pcRange ].
	self changed: #contextStackIndex.
	self triggerEvent: #decorateButtons.
	contextVariablesInspector object: self selectedContext.
	((index := contextVariablesInspector fieldList indexOf: selectedContextSlotName) ~= 0
	 and: [index ~= contextVariablesInspector selectionIndex]) ifTrue: [
		contextVariablesInspector toggleIndex: index].
	receiverInspector object: self receiver.
	isNewMethod ifFalse: [
		self changed: #contentsSelection]! !


!CodeWindow methodsFor: 'GUI building' stamp: 'dhn 6/14/2016 20:59'!
buildMorphicAnnotationsPane

	| aTextMorph |
	aTextMorph _ TextModelMorph
		textProvider: model
		textGetter: #annotation.
	model when: #annotationChanged send: #refetch to: aTextMorph model.
	model when: #decorateButtons send: #decorateButtons to: self.
	aTextMorph
		askBeforeDiscardingEdits: false;
		hideScrollBarsIndefinitely.
	^aTextMorph! !


!TranscriptWindow class methodsFor: 'GUI building' stamp: 'dhn 6/15/2016 10:45'!
openTranscript
	"
	TranscriptWindow openTranscript
	"
	| win |
	win _ TranscriptWindow new.
	win
		setLabel: 'Transcript';
		model: Transcript.
	win layoutMorph addMorph: TranscriptMorph new proportionalHeight: 1.
	win model when: #redraw send: #redrawNeeded to: win.
	win openInWorld.
! !


!Transcript class methodsFor: 'displaying' stamp: 'dhn 6/14/2016 14:00'!
display
	showOnDisplay ifTrue: [
		self displayOn: Display.
		DisplayScreen screenUpdateRequired: innerRectangle ].
	"So any morph in front of us is repaired when Morphic cycles.
	This includes, for instance, the TranscriptWindow that shows our contents if showOnDisplay is false"
	self triggerEvent: #redraw! !

!methodRemoval: CodeWindow #update:!
CodeWindow removeSelector: #update:!
!methodRemoval: Morph #update:!
Morph removeSelector: #update:!
