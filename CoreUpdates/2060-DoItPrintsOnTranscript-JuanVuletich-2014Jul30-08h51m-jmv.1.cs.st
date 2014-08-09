'From Cuis 4.2 of 25 July 2013 [latest update: #2059] on 30 July 2014 at 9:03:42.895057 am'!

!PasteUpMorph methodsFor: 'windows' stamp: 'jmv 7/30/2014 09:00'!
findATranscript
	"Locate a transcript, open it, and bring it to the front.  Create one if necessary"

	self
		findAWindowSatisfying: [ :aWindow | aWindow model == Transcript]
		orMakeOneUsing: [ TranscriptWindow openTranscript ]! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 7/30/2014 09:01'!
doIt
	"Set the context to include pool vars of the model.  Then evaluate.
	Print the result on the Transcript"
	^ self evaluateSelectionAndDo: [ :result |
		result print
		] ifFail: nil! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 7/30/2014 09:02'!
explain
	"Try to shed some light on what kind of entity the current selection is. 
	The selection must be a single token or construct. Insert the answer after 
	the selection. Send private messages whose names begin with 'explain' 
	that return a string if they recognize the selection, else nil."

	| string tiVars cgVars selectors delimitors numbers sorry reply symbol provider |
	sorry _ '"Sorry, I can''t explain that.  Please select a single
token, construct, or special character.'.
	sorry _ sorry , (morph canDiscardEdits
		ifFalse: ['  Also, please cancel or accept."']
		ifTrue: ['"']).
	(string _ self selection asString) isEmpty
		ifTrue: [reply _ '']
		ifFalse: [
			string _ string withBlanksTrimmed.
			"Remove space, tab, cr"
			"Temps and Instance vars need only test strings that are all letters"
			(string detect: [:char | char isGenerallyValidInIdentifiers not]
				ifNone: nil) ifNil: [
					tiVars _ (self explainTemp: string)
						ifNil: [self explainInst: string]].
					
			provider _ self codeProvider.
			(tiVars == nil and: [provider respondsTo: #explainSpecial:])
				ifTrue: [tiVars _ provider explainSpecial: string].
			tiVars _ tiVars
				ifNil: [ '']
				ifNotNil: [ tiVars , '\' withNewLines].
			"Context, Class, Pool, and Global vars, and Selectors need 
			only test symbols"
			(Symbol hasInterned: string ifTrue: [:s | symbol _ s])
				ifTrue: [
					cgVars _ (self explainCtxt: symbol) 
						ifNil: [ (self explainClass: symbol)
							ifNil: [ self explainGlobal: symbol]].
					"See if it is a Selector (sent here or not)"
					selectors _ (self explainMySel: symbol)
						ifNil: [(self explainPartSel: string)
							ifNil: [ self explainAnySel: symbol]]]
				ifFalse: [selectors _ self explainPartSel: string].
			cgVars _ cgVars
				ifNil: [ '']
				ifNotNil: [cgVars , '\' withNewLines].
			selectors _ selectors
				ifNil: [ '']
				ifNotNil: [ selectors , '\' withNewLines].
			delimitors _ string size = 1
				ifTrue: ["single special characters"
					self explainChar: string]
				ifFalse: ["matched delimitors"
					self explainDelimitor: string].
			numbers _ self explainNumber: string.
			numbers ifNil: [numbers _ ''].
			delimitors ifNil: [delimitors _ ''].
			reply _ tiVars , cgVars , selectors , delimitors , numbers].
	reply size = 0 ifTrue: [reply _ sorry].
	self runningWorld ifNotNil: [ :w | w findATranscript ].
	reply print! !

!SmalltalkEditor methodsFor: 'new selection' stamp: 'jmv 7/30/2014 08:51'!
notify: aString at: anInteger in: aStream 
	"The compilation of text failed. The syntax error is noted as the argument, 
	aString. Insert it in the text at starting character position anInteger."
	"This is a user command, and generates undo"

	self insertAndSelect: aString at: (anInteger max: 1)! !


!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 7/30/2014 09:01'!
findATranscript: evt
	"Locate a transcript, open it, and bring it to the front.  Create one if necessary"

	self findATranscript! !


!PasteUpMorph reorganize!
('accessing' activeHand color:)
('alarms-scheduler' addAlarm:withArguments:for:at: removeAlarm:for:)
('caching' releaseCachedState)
('change reporting' invalidateDisplayRect:from: redrawNeeded)
('classification' isWorldMorph)
('drawing' drawOn:)
('dropping/grabbing' acceptDroppingMorph:event: allowsMorphDrop allowsSubmorphDrag)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('events' click:localPosition: mouseButton1Down:localPosition: windowEvent:)
('event handling testing' handlesMouseDown:)
('event handling' mouseButton2Activity wantsWindowEvent: windowEventHandler)
('geometry' externalizeDisplayBounds: externalizeToWorld: internalizeFromWorld: morphPositionInWorld privateExtent:)
('initialization' clearCanvas clearWaitDelay defaultBorderColor defaultBorderWidth defaultColor)
('interaction loop' doOneCycleNow)
('menu & halo' addCustomMenuItems:hand: addWorldHaloMenuItemsTo:hand: deleteBalloonTarget:)
('misc' backgroundImage backgroundImageData: buildMagnifiedBackgroundImage)
('printing' printOn:)
('project state' canvas firstHand hands handsDo: handsReverseDo: viewBox viewBox:)
('stepping' cleanseStepList runStepMethods startStepping:at:selector:stepTime: stopStepping:selector: stopSteppingMorph:)
('stepping and presenter' wantsSteps)
('structure' world)
('submorphs-accessing' allMorphsDo:)
('submorphs-add/remove' addAllMorphs:)
('testing' isReallyVisible stepTime)
('world menu' bringWindowsFullOnscreen closeUnchangedWindows collapseAll collapseNonWindows deleteNonWindows expandAll findAChangeSorter: findAFileList: findAMessageNamesWindow: findATranscript: findAWindowSatisfying:orMakeOneUsing: findDirtyBrowsers: findDirtyWindows: findWindow: invokeWorldMenu)
('world state' addMorph:centeredNear: allNonFlapRelatedSubmorphs deleteAllHalos displayWorld displayWorldSafely doOneCycle flashRects:color: fullRepaintNeeded haloMorphs privateOuterDisplayWorld restoreMorphicDisplay startSteppingSubmorphsOf: worldState:)
('halos and balloon help' wantsHaloHandleWithSelector:inHalo:)
('object serialization' objectForDataStream:)
('windows' findATranscript)
!

