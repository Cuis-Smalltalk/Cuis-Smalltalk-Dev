'From Cuis 5.0 of 7 November 2016 [latest update: #3531] on 24 December 2018 at 10:41:52 am'!
!classDefinition: #DropFilesAction category: #'Morphic-Worlds'!
Object subclass: #DropFilesAction
	instanceVariableNames: 'shouldAskForCancel dropFilesEvent cancelBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Worlds'!
!classDefinition: #DropFilesEvent category: #'Morphic-Events'!
MorphicEvent subclass: #DropFilesEvent
	instanceVariableNames: 'position wasHandled numberOfFiles'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!

!Morph methodsFor: 'dropping/grabbing' stamp: 'HAW 12/24/2018 07:21:44'!
dropFiles: aDropFilesEvent

	"I do nothing, subclasses should redefine if they handle this event - Hernan"! !

!Morph methodsFor: 'event handling testing' stamp: 'HAW 12/24/2018 07:13:50'!
allowsFilesDrop
	"Answer whether we accept dropping files. By default answer false."

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'allowsFilesDrop'! !

!Morph methodsFor: 'events-processing' stamp: 'HAW 12/24/2018 09:28:32'!
processDropFiles: aDropFilesEvent localPosition: localEventPosition
	"Handle a dropping file."

	aDropFilesEvent wasHandled ifTrue: [ ^self ]. 
	
	aDropFilesEvent wasHandled: true.
	self dropFiles: aDropFilesEvent! !


!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'HAW 12/24/2018 07:13:56'!
allowsFilesDrop

	^ true! !

!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'HAW 12/24/2018 10:27:39'!
dropFiles: aDropFilesEvent

	(DropFilesAction for: aDropFilesEvent) value! !


!HandMorph methodsFor: 'events-processing' stamp: 'HAW 12/24/2018 09:25:29'!
startDropFilesEventDispatch: aDropFilesEvent

	owner dispatchEvent: aDropFilesEvent localPosition: aDropFilesEvent eventPosition.
	self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'private events' stamp: 'HAW 12/24/2018 09:29:07'!
generateDropFilesEvent: evtBuf 
	"Generate the appropriate mouse event for the given raw event buffer"

	| position stamp numberOfFiles dragType |
	
	stamp := evtBuf second.
	stamp = 0 ifTrue: [stamp := Time localMillisecondClock].
	dragType := evtBuf third.
	position := evtBuf fourth @ evtBuf fifth.
	numberOfFiles := evtBuf seventh.
	
	^ dragType = 4 ifTrue: [ DropFilesEvent at: position with: numberOfFiles from: self].
! !


!DropFilesAction methodsFor: 'initialization' stamp: 'HAW 12/24/2018 10:29:45'!
initializeFor: aDropFilesEvent 

	dropFilesEvent := aDropFilesEvent.
	shouldAskForCancel := aDropFilesEvent numberOfFiles > 1! !

!DropFilesAction methodsFor: 'evaluating' stamp: 'HAW 12/24/2018 10:34:38'!
value

	cancelBlock := [ ^self ].
	dropFilesEvent fileNamesDo: [ :fileName | self fileNamedDropped: fileName ]
		
! !

!DropFilesAction methodsFor: 'evaluating - private' stamp: 'HAW 12/24/2018 10:33:29'!
ask: aQueation onYes: aYesBlock 

	| yesSelected |
	
	yesSelected := shouldAskForCancel 
		ifTrue: [ self confirm: aQueation orCancel: cancelBlock ]
		ifFalse: [ self confirm: aQueation ].
		
	^yesSelected ifTrue: aYesBlock ! !

!DropFilesAction methodsFor: 'evaluating - private' stamp: 'HAW 12/24/2018 10:33:54'!
canBeFiledIn: aFileName

	^aFileName endsWith: '.st'! !

!DropFilesAction methodsFor: 'evaluating - private' stamp: 'HAW 12/24/2018 10:33:43'!
canBeInstalled: aFileName

	^(aFileName endsWith: '.cs.st') or: [ aFileName endsWith: '.pck.st' ]! !

!DropFilesAction methodsFor: 'evaluating - private' stamp: 'HAW 12/24/2018 10:36:41'!
fileNamedDropped: aFileName 

	(self canBeInstalled: aFileName) 
		ifTrue: [ self ifUserWantsInstall: aFileName ]
		ifFalse: [ (self canBeFiledIn: aFileName) 
			ifTrue: [ self ifUserWantsFileIn: aFileName ]
			ifFalse: [ self inform: 'Dropped file ', aFileName, ' not supported' ]]! !

!DropFilesAction methodsFor: 'evaluating - private' stamp: 'HAW 12/24/2018 10:32:40'!
ifUserWantsFileIn: aFileName 

	self ask: 'Do you want to file in ', aFileName, ' ?' onYes:  [ ChangeSet fileIn: aFileName asFileEntry ] ! !

!DropFilesAction methodsFor: 'evaluating - private' stamp: 'HAW 12/24/2018 10:32:12'!
ifUserWantsInstall: aFileName 

	self ask: 'Do you want to install ', aFileName, ' ?' onYes: [ ChangeSet install: aFileName asFileEntry ]
		! !


!DropFilesAction class methodsFor: 'instance creation' stamp: 'HAW 12/24/2018 10:29:14'!
for: aDropFilesEvent

	^self new initializeFor: aDropFilesEvent 
	! !


!DropFilesEvent methodsFor: 'initialization' stamp: 'HAW 12/24/2018 09:32:07'!
initializeAt: aPosition with: aNumberOfFiles from: aHand

	position := aPosition.
	numberOfFiles := aNumberOfFiles.
	source := aHand.
	wasHandled := false.! !

!DropFilesEvent methodsFor: 'testing' stamp: 'HAW 12/24/2018 09:27:00'!
isDropEvent

	^true! !

!DropFilesEvent methodsFor: 'testing' stamp: 'HAW 12/24/2018 09:27:13'!
wasHandled

	^wasHandled! !

!DropFilesEvent methodsFor: 'testing' stamp: 'HAW 12/24/2018 09:27:26'!
wasHandled: aBool

	"This is ugly, and means that events are copied in many places..."
	self flag: #jmvVer.

	wasHandled _ aBool! !

!DropFilesEvent methodsFor: 'dispatching' stamp: 'HAW 12/24/2018 09:24:16'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Drop is done on the innermost target that accepts it."

	| eventPositionInChild |

	"Try to get out quickly"
	(aMorph fullContainsPoint: positionInAMorph) ifFalse: [ ^#rejected ].

	"Go looking if any of our submorphs wants it"
	aMorph submorphsDo: [ :eachChild |
		eventPositionInChild _ eachChild internalize: positionInAMorph.
		(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [ ^self ]].

	(aMorph allowsFilesDrop and: [ aMorph containsPoint: positionInAMorph event: self ])
		ifTrue: [^ self sentTo: aMorph localPosition: positionInAMorph ].
	
	^#rejected! !

!DropFilesEvent methodsFor: 'dispatching' stamp: 'HAW 12/24/2018 07:20:35'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into aMorph"

	^aMorph processDropFiles: self localPosition: positionInAMorph! !

!DropFilesEvent methodsFor: 'dispatching' stamp: 'HAW 12/24/2018 07:53:42'!
startDispatchFrom: aHand
	"double dispatch the event dispatch"

	aHand startDropFilesEventDispatch: self! !

!DropFilesEvent methodsFor: 'position' stamp: 'HAW 12/24/2018 09:30:41'!
eventPosition

	^position! !

!DropFilesEvent methodsFor: 'primitives' stamp: 'HAW 12/24/2018 09:39:09'!
primDropRequestFileHandle: dropIndex
	"Primitive. Return the (read-only) file handle for some file that was just dropped onto Squeak.
	Fail if dropIndex is out of range or the primitive is not supported."
	<primitive: 'primitiveDropRequestFileHandle' module:'DropPlugin'>
	^nil! !

!DropFilesEvent methodsFor: 'primitives' stamp: 'HAW 12/24/2018 09:38:59'!
primDropRequestFileName: dropIndex
	"Primitive. Return the file name for some file that was just dropped onto Squeak.
	Fail if dropIndex is out of range or the primitive is not supported."
	<primitive: 'primitiveDropRequestFileName' module:'DropPlugin'>
	^nil! !

!DropFilesEvent methodsFor: 'files' stamp: 'HAW 12/24/2018 09:38:25'!
fileNamesDo: aBlock

	1 to: numberOfFiles do: [ :fileNumber | | fileName |
		fileName := self primDropRequestFileName: fileNumber.
		fileName ifNotNil: aBlock ]! !

!DropFilesEvent methodsFor: 'files' stamp: 'HAW 12/24/2018 10:34:28'!
numberOfFiles

	^numberOfFiles! !


!DropFilesEvent class methodsFor: 'instance creation' stamp: 'HAW 12/24/2018 09:32:07'!
at: aPosition with: aNumberOfFiles from: aHand

	^self new initializeAt: aPosition with: aNumberOfFiles from: aHand

! !


!StandardFileStream class methodsFor: 'error handling' stamp: 'HAW 12/24/2018 09:50:28'!
readOnlyFileDoesNotExistUserHandling: fullFileName

	| dir files choices selection newName fileName |
	dir _ fullFileName asFileEntry parent.
	files _ dir fileNames.
	fileName _ fullFileName asFileEntry name.
	choices _ fileName correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	selection _ (PopUpMenu labelArray: choices lines: (Array with: 5) )
		startUpWithCaption: fullFileName asFileEntry name, '
does not exist.'.
	selection = choices size ifTrue:["cancel" ^ nil "should we raise another exception here?"].
	selection < (choices size - 1) ifTrue: [
		newName _ (dir pathName , '/', (choices at: selection))].
	selection = (choices size - 1) ifTrue: [
		newName _ FillInTheBlankMorph 
							request: 'Enter a new file name' 
							initialAnswer: fileName.
		"If Cancel was pressed, no file should be opened - Hernan"
		newName isEmpty ifTrue: [ ^nil ]].
	newName = '' ifFalse: [^ FileIOAccessor default privateReadOnlyFile: newName asFileEntry ].
	^ self error: 'Could not open a file'! !


!HandMorph methodsFor: 'event handling' stamp: 'HAW 12/24/2018 09:29:37'!
processEventQueue
	"Process user input events from the local input devices."

	| evt evtBuf type hadAny mcs |
	mcs _ mouseClickState.
	hadAny := false.
	[ (evtBuf := Sensor nextEvent) isNil ] whileFalse: [
		evt := nil.	"for unknown event types"
		type := evtBuf first.
		type = EventSensor eventTypeMouse
			ifTrue: [ evt _ self generateMouseEvent: evtBuf ] ifFalse: [
		type = EventSensor eventTypeKeyboard 
			ifTrue: [ evt _ self generateKeyboardEvent: evtBuf ] ifFalse: [
		type = EventSensor eventTypeWindow
			ifTrue: [ evt _ self generateWindowEvent: evtBuf ] ifFalse: [
		type = EventSensor eventTypeDragDropFiles
			ifTrue: [evt _ self generateDropFilesEvent: evtBuf]]]].
		"All other events are ignored"
		evt
			ifNil: [ 
				"I have to consume all eventTypeDragDropFiles of type 2 quicky, that is why
				I check if it was an eventTypeDragDropFiles to continue in the loop - Hernan"
				type ~= EventSensor eventTypeDragDropFiles ifTrue: [^hadAny]]
			ifNotNil: [
				"Finally, handle it"
				self startEventDispatch: evt.
				hadAny := true.
				"For better user feedback, return immediately after a mouse event has been processed."
				evt isMouse ifTrue: [ ^hadAny ]]].
	"note: if we come here we didn't have any mouse events"
	mcs 
		ifNotNil: [ 
			"No mouse events during this cycle. Make sure click states time out accordingly"
			mcs
				handleEvent: (self lastMouseEvent asMouseMove: (Time localMillisecondClock - self lastMouseEventTime max: 0))
				from: self ].
	^hadAny! !


!ChangeSet class methodsFor: 'services' stamp: 'HAW 12/24/2018 09:44:22'!
fileIn: aFileEntry
	"File in the entire contents of the file specified by the name provided"

	aFileEntry ifNil: [^ Smalltalk beep ].
	aFileEntry readStreamDo: [ :stream |
		stream ifNotNil: [ stream fileIn ]]! !

!methodRemoval: DropFilesEvent #inittializeAt:with:from:!
DropFilesEvent removeSelector: #inittializeAt:with:from:!
!methodRemoval: DropFilesEvent #sentTo:!
DropFilesEvent removeSelector: #sentTo:!
!methodRemoval: DropFilesEvent #type!
DropFilesEvent removeSelector: #type!
!methodRemoval: StandardFileStream #primDropRequestFileHandle:!
StandardFileStream removeSelector: #primDropRequestFileHandle:!
!methodRemoval: StandardFileStream #primDropRequestFileName:!
StandardFileStream removeSelector: #primDropRequestFileName:!

!StandardFileStream reorganize!
('open/close' close closed ensureOpen open:forWrite: openReadOnly reopen)
('properties-setting' isBinary isReadOnly readOnly readWrite useBytes useCharacters)
('access' directory fileEntry fullName isDirectory localName name peekFor: printOn: reset size)
('read, write, position' atEnd basicNext crc16 findString: findStringFromEnd: flush next next: next:into:startingAt: next:putAll:startingAt: nextPut: nextPutAll: nextWordsInto: padToEndIfCantTruncate peek peekLast position position: readInto:startingAt:count: setToEnd skip: truncate truncate: upTo: upToEnd)
('primitives' primAtEnd: primClose: primCloseNoError: primFlush: primGetPosition: primOpen:writable: primRead:into:startingAt:count: primSetPosition:to: primSize: primSizeNoError: primTruncate:to: primWrite:from:startingAt:count:)
('registry' register unregister)
('finalization' actAsExecutor finalize)
('private' collectionSpecies disableReadBuffering enableReadBuffering flushReadBuffer)
('*Compression' compressFile)
!


!PasteUpMorph reorganize!
('accessing' activeHand color:)
('alarms-scheduler' addAlarm:withArguments:for:at: removeAlarm:for:)
('caching' releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:from: redrawNeeded removedMorph:)
('classification' isWorldMorph)
('drawing' drawOn:)
('dropping/grabbing' acceptDroppingMorph:event: allowsFilesDrop allowsMorphDrop allowsSubmorphDrag dropFiles:)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('events' click:localPosition: mouseButton1Down:localPosition: windowEvent:)
('event handling testing' handlesMouseDown:)
('event handling' mouseButton2Activity wantsWindowEvent: windowEventHandler)
('geometry' externalizeDisplayBounds: externalizeToWorld: internalizeFromWorld: morphPositionInWorld privateExtent:)
('initialization' clearCanvas clearWaitDelay defaultBorderColor defaultBorderWidth defaultColor)
('interaction loop' doOneCycleNow mainLoop runProcess)
('menu & halo' addCustomMenuItems:hand: addWorldHaloMenuItemsTo:hand: deleteBalloonTarget:)
('misc' backgroundImage backgroundImageData: buildMagnifiedBackgroundImage)
('printing' printOn:)
('project state' canvas firstHand hands handsDo: handsReverseDo: setCanvas: viewBox)
('stepping' startStepping:at:selector:stepTime: stopStepping:selector: stopSteppingMorph:)
('stepping and presenter' wantsSteps)
('structure' world)
('submorphs-accessing' allMorphsDo:)
('testing' isReallyVisible stepTime)
('world menu' bringWindowsFullOnscreen closeUnchangedWindows collapseAll collapseNonWindows deleteNonWindows findAChangeSorter: findAFileList: findAMessageNamesWindow: findATranscript: findAWindowSatisfying:orMakeOneUsing: findDirtyBrowsers: findDirtyWindows: findWindow: invokeWorldMenu restoreAll)
('world state' addMorph:centeredNear: allNonWindowRelatedSubmorphs deleteAllHalos displayWorld displayWorldSafely doOneCycle doOneMinimalCycleNow fillRects: fullRepaintNeeded haloMorphs privateOuterDisplayWorld restoreDisplay whenUIinSafeState: worldState:)
('halos and balloon help' wantsHaloHandleWithSelector:inHalo:)
('object serialization' objectForDataStream:)
('windows' findATranscript)
('taskbar' hideTaskbar showTaskbar taskbar taskbarDeleted)
!


!DropFilesAction reorganize!
('initialization' initializeFor:)
('evaluating' value)
('evaluating - private' ask:onYes: canBeFiledIn: canBeInstalled: fileNamedDropped: ifUserWantsFileIn: ifUserWantsInstall:)
!


!DropFilesAction class reorganize!
('instance creation' for:)
!


!DropFilesEvent reorganize!
('initialization' initializeAt:with:from:)
('testing' isDropEvent wasHandled wasHandled:)
('dispatching' dispatchWith:localPosition: sentTo:localPosition: startDispatchFrom:)
('position' eventPosition)
('primitives' primDropRequestFileHandle: primDropRequestFileName:)
('files' fileNamesDo: numberOfFiles)
!


!DropFilesEvent class reorganize!
('instance creation' at:with:from:)
!

