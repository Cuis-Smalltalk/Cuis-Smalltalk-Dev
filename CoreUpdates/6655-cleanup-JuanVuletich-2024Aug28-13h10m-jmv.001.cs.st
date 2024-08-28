'From Cuis7.1 [latest update: #6654] on 28 August 2024 at 1:19:07 pm'!

!DebuggerWindow methodsFor: 'initialization' stamp: 'jmv 8/28/2024 13:16:47'!
model: anObject
	"Set my model and make me me a dependent of the given object."

	super model: anObject.
	model when: #closeViews send: #closeView to: self.! !


!PreDebugWindow methodsFor: 'initialization' stamp: 'jmv 8/28/2024 13:16:55'!
model: anObject
	"Set my model and make me me a dependent of the given object."

	super model: anObject.
	model when: #closeViews send: #closeView to: self.! !


!CodeWindow methodsFor: 'initialization' stamp: 'jmv 8/28/2024 13:16:29'!
model: anObject
	"Set my model and make me me a dependent of the given object."

	super model: anObject.

	"Avoid double registration"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.
	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #classAdded: to: self;
		when: #classCommented send: #updateListsAndCode to: self;
		when: #classDefinitionChanged send: #updateListsAndCode to: self;
		when: #classRecategorized send: #updateListsAndCode to: self;
		when: #classRemoved send: #updateListsAndCode to: self;
		when: #classRenamed send: #classRenamed:from:to:inCategory: to: self;
		when: #classReorganized send: #updateListsAndCode to: self;
		when: #methodAddedInProtocol send: #updateListsAndCode to: self;
		when: #methodChanged send: #updateListsAndCode to: self;
		when: #methodRemoved send: #updateListsAndCode to: self;
		when: #selectorRecategorized send: #updateListsAndCode to: self.! !


!ChangeSet class methodsFor: 'class initialization' stamp: 'jmv 8/28/2024 13:14:05'!
initialize
	"
	ChangeSet initialize
	"
	AllChangeSets := OrderedCollection new.

	"Avoid double registration"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.
	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #classAdded:inCategory: to: self;
		when: #classCommented send: #classCommented: to: self;
		when: #classDefinitionChanged send: #classDefinitionChangedFrom:to: to: self;
		when: #classRecategorized send: #classRecategorized:from:to: to: self;
		when: #classRemoved send: #classRemoved:fromCategory: to: self;
		when: #aboutToRenameClass send: #aboutToRenameClass:from:to:inCategory: to: self;
		when: #classReorganized send: #classReorganized: to: self;
		when: #methodAddedInProtocol send: #methodAdded:selector:inProtocol:class:requestor: to: self;
		when: #methodChanged send: #methodChangedFrom:to:selector:inClass:requestor: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self;
		when: #selectorRecategorized send: #selectorRecategorized:from:to:inClass: to: self.! !

!methodRemoval: ChangeSet class #initializeNotificationActions stamp: 'jmv 8/28/2024 13:12:08'!
ChangeSet class removeSelector: #initializeNotificationActions!
!methodRemoval: ChangeSet class #registerNotificationActions stamp: 'jmv 8/28/2024 13:14:12'!
ChangeSet class removeSelector: #registerNotificationActions!
!methodRemoval: ChangeSet class #removeNotificationActions stamp: 'jmv 8/28/2024 13:13:02'!
ChangeSet class removeSelector: #removeNotificationActions!
!methodRemoval: PreDebugWindow #registerNotificationActions stamp: 'jmv 8/28/2024 13:17:12'!
PreDebugWindow removeSelector: #registerNotificationActions!
!methodRemoval: DebuggerWindow #registerNotificationActions stamp: 'jmv 8/28/2024 13:17:12'!
DebuggerWindow removeSelector: #registerNotificationActions!
!methodRemoval: CodeWindow #initializeNotificationActions stamp: 'jmv 8/28/2024 13:12:07'!
CodeWindow removeSelector: #initializeNotificationActions!
!methodRemoval: CodeWindow #registerNotificationActionsIfModelNotNil stamp: 'jmv 8/28/2024 13:14:28'!
CodeWindow removeSelector: #registerNotificationActionsIfModelNotNil!
!methodRemoval: CodeWindow #registerNotificationActions stamp: 'jmv 8/28/2024 13:17:16'!
CodeWindow removeSelector: #registerNotificationActions!
!methodRemoval: CodeWindow #removeNotificationActions stamp: 'jmv 8/28/2024 13:13:01'!
CodeWindow removeSelector: #removeNotificationActions!

!CodeWindow reorganize!
('GUI building' addShiftClickEventHandlerFor: buildLowerPanes buildMorphicAnnotationsPane buildMorphicCodePane createCodePaneMorph defaultAnnotationPaneHeight lineDiffButton lineDiffButtonHelp optionalButtonRow optionalButtonTuples prettyLineDiffButton prettyLineDiffButtonHelp prettyWordDiffButton prettyWordDiffButtonHelp wordDiffButton wordDiffButtonHelp)
('accessing' buttonWithSelector: inheritanceButton)
('updating' classAdded: classRenamed:from:to:inCategory: decorateButtons decorateForInheritance updateListsAndCode updateListsAndCodeNow)
('top window' activateAndSendTopToBack:)
('menu commands' browseClassRefs browseClassVarRefs browseFullProtocol browseHierarchy browseHierarchyAndReplaceMe browseImplementors browseImplementorsOfMessages browseInstVarRefs browseLocalImplementorsOfMessages browseLocalSenders browseLocalSendersOfMessages browseMethodFull browseMethodFullAndReplaceMe browseSenders browseSendersOfMessages browseSubImplementors browseSuperImplementors browseUnusedMethods browseVersions findMethodInChangeSets inspectInstances inspectSubInstances makeSampleInstance methodInheritance methodInheritanceAndReplaceMe openSingleMessageBrowser shiftClickPressedOn:at: showUnreferencedClassVars showUnreferencedInstVars)
('menu building' addContentsTogglesTo: offerWhatToShowMenu)
('misc' getSelectorAndSendQuery:to: getSelectorAndSendQuery:to:with: selectMessageAndEvaluate: sendQuery:to: useSelector:orGetSelectorAndSendQuery:to:)
('initialization' model:)
!


!DebuggerWindow reorganize!
('GUI building' buildLowerPanes buildMorphicWindow contextVariableInspector contextVariableInspectorText createCodePaneMorph customButtonRow customButtonSpecs initialExtent optionalButtonTuples receiverInspector receiverInspectorText stackList windowColor)
('menu commands' contextClassBrowse contextClassHierarchy contextClassProtocol exploreContextSelection exploreReceiverSelection inspectContextSelection inspectReceiverSelection receiverClassBrowse receiverClassHierarchy receiverClassProtocol returnValue shiftClickPressedOn:at:)
('open/close' closeView delete)
('user interface' ifOkToChangeCodePaneDo: okToChangeCodePane okToChangeContextVariableInspectorText okToChangeDueTo: okToChangeReceiverInspectorText)
('context stack menu' createMethod doStep fullStack peelToFirst proceed restart runToCursor send stepIntoBlock where)
('menu & kbd shortcuts' contextStackMenuSpec contextVariablesListMenuSpec receiverVariablesListMenuSpec)
('initialization' model:)
!


!PreDebugWindow reorganize!
('GUI building' buildMorphicWindowMessage: buttonRowForPreDebugWindow: initialExtent preDebugButtonSpec windowColor)
('button actions' abandon createMethod debug delete proceed)
('open/close' closeView)
('initialization' model:)
!

ChangeSet initialize!
