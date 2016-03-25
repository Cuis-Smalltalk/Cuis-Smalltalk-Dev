'From Cuis 4.2 of 25 July 2013 [latest update: #2706] on 25 March 2016 at 12:45:02.052219 pm'!

!VersionsBrowserWindow commentStamp: 'jmv 3/25/2016 12:08' prior: 0!
Morphic view for VersionsBrowser models. See category 'GUI building'.!


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 3/25/2016 12:35'!
updatePaneIfNeeded
	| aMethod |
	aMethod _ classOfMethod compiledMethodAt: selectorOfMethod ifAbsent: [^ false].
	aMethod == currentCompiledMethod
		ifFalse: [
			"Do not attempt to formulate if there is no source pointer.
			It probably means it has been recompiled, but the source hasn't been written
			(as during a display of the 'save text simply?' confirmation)."
			aMethod last ~= 0 ifTrue: [self reformulateList]].
	^ true
! !


!ClassCommentVersionsBrowser methodsFor: 'basic function' stamp: 'jmv 3/25/2016 12:06'!
updatePaneIfNeeded
	| aComment |
	aComment _ classOfMethod organization commentRemoteStr.
	aComment == currentCompiledMethod
		ifFalse:
			["Do not attempt to formulate if there is no source pointer.
			It probably means it has been recompiled, but the source hasn't been written
			(as during a display of the 'save text simply?' confirmation)."
			aComment last ~= 0 ifTrue: [self reformulateList]].
	^ true
! !


!MorphicLocation methodsFor: 'testing' stamp: 'jmv 3/25/2016 12:24'!
is: aSymbol
	^aSymbol == #MorphicLocation or: [ super is: aSymbol ]! !


!CodeWindow methodsFor: 'updating' stamp: 'jmv 3/25/2016 12:06'!
updateListsAndCode
	"All code windows receive this message on any code change in the system"
	"quizas pensar un nombre mejor"
	self canDiscardEdits ifTrue: [
		self allMorphsDo: [ :m | (m is: #PluggableListMorph) ifTrue: [ m verifyContents ]].
		model updatePaneIfNeeded ]! !

!CodeWindow methodsFor: 'initialization' stamp: 'jmv 3/24/2016 22:55'!
model: anObject
	"Set my model and make me me a dependent of the given object."
	super model: anObject.

	"Avoid double registration"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.
	model ifNotNil: [ "model set to nil on delete"
		SystemChangeNotifier uniqueInstance
			when: #classAdded send: #updateListsAndCode to: self;
			when: #classCommented send: #updateListsAndCode to: self;
			when: #classDefinitionChanged send: #updateListsAndCode to: self;
			when: #classRecategorized send: #updateListsAndCode to: self;
			when: #classRemoved send: #updateListsAndCode to: self;
			when: #classRenamed send: #updateListsAndCode to: self;
			when: #classReorganized send: #updateListsAndCode to: self;
			when: #methodAdded send: #updateListsAndCode to: self;
			when: #methodAddedInProtocol send: #updateListsAndCode to: self;
			when: #methodChanged send: #updateListsAndCode to: self;
			when: #methodRemoved send: #updateListsAndCode to: self;
			when: #selectorRecategorized send: #updateListsAndCode to: self ]! !


!VersionsBrowserWindow methodsFor: 'updating' stamp: 'jmv 3/25/2016 12:07'!
updateListsAndCode
	"All code windows receive this message on any code change in the system"
	"quizas pensar un nombre mejor"
	model updatePaneIfNeeded! !


!ChangeSorterWindow methodsFor: 'updating' stamp: 'jmv 3/25/2016 12:18'!
updateListsAndCode
	"All code windows receive this message on any code change in the system"
	"quizas pensar un nombre mejor"
	
	"
	updateIfNecessary == updateIfNecessary
	"
	model updateIfNecessary! !


!Object methodsFor: 'stepping' stamp: 'jmv 3/25/2016 12:33'!
stepAt: millisecondSinceLast in: aWindow
	"Remember to answer true to #wantsSteps!!"
	"Ideally, a model should never know about the existance of a window on it..."
"quda solo para modelos que lo necesiten. eliminar los que se pueda."
"eliminar el arg window"! !

!Object methodsFor: 'stepping' stamp: 'jmv 3/25/2016 12:43'!
wantsSteps
	"Overridden by morphic classes whose instances want to be stepped all the time,
	or by model classes who want their morphic views to be stepped all the time.
	
	Some classes might answer false to this message, and call
		#startStepping 
		#startSteppingStepTime:
		#stopStepping
	as appropriate"

	^ false! !


!ObjectExplorer methodsFor: 'monitoring' stamp: 'jmv 3/25/2016 12:27'!
stepAt: millisecondSinceLast in: aWindow

	"If there's anything in my monitor list, see if the strings have changed."
	| string changes |
"el exprolrer USA stepping"
"true ifTrue: [ ^self ]."
	changes _ false.
	self monitorList keysAndValuesDo: [ :k :v |
		k ifNotNil: [
			k refresh.
			(string _ k asString) ~= v ifTrue: [
				self monitorList at: k put: string. changes _ true ]]].
	changes ifTrue: [ | sel |
		sel _ currentSelection.
		self changed: #getList.
		self noteNewSelection: sel ]! !


!ProcessBrowser methodsFor: 'stepping' stamp: 'jmv 3/25/2016 12:22'!
stepAt: millisecondSinceLast in: aWindow
"el process browser USA stepping"
"true ifTrue: [ ^self ]."
	self updateProcessList! !


!CodeProvider methodsFor: 'self-updating' stamp: 'jmv 3/25/2016 12:29'!
stepAt: millisecondSinceLast in: aSystemWindow
"eliminar stepping.
eliminar wantsSteps
eliminar arg window si queda alguno"
"
habilitar esto, y asegurarse que no se llama mas!!
#step print.
"
true ifTrue: [ ^self ].
	self updateListsAndCodeIn: aSystemWindow! !

!CodeProvider methodsFor: 'self-updating' stamp: 'jmv 3/25/2016 12:02'!
updateListsAndCodeIn: aWindow
"Estos metodos migran del modelo a la vista, y son llamados por eventos en vez de desde steps."
	"This is done because we are not the real model (i.e. Smalltalk). Some other browser might change our contents, without our model knowing it."
"Por ahora desactivo. Cuando no quede NINGUNO, eliminar"
true ifTrue: [ ^self ].
	aWindow canDiscardEdits ifTrue: [
		aWindow updatablePanes do: [ :aPane | aPane verifyContents ].
		self updatePaneIfNeeded ]! !

!CodeProvider methodsFor: 'self-updating' stamp: 'jmv 3/25/2016 12:34'!
updatePaneIfNeeded
"llamarlo updateIfNeeded"
	self didCodeChangeElsewhere
		ifTrue: [
			self acceptedContentsChanged]! !


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 3/25/2016 12:01'!
updateListsAndCodeIn: aWindow
	| aMethod |
"Por ahora desactivo. Cuando no quede NINGUNO, eliminar"
true ifTrue: [ ^self ].
	aMethod _ classOfMethod compiledMethodAt: selectorOfMethod ifAbsent: [^ false].
	aMethod == currentCompiledMethod
		ifFalse:
			["Do not attempt to formulate if there is no source pointer.
			It probably means it has been recompiled, but the source hasn't been written
			(as during a display of the 'save text simply?' confirmation)."
			aMethod last ~= 0 ifTrue: [self reformulateList]].
	^ true
! !


!ClassCommentVersionsBrowser methodsFor: 'basic function' stamp: 'jmv 3/25/2016 12:08'!
updateListsAndCodeIn: aWindow
	"This is done because we are not the real model (i.e. Smalltalk). Some other browser might change our contents, without our model knowing it."
	| aComment |
"Por ahora desactivo. Cuando no quede NINGUNO, eliminar"
true ifTrue: [ ^self ].
	aComment _ classOfMethod organization commentRemoteStr.
	aComment == currentCompiledMethod
		ifFalse:
			["Do not attempt to formulate if there is no source pointer.
			It probably means it has been recompiled, but the source hasn't been written
			(as during a display of the 'save text simply?' confirmation)."
			aComment last ~= 0 ifTrue: [self reformulateList]].
	^ true
! !


!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 3/25/2016 12:30'!
updateIfNecessary
	"Recompute all of my panes."
"llamarlo updateIfNeeded"
	| newList |
	myChangeSet ifNil: [^ self].  "Has been known to happen though shouldn't"
	myChangeSet isMoribund ifTrue: [
		self changed: #changeSetList.
		^ self showChangeSet: ChangeSet changeSetForBaseSystem ].

	newList _ self changeSetList.
	(priorChangeSetList == nil or: [priorChangeSetList ~= newList])
		ifTrue: [
			priorChangeSetList _ newList.
			self changed: #changeSetList ].

	newList _ self changeSetDirtyFlags.
	(priorDirtyFlags == nil or: [priorDirtyFlags ~= newList])
		ifTrue: [
			priorDirtyFlags _ newList.
			self changed: #changeSetDirtyFlags ]! !

!ChangeSorter methodsFor: 'self-updating' stamp: 'jmv 3/25/2016 12:29'!
stepAt: millisecondSinceLast in: aSystemWindow

"eliminar stepping.
eliminar wantsSteps
eliminar arg window si queda alguno"
#step print.
true ifTrue: [ ^self ].
	self updateIfNecessary! !


!Inspector methodsFor: 'updating' stamp: 'jmv 3/25/2016 12:10'!
updateListsAndCodeIn: aWindow
	"This is done because we are not the real model (i.e. Smalltalk). Some other browser might change our contents, without our model knowing it."
"Por ahora desactivo. Cuando no quede NINGUNO, eliminar"
true ifTrue: [ ^self ].
	aWindow canDiscardEdits ifFalse: [^ self].
	aWindow updatablePanes do: [:aPane | aPane verifyContents]! !

!Inspector methodsFor: 'stepping' stamp: 'jmv 3/25/2016 12:21'!
stepAt: millisecondSinceLast in: aSystemWindow
	| newText |
"el inspector USA stepping"
"true ifTrue: [ ^self ]."
	newText := self contentsIsString
		ifTrue: [self selection]
		ifFalse: ["keep it short to reduce time to compute it"
			self selectionPrintString ].
	newText = acceptedContentsCache ifFalse: [
		acceptedContentsCache _ newText.
		self acceptedContentsChanged ]! !


!MorphicTranslation methodsFor: 'comparing' stamp: 'jmv 3/25/2016 12:25'!
= aMorphicTranslation
	(aMorphicTranslation is: #MorphicLocation) ifFalse: [ ^false ].
	aMorphicTranslation isPureTranslation ifFalse: [ ^false ].
	^self translation = aMorphicTranslation translation! !


!Morph methodsFor: 'copying' stamp: 'jmv 3/25/2016 12:39'!
duplicate
	"Make and return a duplicate of the receiver"

	| newMorph |
	self okayToDuplicate ifFalse: [^ self].
	newMorph _ self copy.

	newMorph wantsSteps ifTrue: [ newMorph startStepping ].

	^ newMorph! !

!Morph methodsFor: 'initialization' stamp: 'jmv 3/25/2016 12:41'!
intoWorld: aWorld
	"The receiver has just appeared in a new world. Note:
		* aWorld can be nil (due to optimizations in other places)
		* owner is already set
		* owner's submorphs may not include receiver yet.
	Important: Keep this method fast - it is run whenever morphs are added."
	aWorld ifNil: [ ^self ].
	self wantsSteps ifTrue: [ self startStepping ].
	self submorphsDo: [ :m | m intoWorld: aWorld ]! !


!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 3/25/2016 12:41'!
startSteppingSubmorphsOf: aMorph
	"Ensure that all submorphs of the given morph that want to be stepped 
	are added to the step list. Typically used after adding a morph to the world."

	aMorph allMorphsDo: [ :m |
		m wantsSteps ifTrue: [ m startStepping ]]! !


!SystemWindow methodsFor: 'stepping' stamp: 'jmv 3/25/2016 12:42'!
wantsSteps
	"Return true if the model wants its view to be stepped.  For an open system window, we give the model to offer an opinion"

	^ model wantsSteps! !


!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 3/25/2016 12:36'!
startAutoUpdate
	self startStepping.
	isStepping _ true! !


!Preferences class methodsFor: 'standard queries' stamp: 'jmv 3/25/2016 12:28'!
smartUpdating
"borrar"
	^ self
		valueOfFlag: #smartUpdating
		ifAbsent: [ true ].! !

!methodRemoval: CodeProvider #wantsSteps!
CodeProvider removeSelector: #wantsSteps!

!MorphicLocation reorganize!
('testing' is:)
!


!VersionsBrowserWindow reorganize!
('GUI building' buildMorphicWindow optionalModelButtonTuples windowColor)
('menu building' classCommentVersionsMenu listMenu methodVersionsMenu)
('keyboard shortcuts' changeListKey:from:)
('updating' updateListsAndCode)
!


!ChangeSorterWindow reorganize!
('GUI building' buildMorphicWindow initialExtent windowColor)
('menu building' changeSetMenu classListMenu messageMenu)
('menu commands' browseMethodConflicts browseVersions)
('keyboard shortcuts' changeSetListKey:from: classListKey:from: messageListKey:from:)
('updating' updateListsAndCode)
!

