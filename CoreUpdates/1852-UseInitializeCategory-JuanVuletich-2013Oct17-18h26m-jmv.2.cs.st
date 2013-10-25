'From Cuis 4.2 of 25 July 2013 [latest update: #1851] on 17 October 2013 at 7:10:55.842047 pm'!

!ProtoObject methodsFor: 'initialization' stamp: 'md 11/18/2003 10:33'!
initialize
	"Subclasses should redefine this method to perform initializations on instance creation"! !


!ActiveModel class methodsFor: 'initialization' stamp: 'jmv 6/2/2011 09:36'!
flushEventSystem
	"ActiveModel flushEventSystem"
	self actionMaps keysAndValuesDo: [ :rcvr :evtDict | 
		rcvr ifNotNil:[
			"make sure we don't modify evtDict while enumerating"
			evtDict keys do: [ :evtName | | msgSet |
				msgSet _ evtDict at: evtName ifAbsent: nil.
				msgSet ifNil: [ rcvr removeActionsForEvent: evtName]]]].
	self actionMaps finalizeValues. ! !


!PointerExplorer methodsFor: 'initialization' stamp: 'jmv 9/2/2012 22:18'!
initialize
	super initialize.
	includeWeakRefs _ false! !


!CodePackageList methodsFor: 'initialization' stamp: 'jmv 3/28/2012 15:07'!
initialize
	self updatePackageList.
	CodePackage
		when: #installedPackagesChanged
		send: #updatePackageList
		to: self! !


!ProcessBrowser methodsFor: 'initialization' stamp: 'jmv 6/7/2011 17:24'!
initialize
	methodText _ ''.
	selectedContext _ nil.
	searchString _ ''.
	self updateProcessList; processListIndex: 1! !

!ProcessBrowser methodsFor: 'initialization' stamp: 'jmv 10/8/2012 23:28'!
startCPUWatcher
	"Answers whether I started the CPUWatcher"

	CPUWatcher isMonitoring ifFalse: [
		CPUWatcher startMonitoringPeriod: 1 rate: 100 threshold: 0.85 suspendPorcine: false.
		^true
	].
	^false
! !

!ProcessBrowser methodsFor: 'initialization' stamp: 'jmv 10/3/2012 11:12'!
stopCPUWatcher

	CPUWatcher stopMonitoring.
	self updateProcessList! !


!SimpleServiceEntry methodsFor: 'initialization' stamp: 'jmv 4/19/2011 11:17'!
initialize
	triggerFileListChanged _ false! !


!SystemChangeNotifier methodsFor: 'initialization' stamp: 'jmv 9/28/2009 09:52'!
initialize

	silenceLevel _ 0! !


!TestRunner methodsFor: 'initialization' stamp: 'jmv 8/23/2010 09:56'!
gatherTestNames
	^ (self testCases collect: [ :each |
		each name ]) asArray sort.! !

!TestRunner methodsFor: 'initialization' stamp: 'nk 8/6/2003 09:02'!
initialize

	result := TestResult new.
	passFail := 'N/A'.
	details := '...'.
	failures := OrderedCollection new.
	errors := OrderedCollection new.
	tests := self gatherTestNames.
	selectedSuite := 0.
	selectedFailureTest := 0.
	selectedErrorTest := 0.
	selectedSuites := tests collect: [:ea | true].
	running := nil.
	runSemaphore := Semaphore new! !

!TestRunner methodsFor: 'initialization' stamp: 'cwp 2/5/2004 21:35'!
testCases
	^ TestCase allSubclasses reject: [:cls | cls isAbstract]! !


!TextModel methodsFor: 'initialization' stamp: 'jmv 12/19/2011 14:15'!
flushUndoRedoCommands

	undoRedoCommands _ ReadWriteStream on: Array new.
	Smalltalk garbageCollect 	"To actually collect unused imported styles"! !

!TextModel methodsFor: 'initialization' stamp: 'bp 12/11/2011 03:41'!
initialize
	"Initialize the state of the receiver with its default contents."

	actualContents _ '' asText.
	self flushUndoRedoCommands! !

!TextModel methodsFor: 'initialization' stamp: 'jmv 4/17/2011 22:25'!
openLabel: aString 
	"Create a standard system view of the model, me, and open it."
	SystemWindow editText: self label: aString wrap: true! !


!PluggableTextModel methodsFor: 'initialization' stamp: 'jmv 9/20/2011 09:51'!
initWith: aTextProvider
	"aTextProvider can be a kind of TextProvider, or perhaps a more exotic object, like an ObjectExplorer or a TranscriptStream."

	textProvider _ aTextProvider.
	undoRedoCommands _ ReadWriteStream on: Array new! !


!Workspace methodsFor: 'initialization' stamp: 'jmv 8/6/2011 23:52'!
initialize
	
	super initialize.
	self initializeBindings.
	mustDeclareVariables := false! !


!Browser methodsFor: 'initialization' stamp: 'sw 1/13/2000 16:45'!
defaultBrowserTitle
	^ 'System Browser'! !

!Browser methodsFor: 'initialization' stamp: 'nk 2/13/2001 13:25'!
labelString
	^self selectedClass ifNil: [ self defaultBrowserTitle ]
		ifNotNil: [ self defaultBrowserTitle, ': ', self selectedClass printString ].
! !

!Browser methodsFor: 'initialization' stamp: 'jmv 1/2/2012 14:32'!
methodCategoryChanged
	self changed: #messageCategoryList.
	self changed: #messageList.
	self triggerEvent: #annotationChanged.
	self messageListIndex: 0! !

!Browser methodsFor: 'initialization' stamp: 'jmv 9/2/2013 09:27'!
setClass: aBehavior selector: aSymbol
	"Set the state of a new, uninitialized Browser."

	| isMeta aClass messageCatIndex |
	aBehavior ifNil: [^ self].
	aBehavior isMeta
		ifTrue: [
			isMeta _ true.
			aClass _ aBehavior soleInstance]
		ifFalse: [
			isMeta _ false.
			aClass _ aBehavior].
	self selectCategoryForClass: aClass.
	self classListIndex: (
		(SystemOrganization listAtCategoryNamed: selectedSystemCategory)
			indexOf: aClass name).
	self metaClassIndicated: isMeta.
	aSymbol ifNil: [^ self].
	messageCatIndex _ aBehavior organization numberOfCategoryOfElement: aSymbol.
	self messageCategoryListIndex: (messageCatIndex > 0
		ifTrue: [messageCatIndex + 1]
		ifFalse: [0]).
	messageCatIndex = 0 ifTrue: [^ self].
	self messageListIndex: (
		(aBehavior organization listAtCategoryNumber: messageCatIndex)
			indexOf: aSymbol).! !

!Browser methodsFor: 'initialization' stamp: 'sw 5/26/1999 23:46'!
setSelector: aSymbol
	"Make the receiver point at the given selector, in the currently chosen class"

	| aClass messageCatIndex |
	aSymbol ifNil: [^ self].
	(aClass _ self selectedClassOrMetaClass) ifNil: [^ self].
	messageCatIndex _ aClass organization numberOfCategoryOfElement: aSymbol.
	self messageCategoryListIndex: messageCatIndex + 1.
	messageCatIndex = 0 ifTrue: [^ self].
	self messageListIndex:
			((aClass organization listAtCategoryNumber: messageCatIndex)
					indexOf: aSymbol)! !

!Browser methodsFor: 'initialization' stamp: 'jmv 6/8/2011 08:47'!
systemOrganizer: aSystemOrganizer
	"Initialize the receiver as a perspective on the system organizer, 
	aSystemOrganizer. Typically there is only one--the system variable 
	SystemOrganization."

	systemOrganizer _ aSystemOrganizer.
	selectedSystemCategory _ nil.
	selectedClassName _ nil.
	selectedMessageCategory _ nil.
	selectedMessage _ nil.
	metaClassIndicated _ false.
	self setClassOrganizer.
	self editSelection: #none.! !


!CodeFileBrowser methodsFor: 'initialization' stamp: 'jmv 11/2/2011 15:22'!
defaultBrowserTitle
	^ 'File Contents Browser'! !


!Debugger methodsFor: 'initialization' stamp: 'jmv 4/15/2011 17:28'!
openFullMorphicLabel: aLabelString
	"Open a full morphic debugger with the given label"

	| oldContextStackIndex |
	oldContextStackIndex _ contextStackIndex.
	self expandStack. "Sets contextStackIndex to zero."

	DebuggerWindow open: self label: aLabelString.
	self toggleContextStackIndex: oldContextStackIndex! !

!Debugger methodsFor: 'initialization' stamp: 'jmv 5/9/2013 23:37'!
openFullNoSuspendLabel: aString
	"Create and schedule a full debugger with the given label. Do not terminate the current active process."

	self openFullMorphicLabel: aString.
	ProjectX newProcessIfUIX: interruptedProcess! !

!Debugger methodsFor: 'initialization' stamp: 'jmv 5/9/2013 23:37'!
openNotifierContents: msgString label: label
	"Create and schedule a notifier view with the given label and message. A notifier view shows just the message or the first several lines of the stack, with a menu that allows the user to open a full debugger if so desired."
	"NOTE: When this method returns, a new process has been scheduled to run the windows, and thus this notifier, but the previous active porcess has not been suspended.  The sender will do this."
	| msg |
	Sensor flushKeyboard.
	savedCursor _ Sensor currentCursor.
	Sensor currentCursor: Cursor normal.
	msg _ (label beginsWith: 'Space is low')
		ifTrue: [ self lowSpaceChoices, (msgString ifNil: ['']) ]
		ifFalse: [ msgString ].

	ProjectX newProcessIfUIX: interruptedProcess.
	WorldState addDeferredUIMessage: [
		PreDebugWindow open: self label: label message: msg ].
	^self! !

!Debugger methodsFor: 'initialization' stamp: 'jmv 6/15/2011 09:05'!
windowIsClosing
	"My window is being closed; clean up. Restart the low space watcher."

	interruptedProcess
		ifNil: [^ self].
	interruptedProcess terminate.
	interruptedProcess _ nil.
	contextStackIndex _ 0.
	contextStack _ nil.
	contextStackTop _ nil.
	receiverInspector _ nil.
	contextVariablesInspector _ nil.
	Smalltalk installLowSpaceWatcher.  "restart low space handler"
! !


!Inspector methodsFor: 'initialization' stamp: 'jmv 11/25/2010 12:33'!
initialize

	super initialize.
	acceptedContentsCache _ ''.
	selectionIndex := 0! !

!Inspector methodsFor: 'initialization' stamp: 'sd 11/20/2005 21:27'!
inspect: anObject 
	"Initialize the receiver so that it is inspecting anObject. There is no current selection.
	
	Normally the receiver will be of the correct class (as defined by anObject inspectorClass),
	because it will have just been created by sedning inspect to anObject.   However, the
	debugger uses two embedded inspectors, which are re-targetted on the current receiver
	each time the stack frame changes.  The left-hand inspector in the debugger has its
	class changed by the code here.  Care should be taken if this method is overridden to
	ensure that the overriding code calls 'super inspect: anObject', or otherwise ensures that 
	the class of these embedded inspectors are changed back."

	| c |
	c := anObject inspectorClass.
	(self class ~= c and: [self class format = c format]) ifTrue: [
		self primitiveChangeClassTo: c basicNew].
	
	"Set 'object' before sending the initialize message, because some implementations
	of initialize (e.g., in DictionaryInspector) require 'object' to be non-nil."
	
	object := anObject. 
	self initialize! !


!DictionaryInspector methodsFor: 'initialization' stamp: 'jmv 12/10/2009 23:06'!
initialize

	super initialize.
	self calculateKeyArray! !


!WeakSetInspector methodsFor: 'initialization' stamp: 'sd 11/20/2005 21:27'!
initialize
	super initialize.
	flagObject := object instVarNamed: 'flag'. ! !


!Behavior methodsFor: 'initialization' stamp: 'ar 7/15/1999 16:39'!
superclass: aClass methodDictionary: mDict format: fmt
	"Basic initialization of the receiver.
	Must only be sent to a new instance; else we would need Object flushCache."
	superclass _ aClass.
	format _ fmt.
	methodDict _ mDict.! !

!Behavior methodsFor: 'release' stamp: 'ar 9/10/1999 17:33'!
obsolete
	"Invalidate and recycle local messages,
	e.g., zap the method dictionary if can be done safely."
	self canZapMethodDictionary
		ifTrue:[ methodDict _ MethodDictionary new ].! !


!ClassDescription methodsFor: 'initialization' stamp: 'ar 11/22/1999 10:09'!
doneCompiling
	"A ClassBuilder has finished the compilation of the receiver.
	This message is a notification for a class that needs to do some
	cleanup / reinitialization after it has been recompiled."! !

!ClassDescription methodsFor: 'initialization' stamp: 'NS 4/6/2004 15:31'!
superclass: aClass methodDictionary: mDict format: fmt
	"Basic initialization of the receiver"
	super superclass: aClass methodDictionary: mDict format: fmt.
	instanceVariables _ nil.
	self organization: nil.! !

!ClassDescription methodsFor: 'initialization' stamp: 'ar 3/1/2001 23:25'!
updateInstances: oldInstances from: oldClass isMeta: isMeta
	"Recreate any existing instances of the argument, oldClass, as instances of the receiver, which is a newly changed class. Permute variables as necessary. Return the array of old instances (none of which should be pointed to legally by anyone but the array)."
	"If there are any contexts having an old instance as receiver it might crash the system because the layout has changed, and the method only knows about the old layout."
	| map variable instSize newInstances |

	oldInstances isEmpty ifTrue:[^#()]. "no instances to convert"
	isMeta ifTrue: [
		oldInstances size = 1 ifFalse:[^self error:'Metaclasses can only have one instance'].
		self soleInstance class == self ifTrue:[
			^self error:'Metaclasses can only have one instance']].
	map _ self instVarMappingFrom: oldClass.
	variable _ self isVariable.
	instSize _ self instSize.
	newInstances _ Array new: oldInstances size.
	1 to: oldInstances size do:[:i|
		newInstances at: i put: (
			self newInstanceFrom: (oldInstances at: i) variable: variable size: instSize map: map)].
	"Now perform a bulk mutation of old instances into new ones"
	oldInstances elementsExchangeIdentityWith: newInstances.
	^newInstances "which are now old"! !

!ClassDescription methodsFor: 'initialization' stamp: 'jmv 2/17/2010 23:00'!
updateInstancesFrom: oldClass
	"Recreate any existing instances of the argument, oldClass, as instances of 
	the receiver, which is a newly changed class. Permute variables as 
	necessary. Return the array of old instances (none of which should be
	pointed to legally by anyone but the array)."
	"ar 7/15/1999: The updating below is possibly dangerous. If there are any
	contexts having an old instance as receiver it might crash the system if
	the new receiver in which the context is executed has a different layout.
	See bottom below for a simple example:"
	| oldInstances |
	oldInstances _ oldClass allInstances asArray.
	^self updateInstances: oldInstances from: oldClass isMeta: self isMeta.

"	| crashingBlock class |
	class _ Object subclass: #CrashTestDummy
		instanceVariableNames: 'instVar'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Crash-Test'.
	class compile:'instVar: value instVar _ value'.
	class compile:'crashingBlock ^[instVar]'.
	crashingBlock _ (class new) instVar: 42; crashingBlock.
	Object subclass: #CrashTestDummy
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Crash-Test'.
	crashingBlock.
	crashingBlock value.
	"
! !

!ClassDescription methodsFor: 'release' stamp: 'NS 4/6/2004 15:32'!
obsolete
	"Make the receiver obsolete."
	superclass removeSubclass: self.
	self organization: nil.
	super obsolete.! !


!Class methodsFor: 'initialization' stamp: 'jmv 3/2/2010 16:52'!
declare: varString 
	"Declare class variables common to all instances. Answer whether 
	recompilation is advisable."

	| newVars conflicts |
	newVars _ 
		(Smalltalk actualScannerClass new scanFieldNames: varString)
			collect: [:x | x asSymbol].
	newVars do: [:var | 
		var first isLowercase
			ifTrue: [self error: var, ' class variable name should be capitalized; proceed to include anyway.']].
	conflicts _ false.
	classPool
		ifNotNil: [
			(classPool keys reject: [:x | newVars includes: x]) 
				do: [:var | self removeClassVarName: var]].
	(newVars reject: [:var | self classPool includesKey: var])
		do: [:var | "adding"
			"check if new vars defined elsewhere"
			(self bindingOf: var)
				ifNotNil: [ 
					self error: var , ' is defined elsewhere'.
					conflicts _ true]].
	newVars size > 0
		ifTrue: [
			classPool _ self classPool.
			"in case it was nil"
			newVars do: [:var | classPool declare: var from: Undeclared]].
	^conflicts! !

!Class methodsFor: 'initialization' stamp: 'jmv 3/13/2012 23:13'!
sharing: poolString 
	"Set up sharedPools. Answer whether recompilation is advisable."
	| oldPools |
	oldPools _ self sharedPools.
	sharedPools _ OrderedCollection new.
	(Smalltalk actualScannerClass new scanFieldNames: poolString) do: 
		[:poolName | 
		sharedPools add: (Smalltalk at: poolName asSymbol ifAbsent:[
			(self confirm: 'The pool dictionary ', poolName,' does not exist.',
						'\Do you want it automatically created?' withNewLines)
				ifTrue:[Smalltalk at: poolName asSymbol put: Dictionary new]
				ifFalse:[^self error: poolName,' does not exist']])].
	sharedPools isEmpty ifTrue: [sharedPools _ nil].
	^oldPools anySatisfy: [ :pool |
		self sharedPools noneSatisfy: [ :p | p == pool ]]! !

!Class methodsFor: 'initialization' stamp: 'NS 4/6/2004 15:32'!
superclass: sup methodDict: md format: ft name: nm organization: org instVarNames: nilOrArray classPool: pool sharedPools: poolSet 
	"Answer an instance of me, a new class, using the arguments of the 
	message as the needed information.
	Must only be sent to a new instance; else we would need Object flushCache."

	superclass _ sup.
	methodDict _ md.
	format _ ft.
	name _ nm.
	instanceVariables _ nilOrArray.
	classPool _ pool.
	sharedPools _ poolSet.
	self organization: org.! !

!Class methodsFor: 'initialization' stamp: 'ar 7/20/1999 11:23'!
superclass: aClass methodDictionary: mDict format: fmt
	"Basic initialization of the receiver"
	super superclass: aClass methodDictionary: mDict format: fmt.
	subclasses _ nil. "Important for moving down the subclasses field into Class"
! !

!Class methodsFor: 'release' stamp: 'ar 9/10/1999 17:34'!
obsolete
	"Change the receiver and all of its subclasses to an obsolete class."
	self == Object 
		ifTrue:[^self error:'Object is NOT obsolete'].
	name _ 'AnObsolete' , name.
	Object class instSize + 1 to: self class instSize do:
		[:i | self instVarAt: i put: nil]. "Store nil over class instVars."
	classPool _ nil.
	sharedPools _ nil.
	self class obsolete.
	super obsolete.
! !

!Class methodsFor: 'release' stamp: 'ar 3/1/2001 22:28'!
removeFromSystem
	"Forget the receiver from the Smalltalk global dictionary. Any existing 
	instances will refer to an obsolete version of the receiver."
	self removeFromSystem: true.! !

!Class methodsFor: 'release' stamp: 'jmv 3/13/2009 09:54'!
removeFromSystem: logged
	"Forget the receiver from the Smalltalk global dictionary. Any existing 
	instances will refer to an obsolete version of the receiver."
	
	"keep the class name and category for triggering the system change message. If we wait to long, then we get obsolete information which is not what we want."

	"tell class to unload itself"
	self unload.
	self superclass ifNotNil:
		["If we have no superclass there's nothing to be remembered"
		self superclass addObsoleteSubclass: self].
	Smalltalk forgetClass: self logged: logged.
	self obsolete.! !

!Class methodsFor: 'release' stamp: 'ar 3/1/2001 22:29'!
removeFromSystemUnlogged
	"Forget the receiver from the Smalltalk global dictionary. Any existing instances will refer to an obsolete version of the receiver.  Do not log the removal either to the current change set nor to the system changes log"
	^self removeFromSystem: false! !

!Class methodsFor: 'release' stamp: 'mir 9/25/2008 15:05'!
unload
	"Sent when a the class is removed.  Does nothing, but may be overridden by (class-side) subclasses."
	""
! !


!Metaclass methodsFor: 'initialization' stamp: 'ar 7/13/1999 04:52'!
adoptInstance: oldInstance from: oldMetaClass 
	"Recreate any existing instances of the argument, oldClass, as instances of 
	the receiver, which is a newly changed class. Permute variables as 
	necessary."
	thisClass class == self ifTrue:[^self error:'Metaclasses have only one instance'].
	oldMetaClass isMeta ifFalse:[^self error:'Argument must be Metaclass'].
	oldInstance class == oldMetaClass ifFalse:[^self error:'Not the class of argument'].
	^thisClass _ self 
		newInstanceFrom: oldInstance 
		variable: self isVariable 
		size: self instSize 
		map: (self instVarMappingFrom: oldMetaClass)! !

!Metaclass methodsFor: 'initialization' stamp: 'jmv 4/6/2008 23:12'!
instanceVariableNames: instVarString 
	"Declare additional named variables for my instance."
	| answer |
	answer _ ClassBuilder new
		class: self
		instanceVariableNames: instVarString.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer! !


!Pen methodsFor: 'initialization' stamp: 'jm 4/28/1998 04:02'!
defaultNib: widthInteger 
	"Nib is the tip of a pen. This sets up the pen, with a nib of width widthInteger. You can also set the shape of the pen nib using:
		roundNib: widthInteger, or
		squareNib: widthInteger, or
		sourceForm: aForm"
"Example:
	| bic |
	bic _ Pen new sourceForm: Cursor normal.
	bic combinationRule: Form paint; turn: 90.
	10 timesRepeat: [bic down; go: 3; up; go: 10]."

	self color: Color black.
	self squareNib: widthInteger.
! !

!Pen methodsFor: 'initialization' stamp: 'jm 4/28/1998 04:03'!
roundNib: diameter
	"Makes this pen draw with a round dot of the given diameter."

	self sourceForm: (Form dotOfSize: diameter).
	combinationRule _ Form paint.
! !

!Pen methodsFor: 'initialization' stamp: 'jm 4/28/1998 04:03'!
squareNib: widthInteger 
	"Makes this pen draw with a square nib of the given width."

	self sourceForm: (Form extent: widthInteger @widthInteger) fillBlack.
	self combinationRule: Form over.  "a bit faster than paint mode"
! !


!BlockClosure methodsFor: 'initialization' stamp: 'eem 9/3/2008 14:08'!
outerContext: aContext startpc: aStartpc numArgs: argCount copiedValues: anArrayOrNil
	outerContext := aContext.
	startpc := aStartpc.
	numArgs := argCount.
	1 to: self numCopiedValues do:
		[:i|
		self at: i put: (anArrayOrNil at: i)]! !


!ChangeSet methodsFor: 'testing'!
isMoribund
	"Answer whether the receiver is obsolete and about to die; part of an effort to get such guys cleared out from the change sorter.  2/7/96 sw"

	^ name == nil ! !

!ChangeSet methodsFor: 'initialization' stamp: 'jmv 4/25/2012 10:24'!
clear 
	"Reset the receiver to be empty.  "

	changeRecords _ Dictionary new.
	preamble _ nil.
	postscript _ nil.
	self hasUnsavedChanges: false.
	self isForBaseSystem: true 	"Not a great default, but at least some Boolean"! !

!ChangeSet methodsFor: 'initialization' stamp: 'jmv 2/23/2010 00:11'!
initialize 
	"Initialize the receiver to be empty."

	name ifNil:
		[^ self error: 'All changeSets must be registered, as in ChangeSorter newChangeSet'].
	self clear.
! !

!ChangeSet methodsFor: 'initialization' stamp: 'di 3/23/2000 12:14'!
wither
	"The receiver is to be clobbered.  Clear it out.  2/7/96 sw"

	self clear.
	name _ nil! !


!CharacterScanner methodsFor: 'initialization' stamp: 'jmv 9/23/2010 08:32'!
initialize
	destX _ destY _ leftMargin _ rightMargin _ 0.! !


!ClassBuilder methodsFor: 'initialization' stamp: 'ar 3/3/2001 00:29'!
doneCompiling: aClass
	"The receiver has finished modifying the class hierarchy.
	Do any necessary cleanup."
	aClass doneCompiling.
	Behavior flushObsoleteSubclasses.! !

!ClassBuilder methodsFor: 'initialization' stamp: 'jmv 11/27/2008 16:00'!
initialize
	instVarMap _ IdentityDictionary new.! !


!Clipboard methodsFor: 'initialization' stamp: 'jmv 1/18/2011 22:22'!
initialize
	contents _ nil.
	recent _ OrderedCollection new.! !


!CodeFile methodsFor: 'initialization' stamp: 'jmv 10/10/2012 13:56'!
fromFileNamed: aName
	| stream |
	fullName _ aName.
	stream _ FileStream readOnlyFileNamed: aName.
	[ self buildFrom: stream ] ensure: [ stream close ]! !

!CodeFile methodsFor: 'initialization' stamp: 'jmv 11/2/2011 15:27'!
initialize
	classes _ Dictionary new.
	classOrder _ OrderedCollection new.
	sourceSystem _ ''.
	doIts _ OrderedCollection new! !


!CodePackageFile methodsFor: 'initialization' stamp: 'jmv 7/14/2013 17:25'!
buildFileStream: aFileStream packageName: pkName fullName: fullFileName
	"Just build the CodePackageFile object. Don't install the code."

	| classesDefined classesExtended classesToDeleteButCant classesToReallyDelete packageInMemory |
	packageName _ pkName.
	fullName _ fullFileName.
	"This will most likely be updated with the actual data from the file, calling #provides:"
	featureSpec provides: (Feature name: pkName version: 1 revision: 0).
	"Don't register a package!!"
	packageInMemory _ CodePackage
		named: packageName
		createIfAbsent: true
		registerIfNew: false.
	self buildFrom: aFileStream.
	"Compute stuff no longer in package: Should be removed from system."
	classesDefined _ Set new.
	classesExtended _ Set new.
	classes do: [ :pseudoClass |
		pseudoClass hasDefinition
			ifTrue: [ classesDefined add: pseudoClass name ]
			ifFalse: [ classesExtended add: pseudoClass name ]].
	classesToRemove _ packageInMemory classNames difference: classesDefined.
	"Add here:
		- classes in classesToDelete, that #allCallsOn answers selectors that aren't in classesToDelete or methodsToRemove
		- classes with #subclasses that aren't in classesToDelete.
		- classes with existing instances (#instanceCount)? Not really sure... Maybe sole instance referenced from classVar or such...
		- something else I forgot?
	Warning: This search for stuff that can't be removed must be iterated again until it doesn't find any more."
	classesToDeleteButCant _ classesToRemove intersection: classesExtended.
	classesToReallyDelete _ classesToRemove difference: classesToDeleteButCant.
	"Methods. Could also mean classes that can't be deleted!! (include in the iteration)
	Warn if deleting last implementor of sent messages?"
	methodsToRemove _ packageInMemory methods asSet difference: self allMethodReferences.
	methodsToRemove _ methodsToRemove reject: [ :methodReference | classesToReallyDelete includes: methodReference classSymbol ].

"
	'=============' print.
	('classesToRemove: ', classesToRemove printString) print.
	('classesToDeleteButCant: ', classesToDeleteButCant printString) print.
	('classesToReallyDelete: ', classesToReallyDelete printString) print.
	'=============' print.
	'methodsToRemove: ' print.
	methodsToRemove do: [ :methodReference | methodReference print ].
	'=============' print.
"! !

!CodePackageFile methodsFor: 'initialization' stamp: 'jmv 7/7/2013 22:36'!
initialize
	super initialize.
	description _ ''.
	featureSpec _ FeatureSpec new! !

!CodePackageFile methodsFor: 'initialization' stamp: 'jmv 3/21/2012 17:38'!
installFileStream: aFileStream packageName: pkName fullName: fullFileName

	self buildFileStream: aFileStream packageName: pkName fullName: fullFileName.
	aFileStream reset.
	self install: aFileStream! !


!Bitmap methodsFor: 'initialization' stamp: 'ar 12/23/1999 14:35'!
fromByteStream: aStream 
	"Initialize the array of bits by reading integers from the argument, 
	aStream."
	aStream nextWordsInto: self! !


!ByteArray methodsFor: 'initialization' stamp: 'KenD 8/10/2013 09:01'!
readHexFrom: aStream
	"Initialize the receiver from a hexadecimal string representation
		ByteArray readHexFrom: '1E1e'
		ByteArray readHexFrom: '1e1E'
		ByteArray readHexFrom: '1e1e'
	Note: lowercase hex digits are supported here, because we are reading strictly bytes.
	But are not really general, because lowercase e is used to denote scientific notation:
		16r1e0 = 1
		16r1E0 = 480
	"
	| map v ch value |
	map := '0123456789ABCDEF'.
	1 to: self size do: [ :i |
		ch := aStream next asUppercase.
		v := (map indexOf: ch) - 1.
		 (v between: 0 and: 15) ifFalse: [ ^self error: 'Hex digit expected' ].
		value := v bitShift: 4.
		ch := aStream next asUppercase.
		v := (map indexOf: ch) - 1.
		(v between: 0 and: 15) ifFalse: [ ^self error: 'Hex digit expected' ].
		value := value + v.
		self at: i put: value.
	]! !


!CompiledMethod methodsFor: 'initialization'!
copyWithTrailerBytes: bytes
"Testing:
	(CompiledMethod compiledMethodAt: #copyWithTrailerBytes:)
		tempNamesPut: 'copy end '
"
	| copy end start |
	start _ self initialPC.
	end _ self endPC.
	copy _ CompiledMethod newMethod: end - start + 1 + bytes size
				header: self header.
	1 to: self numLiterals do: [:i | copy literalAt: i put: (self literalAt: i)].
	start to: end do: [:i | copy at: i put: (self at: i)].
	1 to: bytes size do: [:i | copy at: end + i put: (bytes at: i)].
	^ copy! !

!CompiledMethod methodsFor: 'initialization' stamp: 'jmv 3/2/2011 16:56'!
needsFrameSize: newFrameSize
	"Set the largeFrameBit to accomodate the newFrameSize"
	| largeFrameBit header |
	largeFrameBit _ 16r20000.
	(self numTemps + newFrameSize) > LargeFrame ifTrue:
		[^ self error: 'Cannot compile -- stack including temps is too deep'].
	header _ self objectAt: 1.
	(header bitAnd: largeFrameBit) ~= 0
		ifTrue: [header _ header - largeFrameBit].
	self objectAt: 1 put: header
 			+ ( ((self numTemps + newFrameSize) > SmallFrame or: [ self primitive = 84 "perform:withArguments:"])
					ifTrue: [largeFrameBit]
					ifFalse: [0])! !


!Semaphore methodsFor: 'initialization'!
initSignals
	"Consume any excess signals the receiver may have accumulated."

	excessSignals _ 0.! !

!Semaphore methodsFor: 'initialization'!
terminateProcess
	"Terminate the process waiting on this semaphore, if any."

	self isEmpty ifFalse: [ self removeFirst terminate ].! !


!CuisSourceFileArray methodsFor: 'initialization' stamp: 'jmv 5/26/2011 08:37'!
initialize
	files _ Array new: 2.
	files at: 1 put: (SourceFiles at: 1).
	files at: 2 put: (SourceFiles at: 2).
	pointerScale _ 32	"To change the pointerScale (and therefore the size limit for Sources and Changes, change this value, and use a MigratingSourceFileArray to create the new Sources and Changes files. See class comment there."! !

!CuisSourceFileArray methodsFor: 'initialization' stamp: 'jmv 5/22/2011 23:34'!
initialize: nFiles
	files _ Array new: nFiles.
	pointerScale _ 32! !


!StandardSourceFileArray methodsFor: 'initialization' stamp: 'hmm 4/25/2000 21:20'!
initialize
	files _ Array new: 2.
	files at: 1 put: (SourceFiles at: 1).
	files at: 2 put: (SourceFiles at: 2)! !

!StandardSourceFileArray methodsFor: 'initialization' stamp: 'ar 5/17/2000 18:28'!
initialize: nFiles
	files _ Array new: nFiles! !


!StandardSourceFileArray class methodsFor: 'initialization' stamp: 'nk 7/30/2004 21:50'!
install
	"Replace SourceFiles by an instance of me with the standard sources and changes files.
	This only works if SourceFiles is either an Array or an instance of this class"

	"StandardSourceFileArray install"

	SourceFiles := self new! !

!StandardSourceFileArray class methodsFor: 'initialization' stamp: 'ar 5/17/2000 18:27'!
new: nFiles
	^self new initialize: nFiles.! !


!WeakRegistry methodsFor: 'initialization' stamp: 'jmv 3/10/2010 10:22'!
initialize: n
	valueDictionary _ WeakIdentityKeyDictionary new: n.
	accessLock _ Semaphore forMutualExclusion! !


!DebuggerMethodMap methodsFor: 'initialization' stamp: 'eem 6/5/2008 09:21'!
forMethod: aMethod "<CompiledMethod>" methodNode: theMethodNode "<MethodNode>"
	methodReference := WeakArray with: aMethod.
	methodNode := theMethodNode.
	self markRecentlyUsed! !


!DebuggerMethodMapForBlueBookMethods methodsFor: 'initialization' stamp: 'eem 6/5/2008 10:34'!
forMethod: aMethod "<CompiledMethod>" methodNode: aMethodNode "<MethodNode>"
	super forMethod: aMethod methodNode: aMethodNode.
	tempNames := methodNode encoder tempNames! !


!Form methodsFor: 'initialization'!
fromDisplay: aRectangle 
	"Create a virtual bit map from a user specified rectangular area on the 
	display screen. Reallocates bitmap only if aRectangle ~= the receiver's 
	extent."

	(width = aRectangle width and: [height = aRectangle height])
		ifFalse: [self setExtent: aRectangle extent depth: depth].
	self
		copyBits: (aRectangle origin extent: self extent)
		from: Display
		at: 0 @ 0
		clippingBox: self boundingBox
		rule: Form over
		fillColor: nil! !

!Form methodsFor: 'initialization' stamp: 'ar 6/16/2002 18:39'!
swapEndianness
	"Swap from big to little endian pixels and vice versa"
	depth := 0 - depth.! !

!Form methodsFor: 'system startup' stamp: 'ar 5/28/2000 18:45'!
shutDown
	"The system is going down. Try to preserve some space"
	self hibernate! !


!DisplayScreen methodsFor: 'system startup' stamp: 'ar 5/28/2000 11:25'!
shutDown 
	"Minimize Display memory saved in image"
	self setExtent: 240@120 depth: depth! !


!SimpleEditor methodsFor: 'initialization' stamp: 'jmv 11/4/2008 13:08'!
changeString: aString
	"Install aString as the one to be edited by the receiver."

	string _ aString.
	self resetState! !

!SimpleEditor methodsFor: 'initialization' stamp: 'jmv 11/4/2008 13:42'!
initialize
	"Initialize the state of the receiver. Subclasses should include 'super 
	initialize' when redefining this message to insure proper initialization."

	super initialize.
	self changeString: ''! !

!SimpleEditor methodsFor: 'initialization' stamp: 'jmv 10/16/2013 22:37'!
resetState 
	"Establish the initial conditions for editing the paragraph: place text cursor 
	before first character, set the emphasis to that of the first character,
	and save the paragraph for purposes of canceling."

	pointIndex _ 1.	"Like pointBlock in TextEditor"
	markIndex _ 1.	"Like markBlock in TextEditor"! !


!TextEditor methodsFor: 'initialization' stamp: 'jmv 12/29/2011 13:16'!
initialize
	selectionStartBlocks _ #().
	selectionStopBlocks _ #().
	initialSelectionStart _ nil.
	initialSelectionStop _ nil.
	doWordSelection _ false.
	doParagraphSelection _ false! !

!TextEditor methodsFor: 'initialization' stamp: 'jmv 10/16/2013 22:39'!
resetState 
	"Establish the initial conditions for editing the paragraph: place text cursor 
	before first character and set the emphasis to that of the first character"

	markBlock _ textComposition defaultCharacterBlock.
	pointBlock _ markBlock.
	self setEmphasisHereFromText.
	selectionStartBlocks _ #().
	selectionStopBlocks _ #()! !

!TextEditor methodsFor: 'initialization' stamp: 'jmv 10/16/2013 20:10'!
textComposition: aTextComposition 
	"Install aTextComposition as the one to be edited by the receiver."

	textComposition _ aTextComposition! !


!EllipseMidpointTracer methodsFor: 'initialization' stamp: 'ar 6/28/1999 15:33'!
on: aRectangle
	rect _ aRectangle.
	a _ rect width // 2.
	b _ rect height // 2.
	x _ 0.
	y _ b.
	aSquared _ a * a.
	bSquared _ b * b.
	d1 _ bSquared - (aSquared * b) + (0.25 * aSquared).
	d2 _ nil.
	inFirstRegion _ true.! !


!FileDoesNotExistException methodsFor: 'initialization' stamp: 'jmv 7/29/2013 23:46'!
initialize
	super initialize.
	readOnly _ false! !


!MessageNotUnderstood methodsFor: 'initialization' stamp: 'eem 12/19/2008 10:41'!
initialize
	super initialize.
	reachedDefaultHandler := false! !


!NotYetImplemented methodsFor: 'initialization' stamp: 'jcg 10/21/2009 00:41'!
receiverClass: cls selector: sel
	receiverClass := cls.
	selector := sel.! !


!UndeclaredVariableWarning methodsFor: 'initialization' stamp: 'ar 2/13/2010 15:21'!
defaultResumeValue
	"Answer the value that by default should be returned if the exception is resumed"
	^true! !

!UndeclaredVariableWarning methodsFor: 'initialization' stamp: 'eem 7/27/2008 17:37'!
name: aString selector: aSymbolOrNil class: aBehavior
	name := aString.
	selector := aSymbolOrNil.
	class := aBehavior! !


!ExternalSemaphoreTable class methodsFor: 'initialization' stamp: 'JMM 6/6/2000 20:32'!
initialize
	ProtectTable _ Semaphore forMutualExclusion! !


!Feature methodsFor: 'initialization' stamp: 'KenD 6/26/2013 16:54'!
name: theName version: theVersion revision: theRevision

	name     := theName.
	version  := theVersion.
	revision := theRevision! !


!FeatureRequirement methodsFor: 'initialization' stamp: 'jmv 7/28/2013 23:54'!
name: featureName minVersion: minVer minRevision: rev maxVersion: maxVer

	name := featureName asSymbol.
	minVersion := minVer.
	minRevision := rev.
	maxVersion := maxVer! !


!FeatureSpec methodsFor: 'initialization' stamp: 'jmv 7/7/2013 22:36'!
initialize
	requires _ Set new! !

!FeatureSpec methodsFor: 'initialization' stamp: 'KenD 6/26/2013 19:20'!
provides: providesFeatureSpec requires: setOfRequiresFeatureSpec

	provides := providesFeatureSpec.
	requires := setOfRequiresFeatureSpec.! !


!InputSensor methodsFor: 'initialization' stamp: 'ar 9/26/2000 21:35'!
flushEvents
	"Do nothing"! !

!InputSensor methodsFor: 'initialization' stamp: 'nk 4/12/2004 19:45'!
shutDown
	InterruptWatcherProcess ifNotNil: [
		InterruptWatcherProcess terminate.
		InterruptWatcherProcess _ nil ].! !

!InputSensor methodsFor: 'initialization' stamp: 'ar 7/23/2000 00:00'!
startUp
	self installInterruptWatcher.! !


!EventSensor methodsFor: 'initialization' stamp: 'jmv 7/23/2013 23:35'!
initialize
	"Initialize the receiver"
	mouseButtons := 0.
	mousePosition := 0 @ 0.
	self setInterruptKey: (interruptKey ifNil: [$. asciiValue bitOr: 16r0800 ]). 	"cmd-."
	interruptSemaphore := (Smalltalk specialObjectsArray at: 31) ifNil: [Semaphore new].
	self flushAllButDandDEvents.
	inputSemaphore := Semaphore new.
	hasInputSemaphore := false.! !

!EventSensor methodsFor: 'initialization' stamp: 'nk 4/12/2004 20:13'!
shutDown
	super shutDown.
	EventTicklerProcess ifNotNil: [
		EventTicklerProcess terminate.
		EventTicklerProcess _ nil. ].
	inputSemaphore ifNotNil:[Smalltalk unregisterExternalObject: inputSemaphore].
! !

!EventSensor methodsFor: 'initialization' stamp: 'jmv 2/12/2008 19:04'!
startUp
	"Run the I/O process"
	self initialize.
	self primSetInputSemaphore: (Smalltalk registerExternalObject: inputSemaphore).
	super startUp.
	self installEventTickler.
	self flushAllButDandDEvents.

	"Attempt to discover whether the input semaphore is actually being signaled."
	hasInputSemaphore := false.
	inputSemaphore initSignals.
! !


!BlockLocalTempCounter methodsFor: 'initialization' stamp: 'jmv 8/5/2011 19:31'!
tempCountForBlockAt: pc in: method
	"Compute the number of local temporaries in a block.
	 If the block begins with a sequence of push: nil bytecodes then some of
	 These could be initializing local temps.  We can only reliably disambuguate
	 them from other uses of nil by parsing the stack and seeing what the offset
	 of the stack pointer is at the end of the block.

	 There are short-cuts.  The ones we take here are
		- if there is no sequence of push nils there can be no local temps
		- we follow forward jumps to shorten the amount of scanning"
	stackPointer := 0.
	scanner := InstructionStream new method: method pc: pc.
	scanner interpretNextInstructionFor: self.
	blockEnd ifNil:
		[self error: 'pc is not that of a block'].
	scanner nextByte = Encoder pushNilCode ifTrue:
		[joinOffsets := Dictionary new.
		 [scanner pc < blockEnd] whileTrue:
			[scanner interpretNextInstructionFor: self]].
	^stackPointer! !

!BlockLocalTempCounter methodsFor: 'initialization' stamp: 'jmv 3/13/2012 12:16'!
testTempCountForBlockAt: startPc in: method
	"Compute the number of local temporaries in a block.
	 If the block begins with a sequence of push: nil bytecodes then some of
	 These could be initializing local temps.  We can only reliably disambuguate
	 them from other uses of nil by parsing the stack and seeing what the offset
	 of the stack pointer is at the end of the block.There are short-cuts.  The only
	 one we take here is
		- if there is no sequence of push nils there can be no local temps"

	| symbolicLines line prior thePc |
	symbolicLines := Dictionary new.
	method symbolicLinesDo:
		[:pc :lineForPC| symbolicLines at: pc put: lineForPC].
	stackPointer := 0.
	scanner := InstructionStream new method: method pc: startPc.
	scanner interpretNextInstructionFor: self.
	blockEnd ifNil:
		[self error: 'pc is not that of a block'].
	scanner nextByte = Encoder pushNilCode ifTrue:
		[joinOffsets := Dictionary new.
		 [scanner pc < blockEnd] whileTrue:
			[line := symbolicLines at: scanner pc.
			 prior := stackPointer.
			 thePc := scanner pc.
			 scanner interpretNextInstructionFor: self.
			 Transcript newLine; print: prior; nextPutAll: '->'; print: stackPointer;  tab; print: thePc; tab; nextPutAll: line]].
	^stackPointer! !


!BlockStartLocator methodsFor: 'initialization' stamp: 'eem 5/31/2008 13:43'!
initialize
	nextJumpIsAroundBlock := false! !


!InstVarRefLocator methodsFor: 'initialization' stamp: 'md 4/8/2003 11:35'!
interpretNextInstructionUsing: aScanner 
	
	bingo _ false.
	aScanner interpretNextInstructionFor: self.
	^bingo! !


!InstructionPrinter methodsFor: 'initialization' stamp: 'ajh 2/9/2003 14:16'!
indent: numTabs

	indent _ numTabs! !

!InstructionPrinter methodsFor: 'initialization' stamp: 'eem 5/29/2008 13:26'!
printInstructionsOn: aStream 
	"Append to the stream, aStream, a description of each bytecode in the
	 instruction stream."
	
	| end |
	stream := aStream.
	scanner := InstructionStream on: method.
	end := method endPC.
	oldPC := scanner pc.
	innerIndents := Array new: end withAll: 0.
	[scanner pc <= end] whileTrue:
		[scanner interpretNextInstructionFor: self]! !

!InstructionPrinter methodsFor: 'initialization' stamp: 'eem 8/4/2008 16:26'!
printInstructionsOn: aStream do: aBlock
	"Append to the stream, aStream, a description of each bytecode in the
	 instruction stream. Evaluate aBlock with the receiver, the scanner and
	 the stream after each instruction."

	| end |
	stream := aStream.
	scanner := InstructionStream on: method.
	end := method endPC.
	oldPC := scanner pc.
	innerIndents := Array new: end withAll: 0.
	[scanner pc <= end] whileTrue:
		[scanner interpretNextInstructionFor: self.
		 aBlock value: self value: scanner value: stream]! !


!RelativeInstructionPrinter methodsFor: 'initialization' stamp: 'eem 5/15/2008 10:56'!
printCode: aBoolean
	printCode := aBoolean! !


!BlockContext methodsFor: 'initialization' stamp: 'ls 6/21/2000 17:42'!
home: aContextPart startpc: position nargs: anInteger 
	"This is the initialization message. The receiver has been initialized with 
	the correct size only."

	home _ aContextPart.
	pc _ startpc _ position.
	nargs _ anInteger.
	stackp _ 0.! !

!BlockContext methodsFor: 'initialization' stamp: 'ajh 7/18/2003 21:49'!
privRefresh
	"Reinitialize the receiver so that it is in the state it was at its creation."

	pc := startpc.
	self stackp: 0.
	nargs timesRepeat: [  "skip arg popping"
		self nextInstruction selector = #popIntoTemporaryVariable:
			ifFalse: [self halt: 'unexpected bytecode instruction']
	].
! !


!MethodContext methodsFor: 'initialization' stamp: 'eem 9/10/2009 15:49'!
privRefresh
	"Reinitialize the receiver so that it is in the state it was at its creation."

	closureOrNil
		ifNotNil:
			[pc := closureOrNil startpc.
			self stackp: closureOrNil numArgs + closureOrNil numCopiedValues.
			1 to: closureOrNil numCopiedValues do:
				[:i | self tempAt: closureOrNil numArgs + i put: (closureOrNil at: i)]]
		ifNil:
			[pc := method initialPC.
			self stackp: method numTemps.
			method numArgs+1 to: method numTemps do:
				[:i | self tempAt: i put: nil]]! !

!MethodContext methodsFor: 'initialization' stamp: 'jmv 1/12/2010 11:48'!
privRefreshWith: aCompiledMethod 
	"Reinitialize the receiver as though it had been for a different method. 
	 Used by a Debugger when one of the methods to which it refers is 
	 recompiled."

	(aCompiledMethod is: #CompiledMethod) ifFalse: [
		self error: 'method can only be set to aCompiledMethod'].
	method := aCompiledMethod.
	self assert: closureOrNil == nil.
	"was: receiverMap := nil."
	self privRefresh! !


!Decompiler methodsFor: 'initialization' stamp: 'jmv 12/17/2012 09:36'!
initSymbols: aClass
	| argCount |
	constructor method: method class: aClass literals: method literals.
	constTable _ constructor codeConstants.
	instVars _ Array new: aClass instSize.
	tempVarCount _ method numTemps.
	argCount _ method numArgs.
	"(tempVars isNil
	 and: [method holdsTempNames]) ifTrue:
		[tempVars := method tempNamesString]."
	tempVars isString
		ifTrue:
			[blockStartsToTempVars _ self mapFromBlockStartsIn: method
											toTempVarsFrom: tempVars
											constructor: constructor.
			 tempVars _ blockStartsToTempVars at: method initialPC]
		ifFalse:
			[| namedTemps |
			namedTemps _ tempVars ifNil: [(1 to: tempVarCount) collect: [ :i | 
					(i <= argCount ifTrue: ['arg'] ifFalse: ['temp']), i printString]].
			tempVars _ (1 to: tempVarCount) collect:
							[:i | i <= namedTemps size
								ifTrue: [constructor codeTemp: i - 1 named: (namedTemps at: i)]
								ifFalse: [constructor codeTemp: i - 1]]].
	1 to: method numArgs do:
		[:i|
		(tempVars at: i)
			beMethodArg]! !

!Decompiler methodsFor: 'initialization' stamp: 'jmv 1/16/2011 23:55'!
mapFromBlockStartsIn: aMethod toTempVarsFrom: schematicTempNamesString constructor: aDecompilerConstructor
	| map |
	map := aMethod
				mapFromBlockKeys: aMethod startpcsToBlockExtents keys sort
				toSchematicTemps: schematicTempNamesString.
	map keysAndValuesDo:
		[:startpc :tempNameTupleVector|
		tempNameTupleVector isEmpty ifFalse:
			[| subMap numTemps tempVector |
			subMap := Dictionary new.
			"Find how many temp slots there are (direct & indirect temp vectors)
			 and for each indirect temp vector find how big it is."
			tempNameTupleVector do:
				[:tuple|
				tuple last isArray
					ifTrue:
						[subMap at: tuple last first put: tuple last last.
						 numTemps := tuple last first]
					ifFalse:
						[numTemps := tuple last]].
			"create the temp vector for this scope level."
			tempVector := Array new: numTemps.
			"fill it in with any indirect temp vectors"
			subMap keysAndValuesDo:
				[:index :size|
				tempVector at: index put: (Array new: size)].
			"fill it in with temp nodes."
			tempNameTupleVector do:
				[:tuple| | itv |
				tuple last isArray
					ifTrue:
						[itv := tempVector at: tuple last first.
						 itv at: tuple last last
							put: (aDecompilerConstructor
									codeTemp: tuple last last - 1
									named: tuple first)]
					ifFalse:
						[tempVector
							at: tuple last
							put: (aDecompilerConstructor
									codeTemp: tuple last - 1
									named: tuple first)]].
			"replace any indirect temp vectors with proper RemoteTempVectorNodes"
			subMap keysAndValuesDo:
				[:index :size|
				tempVector
					at: index
					put: (aDecompilerConstructor
							codeRemoteTemp: index
							remoteTemps: (tempVector at: index))].
			"and update the entry in the map"
			map at: startpc put: tempVector]].
	^map! !

!Decompiler methodsFor: 'initialization' stamp: 'eem 6/29/2009 09:41'!
withTempNames: tempNames "<Array|String>"
	"Optionally initialize the temp names to be used when decompiling.
	 For backward-copmpatibility, if tempNames is an Array it is a single
	 vector of temp names, probably for a blue-book-compiled method.
	 If tempNames is a string it is a schematic string that encodes the
	 layout of temp vars in the method and any closures/blocks within it.
	 Decoding encoded tempNames is done in decompile:in:method:using:
	 which has the method from which to derive blockStarts.
	 See e.g. BytecodeEncoder>>schematicTempNamesString for syntax."
	tempVars := tempNames! !


!QSystemTally methodsFor: 'initialization' stamp: 'ar 6/11/2007 22:07'!
class: aClass method: aCompiledMethod
	class := aClass.
	method := aCompiledMethod.
	tally := 0.! !

!QSystemTally methodsFor: 'initialization' stamp: 'ar 6/11/2007 22:50'!
primitives: anInteger
	"Make the receiver be a node of unassigned primitives"
	tally := anInteger.
	method := nil. "indicates primitives"! !


!DateAndTime class methodsFor: 'initialization' stamp: 'jmv 4/17/2013 14:18'!
initialize
	"
	self initialize
	"
	LastTickSemaphore := Semaphore forMutualExclusion.
	LastTick := 0.
	Smalltalk addToStartUpList: self.
	self startUp: true! !

!DateAndTime class methodsFor: 'initialization' stamp: 'jmv 4/17/2013 13:50'!
startUp: resuming
	resuming ifFalse: [ ^ self ].
	Time initializeMillisecondClockMask! !


!Duration methodsFor: 'initialization' stamp: 'nk 3/30/2004 10:01'!
initialize
	self seconds: 0 nanoSeconds: 0.
! !


!MessageTally methodsFor: 'initialization' stamp: 'nk 3/8/2004 12:29'!
initialize
	maxClassNameSize _ self class defaultMaxClassNameSize.
	maxClassPlusSelectorSize _ self class defaultMaxClassPlusSelectorSize.
	maxTabs _ self class defaultMaxTabs.! !

!MessageTally methodsFor: 'initialization' stamp: 'jmv 4/17/2013 12:06'!
spyAllEvery: millisecs on: aBlock
	"Create a spy and spy on the given block at the specified rate."
	"Spy all the system processes"

	| myDelay time0 |
	aBlock isBlock
		ifFalse: [ self error: 'spy needs a block here' ].
	self class: aBlock receiver class method: aBlock method.
		"set up the probe"
	myDelay := Delay forMilliseconds: millisecs.
	time0 := Time localMillisecondClock.
	gcStats _ Smalltalk getVMParameters.
	Timer ifNotNil: [ self error: 'it seems a tally is already running' ].
	Timer _ [
		[true] whileTrue: [
			| startTime observedProcess |
			startTime := Time localMillisecondClock.
			myDelay wait.
			observedProcess := Processor preemptedProcess.
			self
				tally: observedProcess suspendedContext
				in: observedProcess
				"tally can be > 1 if ran a long primitive"
				by: (Time localMillisecondClock - startTime) // millisecs].
		nil] newProcess.
	Timer priority: Processor timingPriority-1.
	Timer name: 'MessageTally'.
		"activate the probe and evaluate the block"
	Timer resume.
	^ aBlock ensure: [
		"cancel the probe and return the value"
		"Could have already been terminated. See #terminateTimerProcess"
		Timer ifNotNil: [
			Timer terminate.
			Timer _ nil ].
		"Collect gc statistics"
		Smalltalk getVMParameters keysAndValuesDo: [ :idx :gcVal |
			gcVal isNumber ifTrue: [
				gcStats at: idx put: (gcVal - (gcStats at: idx))]].
		time := Time localMillisecondClock - time0]! !

!MessageTally methodsFor: 'initialization' stamp: 'jmv 5/10/2013 11:25'!
spyEvery: millisecs on: aBlock
	"Create a spy and spy on the given block at the specified rate."
	"Spy only on the active process (in which aBlock is run)"

	| myDelay time0 observedProcess |
	aBlock isBlock
		ifFalse: [ self error: 'spy needs a block here' ].
	self class: aBlock receiver class method: aBlock method.
		"set up the probe"
	observedProcess _ Processor activeProcess.
	myDelay := Delay forMilliseconds: millisecs.
	time0 := Time localMillisecondClock.
	gcStats _ Smalltalk getVMParameters.
	Timer ifNotNil: [ self error: 'it seems a tally is already running' ].
	Timer _ [
		[ true ] whileTrue: [
			| startTime |
			startTime := Time localMillisecondClock.
			myDelay wait.

			self
				tally: Processor preemptedProcess suspendedContext
				in: (observedProcess isTheLastPreempted ifTrue: [observedProcess])
				"tally can be > 1 if ran a long primitive"
				by: (Time localMillisecondClock - startTime) // millisecs].
		nil] newProcess.
	Timer priority: Processor timingPriority-1.
	Timer name: 'MessageTally'.
		"activate the probe and evaluate the block"
	Timer resume.
	^ aBlock ensure: [
		"cancel the probe and return the value"
		"Could have already been terminated. See #terminateTimerProcess"
		Timer ifNotNil: [
			Timer terminate.
			Timer _ nil ].
		"Collect gc statistics"
		Smalltalk getVMParameters keysAndValuesDo: [ :idx :gcVal |
			gcVal isNumber ifTrue: [
				gcStats at: idx put: (gcVal - (gcStats at: idx))]].
		time := Time localMillisecondClock - time0]! !

!MessageTally methodsFor: 'initialization' stamp: 'jmv 5/10/2013 11:25'!
spyEvery: millisecs onProcess: aProcess forMilliseconds: msecDuration
	"Create a spy and spy on the given process at the specified rate."
	| myDelay time0 endTime observedProcess sem |
	(aProcess isKindOf: Process)
		ifFalse: [self error: 'spy needs a Process here'].
	self class: aProcess suspendedContext receiver class method: aProcess suspendedContext method.
	"set up the probe"
	observedProcess _ aProcess.
	myDelay _ Delay forMilliseconds: millisecs.
	time0 _ Time localMillisecondClock.
	endTime _ time0 + msecDuration.
	sem _ Semaphore new.
	gcStats _ Smalltalk getVMParameters.
	Timer ifNotNil: [ self error: 'it seems a tally is already running' ].
	Timer _ [
			[
				| startTime |
				startTime _ Time localMillisecondClock.
				myDelay wait.
				self
					tally: Processor preemptedProcess suspendedContext
					in: (observedProcess isTheLastPreempted ifTrue: [ observedProcess ])
					"tally can be > 1 if ran a long primitive"
					by: (Time localMillisecondClock - startTime) // millisecs.
				startTime < endTime
			] whileTrue.
			sem signal.
		] newProcess.
	Timer priority: Processor timingPriority-1.
	Timer name: 'MessageTally'.
		"activate the probe and evaluate the block"
	Timer resume.
	"activate the probe and wait for it to finish"
	sem wait.
	"Collect gc statistics"
	Smalltalk getVMParameters keysAndValuesDo: [ :idx :gcVal |
		gcVal isNumber ifTrue: [
			gcStats at: idx put: (gcVal - (gcStats at: idx))]].
	time _ Time localMillisecondClock - time0! !


!MatrixTransform2x3 methodsFor: 'initialization' stamp: 'ar 11/2/1998 23:17'!
setIdentiy
	"Initialize the receiver to the identity transformation (e.g., not affecting points)"
	self
		a11: 1.0; a12: 0.0; a13: 0.0;
		a21: 0.0; a22: 1.0; a23: 0.0.! !


!Monitor methodsFor: 'initialization' stamp: 'NS 4/13/2004 16:12'!
initialize
	mutex _ Semaphore forMutualExclusion.
	queuesMutex _ Semaphore forMutualExclusion.
	nestingLevel _ 0.! !


!Morph class methodsFor: 'release' stamp: 'SD 11/15/2001 22:22'!
unload

	FileList unregisterFileReader: self ! !


!HandleMorph methodsFor: 'initialization' stamp: 'di 11/3/97 16:34'!
forEachPointDo: aBlock
	pointBlock _ aBlock! !


!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 6/2/2011 15:07'!
model: anObject stateGetter: getStateSel action: actionSel label: aString

	getStateSelector _ getStateSel.
	actionSelector _ actionSel.
	self model: anObject.
	self label: aString! !


!ScrollBar methodsFor: 'initialization' stamp: 'jmv 4/12/2012 22:11'!
buttonClass
	^PluggableButtonMorph! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 9/8/2012 20:16'!
initialize
	super initialize.
	extent _ self class scrollbarThickness @ 100.
	value _ 0.0.
	self recreateSubmorphs.
	scrollDelta _ 0.02.
	pageDelta _ 0.2! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 9/22/2012 15:39'!
initializeDownButton
	"initialize the receiver's downButton"

	| e |
	e _ self buttonExtent.
	downButton _ self buttonClass new.
	downButton model: self.
	self addMorph: downButton.
	downButton
		morphPosition: extent - borderWidth - e;
		morphExtent: e@e.
	self isHorizontal
		ifTrue: [ downButton updateRightButtonImage ]
		ifFalse: [ downButton updateDownButtonImage ]! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 9/19/2012 17:47'!
initializeSlider
	"initialize the receiver's slider"

	sliderShadow _ RectangleLikeMorph new.
	self addMorph: sliderShadow.
	sliderShadow hide.
		
	slider _ self sliderClass new.
	slider model: self.
	slider grabSelector: #sliderGrabbedAt:.
	slider dragSelector: #scrollTo:.
	slider action: #sliderReleased.
	self addMorph: slider.

	self computeSlider! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 9/22/2012 15:39'!
initializeUpButton
	"initialize the receiver's upButton"

	| e |
	e _ self buttonExtent.
	upButton _ self buttonClass new.
	upButton model: self.
	self addMorph: upButton.
	upButton
		morphPosition: borderWidth@borderWidth;
		morphExtent: e@e.
	self isHorizontal
		ifTrue: [ upButton updateLeftButtonImage ]
		ifFalse: [ upButton updateUpButtonImage ].! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:55'!
recreateSubmorphs
	self removeAllMorphs.
	self
		initializeUpButton;
		initializeDownButton;
		initializeSlider.
	"Set color for submorphs"
	self color: color.! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 4/12/2012 22:12'!
sliderClass
	^DraggeableButtonMorph! !


!MorphicEvent methodsFor: 'initialization' stamp: 'jmv 11/14/2011 18:20'!
resetHandlerFields
	"Reset anything that is used to cross-communicate between two eventual handlers during event dispatch"
	self flag: #jmvVer. "All this stuff of copying events to reset this data is pretty ugly."! !


!DropEvent methodsFor: 'initialization' stamp: 'jmv 11/14/2011 18:20'!
resetHandlerFields
	"Reset anything that is used to cross-communicate between two eventual handlers during event dispatch"
	self flag: #jmvVer. "All this stuff of copying events to reset this data is pretty ugly."
	wasHandled _ false.! !


!UserInputEvent methodsFor: 'initialization' stamp: 'jmv 8/17/2012 16:48'!
resetHandlerFields
	"Reset anything that is used to cross-communicate between two eventual handlers during event dispatch"
	self flag: #jmvVer. "All this stuff of copying events to reset this data is pretty ugly."
	wasHandled _ false.! !


!MouseButtonEvent methodsFor: 'initialization' stamp: 'jmv 8/17/2012 16:48'!
resetHandlerFields
	"Reset anything that is used to cross-communicate between two eventual handlers during event dispatch"
	self flag: #jmvVer. "All this stuff of copying events to reset this data is pretty ugly."
	super resetHandlerFields.
	eventHandler _ nil! !


!MouseClickState methodsFor: 'initialization' stamp: 'jmv 1/27/2013 00:12'!
client: aMorph drag: aDragSelector click: aClickSelector clickAndHalf: aClickAndHalfSelector dblClick: aDblClickSelector dblClickAndHalf: aDblClickAndHalfSelector tripleClick: aTripleClickSelector event: firstClickEvent sendMouseButton2Activity: aBoolean

	clickClient _ aMorph.
	dragSelector _ aDragSelector.
	clickSelector _ aClickSelector.
	clickAndHalfSelector _ aClickAndHalfSelector.
	dblClickSelector _ aDblClickSelector.
	dblClickAndHalfSelector _ aDblClickAndHalfSelector.
	tripleClickSelector _ aTripleClickSelector.
	sendMouseButton2Activity _ aBoolean.
	buttonDownCount _ 1.
	buttonUpCount _ 0.
	dragDone _ false.
	clickDone _ false.
	clickAndHalfDone _ false.
	doubleClickDone _ false.
	doubleClickAndHalfDone _ false.
	lastClickDown _ firstClickEvent! !


!MouseOverHandler methodsFor: 'initialization' stamp: 'ar 9/28/2000 17:08'!
initialize
	mouseOverMorphs _ #().! !


!Mutex methodsFor: 'initialization' stamp: 'das 11/3/2005 22:53'!
initialize
	semaphore := Semaphore forMutualExclusion.! !


!ObjectFinalizer methodsFor: 'initialization' stamp: 'ar 5/19/2003 20:12'!
receiver: aReceiver selector: aSelector argument: anObject
	receiver := aReceiver.
	selector := aSelector.
	arguments := Array with: anObject! !


!AssignmentNode methodsFor: 'initialization'!
toDoIncrement: var
	var = variable ifFalse: [^ nil].
	(value isMemberOf: MessageNode) 
		ifTrue: [^ value toDoIncrement: var]
		ifFalse: [^ nil]! !

!AssignmentNode methodsFor: 'initialization'!
value
	^ value! !

!AssignmentNode methodsFor: 'initialization'!
variable: aVariable value: expression

	variable := aVariable.
	value := expression! !

!AssignmentNode methodsFor: 'initialization' stamp: 'di 3/22/1999 12:00'!
variable: aVariable value: expression from: encoder

	(aVariable isMemberOf: MessageAsTempNode)
		ifTrue: ["Case of remote temp vars"
				^ aVariable store: expression from: encoder].
	variable := aVariable.
	value := expression! !

!AssignmentNode methodsFor: 'initialization' stamp: 'hmm 7/15/2001 21:17'!
variable: aVariable value: expression from: encoder sourceRange: range

	encoder noteSourceRange: range forNode: self.
	^self
		variable: aVariable
		value: expression
		from: encoder! !


!BlockNode methodsFor: 'initialization' stamp: 'eem 5/20/2008 13:40'!
arguments: argNodes statements: statementsCollection returns: returnBool from: encoder
	"Compile."

	arguments := argNodes.
	statements := statementsCollection size > 0
				ifTrue: [statementsCollection]
				ifFalse: [argNodes size > 0
						ifTrue: [statementsCollection copyWith: arguments last]
						ifFalse: [Array with: NodeNil]].
	optimized := false.
	returns := returnBool! !

!BlockNode methodsFor: 'initialization' stamp: 'eem 8/4/2008 14:12'!
noteSourceRangeStart: start end: end encoder: encoder
	"Note two source ranges for this node.  One is for the debugger
	 and is of the last expression, the result of the block.  One is for
	 source analysis and is for the entire block."
	encoder
		noteSourceRange: (start to: end)
		forNode: self closureCreationNode.
	startOfLastStatement
		ifNil:
			[encoder
				noteSourceRange: (start to: end)
				forNode: self]
		ifNotNil:
			[encoder
				noteSourceRange: (startOfLastStatement to: end - 1)
				forNode: self]! !

!BlockNode methodsFor: 'initialization' stamp: 'eem 5/20/2008 13:40'!
statements: statementsCollection returns: returnBool 
	"Decompile."

	| returnLast |
	returnLast := returnBool.
	returns := false.
	statements := 
		(statementsCollection size > 1 
			and: [(statementsCollection at: statementsCollection size - 1) 
					isReturningIf])
				ifTrue: 
					[returnLast := false.
					statementsCollection allButLast]
				ifFalse: [statementsCollection size = 0
						ifTrue: [Array with: NodeNil]
						ifFalse: [statementsCollection]].
	arguments := #().
	temporaries := #().
	optimized := false.
	returnLast ifTrue: [self returnLast]! !


!BraceNode methodsFor: 'initialization'!
elements: collection
	"Decompile."

	elements := collection! !

!BraceNode methodsFor: 'initialization'!
elements: collection sourceLocations: locations
	"Compile."

	elements := collection.
	sourceLocations := locations! !

!BraceNode methodsFor: 'initialization' stamp: 'di 11/19/1999 11:06'!
matchBraceStreamReceiver: receiver messages: messages

	((receiver isMessage: #braceStream: receiver: nil arguments: [:arg | arg isConstantNumber])
		and: [messages last isMessage: #braceArray receiver: nil arguments: nil])
		ifFalse: [^ nil "no match"].

	"Appears to be a long form brace construct"
	self elements: (messages allButLast collect:
		[:msg | (msg isMessage: #nextPut: receiver: nil arguments: nil)
					ifFalse: [^ nil "not a brace element"].
		msg arguments first])! !

!BraceNode methodsFor: 'initialization' stamp: 'di 11/19/1999 11:19'!
matchBraceWithReceiver: receiver selector: selector arguments: arguments

	selector = (self selectorForShortForm: arguments size)
		ifFalse: [^ nil "no match"].

	"Appears to be a short form brace construct"
	self elements: arguments! !


!CascadeNode methodsFor: 'initialization'!
receiver: receivingObject messages: msgs
	" Transcript show: 'abc'; cr; show: 'def' "

	receiver := receivingObject.
	messages := msgs! !


!DecompilerConstructor methodsFor: 'initialization'!
method: aMethod class: aClass literals: literals

	method := aMethod.
	instVars := aClass allInstVarNames.
	nArgs := method numArgs.
	literalValues := literals! !


!Encoder methodsFor: 'initialization'!
fillDict: dict with: nodeClass mapping: keys to: codeArray
	| codeStream |
	codeStream := ReadStream on: codeArray.
	keys do: 
		[:key | dict 
				at: key
				put:  (nodeClass new name: key key: key code: codeStream next)]! !

!Encoder methodsFor: 'initialization' stamp: 'jmv 8/28/2010 11:17'!
init: aClass context: aContext notifying: req
	requestor := req.
	class := aClass.
	nTemps := 0.
	supered := false.
	self initScopeAndLiteralTables.
	"No Tweak Fields in Cuis"
	class variablesAndOffsetsDo: [ :variable "<String>" :offset "<Integer>" |
		scopeTable
			at: variable
			put: (offset >= 0
					ifTrue: [InstanceVariableNode new
								name: variable index: offset]
					ifFalse: [MaybeContextInstanceVariableNode new
								name: variable index: offset negated])].
	aContext ifNotNil: [
		| homeNode |
		homeNode := self bindTemp: self doItInContextName.
		"0th temp = aContext passed as arg"
		aContext tempNames withIndexDo: [ :variable :index |
			scopeTable
				at: variable
				put: (MessageAsTempNode new
						receiver: homeNode
						selector: #namedTempAt:
						arguments: (Array with: (self encodeLiteral: index))
						precedence: 3
						from: self)]].
	sourceRanges := Dictionary new: 32.
	globalSourceRanges := OrderedCollection new: 32! !

!Encoder methodsFor: 'initialization' stamp: 'nice 3/30/2011 23:04'!
initScopeAndLiteralTables

	scopeTable := StdVariables copy.
	litSet := StdLiterals copy.
	"comments can be left hanging on nodes from previous compilations.
	 probably better than this hack fix is to create the nodes afresh on each compilation."
	scopeTable do:
		[:varNode| varNode comment: nil].
	litSet do:
		[:varNode| varNode comment: nil].
	selectorSet := StdSelectors copy.
	litIndSet := Dictionary new: 16.
	literalStream := WriteStream on: (Array new: 32).
	addedSelectorAndMethodClassLiterals := false.
	optimizedSelectors := Set new! !

!Encoder methodsFor: 'initialization'!
noteSuper

	supered := true! !

!Encoder methodsFor: 'initialization' stamp: 'ajh 7/21/2003 00:53'!
temps: tempVars literals: lits class: cl 
	"Decompile."

	supered := false.
	class := cl.
	nTemps := tempVars size.
	tempVars do: [:node | scopeTable at: node name put: node].
	literalStream := ReadStream on: lits.
	literalStream position: lits size.
	sourceRanges := Dictionary new: 32.
	globalSourceRanges := OrderedCollection new: 32.
! !

!Encoder methodsFor: 'release'!
release

	requestor := nil! !


!BytecodeEncoder methodsFor: 'initialization' stamp: 'eem 7/24/2008 17:24'!
streamToMethod: aCompiledMethod
	stream := WriteStream with: aCompiledMethod.
	stream position: aCompiledMethod initialPC - 1! !


!EncoderForLongFormV3 methodsFor: 'initialization' stamp: 'eem 5/15/2008 14:11'!
initScopeAndLiteralTables
	super initScopeAndLiteralTables.
	"Start with an empty selector set to avoid the special selectors."
	selectorSet := Dictionary new: 16! !


!LeafNode methodsFor: 'initialization' stamp: 'ar 3/26/2004 15:44'!
key: object code: byte

	key := object.
	code := byte! !

!LeafNode methodsFor: 'initialization' stamp: 'eem 5/14/2008 15:56'!
key: object index: i type: type

	key := object.
	code := (self code: i type: type).
	index := i! !

!LeafNode methodsFor: 'initialization' stamp: 'ar 3/26/2004 15:44'!
name: ignored key: object code: byte

	key := object.
	code := byte! !


!LiteralNode methodsFor: 'initialization' stamp: 'eem 5/14/2008 09:30'!
name: literal key: object index: i type: type
	"For compatibility with Encoder>>name:key:class:type:set:"
	^self key: object index: i type: type! !


!SelectorNode methodsFor: 'initialization' stamp: 'eem 5/14/2008 09:30'!
name: literal key: object index: i type: type
	"For compatibility with Encoder>>name:key:class:type:set:"
	^self key: object index: i type: type! !


!VariableNode methodsFor: 'initialization'!
asStorableNode: encoder
	^ self! !

!VariableNode methodsFor: 'initialization' stamp: 'eem 12/1/2008 13:51'!
name: string
	"Change name"

	name := string! !

!VariableNode methodsFor: 'initialization' stamp: 'eem 5/14/2008 09:33'!
name: varName index: i type: type
	"Only used for initting instVar refs"
	^self name: varName key: varName index: i type: type! !

!VariableNode methodsFor: 'initialization' stamp: 'ar 3/26/2004 15:46'!
name: string key: object code: byte
	"Only used for initting std variables, nil, true, false, self, etc."
	name := string.
	key := object.
	code := byte! !

!VariableNode methodsFor: 'initialization' stamp: 'eem 5/14/2008 16:01'!
name: varName key: objRef index: i type: type
	"Only used for initting global (litInd) variables"
	^self name: varName key: objRef code: (self code: (index := i) type: type)! !


!InstanceVariableNode methodsFor: 'initialization' stamp: 'eem 5/13/2008 10:17'!
name: varName index: varIndex
	^self name: varName index: varIndex-1 type: LdInstType! !


!TempVariableNode methodsFor: 'initialization' stamp: 'eem 9/8/2008 18:27'!
name: varName index: i type: type scope: level
	"Only used for initting temporary variables"
	hasDefs := hasRefs := false.
	scope := level.
	^super name: varName key: varName index: i type: type! !

!TempVariableNode methodsFor: 'initialization'!
nowHasDef
	hasDefs := true! !

!TempVariableNode methodsFor: 'initialization'!
nowHasRef
	hasRefs := true! !

!TempVariableNode methodsFor: 'initialization'!
scope: level
	"Note scope of temporary variables.
	Currently only the following distinctions are made:
		0	outer level: args and user-declared temps
		1	block args and doLimiT temps
		-1	a block temp that is no longer active
		-2	a block temp that held limit of to:do:"
	scope := level! !


!MessageNode methodsFor: 'initialization' stamp: 'nice 3/2/2011 23:57'!
receiver: rcvr selector: selNode arguments: args precedence: p 
	"Decompile."

	self receiver: rcvr
		arguments: args
		precedence: p.
	originalSelector := selNode key.
	selNode code == #macro
		ifTrue: [self noteSpecialSelector: selNode key]
		ifFalse: [special := 0].
	selector := selNode.
	"self pvtCheckForPvtSelector: encoder"
	"We could test code being decompiled, but the compiler should've checked already. And where to send the complaint?"! !

!MessageNode methodsFor: 'initialization' stamp: 'nice 3/2/2011 23:55'!
receiver: rcvr selector: aSelector arguments: args precedence: p from: encoder 
	"Compile."

	self receiver: rcvr
		arguments: args
		precedence: p.
	originalSelector := aSelector.
	self noteSpecialSelector: aSelector.
	(self transform: encoder)
		ifTrue: 
			[selector isNil ifTrue:
				[selector := SelectorNode new 
								key: (MacroSelectors at: special)
								code: #macro]]
		ifFalse: 
			[selector := encoder encodeSelector: aSelector.
			rcvr == NodeSuper ifTrue: [encoder noteSuper]].
	self pvtCheckForPvtSelector: encoder! !

!MessageNode methodsFor: 'initialization' stamp: 'eem 7/18/2008 16:26'!
receiver: rcvr selector: selName arguments: args precedence: p from: encoder sourceRange: range
	"Compile."
	((selName == #future) or:[selName == #future:]) ifTrue:
		[Smalltalk at: #FutureNode ifPresent:
			[:futureNode|
			^futureNode new
				receiver: rcvr
				selector: selName
				arguments: args
				precedence: p
				from: encoder
				sourceRange: range]].
	(rcvr isFutureNode
	 and: [rcvr futureSelector == nil]) ifTrue:
		"Transform regular message into future"
		[^rcvr futureMessage: selName
			arguments: args
			from: encoder
			sourceRange: range].

	encoder noteSourceRange: range forNode: self.
	^self
		receiver: rcvr
		selector: selName
		arguments: args
		precedence: p
		from: encoder! !

!MessageNode methodsFor: 'initialization' stamp: 'tk 10/26/2000 15:37'!
selector: sel
	selector := sel! !


!MethodNode methodsFor: 'initialization' stamp: 'tk 8/3/1999 12:47'!
block
	^ block! !

!MethodNode methodsFor: 'initialization' stamp: 'ajh 1/24/2003 17:37'!
selector: symbol

	selectorOrFalse := symbol! !

!MethodNode methodsFor: 'initialization' stamp: 'jcg 9/30/2009 00:55'!
selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim 
	
	self 
		selector: selOrFalse
		arguments: args
		precedence: p
		temporaries: temps
		block: blk encoder:
		anEncoder 
		primitive: prim 
		properties: AdditionalMethodState new.! !

!MethodNode methodsFor: 'initialization' stamp: 'ar 1/4/2002 00:23'!
selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim properties: propDict
	"Initialize the receiver with respect to the arguments given."

	encoder := anEncoder.
	selectorOrFalse := selOrFalse.
	precedence := p.
	arguments := args.
	temporaries := temps.
	block := blk.
	primitive := prim.
	properties := propDict.! !

!MethodNode methodsFor: 'initialization' stamp: 'ajh 1/22/2003 17:53'!
sourceText: stringOrText

	sourceText := stringOrText! !


!ReturnNode methodsFor: 'initialization'!
expr: e

	expr := e! !

!ReturnNode methodsFor: 'initialization'!
expr: e encoder: encoder sourceRange: range

	expr := e.
	encoder noteSourceRange: range forNode: self! !


!OptimizedBlockLocalTempReadBeforeWrittenVisitor methodsFor: 'initialization' stamp: 'eem 9/5/2009 21:03'!
initialize
	inOptimizedBlock := false! !


!ParseNodeEnumerator methodsFor: 'initialization' stamp: 'eem 7/20/2009 19:44'!
ofBlock: aBlock
	theBlock := aBlock! !

!ParseNodeEnumerator methodsFor: 'initialization' stamp: 'eem 8/31/2010 11:24'!
ofBlock: aBlock select: aSelectBlock
	theBlock := aBlock.
	theSelectBlock := aSelectBlock! !


!VariableScopeFinder methodsFor: 'initialization' stamp: 'eem 8/12/2010 16:18'!
ofVariable: aVariableNode
	theVariable := aVariableNode! !


!ParseStack methodsFor: 'initialization'!
init

	length := position := 0! !


!CustomMenu methodsFor: 'initialization' stamp: 'jmv 4/15/2011 15:07'!
initialize

	labels _ OrderedCollection new.
	selections _ OrderedCollection new.
	dividers _ OrderedCollection new.
	lastDivider _ 0.
	arguments _ OrderedCollection new! !

!CustomMenu methodsFor: 'initialization' stamp: 'sw 8/18/1998 12:01'!
title: aTitle
	title _ aTitle! !


!SHTextStylerST80 methodsFor: 'initialization' stamp: 'jmv 12/22/2010 15:13'!
initialize
	super initialize.
	disableFormatAndConvert _ false! !


!Scanner methodsFor: 'initialization' stamp: 'jmv 4/14/2010 11:20'!
initScanner

	buffer := WriteStream on: (String new: 40).
	typeTable := TypeTable.
	isForTokenization := false! !

!Scanner methodsFor: 'initialization' stamp: 'jmv 8/28/2010 11:30'!
initScannerForTokenization
	"Don't raise xIllegal when enocuntering an _"
	"Simpler implementation for Cuis"
	isForTokenization _ true! !

!Scanner methodsFor: 'initialization'!
scan: inputStream 
	"Bind the input stream, fill the character buffers and first token buffer."

	source := inputStream.
	self step.
	self step.
	self scanToken! !


!SystemVersion methodsFor: 'initialization' stamp: 'jmv 7/7/2013 17:57'!
versionMajor: aNumber versionMinor: anotherNumber
	versionMajor _ aNumber.
	versionMinor _ anotherNumber.
	date _ Date today.
	updates _ OrderedCollection new.
! !


!AttributesReplaceCommand methodsFor: 'initialization' stamp: 'jmv 9/20/2011 18:32'!
old: oldAttributes new: newAttributes start: startIndex stop: stopIndex
	old _ oldAttributes.
	new _ newAttributes.
	start _ startIndex.
	stop _ stopIndex! !


!TextReplaceCommand methodsFor: 'initialization' stamp: 'jmv 9/20/2011 09:26'!
old: oldStringOrText new: newStringOrText at: anInteger
	old _ oldStringOrText.
	new _ newStringOrText.
	position _ anInteger! !


!TextLine methodsFor: 'initialization' stamp: 'jmv 9/29/2009 09:28'!
initialize
	isFirstLine _ false! !


!EmptyLine methodsFor: 'initialization' stamp: 'jmv 6/29/2010 10:58'!
initialize
	super initialize.
	internalSpaces _ 0.
	paddingWidth _ 0.
	isFirstLine _ true! !


!Theme methodsFor: 'initialization' stamp: 'cbr 5/16/2011 13:31'!
initialize

	| menuDecorations |

	"build a dictionary wordings -> icon to decorate the menus all over the image"
	
	super initialize.

	self flag: #todo. "Consider moving menuItemIcons over to the class side,  and removing this method. --cbr"

	menuItemIcons _ Dictionary new.
	menuDecorations _ self menuDecorations as: Dictionary.
	menuDecorations keysDo: [ :key |
		key do: [ :str |
			| form symbol |
				symbol _  menuDecorations at: key.
				form _ self perform: symbol.

			menuItemIcons
				at: str asLowercase 
				put: form
		]
	]
! !


!Theme reorganize!
('colors' background buttonColorFrom: buttonLabel focusIndicator line missingCommentTextColor paneBackgroundFrom: text textCursor textHighlight textHighlightFocused: unfocusedTextHighlightFrom: windowLabel)
('menu colors' menu menuHighlight menuText menuTitleBar)
('tool colors' browser changeList changeSorter debugger defaultWindowColor dualChangeSorter fileContentsBrowser fileList messageNames messageSet object packageList testRunner textEditor transcript versionsBrowser workspace)
('widget colors' acceptButton cancelButton listHighlightFocused: listMultiHighlightFocused: listSelectedRowText listUnselectedRowText textPane)
('other options' buttonGradientBottomFactor buttonGradientHeight buttonGradientTopFactor embossedButtonLabels embossedTitles roundButtons roundWindowCorners roundedButtonRadius roundedWindowRadius steButtons titleGradientBottomFactor titleGradientExtraLightness titleGradientTopFactor useButtonGradient useUniformColors useWindowTitleGradient)
('private - shout mappings' argumentTypes blockLevelFive blockLevelFour blockLevelSeven blockLevelSix blockTemps defaults firstBlockLevel generateShoutConfig globals incompleteMessages instVar italic literals messages methodTags nilly pseudoVariables symbols tempBar tempVars undefined)
('menus' allIcons basicIcons menuDecorations miscellaneousIcons noIcons)
('icons' acceptIcon addressBookIcon appearanceIcon blankIcon cancelIcon changesIcon chatIcon classIcon clockIcon closeIcon collapseIcon copyIcon cutIcon dateIcon debugIcon deleteIcon developmentIcon displayIcon doItIcon editFindReplaceIcon emblemImportantIcon exitFullscreenIcon expandIcon exploreIcon fileOutIcon findIcon fontXGenericIcon formatJustifyCenterIcon formatJustifyFillIcon formatJustifyLeftIcon formatJustifyRightIcon genericTextIcon globeIcon goBottomIcon goDownIcon goTopIcon goUpIcon graphicsIcon halfRefreshIcon haloCollapseIcon haloColorIcon haloDebugIcon haloDismissIcon haloDragIcon haloDuplicateIcon haloFontEmphasisIcon haloFontSizeIcon haloGrabIcon haloHelpIcon haloMenuIcon haloRotateIcon haloScaleIcon helpIcon inspectIcon instanceIcon junkIcon keyboardShortcutsIcon listAddIcon listRemoveIcon mailForwardIcon mailMarkJunkIcon mediaPlaybackStartIcon morphsIcon newFolderIcon newIcon newWindowIcon openIcon packageIcon pasteIcon preferencesDesktopFontIcon preferencesIcon printIcon printerIcon pushPinIcon quitIcon redoIcon removableMediaIcon saveAndQuitIcon saveAsIcon saveAsNewVersionIcon saveIcon scriptIcon selectAllIcon sendReceiveIcon speadsheetTemplateIcon spreadsheetIcon stillCameraIcon switchIcon systemFileManagerIcon systemIcon systemMonitorIcon terminalIcon textEditorIcon undoIcon updateIcon usersIcon viewFullscreenIcon wallpaperIcon warningIcon weatherFewCloudsIcon windowIcon windowMenuIcon worldIcon)
('icon lookup' appendExtensionToContentSpec: fetch: prepend:toContentSpec:)
('accessing' decorateMenu:)
('shout' italizeArguments shout)
('initialization' initialize)
!


!EmptyLine reorganize!
('accessing' paragraphStyle paragraphStyle:)
('testing' isEmptyLine)
('initialization' initialize)
!


!TextLine reorganize!
('accessing' baseline bottom bottomRight first internalSpaces internalSpaces: isFirstLine isFirstLine: last left leftMargin leftMargin: leftMarginForAlignment: lineHeight paddingWidth paddingWidth: rectangle rectangle: right rightMargin stop: top topLeft width)
('comparing' = hash)
('printing' printOn:)
('scanning' justifiedPadFor:)
('updating' slideIndexBy:andMoveTopTo:)
('private' firstIndex:lastIndex: internalSpaces:paddingWidth: lineHeight:baseline:)
('testing' isEmptyLine)
('initialization' initialize)
!


!TextReplaceCommand reorganize!
('actions' doOn: undoOn:)
('printing' printOn:)
('initialization' old:new:at:)
!


!AttributesReplaceCommand reorganize!
('actions' doOn: undoOn:)
('printing' printOn:)
('initialization' old:new:start:stop:)
!


!SystemVersion reorganize!
('accessing' date date: datedVersion highestUpdate highestUpdate: registerUpdate: unregisterUpdate: updates version versionRevision)
('printing' printOn:)
('testing' isCuis isPharo isSqueak)
('initialization' versionMajor:versionMinor:)
!


!Scanner reorganize!
('expression types' advance checkpoint scanAllTokenPositionsInto: scanForFindSelectorUpTo: scanLitByteVec scanLitVec scanLitWord scanStringStruct scanToken step)
('error handling' errorMultibyteCharacter notify: notify:at: offEnd:)
('public access' scanFieldNames: scanMessageParts: scanStringStruct: scanTokenPositionsIn:into: scanTokens: typedScanTokens:)
('multi-character scans' typeTableAt: xBinary xColon xDelimiter xDigit xDollar xDoubleQuote xIllegal xLetter xLitQuote xSingleQuote xUnderscore)
('private' allowUnderscoreAssignments allowUnderscoreSelectors)
('initialization' initScanner initScannerForTokenization scan:)
!


!SHTextStylerST80 reorganize!
('private' attributesFor: convertAssignmentsToAnsi convertAssignmentsToLeftArrow privateFormatAndConvert privateStyle privateStyle: rangesIn:setWorkspace: replaceStringForRangesWithType:with:in: setAttributesIn:fromRanges:in:)
('accessing' classOrMetaClass: disableFormatAndConvert workspace:)
('initialization' initialize)
!


!CustomMenu reorganize!
('construction' add:action: addLine addList: addStayUpIcons balloonTextForLastItem: labels:lines: labels:lines:selections:)
('invocation' startUp startUpWithCaption:)
('compatibility' add:target:selector: add:target:selector:argument: add:target:selector:argumentList: arguments)
('private' build)
('initialization' initialize title:)
!


!ParseStack reorganize!
('accessing' pop: position: push: size)
('results' position)
('printing' printOn:)
('initialization' init)
!


!VariableScopeFinder reorganize!
('private' enclosingNodeFor:of:)
('visiting' visitAssignmentNode: visitBlockNode: visitBraceNode: visitCascadeNode: visitCommentNode: visitFieldNode: visitFutureNode: visitInstanceVariableNode: visitLiteralNode: visitLiteralVariableNode: visitMessageNode: visitMethodNode: visitNewArrayNode: visitRemoteTempVectorNode: visitReturnNode: visitSelectorNode: visitTempVariableNode: visitUndeclaredVariableNode: visitVariableNode:)
('initialization' ofVariable:)
!


!ParseNodeEnumerator reorganize!
('visiting' visitAssignmentNode: visitBlockNode: visitBraceNode: visitCascadeNode: visitCommentNode: visitFieldNode: visitFutureNode: visitInstanceVariableNode: visitLiteralNode: visitLiteralVariableNode: visitMessageNode: visitMessageNodeInCascade: visitMethodNode: visitNewArrayNode: visitRemoteTempVectorNode: visitReturnNode: visitSelectorNode: visitTempVariableNode: visitVariableNode:)
('initialization' ofBlock: ofBlock:select:)
!


!OptimizedBlockLocalTempReadBeforeWrittenVisitor reorganize!
('accessing' readBeforeWritten)
('visiting' visitAssignmentNode: visitBlockNode: visitMessageNode: visitTempVariableNode:)
('initialization' initialize)
!


!ReturnNode reorganize!
('visiting' accept:)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('converting' asReturnNode)
('code generation' code)
('code generation (new scheme)' emitCodeForReturn:encoder: emitCodeForValue:encoder: sizeCodeForReturn: sizeCodeForValue:)
('printing' expr printOn:indent: printWithClosureAnalysisOn:indent:)
('testing' isReturnSelf isSpecialConstant isVariableReference)
('initialization' expr: expr:encoder:sourceRange:)
!


!MethodNode reorganize!
('visiting' accept:)
('accessing' arguments arguments: body primitiveErrorVariableName removeProperty: temporaries temporaries:)
('converting' asColorizedSmalltalk80Text decompileString decompileText preen preenLocalIfNotNilArg)
('code generation' encoder generate generate: generate:ifQuick: generateWithTempNames parserClass properties selector selectorNode)
('printing' methodClass printOn: printPragmasOn: printPrimitiveOn: printPropertiesOn: printWithClosureAnalysisOn: sourceText tempNames)
('source mapping' rawSourceRanges rawSourceRangesAndMethodDo:)
('primitive error codes' removeAndRenameLastTempIfErrorCode)
('debugger support' blockExtentsToTempsMap hasGeneratedMethod schematicTempNamesString)
('code generation (closures)' addLocalsToPool: ensureClosureAnalysisDone locationCounter noteBlockEntry: noteBlockExit: referencedValuesWithinBlockExtent:)
('initialization' block selector: selector:arguments:precedence:temporaries:block:encoder:primitive: selector:arguments:precedence:temporaries:block:encoder:primitive:properties: sourceText:)
!


!MessageNode reorganize!
('visiting' accept: argumentsInEvaluationOrder)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('equation translation' arguments arguments: eval receiver receiver: selector)
('printing' macroPrinter precedence printCaseOn:indent: printIfNil:indent: printIfNilNotNil:indent: printIfOn:indent: printKeywords:arguments:on:indent: printOn:indent: printOn:indent:precedence: printParenReceiver:on:indent: printReceiver:on:indent: printToDoOn:indent: printWhileOn:indent: printWithClosureAnalysisCaseOn:indent: printWithClosureAnalysisIfNil:indent: printWithClosureAnalysisIfNilNotNil:indent: printWithClosureAnalysisIfOn:indent: printWithClosureAnalysisKeywords:arguments:on:indent: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence: printWithClosureAnalysisParenReceiver:on:indent: printWithClosureAnalysisReceiver:on:indent: printWithClosureAnalysisToDoOn:indent: printWithClosureAnalysisWhileOn:indent: test)
('testing' canCascade ensureCanCascade: isComplex isMessage isMessage:receiver:arguments: isMessageNode isNilIf isOptimized isOptimizedLoop isReturningIf toDoIncrement: toDoLimit:)
('cascading' cascadeReceiver)
('private' checkBlock:as:from: checkBlock:as:from:maxArgs: ifNilReceiver pvtCheckForPvtSelector: receiver:arguments:precedence:)
('code generation' emitCodeForEffect:encoder: emitCodeForToDo:encoder:value: emitCodeForValue:encoder: sizeCodeForCase:value: sizeCodeForEffect: sizeCodeForToDo:value: sizeCodeForValue:)
('code generation (new scheme)' emitCodeForCase:encoder:value: emitCodeForIf:encoder:value: emitCodeForIfNil:encoder:value: emitCodeForWhile:encoder:value: sizeCodeForIf:value: sizeCodeForIfNil:value: sizeCodeForWhile:value:)
('macro transformations' noteSpecialSelector: toDoFromWhileWithInit: transform: transformAnd: transformBoolean: transformCase: transformIfFalse: transformIfFalseIfTrue: transformIfNil: transformIfNilIfNotNil: transformIfNotNilIfNil: transformIfTrue: transformIfTrueIfFalse: transformOr: transformToDo: transformWhile:)
('initialization' receiver:selector:arguments:precedence: receiver:selector:arguments:precedence:from: receiver:selector:arguments:precedence:from:sourceRange: selector:)
!


!TempVariableNode reorganize!
('visiting' accept:)
('code generation (closures)' addReadWithin:at: addWriteWithin:at: analyseClosure: analyseTempsWithin:rootNode:assignmentPools: beingAssignedToAnalyseTempsWithin:rootNode:assignmentPools: definingScope definingScope: index: isDefinedWithinBlockExtent: isIndirectTempVector isReferencedWithinBlockExtent: referenceScopesAndIndicesDo:)
('testing' assignmentCheck:at: beBlockArg beMethodArg isArg isBlockArg isDeclaredAtMethodLevel isMethodArg isRemote isTemp isUndefTemp isUnusedTemp remoteNode scope)
('printing' printDefinitionForClosureAnalysisOn: printOn:indent: printWithClosureAnalysisOn:indent:)
('decompiler' remoteNode:)
('code generation' emitCodeForLoad:encoder: emitCodeForStore:encoder: emitCodeForStorePop:encoder: emitCodeForValue:encoder: sizeCodeForLoad: sizeCodeForStore: sizeCodeForStorePop: sizeCodeForValue:)
('initialization' name:index:type:scope: nowHasDef nowHasRef scope:)
!


!InstanceVariableNode reorganize!
('visiting' accept:)
('code generation (new scheme)' emitCodeForStore:encoder: emitCodeForStorePop:encoder: emitCodeForValue:encoder: sizeCodeForStore: sizeCodeForStorePop: sizeCodeForValue:)
('initialization' name:index:)
!


!VariableNode reorganize!
('visiting' accept:)
('testing' assignmentCheck:at: index isSelfPseudoVariable isUndeclared isVariableNode isVariableReference varNodeType)
('code generation (closures)' beingAssignedToAnalyseTempsWithin:rootNode:assignmentPools:)
('tiles' currentValueIn: variableGetterBlockIn:)
('code generation (new scheme)' emitCodeForLoad:encoder: emitCodeForReturn:encoder: emitCodeForStore:encoder: emitCodeForStorePop:encoder: emitCodeForValue:encoder: sizeCodeForStore: sizeCodeForStorePop: sizeCodeForValue:)
('code generation' fieldOffset sizeCodeForReturn:)
('accessing' name)
('printing' printOn:indent: printWithClosureAnalysisOn:indent:)
('initialization' asStorableNode: name: name:index:type: name:key:code: name:key:index:type:)
!


!SelectorNode reorganize!
('visiting' accept:)
('code generation' reserve:)
('code generation (new scheme)' emitCode:args:encoder: emitCode:args:encoder:super: emitCodeForEffect:encoder: emitCodeForValue:encoder: sizeCode:args:super: sizeCodeForEffect: sizeCodeForValue:)
('inappropriate' emitForEffect:on: emitForValue:on: sizeForEffect: sizeForValue:)
('testing' isForFFICall isPvtSelector)
('printing' key: printAsFFICallWithArguments:on:indent: printOn:indent: printWithClosureAnalysisOn:indent:)
('initialization' name:key:index:type:)
!


!LiteralNode reorganize!
('visiting' accept:)
('code generation (new scheme)' emitCodeForValue:encoder: sizeCodeForValue:)
('evaluation' eval)
('testing' isConstantNumber isLiteralNode isSpecialConstant literalValue)
('printing' printOn:indent: printWithClosureAnalysisOn:indent:)
('initialization' name:key:index:type:)
!


!LeafNode reorganize!
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('code generation' code reserve:)
('private' code:type:)
('code generation (new scheme)' emitCodeForEffect:encoder: emitCodeForLoad:encoder: sizeCodeForEffect: sizeCodeForLoad: sizeCodeForValue:)
('accessing' key)
('initialization' key:code: key:index:type: name:key:code:)
!


!EncoderForLongFormV3 reorganize!
('bytecode generation' genBranchPopFalse: genBranchPopTrue: genDup genJump: genJumpLong: genPop genPushInstVar: genPushInstVarLong: genPushLiteral: genPushLiteralVar: genPushReceiver genPushSpecialLiteral: genPushTemp: genPushThisContext genReturnReceiver genReturnSpecialLiteral: genReturnTop genReturnTopToCaller genSend:numArgs: genSendSuper:numArgs: genStoreInstVar: genStoreInstVarLong: genStoreLiteralVar: genStorePopInstVar: genStorePopInstVarLong: genStorePopLiteralVar: genStorePopTemp: genStoreTemp:)
('initialization' initScopeAndLiteralTables)
!


!BytecodeEncoder reorganize!
('temps' bindAndJuggle: bindBlockArg:within: bindBlockTemp: bindBlockTemp:within: bindTemp: blockExtentsToTempsMap noteBlockExtent:hasLocals:)
('testing' hasGeneratedMethod supportsClosureOpcodes)
('special literal encodings' if:isSpecialLiteralForPush: if:isSpecialLiteralForReturn:)
('accessing' methodStreamPosition rootNode rootNode:)
('opcode sizing' nextPut: sizeBranchPopFalse: sizeBranchPopTrue: sizeDup sizeJump: sizeJumpLong: sizeOpcodeSelector:withArguments: sizePop sizePushClosureCopyNumCopiedValues:numArgs:jumpSize: sizePushConsArray: sizePushInstVar: sizePushInstVarLong: sizePushLiteral: sizePushLiteralVar: sizePushNewArray: sizePushReceiver sizePushRemoteTemp:inVectorAt: sizePushSpecialLiteral: sizePushTemp: sizePushThisContext sizeReturnReceiver sizeReturnSpecialLiteral: sizeReturnTop sizeReturnTopToCaller sizeSend:numArgs: sizeSendSuper:numArgs: sizeStoreInstVar: sizeStoreInstVarLong: sizeStoreLiteralVar: sizeStorePopInstVar: sizeStorePopInstVarLong: sizeStorePopLiteralVar: sizeStorePopRemoteTemp:inVectorAt: sizeStorePopTemp: sizeStoreRemoteTemp:inVectorAt: sizeStoreTemp:)
('bytecode generation' outOfRangeError:index:range:to:)
('results' printSchematicTempNamesOn:blockExtents:fromIndex: schematicTempNamesString)
('initialization' streamToMethod:)
!


!Encoder reorganize!
('visiting' accept:)
('results' allLiterals associationForClass literals tempNames tempNodes tempsAndBlockArgs undeclaredTemps unusedTempNames)
('temps' autoBind: bindAndJuggle: bindArg: bindBlockArg:within: bindBlockTemp: bindBlockTemp:within: bindTemp: bindTemp:in: fixTemp: floatTemp: maxTemp newTemp: newUndeclaredTemp:)
('encoding' cantStoreInto: doItInContextName encodeLiteral: encodeSelector: encodeVariable: encodeVariable:ifUnknown: encodeVariable:sourceRange:ifUnknown: environment litIndex: lookupVariable:ifAbsent: noteOptimizedSelector: sharableLitIndex: undeclared:)
('private' bindUndeclaredTemp: classEncoding global:name: interactive lookupInPools:ifFound: name:key:class:type:set: possibleNamesFor: possibleVariablesFor: reallyBind: warnAboutShadowed:)
('source mapping' globalSourceRanges noteSourceRange:forNode: rawSourceRanges sourceMap sourceRangeFor:)
('accessing' methodNodeClass selector selector:)
('error handling' notify: notify:at: requestor:)
('initialization' fillDict:with:mapping:to: init:context:notifying: initScopeAndLiteralTables noteSuper temps:literals:class:)
('release' release)
!


!DecompilerConstructor reorganize!
('visiting' accept:)
('constructor' codeAnyLitInd: codeAnyLiteral: codeAnySelector: codeArguments:block: codeArguments:temps:block: codeAssignTo:value: codeBlock:returns: codeBrace: codeCascade:messages: codeCascadedMessage:arguments: codeConstants codeEmptyBlock codeInst: codeMessage:selector:arguments: codeMethod:block:tempVars:primitive:class: codeSelector:code: codeSuper codeTemp: codeTemp:named: codeThisContext decodeIfNilWithReceiver:selector:arguments:)
('testing' isForClosures)
('initialization' method:class:literals:)
!


!CascadeNode reorganize!
('visiting' accept:)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('code generation (new scheme)' emitCodeForValue:encoder:)
('code generation' sizeCodeForValue:)
('accessing' messages receiver)
('printing' printOn:indent: printOn:indent:precedence: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence:)
('testing' isComplex)
('initialization' receiver:messages:)
!


!BraceNode reorganize!
('visiting' accept:)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools: elements maxElementsForConsArray)
('testing' blockAssociationCheck: numElements)
('enumerating' casesForwardDo: casesReverseDo:)
('code generation (new scheme)' emitCodeForValue:encoder: sizeCodeForValue:)
('code generation' selectorForShortForm:)
('printing' printOn:indent: printWithClosureAnalysisOn:indent:)
('initialization' elements: elements:sourceLocations: matchBraceStreamReceiver:messages: matchBraceWithReceiver:selector:arguments:)
!


!BlockNode reorganize!
('visiting' accept:)
('code generation (closures)' actualScope addHoistedTemps: addRemoteTemp:rootNode: addTempNode: analyseArguments:temporaries:rootNode: analyseTempsWithin:rootNode:assignmentPools: blockExtent computeCopiedValues: constructClosureCreationNode: deoptimize emitCodeForClosureValue:encoder: emitCodeForEvaluatedClosureValue:encoder: ifHasRemoteTempNodeEnsureInitializationStatementExists: makeTemporariesRemovable nilReadBeforeWrittenTemps noteOptimizedIn: optimizedBlockHoistTempsInto: postNumberingProcessTempsWithin:rootNode: reindexingLocalsDo:encoder: remoteTempNodeName removeTempNode:ifAbsent: sizeCodeForClosureValue: sizeCodeForEvaluatedClosureValue:)
('accessing' addArgument: arguments arguments: block closureCreationNode firstArgument nArgsSlot nArgsSlot: numberOfArguments optimized returnLast returnNilIfNoOther returnSelfIfNoOther: startOfLastStatement startOfLastStatement: temporaries temporaries: tempsMark tempsMark:)
('testing' generateAsClosure isBlockNode isComplex isJust: isJustCaseError isQuick printsInNewLine returns)
('code generation' code)
('printing' decompileString printArgumentsOn:indent: printOn:indent: printStatementsOn:indent: printTemporaries:on:doPrior: printWithClosureAnalysisArgumentsOn:indent: printWithClosureAnalysisOn:indent: printWithClosureAnalysisStatementsOn:indent: printWithClosureAnalysisTemporariesOn:indent:)
('code generation (new scheme)' emitCodeExceptLast:encoder: emitCodeForEvaluatedEffect:encoder: emitCodeForEvaluatedValue:encoder: emitCodeForValue:encoder: sizeCodeExceptLast: sizeCodeForEvaluatedEffect: sizeCodeForEvaluatedValue: sizeCodeForValue:)
('equation translation' statements statements:)
('initialization' arguments:statements:returns:from: noteSourceRangeStart:end:encoder: statements:returns:)
!


!AssignmentNode reorganize!
('visiting' accept:)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('code generation (new scheme)' emitCodeForEffect:encoder: emitCodeForValue:encoder: sizeCodeForEffect: sizeCodeForValue:)
('testing' isAssignmentNode isComplex)
('printing' printOn:indent: printOn:indent:precedence: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence:)
('equation translation' variable)
('initialization' toDoIncrement: value variable:value: variable:value:from: variable:value:from:sourceRange:)
!


!ObjectFinalizer reorganize!
('finalization' finalize)
('initialization' receiver:selector:argument:)
!


!Mutex reorganize!
('mutual exclusion' critical:)
('initialization' initialize)
!


!MouseOverHandler reorganize!
('event handling' noticeMouseOver:event: processMouseOver:)
('initialization' initialize)
!


!MouseClickState reorganize!
('actions' didClick didClickAndHalf didDoubleClick didDoubleClickAndHalf didDrag didTripleClick handleEvent:from:)
('private' lastClickLocalPosition)
('initialization' client:drag:click:clickAndHalf:dblClick:dblClickAndHalf:tripleClick:event:sendMouseButton2Activity:)
!


!MouseButtonEvent reorganize!
('accessing' mouseButton1Changed mouseButton2Changed mouseButton3Changed)
('dispatching' dispatchWith:localPosition: sentTo:localPosition:)
('private' setType:position:which:buttons:hand:stamp:)
('initialization' resetHandlerFields)
!


!UserInputEvent reorganize!
('accessing' buttons eventPosition wasHandled wasHandled:)
('modifier state' anyModifierKeyPressed commandAltKeyPressed controlKeyPressed macOptionKeyPressed rawMacOptionKeyPressed shiftPressed)
('initialization' resetHandlerFields)
!


!DropEvent reorganize!
('accessing' contents eventPosition wasHandled wasHandled:)
('dispatching' dispatchWith:localPosition: sentTo:localPosition: startDispatchFrom:)
('testing' isDropEvent)
('private' setPosition:contents:hand:)
('initialization' resetHandlerFields)
!


!MorphicEvent reorganize!
('accessing' hand timeStamp wasHandled wasHandled:)
('comparing' = hash)
('dispatching' dispatchWith:localPosition: sentTo:localPosition: startDispatchFrom:)
('testing' is: isDraggingEvent isDropEvent isKeyboard isKeystroke isMouse isMouseOver isWindowEvent)
('private' setTimeStamp:)
('initialization' resetHandlerFields)
!


!ScrollBar reorganize!
('access' color: interval: scrollDelta:pageDelta: value)
('accessing' adoptWidgetsColor: model:setValueSelector:)
('drawing' drawOn:)
('events' mouseButton1Down:localPosition: mouseStillDown:)
('event handling testing' handlesMouseDown: handlesMouseStillDown:)
('geometry' buttonExtent computeSlider expandSlider freeSliderRoom morphExtent: totalSliderRoom)
('initialization' buttonClass defaultColor initialize initializeDownButton initializeSlider initializeUpButton recreateSubmorphs sliderClass)
('model access' setValue: value:)
('scrolling' scrollByPage scrollDown scrollDown: scrollTo: scrollUp scrollUp: setNextDirectionFromEvent: sliderGrabbedAt: sliderReleased)
('testing' isHorizontal)
!


!PluggableButtonMorph reorganize!
('accessing' actWhen: action: actionSelector adoptWidgetsColor: icon: label: label:font: performAction roundButtonStyle:)
('drawing' draw3DLookOn: drawEmbossedLabelOn: drawInconOn: drawOn: drawRegularLabelOn: drawRoundGradientLookOn: fontToUse iconColor)
('events' mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseEnter: mouseLeave: mouseStillDown:)
('event handling testing' handlesMouseDown: handlesMouseOver: handlesMouseStillDown:)
('event handling' mouseStillDownStepRate)
('initialization' defaultBorderWidth defaultColor initialize model: model:stateGetter:action:label:)
('updating' update:)
('private' getModelState magnifiedIcon)
('testing' is: isPressed isRoundButton mouseIsOver)
('geometry' morphExtent:)
('geometry testing' isOrthoRectangularMorph morphContainsPoint:)
('scrollbar button' updateDownButtonImage updateLeftButtonImage updateRightButtonImage updateUpButtonImage)
!


!HandleMorph reorganize!
('events' keyStroke:)
('initialization' forEachPointDo: initialize)
('stepping and presenter' step)
('testing' stepTime)
!


!Morph class reorganize!
('class initialization' initialize)
('instance creation' initializedInstance)
('new-morph participation' includeInNewMorphMenu)
('release' unload)
!


!Monitor reorganize!
('synchronization' critical:)
('waiting-basic' wait waitUntil: waitWhile:)
('waiting-specific' waitFor: waitUntil:for: waitWhile:for:)
('waiting-timeout' waitFor:maxMilliseconds: waitFor:maxSeconds: waitMaxMilliseconds: waitMaxSeconds: waitUntil:for:maxMilliseconds: waitUntil:for:maxSeconds: waitUntil:maxMilliseconds: waitUntil:maxSeconds: waitWhile:for:maxMilliseconds: waitWhile:for:maxSeconds: waitWhile:maxMilliseconds: waitWhile:maxSeconds:)
('signaling-default' signal signalAll)
('signaling-specific' signal: signalAll: signalReallyAll)
('accessing' cleanup)
('private' checkOwnerProcess defaultQueue enter exit exitAndWaitInQueue:maxMilliseconds: isOwnerProcess privateCleanup queueDict queueFor: signalAllInQueue: signalLock:inQueue: signalQueue: waitInQueue:maxMilliseconds: waitWhile:inQueue:maxMilliseconds:)
('initialization' initialize)
!


!MatrixTransform2x3 reorganize!
('converting coordinates' autoHint: externalizeDelta: externalizePosition: externalizeScalar: internalizeDelta: internalizePosition: internalizeScalar: inverseTransform: transform:)
('accessing' at: at:put: degrees inverseTransformation position radians scale translation)
('comparing' = hash)
('composing' composedWith: composedWith:into:)
('element access' a11 a11: a12 a12: a13 a13: a21 a21: a22 a22: a23 a23:)
('objects from disk' byteSize bytesPerBasicElement restoreEndianness writeOn:)
('printing' matrixPrintString print printMatrixOn: printOn:)
('testing' doesNotRotate isIdentity isPureTranslation)
('private' setPosition: setRadians: setRadians:scale:position: setScale: setTranslation:)
('modifying' addOffset: rotateBy: scaleBy: scaleByNumber:rotateBy:)
('transforming rects' displayBoundsOfInverseTransformOf: displayBoundsOfInverseTransformOf:into: displayBoundsOfTransformOf: displayBoundsOfTransformOf:into: primDisplayBoundsOfInverseTransformOf:into: primDisplayBoundsOfTransformOf:into:)
('inspecting' customizeExplorerContents explorerContents inspectorClass)
('private - primitives' fixRoundingInPrimitives: floatInverseTransform: floatTransform: intInverseTransform: intTransform: primInverseTransform: primTransform:)
('*LinearAlgebra' asMatrix)
('initialization' setIdentiy)
!


!MessageTally reorganize!
('collecting leaves' bump: bump:fromSender: into:fromSender: leavesInto:fromSender:)
('comparing' < = > hash isPrimitives sonsOver: species)
('printing' fullPrintExactOn: fullPrintOn:threshold: leavesPrintExactOn: leavesPrintOn:threshold: printOn: printOn:total:totalTime:tallyExact: printSenderCountsOn: rootPrintOn:total:totalTime:threshold: treePrintOn:tabs:thisTab:total:totalTime:tallyExact:orThreshold:)
('printing format' maxClassNameSize maxClassNameSize: maxClassPlusSelectorSize maxClassPlusSelectorSize: maxTabs maxTabs:)
('reporting' report: report:cutoff: reportGCStatsOn: tally time)
('tallying' bumpBy: tally:by: tally:in:by: tallyPath:by: tallyPath:in:by:)
('private' class:method: copyWithTally: method primitives: process process:)
('accessing' reportOtherProcesses:)
('initialization' initialize spyAllEvery:on: spyEvery:on: spyEvery:onProcess:forMilliseconds:)
!


!Duration reorganize!
('public protocol' totalMilliSeconds totalMinutes totalNanoSeconds totalSeconds)
('ansi protocol' * + - / < = abs days hash hours minutes negated negative positive seconds)
('squeak protocol' // \\ asDelay isZero nanoSeconds printOn: roundTo: truncateTo:)
('private' seconds:nanoSeconds: storeOn: ticks)
('initialization' initialize)
!

DateAndTime initialize!

!DateAndTime class reorganize!
('ansi protocol' clockPrecision now year:day:hour:minute:second: year:day:hour:minute:second:offset: year:month:day:hour:minute:second: year:month:day:hour:minute:second:offset:)
('squeak protocol' date:time: epoch fromString: julianDayNumber: localOffset localTimeZone localTimeZone: midnight new noon readFrom: today tomorrow year:day: year:month:day: year:month:day:hour:minute: year:month:day:hour:minute:second:nanoSecond:offset: yesterday)
('smalltalk-80' fromSeconds:)
('initialization' initialize startUp:)
!


!QSystemTally reorganize!
('converting' asArray asSortedCollection:)
('report' bump:fromSender: fullPrintOn:threshold:time:reportOnly: into:fromSender: leavesInto:fromSender: leavesPrintOn:threshold:time: printOn: printOn:total:totalTime:tallyExact: rootPrintOn:total:totalTime:threshold:reportOnly: sonsOver: treePrintOn:tabs:thisTab:total:totalTime:tallyExact:orThreshold:)
('tallying' bumpBy: tally:in:by: tallyMethod:by: tallyPath:in:by:)
('copying' copyWithTally:)
('testing' isPrimitives)
('accessing' maxClassNameSize maxClassPlusSelectorSize maxTabs method process process: tally)
('initialization' class:method: primitives:)
!


!Decompiler reorganize!
('control' blockForCaseTo: blockTo: checkForBlock:selector:arguments: checkForBlockCopy: checkForClosureCopy:arguments: doClosureCopyCopiedValues:numArgs:blockSize: statementsForCaseTo: statementsTo:)
('instruction decoding' blockReturnTop case: doDup doPop doStore: jump: jump:if: methodReturnConstant: methodReturnReceiver methodReturnTop popIntoLiteralVariable: popIntoReceiverVariable: popIntoRemoteTemp:inVectorAt: popIntoTemporaryVariable: pushActiveContext pushClosureCopyNumCopiedValues:numArgs:blockSize: pushConsArrayWithElements: pushConstant: pushLiteralVariable: pushNewArrayOfSize: pushReceiver pushReceiverVariable: pushRemoteTemp:inVectorAt: pushTemporaryVariable: send:super:numArgs: storeIntoLiteralVariable: storeIntoReceiverVariable: storeIntoRemoteTemp:inVectorAt: storeIntoTemporaryVariable:)
('private' blockScopeRefersOnlyOnceToTemp: constructorForMethod: convertToDoLoop interpretNextInstructionFor: methodRefersOnlyOnceToTemp: popTo: quickMethod sawBlueBookBlock sawClosureBytecode scanBlockScopeFor:from:to:with:scanner:)
('public access' decompile:in: decompile:in:method: decompile:in:method:using: decompileBlock: tempAt:)
('initialization' initSymbols: mapFromBlockStartsIn:toTempVarsFrom:constructor: withTempNames:)
!


!MethodContext reorganize!
('accessing' activeHome activeOuterContext closure contextForLocalVariables hasInstVarRef hasMethodReturn home isExecutingBlock method methodReturnContext outerContext receiver removeSelf tempAt: tempAt:put:)
('private' aboutToReturn:through: instVarAt:put: setSender:receiver:method:arguments: setSender:receiver:method:closure:startpc: startpc)
('private-exceptions' isHandlerContext isUnwindContext receiver: restartWithNewReceiver: swapReceiver:)
('private-debugger' cachesStack hideFromDebugger)
('printing' printDetails: printOn: printString)
('closure support' asContext contextTag isClosureContext)
('nil' cannotReturn:)
('instruction decoding (closures)' blockReturnTop pushConsArrayWithElements:)
('system simulation' pushArgs:from:)
('testing' atEnd isMethodContext)
('inspecting' inspectorClass)
('initialization' privRefresh privRefreshWith:)
!


!BlockContext reorganize!
('accessing' activeHome caller closureHome contextForLocalVariables copyForSaving fixTemps hasInstVarRef hasMethodReturn home isBlock isExecutingBlock method methodReturnContext numArgs receiver reentrant tempAt: tempAt:put:)
('controlling' doWhileFalse: doWhileTrue: repeat repeatWithGCIf: whileFalse whileFalse: whileTrue whileTrue:)
('evaluating' durationToRun ifError: timeToRun value value: value:value: value:value:value: value:value:value:value: valueSupplyingAnswer: valueSupplyingAnswers: valueSuppressingAllMessages valueSuppressingMessages: valueSuppressingMessages:supplyingAnswers: valueWithArguments: valueWithEnoughArguments: valueWithExit valueWithPossibleArgs: valueWithPossibleArgument: valueWithPossibleArgument:and: valueWithin:onTimeout:)
('exceptions' assert ensure: ifCurtailed: on:do: on:do:on:do: on:do:on:do:on:do: onDNU:do: valueUninterruptably)
('instruction decoding' blockReturnTop)
('printing' decompile decompileString fullPrintOn: printOn:)
('scheduling' asContext fork forkAndWait forkAt: forkAt:named: forkNamed: newProcess newProcessWith: valueAt:)
('system simulation' pushArgs:from: stepToSendOrReturn)
('private' aboutToReturn:through: cannotReturn: copyTo:blocks: endPC hideFromDebugger instVarAt:put: myEnv privHome: startpc valueError valueUnpreemptively)
('services' valueWithRequestor:)
('initialization' home:startpc:nargs: privRefresh)
!


!RelativeInstructionPrinter reorganize!
('instruction decoding' jump: jump:if: send:super:numArgs:)
('printing' print: printCode printInstructionsOn: printInstructionsOn:do:)
('initialization' printCode:)
!


!InstructionPrinter reorganize!
('accessing' indent method method: printPC printPC:)
('instruction decoding' blockReturnTop doDup doPop jump: jump:if: methodReturnConstant: methodReturnReceiver methodReturnTop popIntoLiteralVariable: popIntoReceiverVariable: popIntoRemoteTemp:inVectorAt: popIntoTemporaryVariable: pushActiveContext pushClosureCopyNumCopiedValues:numArgs:blockSize: pushConsArrayWithElements: pushConstant: pushLiteralVariable: pushNewArrayOfSize: pushReceiver pushReceiverVariable: pushRemoteTemp:inVectorAt: pushTemporaryVariable: send:super:numArgs: storeIntoLiteralVariable: storeIntoReceiverVariable: storeIntoRemoteTemp:inVectorAt: storeIntoTemporaryVariable:)
('printing' print:)
('initialization' indent: printInstructionsOn: printInstructionsOn:do:)
!


!InstVarRefLocator reorganize!
('instruction decoding' popIntoReceiverVariable: pushReceiverVariable: storeIntoReceiverVariable:)
('initialization' interpretNextInstructionUsing:)
!


!BlockStartLocator reorganize!
('instruction decoding' jump: pushClosureCopyNumCopiedValues:numArgs:blockSize: send:super:numArgs:)
('initialization' initialize)
!


!BlockLocalTempCounter reorganize!
('instruction decoding' blockReturnTop doDup doPop jump: jump:if: methodReturnConstant: methodReturnReceiver methodReturnTop popIntoLiteralVariable: popIntoReceiverVariable: popIntoRemoteTemp:inVectorAt: popIntoTemporaryVariable: pushActiveContext pushClosureCopyNumCopiedValues:numArgs:blockSize: pushConsArrayWithElements: pushConstant: pushLiteralVariable: pushNewArrayOfSize: pushReceiver pushReceiverVariable: pushRemoteTemp:inVectorAt: pushTemporaryVariable: send:super:numArgs:)
('private' doJoin)
('initialization' tempCountForBlockAt:in: testTempCountForBlockAt:in:)
!


!EventSensor reorganize!
('accessing' eventTicklerProcess flushAllButDandDEvents flushEvents nextEvent peekButtons peekEvent peekMousePt peekPosition)
('mouse' createMouseEvent)
('private' eventTickler flushNonKbdEvents installEventTickler isKbdEvent: lastEventPoll primInterruptSemaphore: primKbdNext primKbdPeek primMouseButtons primMousePt primSetInterruptKey:)
('private-I/O' fetchMoreEvents fetchMoreEventsDiscardingMouseEvents: mapButtons:modifiers: primGetNextEvent: primSetInputSemaphore: processKeyboardSensorEvent: processMouseSensorEvent: processSensorEvent:discardingMouseEvents: queueEvent:)
('test' printEventBuffer: test)
('initialization' initialize shutDown startUp)
!


!InputSensor reorganize!
('buttons' buttons)
('cursor' currentCursor currentCursor:)
('joystick' joystickButtons: joystickOn: joystickXY: testJoystick:)
('keyboard' flushKeyboard kbdTest keyboard keyboardPeek keyboardPressed)
('modifier keys' anyModifierKeyPressed commandAltKeyPressed controlKeyPressed macOptionKeyPressed rawMacOptionKeyPressed shiftPressed)
('mouse' isAnyButtonPressed isMouseButton1Pressed isMouseButton2Pressed isMouseButton3Pressed mouseButtons mousePoint mousePoint: noButtonPressed peekButtons peekMousePt waitButton waitButtonOrKeyboard waitClickButton waitNoButton)
('tablet' hasTablet tabletExtent tabletPoint tabletPressure tabletTimestamp)
('user interrupts' eventTicklerProcess installInterruptWatcher interruptWatcherProcess setInterruptKey: userInterruptWatcher)
('private' characterForKeycode: primCursorLocPut: primCursorLocPutAgain: primInterruptSemaphore: primKbdNext primKbdPeek primMouseButtons primMousePt primReadJoystick: primSetInterruptKey: primTabletGetParameters: primTabletRead:)
('initialization' flushEvents shutDown startUp)
!


!FeatureSpec reorganize!
('accessing' name provides provides: requires requires:)
('printing' printOn:)
('initialization' initialize provides:requires:)
!


!FeatureRequirement reorganize!
('accessing' maxVersion minRevision minVersion name packageFileName pathName: requirements)
('requires' findPackageFileAsReqOf: install require requireUnlessIn:asReqOf:main:)
('testing' isAlreadySatisfied)
('printing' printOn:)
('private' cuisAndCuisPackagesSubdirectoriesOf:do: placesToLookForPackagesDo:)
('comparing' =)
('initialization' name:minVersion:minRevision:maxVersion:)
!


!Feature reorganize!
('accessing' name revision version)
('requires')
('testing' satisfies:)
('printing' printOn: printVersionRevisionOn:)
('private')
('incrementing' newRevision newVersion)
('initialization' name:version:revision:)
!

ExternalSemaphoreTable initialize!

!ExternalSemaphoreTable class reorganize!
('accessing' clearExternalObjects externalObjects registerExternalObject: safelyRegisterExternalObject: safelyUnregisterExternalObject: unregisterExternalObject:)
('initialization' initialize)
!


!UndeclaredVariableWarning reorganize!
('exceptionDescription' defaultAction)
('initialization' defaultResumeValue name:selector:class:)
!


!NotYetImplemented reorganize!
('accessing' receiverClass selector)
('handling' defaultAction)
('initialization' receiverClass:selector:)
!


!MessageNotUnderstood reorganize!
('exceptionBuilder' message: messageText receiver:)
('exceptionDescription' defaultAction isResumable message receiver)
('accessing' reachedDefaultHandler)
('initialization' initialize)
!


!FileDoesNotExistException reorganize!
('accessing' readOnly:)
('exceptionDescription' defaultAction)
('initialization' initialize)
!


!EllipseMidpointTracer reorganize!
('computing' stepInY)
('initialization' on:)
!


!TextEditor reorganize!
('accessing' currentAttributes currentCharacterStyleOrNil currentParagraphStyle lastFont lastParagraphStyleOrNil pointBlock replaceSelectionWith: setSearch: text)
('accessing-selection' hasSelection markIndex markIndex: markIndex:pointIndex: pointIndex pointIndex: selection selectionAsStream selectionIntervalsDo: startBlock startIndex stopBlock stopIndex)
('as yet unclassified' totalTextHeight visibleHeight)
('attributes' changeEmphasisOrAlignment offerColorMenu offerFontMenu)
('current selection' recomputeSelection)
('displaying' flash)
('editing keys' align: cancelEdits: changeEmphasis: changeLineEndsToLf: chooseColor compareToClipboard: copyHiddenInfo enclose: hiddenInfo inOutdent:delta: indent: makeCapitalized: makeLowercase: makeUppercase: offerColorMenu: offerFontMenu: outdent: undo:)
('events' clickAndHalf doubleClickAndHalf mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('menu messages' acceptContents align cancelEdits chooseAlignment compareToClipboard copySelection cut find findAgain openHelp pasteRecent setSearchString)
('model access' model:)
('new selection' afterSelectionInsertAndSelect: correctFrom:to:with: deselectAndPlaceCursorAt: insertAndSelect:at: lineSelectAndEmptyCheck: selectFrom:to: selectInterval: selectLine)
('nonediting/nontyping keys' cursorEnd: cursorHome: help: setSearchString:)
('parenblinking' blinkParen blinkParenAt: blinkPrevParen: clearParens)
('scrolling' scrollBy:)
('typing support' addString: backTo: dispatchOn: processKeyStroke: setEmphasisHereFromText setEmphasisHereFromTextForward:)
('typing/selecting keys' backWord: find: findAgain: forwardDelete: selectAll selectAll:)
('private' addAttributesForPasting: applyAttribute: beginningOfLine: endOfLine: findAndReplaceMany: findAndReplaceOnce: indent:fromStream:toStream: isDisjointFrom: moveCursor:forward:event: nullText pageHeight privateCurrentString sameColumn:newLine:forward: storeSelectionInComposition unapplyAttribute: zapMultiSelection)
('binding' bindingOf:)
('commands' insertMorph:at: removeMorph:)
('undo & redo' flushUndoRedoCommands offerUndoHistory redo redo: undo)
('menu' getMenu)
('initialization' initialize resetState textComposition:)
!


!SimpleEditor reorganize!
('accessing' replaceSelectionWith: string)
('accessing-selection' hasSelection markIndex markIndex: markIndex:pointIndex: pointIndex pointIndex: selection startIndex stopIndex)
('editing keys' makeCapitalized: makeLowercase: makeUppercase:)
('events' clickAndHalf mouseDown:index: mouseMove:index:)
('menu messages' copySelection cut)
('new selection' selectFrom:to: selectLine)
('nonediting/nontyping keys' cursorEnd: cursorHome:)
('typing support' addString: backTo: dispatchOn: processKeyStroke:)
('typing/selecting keys' forwardDelete:)
('testing' isAtEnd isAtStart)
('private' privateCurrentString sameColumn:newLine:forward:)
('initialization' changeString: initialize resetState)
!


!DisplayScreen reorganize!
('displaying' copyBits:from:at:clippingBox:rule:fillColor: copyBits:from:at:clippingBox:rule:fillColor:map: flash: forceDamageToScreen:)
('other' deferUpdates: forceDisplayUpdate forceToScreen forceToScreen: fullScreenMode: height newDepth: primitiveDeferUpdates: restore restoreAfter: supportedDisplayDepths supportsDisplayDepth: width)
('disk I/O' objectForDataStream:)
('private' beDisplay findAnyDisplayDepth findAnyDisplayDepthIfNone: newDepthNoRestore: primRetryShowRectLeft:right:top:bottom: primShowRectLeft:right:top:bottom: setExtent:depth:)
('system startup' shutDown)
!


!Form reorganize!
('accessing' bits bits: bitsSize center depth depth: extent getCanvas height nativeDepth offset offset: size width)
('analyzing' cgForPixelValue:orNot: colorsUsed dominantColor innerPixelRectFor:orNot: pixelCompare:with:at: primCountBits rectangleEnclosingPixelsNotOfColor: tallyPixelValuesInRect:into: xTallyPixelValue:orNot: yTallyPixelValue:orNot:)
('bordering' border:width:rule:fillColor: borderFormOfWidth:sharpCorners: borderWidth: shapeBorder:width: shapeBorder:width:interiorPoint:sharpCorners:internal:)
('color mapping' bitPatternFor: colormapIfNeededFor: colormapIfNeededForDepth: makeBWForm: mapColor:to: mapColors:to: maskingMap newColorMap pixelValueFor: reducedPaletteOfSize:)
('converting' as8BitColorForm asCursorForm asFormOfDepth: asFormOfNativeDepth: asGrayScale asGrayScale: colorReduced copyWithColorsReducedTo: icon orderedDither32To16)
('copying' blankCopyOf:scaledBy: contentsOfArea:into: copy: copy:from:in:rule: copy:from:to:rule: copyBits:at:translucent: copyBits:from:at:clippingBox:rule:fillColor: copyBits:from:at:clippingBox:rule:fillColor:map: copyBits:from:at:colorMap: postCopy)
('display box access' boundingBox)
('displaying' displayOn:at:clippingBox:rule:fillColor: displayResourceFormOn: paintBits:at:translucent:)
('encoding' addDeltasFrom: deltaFrom: deltaFrom:at:)
('fileIn/Out' hibernate printOn: readAttributesFrom: readBitsFrom: readFrom: readFromOldFormat: storeBitsOn:base: storeOn: storeOn:base: unhibernate writeAttributesOn: writeBMPfileNamed: writeBitsOn: writeJPEGfileNamed: writeJPEGfileNamed:progressive: writeOn: writeOnMovie: writeUncompressedOn:)
('filling' anyShapeFill convexShapeFill: eraseShape: fill:rule:fillColor: fillFromXColorBlock: fillFromXYColorBlock: fillFromYColorBlock: findShapeAroundSeedBlock: floodFill2:at: floodFillMapFrom:to:mappingColorsWithin:to: shapeFill:interiorPoint: shapeFill:seedBlock:)
('image manipulation' replaceColor:withColor: smear:distance: tallyPixelValues trimBordersOfColor:)
('other' fixAlpha formForColorCount: preMultiplyAlpha primPrintHScale:vScale:landscape:)
('pixel access' colorAt: colorAt:put: isTransparentAt: pixelValueAt: pixelValueAt:put:)
('scaling, rotation' flippedBy:centerAt: magnify:by: magnify:by:smoothing: magnify:to:smoothing: magnifyBy: magnifyTo: rotateBy: rotateBy:smoothing: shrink:by:)
('testing' couldBeTranslucent is: isAllWhite isBigEndian isLittleEndian mightBeTranslucent shouldPreserveContents)
('transitions' fadeImage:at:indexAndMaskDo: fadeImageCoarse:at: fadeImageFine:at: fadeImageHor:at: fadeImageHorFine:at: fadeImageSquares:at: fadeImageVert:at: pageWarp:at:forward: slideImage:at:delta: wipeImage:at:clippingBox:rectForIndex: wipeImage:at:delta: wipeImage:at:delta:clippingBox: zoomIn:orOutTo:at:vanishingPoint: zoomInTo:at: zoomOutTo:at:)
('private' hackBits: initFromArray: privateFloodFillValue: setExtent:depth: setExtent:depth:bits:)
('comparing' =)
('*Graphics-Files-Additional' writePNGfileNamed:)
('*extendedClipboard-Win32' fromWin32BitMap:)
('initialization' fromDisplay: swapEndianness)
('system startup' shutDown)
!


!DebuggerMethodMapForBlueBookMethods reorganize!
('accessing' namedTempAt:in: namedTempAt:put:in: tempNamesForContext:)
('initialization' forMethod:methodNode:)
!


!DebuggerMethodMap reorganize!
('source mapping' abstractSourceMap rangeForPC:contextIsActiveContext: sourceText)
('accessing' markRecentlyUsed method namedTempAt:in: namedTempAt:put:in: tempNamesForContext: tempsAndValuesForContext: timestamp)
('initialization' forMethod:methodNode:)
!


!WeakRegistry reorganize!
('finalization' finalizeValues)
('adding' add: add:executor:)
('accessing' keys size species)
('private' protected:)
('enumerating' do:)
('removing' remove:ifAbsent:)
('printing' printElementsOn:)
('initialization' initialize:)
!


!StandardSourceFileArray class reorganize!
('initialization' install new:)
!


!StandardSourceFileArray reorganize!
('accessing' at: at:put: pointerScaleForWriting size)
('sourcePointer conversion' checkOKToAdd:at:in: fileIndexFromSourcePointer: filePositionFromSourcePointer: sourcePointerFromFileIndex:andPosition:)
('initialization' initialize initialize:)
!


!CuisSourceFileArray reorganize!
('sourcePointer conversion' checkOKToAdd:at:in: fileIndexFromSourcePointer: filePositionFromSourcePointer: sourcePointerFromFileIndex:andPosition:)
('accessing' at: at:put: pointerScaleForWriting size)
('initialization' initialize initialize:)
!


!Semaphore reorganize!
('communication' signal wait waitTimeoutMSecs: waitTimeoutSeconds:)
('mutual exclusion' critical: critical:ifCurtailed: critical:ifError: critical:ifLocked:)
('comparing' = hash)
('testing' isSignaled)
('initialization' initSignals terminateProcess)
!


!CompiledMethod reorganize!
('accessing' clearFlag defaultSelector endPC flag flushCache frameSize initialPC methodClass methodClass: methodClassAssociation methodReference numArgs numLiterals numTemps primitive properties properties: returnField scanner searchForClass searchForSelector selector selector: trailer)
('comparing' = equivalentTo: hash)
('testing' hasNewPropertyFormat hasReportableSlip is: isAbstract isBlueBookCompiled isClosureCompiled isConflict isDisabled isDisabled: isExplicitlyRequired isExplicitlyRequired: isImplicitlyRequired isImplicitlyRequired: isInstalled isProvided isProvided: isQuick isRequired isRequired: isReturnField isReturnSelf isReturnSpecial isSubclassResponsibility isSubclassResponsibility: usesClosureBytecodes)
('printing' abstractSymbolic decompileString longPrintOn: longPrintOn:indent: longPrintRelativeOn:indent: primitiveErrorVariableName printOn: printPrimitiveOn: storeLiteralsOn:forClass: storeOn: symbolic symbolicLinesDo:)
('literals' allLiterals hasLiteral: hasLiteralSuchThat: hasLiteralThorough: header headerDescription indexOfLiteral: literalAt: literalAt:put: literalStrings literals literalsDo: objectAt: objectAt:put: refersToLiteral: sendsSelector: xtraBindings)
('scanning' hasInstVarRef messages messagesDo: messagesSequence readsField: readsRef: scanFor: scanLongLoad: scanLongStore: scanVeryLongLoad:offset: scanVeryLongStore:offset: sendsToSuper writesField: writesRef:)
('source code management' checkOKToAdd:at:in: copyWithTempNames: copyWithTempsFromMethodNode: destroySourcePointer fileIndex filePosition getPreambleFrom:at: getSource getSourceFor:in: getSourceFromFile holdsTempNames linesOfCode putSource:fromParseNode:class:category:inFile:priorMethod: putSource:fromParseNode:class:category:withStamp:inFile:priorMethod: putSource:fromParseNode:inFile:withPreamble: qCompress: qDecompressFrom: setSourcePointer: setSourcePosition:inFile: sourceClass sourceFileStream sourcePointer sourceSelector tempNamesString)
('file in/out' objectForDataStream: readDataFrom:size: storeDataOn: zapSourcePointer)
('evaluating' valueWithReceiver:arguments:)
('decompiling' compilerClass decompile decompileWithTemps decompilerClass methodForDecompile methodNode parserClass)
('breakpoints' hasBreakpoint)
('code analysis' scanForEqSmallConstant)
('debugger support' abstractPCForConcretePC: blockExtentsInto:from:to:scanner:numberer: blockExtentsToTempsMap debuggerMap mapFromBlockKeys:toSchematicTemps: pcPreviousTo: startpcsToBlockExtents tempsSubSequenceFrom:)
('private' markerOrNil penultimateLiteral penultimateLiteral: replace:with:in:)
('accessing-pragmas & properties' pragmaAt: pragmas propertyKeysAndValuesDo: propertyValueAt: propertyValueAt:ifAbsent: propertyValueAt:put: removeProperty: removeProperty:ifAbsent:)
('closures' containsBlockClosures embeddedBlockClosures)
('tracing' outboundPointersDo: referenceDescriptionTo:)
('converting' asString)
('time stamp' author dateAndTime dateMethodLastSubmitted dateSortingValue timeStamp)
('inspecting' explorerContents inspectorClass)
('organization' category)
('initialization' copyWithTrailerBytes: needsFrameSize:)
!


!ByteArray reorganize!
('accessing' atAllPut: byteAt: byteAt:put: byteSize bytesPerElement indexOf:startingAt:)
('platform independent access' doubleAt:bigEndian: doubleAt:put:bigEndian: longAt:bigEndian: longAt:put:bigEndian: shortAt:bigEndian: shortAt:put:bigEndian: unsignedLongAt:bigEndian: unsignedLongAt:put:bigEndian: unsignedShortAt:bigEndian: unsignedShortAt:put:bigEndian:)
('converting' asByteArray asString base64Encoded hex)
('private' asByteArrayPointer defaultElement replaceFrom:to:with:startingAt:)
('comparing' hash)
('printing' printOn: storeOn:)
('testing' isLiteral)
('*FFI-Kernel' asExternalPointer booleanAt: booleanAt:put: doubleAt: doubleAt:put: floatAt: floatAt:put: integerAt:put:size:signed: integerAt:size:signed: isExternalAddress isNull pointerAt: pointerAt:put: signedByteAt: signedByteAt:put: signedCharAt: signedCharAt:put: signedLongAt: signedLongAt:put: signedLongLongAt: signedLongLongAt:put: signedShortAt: signedShortAt:put: structAt:length: structAt:put:length: unsignedByteAt: unsignedByteAt:put: unsignedCharAt: unsignedCharAt:put: unsignedLongAt: unsignedLongAt:put: unsignedLongLongAt: unsignedLongLongAt:put: unsignedShortAt: unsignedShortAt:put: voidAt: voidAt:put:)
('initialization' readHexFrom:)
!


!Bitmap reorganize!
('filing' compress:toByteArray: compressToByteArray decompress:fromByteArray:at: encodeBytesOf:in:at: encodeInt: encodeInt:in:at: readCompressedFrom: restoreEndianness writeOn: writeUncompressedOn:)
('printing' printOn:)
('accessing' atAllPut: bitPatternForDepth: byteAt: byteAt:put: byteSize copyFromByteArray2:to: copyFromByteArray: defaultElement integerAt: integerAt:put: pixelValueForDepth: primFill: replaceFrom:to:with:startingAt:)
('testing' isColormap)
('as yet unclassified' asByteArray copy)
('*Compression' compressGZip)
('initialization' fromByteStream:)
!


!CodePackageFile reorganize!
('services' install:)
('accessing' classesToRemove methodsToRemove requires summary)
('change record types' doIt: possibleDescription: provides: requires:)
('initialization' buildFileStream:packageName:fullName: initialize installFileStream:packageName:fullName:)
!


!CodeFile reorganize!
('accessing' classAt: classes description fullName fullName: name removeClass: removeDoIts renameClass:to: sourceSystem summary)
('private' classDefinition:with: getClass: metaClassDefinition:with: msgClassComment:with: possibleSystemSource: removedMethod:with:)
('change record types' classComment: classDefinition: doIt: method: preamble:)
('fileIn/fileOut' askForDoits fileIn fileInDoits fileOut fileOutDoits: fileOutOn:)
('reading' buildFrom:)
('xtras' allMethodReferences)
('initialization' fromFileNamed: initialize)
!


!Clipboard reorganize!
('accessing' chooseRecentClipping retrieveMorph retrieveObject retrieveStringOrText storeObject:)
('primitives' primitiveClipboardString primitiveClipboardString:)
('private' idFor: noteRecentClipping: retrieveIdOrStringFromOS stringOrIdFor:)
('initialization' initialize)
!


!ClassBuilder reorganize!
('class definition' class:instanceVariableNames:unsafe: name:inEnvironment:subclassOf:type:instanceVariableNames:classVariableNames:poolDictionaries:category: name:subclassOf:type:instanceVariableNames:classVariableNames:poolDictionaries:category: name:subclassOf:type:instanceVariableNames:classVariableNames:poolDictionaries:category:unsafe: needsSubclassOf:type:instanceVariables:from: newSubclassOf:type:instanceVariables:from: recompile:from:to:mutate: silentlyMoveInstVarNamed:from:to:after:)
('class format' computeFormat:instSize:forSuper:ccIndex: format:variable:words:pointers:weak:)
('validation' validateClass:forMoving:downTo: validateClass:forMoving:upTo: validateClassName: validateClassvars:from:forSuper: validateInstvars:from:forSuper: validateSubclass:canKeepLayoutFrom:forSubclassFormat: validateSubclassFormat:from:forSuper:extra: validateSuperclass:forSubclass:)
('private' informUserDuring: privateNewSubclassOf: privateNewSubclassOf:from: recordClass:replacedBy: reservedNames showProgressFor: tooDangerousClasses)
('public' class:instanceVariableNames: moveInstVarNamed:from:to:after: superclass:subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: superclass:variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: superclass:variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: superclass:variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: superclass:weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:)
('class mutation' mutate:to: reshapeClass:toSuper: update:to:)
('initialization' doneCompiling: initialize)
!


!CharacterScanner reorganize!
('private' setActualFont: setFont text: textColor:)
('scanning' placeEmbeddedObject placeEmbeddedObject: scanCharactersFrom:to:in:rightX:stopConditions:kern:)
('stop conditions' doNewLine tab tabDestX)
('initialization' initialize)
!


!ChangeSet reorganize!
('change logging' noteAddClass: noteChangeClass:from: noteClassMoveToOtherPackage: noteMethodMoveToOtherPackage:forClass: noteNewMethod:forClass:selector:priorMethod: noteRecategorizationOfClass: noteRemoveSelector:class:priorMethod:lastMethodInfo: noteRenameClass:as:)
('accessing' author classMovesToOtherPackage classRemoves codePackage codePackage: editPostscript hasPostscript hasUnsavedChanges hasUnsavedChanges: isForBaseSystem isForBaseSystem: methodChanges methodInfoFromRemoval: name name: printOn: removePostscript)
('testing' canHavePreambleAndPostscript isEmpty isMoribund methodsWithoutClassifications okayToRemove)
('method changes' changedMessageList changedMessageListAugmented messageListForChangesWhich:ifNone: methodChangesAtClass: removeSelectorChanges:class: selectorsInClass:)
('class changes' changedClassCategories changedClassNames changedClasses classChangeAt: fatDefForClass: noteClassForgotten: noteClassStructure: noteCommentClass: noteRemovalOf: noteReorganizeClass: trimHistory)
('moving changes' hasPreamble methodsWithAnyInitialsOtherThan: methodsWithInitialsOtherThan: methodsWithoutComments removeClassChanges: removePreamble)
('fileIn/Out' askAddedInstVars: askRemovedInstVars: askRenames:addTo:using: assurePostscriptExists assurePreambleExists checkForConversionMethods checkForSlips defaultChangeSetDirectory fileOut fileOutMethodAdditionsFor:on: fileOutMethodChangesFor:on: fileOutMethodMovedToOtherPackagesFor:on: fileOutMethodRemovalsFor:on: fileOutOn: fileOutPSFor:on: fileOutPostscriptOn: fileOutPreambleOn: objectForDataStream: postscriptString postscriptString: preambleString preambleString: preambleTemplate)
('private' addCoherency: atClass:add: atClass:includes: atSelector:class: changeRecorderFor: fileOutClassDefinition:on: oldNameFor:)
('system change notifications' classAdded:inCategory: classCommented: classDefinitionChangedFrom:to: classRecategorized:from:to: classRemoved:fromCategory: classRenamed:from:to:inCategory: classReorganized: methodAdded:selector:inClass:requestor: methodAdded:selector:inProtocol:class:requestor: methodChangedFrom:to:selector:inClass:requestor: methodRemoved:selector:inProtocol:class: selectorRecategorized:from:to:inClass:)
('initialization' clear initialize wither)
!


!BlockClosure reorganize!
('accessing' argumentCount copiedValueAt: home isBlock method numArgs numCopiedValues outerContext receiver size startpc)
('controlling' repeat repeatWithGCIf: whileFalse whileFalse: whileNil: whileNotNil: whileTrue whileTrue:)
('evaluating' durationToRun ifError: simulateValueWithArguments:caller: timeToRun timeToRunWithoutGC value value: value:value: value:value:value: value:value:value:value: valueAt: valueNoContextSwitch valueNoContextSwitch: valueSupplyingAnswer: valueSupplyingAnswers: valueSuppressingAllMessages valueSuppressingMessages: valueSuppressingMessages:supplyingAnswers: valueWithArguments: valueWithExit valueWithPossibleArgs: valueWithPossibleArgument: valueWithPossibleArgument:and: valueWithin:onTimeout:)
('exceptions' assert ensure: ifCurtailed: on:do: on:do:on:do: on:do:on:do:on:do: onDNU:do: valueUninterruptably)
('printing' decompile fullPrintOn: printOn: printStack:)
('scheduling' asContext fork forkAndWait forkAt: forkAt:named: forkNamed: newProcess newProcessWith:)
('private' asContextWithSender: copyForSaving grabProcessor grabProcessorFor:onTimeout: grabProcessorOnlyFor: reentrant valueUnpreemptively)
('testing' hasMethodReturn isClean isClosure isDead)
('error handing' numArgsError:)
('events-support' asMinimalRepresentation isReceiverOrAnyArgumentGarbage)
('debugger access' sender)
('objects from disk' objectForDataStream:)
('services' timeProfile valueWithRequestor:)
('initialization' outerContext:startpc:numArgs:copiedValues:)
!


!Pen reorganize!
('accessing' direction location)
('operations' color: down fill:color: go: goto: home north place: turn: up)
('geometric designs' dragon: filberts:side: hilbert:side: hilberts: mandala: spiral:angle: web)
('private' location:direction:penDown: sourceForm:)
('initialization' defaultNib: roundNib: squareNib:)
!


!Metaclass reorganize!
('accessing' allInstances category isMeta name soleInstance theMetaClass theNonMetaClass)
('copying' postCopy)
('instance creation' new)
('instance variables' addInstVarName: removeInstVarName:)
('pool variables' classPool)
('class hierarchy' addObsoleteSubclass: addSubclass: obsoleteSubclasses removeObsoleteSubclass: removeSubclass: subclasses subclassesDo: subclassesDoGently:)
('compiling' acceptsLoggingOfCompilation bindingOf: possibleVariablesFor:continuedFrom: wantsChangeSetLogging wantsRecompilationProgressReported)
('fileIn/Out' definition fileOutInitializerOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing: nonTrivial objectForDataStream: storeDataOn:)
('testing' canZapMethodDictionary isObsolete)
('enumerating' allInstancesDo:)
('private' replaceObsoleteInstanceWith:)
('initialization' adoptInstance:from: instanceVariableNames:)
!


!Class reorganize!
('accessing' classPool classPoolFrom: name)
('testing' hasMethods isObsolete)
('copying' copy)
('class name' rename:)
('instance variables' addInstVarName: removeInstVarName:)
('class variables' addClassVarName: allClassVarNames classVarNames ensureClassPool removeClassVarName:)
('pool variables' addSharedPool: allSharedPools removeSharedPool: sharedPools)
('compiling' binding bindingOf: compileAllFrom: localBindingOf: possibleVariablesFor:continuedFrom:)
('subclass creation' subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:)
('fileIn/Out' fileOut fileOutInitializerOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing: fileOutPool:onFileStream: fileOutSharedPoolsOn: objectForDataStream: reformatAll shouldFileOutPool: shouldFileOutPools storeDataOn:)
('accessing class hierarchy' addSubclass: removeSubclass: subclasses subclassesDo: subclassesDoGently:)
('private' setName: spaceUsed)
('initialization' declare: sharing: superclass:methodDict:format:name:organization:instVarNames:classPool:sharedPools: superclass:methodDictionary:format:)
('release' obsolete removeFromSystem removeFromSystem: removeFromSystemUnlogged unload)
!


!ClassDescription reorganize!
('accessing' classVersion comment comment: comment:stamp: hasComment theMetaClass theNonMetaClass)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('printing' classVariablesString instanceVariablesString printOn: printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('instance variables' addInstVarName: allInstVarNamesEverywhere browseClassVarRefs checkForInstVarsOK: chooseClassVarName chooseInstVarAlphabeticallyThenDo: chooseInstVarThenDo: classThatDefinesClassVariable: classThatDefinesInstanceVariable: forceNewFrom: instVarNameForIndex: instVarNames removeInstVarName: renameInstVar:to: renameSilentlyInstVar:to: replaceSilently:to:)
('accessing method dictionary' addAndClassifySelector:withMethod:inProtocol:notifying: addSelector:withMethod:notifying: allMethodCategoriesIntegratedThrough: induceMDFault methodsInCategory: recoverFromMDFault recoverFromMDFaultWithTrace removeCategory: removeSelector: removeSelectorIfInBaseSystem: ultimateSourceCodeAt:ifAbsent:)
('organization' category category: organization organization: whichCategoryIncludesSelector: zapOrganization)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:notifying: compile:classified:withStamp:notifying:logSource: compile:notifying: compileSilently:classified: compileSilently:classified:notifying: instVarNamesAndOffsetsDo: moveInstVarNamed:to:after: noteCompilationOf:meta: wantsChangeSetLogging wantsRecompilationProgressReported)
('fileIn/Out' classComment: classComment:stamp: commentFollows commentStamp: commentStamp:prior: definition definitionPreamble fileOutCategory: fileOutCategory:on:moveSource:toFile: fileOutChangedMessages:on: fileOutChangedMessages:on:moveSource:toFile: fileOutMethod: fileOutOn: fileOutOn:moveSource:toFile: fileOutOrganizationOn: methods methodsFor: methodsFor:priorSource:inFile: methodsFor:stamp: methodsFor:stamp:prior: moveChangesTo: printCategoryChunk:on: printCategoryChunk:on:priorMethod: printCategoryChunk:on:withStamp:priorMethod: printCategoryChunk:withStamp:on: printMethodChunk:withPreamble:on:moveSource:toFile: putClassCommentToCondensedChangesFile: reformatAll reformatMethodAt: reorganize)
('private' errorCategoryName instVarMappingFrom: linesOfCode logMethodSource:forMethodWithNode:inCategory:withStamp:notifying: newInstanceFrom:variable:size:map: setInstVarNames:)
('accessing class hierarchy' classesThatImplementAllOf: printSubclassesOn:level: removeUninstantiatedSubclassesSilently subclasses subclassesDo:)
('method dictionary' allMethodsInCategory:)
('system-support' allUnreferencedClassVariables)
('initialization' doneCompiling superclass:methodDictionary:format: updateInstances:from:isMeta: updateInstancesFrom:)
('release' obsolete)
!


!Behavior reorganize!
('accessing' classDepth compilerClass decompilerClass format methodDict name parserClass sourceCodeTemplate typeOfClass)
('testing' canZapMethodDictionary instSize instSpec isBehavior isBits isBytes isFixed isMeta isObsolete isPointers isVariable isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printHierarchy printOn: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' allowUnderscoreAssignments allowUnderscoreSelectors binding compile: compile:notifying: compileAll compileAllFrom: decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: basicNew basicNew: initializedInstance new new:)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSuperclasses subclasses superclass superclass: withAllSubclasses withAllSuperclasses)
('accessing method dictionary' >> addSelector:withMethod: addSelector:withMethod:notifying: addSelectorSilently:withMethod: allSelectors changeRecordsAt: compiledMethodAt: compiledMethodAt:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: lookupSelector: methodDict: methodDictionary methodDictionary: methodHeaderFor: methodsDo: precodeCommentOrInheritedCommentFor: removeSelector: removeSelectorSilently: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: sourceMethodAt: sourceMethodAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: zapAllMethods)
('accessing instances and variables' allClassVarNames allInstVarNames allInstances allSharedPools allSubInstances allowsSubInstVars classVarNames inspectAllInstances inspectSubInstances instVarNames instanceCount sharedPools someInstance)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass)
('testing method dictionary' allUnsentMessages bindingOf: canUnderstand: classBindingOf: hasMethods includesSelector: thoroughWhichSelectorsReferTo:special:byte: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsReferTo:special:byte: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allSubInstancesDo: allSubclassesDo: allSubclassesDoGently: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDoGently: withAllSuperclassesDo:)
('user interface' allLocalCallsOn: allUnreferencedInstanceVariables crossReference unreferencedInstanceVariables withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicRemoveSelector: becomeCompact becomeCompactSimplyAt: becomeUncompact flushCache indexIfCompact setFormat: spaceUsed)
('system startup' shutDown shutDown: startUp startUp:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses removeAllObsoleteSubclasses removeObsoleteSubclass:)
('as yet unclassified' environment)
('methods' closuresInfoAt: stampAt:)
('system-support' allCallsOn)
('initialization' superclass:methodDictionary:format:)
('release' obsolete)
!


!WeakSetInspector reorganize!
('accessing' fieldList)
('initialization' initialize)
!


!DictionaryInspector reorganize!
('accessing' fieldList)
('selecting' addEntry: calculateKeyArray contentsIsString refreshView replaceSelectionValue: selectedKey selection)
('menu' removeSelection renameEntryTo:)
('private' numberOfFixedFields)
('initialization' initialize)
!


!Inspector reorganize!
('accessing' baseFieldList context: fieldList i1 i2 object object: selectedClass selectedClassOrMetaClass timeOfLastListUpdate update wantsSteps)
('selecting' accept: contentsIsString replaceSelectionValue: selectedSlotName selection selectionIndex selectionPrintString selectionUnmodifiable toggleIndex:)
('code' doItContext doItReceiver)
('code pane menu' autoCompleterClass editorClass)
('updating' updateListsAndCodeIn:)
('object fileIn' convertToCurrentVersion:refStream:)
('private' printStringErrorText)
('stepping' stepAt:in:)
('contents' acceptedStringOrText)
('initialization' initialize inspect:)
!


!Debugger reorganize!
('accessing' acceptedStringOrText contents:notifying: contextVariablesInspector debuggerMap interruptedContext interruptedProcess isNotifier labelString labelString: proceedValue proceedValue: receiver receiverInspector receiverInspectorObject:context:)
('notifier menu' storeLog)
('context stack (message list)' contextStackIndex contextStackList expandStack fullyExpandStack messageListIndex selectedMessage selectedMessageName toggleContextStackIndex:)
('context stack menu' askForCategoryIn:default: doStep down fullStack implement:inClass: peelToFirst restart returnValue: selectPC send stepIntoBlock up where)
('code pane' contentsSelection doItContext doItReceiver pc pcRange)
('code pane menu' runToSelection:)
('message category list' selectedMessageCategoryName)
('class list' selectedClass selectedClassOrMetaClass)
('dependents access' updateInspectors wantsSteps)
('private' checkContextSelection contextStackIndex:oldContextWas: externalInterrupt: lowSpaceChoices newStack: process:controller:context: resetContext: resumeProcess selectedContext)
('breakpoints' toggleBreakOnEntry)
('shout styling' shoutAboutToStyle:)
('initialization' openFullMorphicLabel: openFullNoSuspendLabel: openNotifierContents:label: windowIsClosing)
!


!CodeFileBrowser reorganize!
('accessing' acceptedStringOrText codeFile: contents:notifying: contentsSymbol selectedCodeFile)
('removing' removeClass removeMessage removeMessageCategory removeUnmodifiedCategories removeUnmodifiedClasses removeUnmodifiedMethods)
('class list' classList renameClass selectedClass)
('edit pane' selectedBytecodes selectedMessage)
('diffs' methodDiffFor:class:selector:meta: modifiedClassDefinition)
('fileIn/fileOut' fileIn fileInClass fileInMessage fileInMessageCategories fileOut fileOutClass fileOutMessage fileOutMessageCategories)
('infoView' annotation extraInfo infoViewContents updateInfoView)
('metaclass' selectedClassOrMetaClass setClassOrganizer)
('other' changeMessageCategories: didCodeChangeElsewhere labelString)
('menus' contentsSymbolQuints)
('shout styling' shoutAboutToStyle:)
('initialization' defaultBrowserTitle)
!


!Browser reorganize!
('accessing' acceptedStringOrText contents:notifying: contentsSelection doItReceiver editSelection editSelection: request:initialAnswer:)
('annotation' annotation)
('breakpoints' toggleBreakOnEntry)
('class comment pane' newClassComment:)
('class functions' classCommentText classDefinitionText copyClass createInstVarAccessors defineClass:notifying: editClass editComment explainSpecial: fileOutClass hierarchy makeNewSubclass plusButtonHit removeClass renameClass)
('class list' classList classListIndex classListIndex: recent selectClass: selectedClass selectedClassName)
('code pane' compileMessage:notifying:)
('message category functions' addCategory alphabetizeMessageCategories canShowMultipleMessageCategories categoryOfCurrentMethod changeMessageCategories: editMessageCategories fileOutMessageCategories removeEmptyCategories removeMessageCategory renameCategory showHomeCategory)
('message category list' categorizeAllUncategorizedMethods messageCategoryList messageCategoryListIndex messageCategoryListIndex: messageCategoryListSelection rawMessageCategoryList selectOriginalCategoryForCurrentMethod selectedMessageCategoryName setOriginalCategoryIndexForCurrentMethod)
('message functions' defineMessageFrom:notifying: removeMessage removeMessageFromBrowser)
('message list' messageList messageListIndex messageListIndex: reformulateList selectedMessage selectedMessageName selectedMessageName:)
('metaclass' classCommentIndicated classMessagesIndicated classOrMetaClassOrganizer indicateClassMessages indicateInstanceMessages instanceMessagesIndicated metaClassIndicated metaClassIndicated: selectedClassOrMetaClass setClassOrganizer)
('system category functions' addSystemCategory alphabetizeSystemCategories changeSystemCategories: editSystemCategories fileOutSystemCategory moveSystemCategoryBottom moveSystemCategoryDown moveSystemCategoryTop moveSystemCategoryUp potentialClassNames removeSystemCategory renameSystemCategory systemCategoryBrowser updateSystemCategories)
('system category list' indexIsOne indexIsOne: selectCategoryForClass: selectedEnvironment selectedSystemCategoryName systemCategoryList systemCategoryListIndex systemCategoryListIndex: systemCategorySingleton)
('shout styling' shoutAboutToStyle:)
('initialization' defaultBrowserTitle labelString methodCategoryChanged setClass:selector: setSelector: systemOrganizer:)
!


!Workspace reorganize!
('binding' bindingNamesDo: bindingOf: hasBindingOf: hasBindingThatBeginsWith: initializeBindings)
('code pane menu' autoCompleter editorClass)
('variable declarations' mustDeclareVariableWording toggleVariableDeclarationMode)
('gui' openLabel:)
('shout styling' shouldStyle shoutAboutToStyle: toggleStyling toggleStylingLabel)
('testing' is:)
('initialization' initialize)
!


!PluggableTextModel reorganize!
('accessing' actualContents textGetter:textSetter:selectionGetter: textProvider)
('testing' is: refusesToAccept wantsFrameAdornments)
('misc' autoSelectString getSelection refetch)
('pane menu' autoCompleter editorClass)
('updating' update:)
('commands' acceptContentsFrom:)
('shout styling' shoutAboutToStyle:)
('initialization' initWith:)
!


!TextModel reorganize!
('accessing' actualContents actualContents: basicActualContents: contents: textSize undoRedoCommands undoRedoCommandsPosition)
('pane menu' autoCompleter)
('misc' getSelection refetch)
('testing' isTextEmpty wantsFrameAdornments)
('commands' acceptContentsFrom:)
('shout support' privateAddBoldAt: privateRemoveBoldAt: privateStyleWith: stylerStyled:checkForChanges:)
('undoable commands' basicReplaceAttributesFrom:to:with: basicReplaceFrom:to:with: logUndoAndAddAttribute:from:to: logUndoAndRemoveAttribute:from:to: logUndoAndReplaceFrom:to:with: redoAndEvaluate: undoAndEvaluate:)
('object serialization' convertToCurrentVersion:refStream:)
('initialization' flushUndoRedoCommands initialize openLabel:)
!


!TestRunner reorganize!
('accessing' completedFraction details errors errorsList failures failuresList listSelectionAt: listSelectionAt:put: passFail progressLabel result selectedTests tests timeSinceLastPassAsString:)
('constants' runButtonState stopButtonState)
('menus' deselectAll invertSelections selectAll setFilter terminateRun)
('processing' addTestsFor:toSuite: debugErrorTest: debugFailureTest: refreshTests runOneTest runProfiledTests runSuite: runSuiteProfiled: runTests selectedErrorTest selectedFailureTest selectedSuite selectedSuite:)
('test processing' errorLog showResult showResultDefects showResultSummary)
('updating' displayDetails: displayErrors: displayFailures: displayPassFail: refreshTR runWindow update: updateDetails: updateErrors: updateFailures: updatePassFail: updateWindow:)
('initialization' gatherTestNames initialize testCases)
!


!SystemChangeNotifier reorganize!
('private' triggerEvent: triggerEvent:withArguments:)
('system triggers' classAdded:inCategory: classCommented: classDefinitionChangedFrom:to: classRecategorized:from:to: classRemoved:fromCategory: classRenamed:from:to:inCategory: classReorganized: evaluated:context: methodAdded:selector:inClass:requestor: methodAdded:selector:inProtocol:class:requestor: methodChangedFrom:to:selector:inClass:requestor: methodRemoved:selector:inProtocol:class: selectorRecategorized:from:to:inClass:)
('public' doSilently: isBroadcasting)
('initialization' initialize)
!


!SimpleServiceEntry reorganize!
('accessing' argumentGetter: argumentProvider: buttonLabel buttonLabel: description description: extraSelector extraSelector: label provider provider:label:selector: selector triggerFileListChanged)
('performing service' getArgumentsFrom: performService provider:label:selector:description:)
('services menu' addServiceFor:toMenu: useLineAfter useLineAfter:)
('extra' performExtraFor: requestExtraSelector)
('printing' printOn:)
('initialization' initialize)
!


!ProcessBrowser reorganize!
('accessing' context: pointers processList processListIndex processListIndex: searchString selectedMethod selectedProcess selectedProcess: selectedSelector stackList stackListIndex stackListIndex: text)
('menus' selectedClass)
('process actions' resumeProcess signalSemaphore suspendProcess terminateProcess wasProcessSuspendedByProcessBrowser:)
('process list' findContext: nextContext notify:at:in: processNameList updateProcessList)
('stack list' changeStackListTo: moreStack pcRange selectedContext stackNameList updateStackList updateStackList:)
('views' autoCompleterClass)
('stepping' step)
('initialization' initialize startCPUWatcher stopCPUWatcher)
!


!CodePackageList reorganize!
('accessing' description description: editorClass packageDirtyFlags packageFullNames packageNames packages selection selectionIndex selectionIndex: summary)
('events' updateDirtyFlags updatePackageList)
('commands' save)
('initialization' initialize)
!


!PointerExplorer reorganize!
('accessing' getList includeWeakRefs includeWeakRefs:)
('initialization' initialize)
!


!ActiveModel class reorganize!
('accessing' actionMapFor: actionMaps updateableActionMapFor:)
('releasing' releaseActionMapFor:)
('initialization' flushEventSystem)
!


!ProtoObject reorganize!
('testing' ifNil: ifNil:ifNotNil: ifNotNil: ifNotNil:ifNil: isInMemory isNil notNil)
('comparing' == identityHash ~~)
('system primitives' become: cannotInterpret: doesNotUnderstand: nextInstance nextObject statePointsTo:)
('objects from disk' rehash)
('debugging' flag: withArgs:executeMethod:)
('apply primitives' tryNamedPrimitive tryNamedPrimitive: tryNamedPrimitive:with: tryNamedPrimitive:with:with: tryNamedPrimitive:with:with:with: tryNamedPrimitive:with:with:with:with: tryNamedPrimitive:with:with:with:with:with: tryNamedPrimitive:with:with:with:with:with:with: tryNamedPrimitive:with:with:with:with:with:with:with: tryPrimitive:withArgs:)
('tracing' pointsTo: referenceDescriptionTo:)
('initialization' initialize)
!

