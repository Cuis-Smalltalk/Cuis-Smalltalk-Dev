'From Cuis 4.1 of 12 December 2012 [latest update: #1711] on 8 June 2013 at 3:54:01.45309 pm'!

!SystemDictionary methodsFor: 'ui' stamp: 'jmv 6/8/2013 15:51'!
beep
	Preferences soundsEnabled ifTrue: [
		Smalltalk
			at: #SampledSound
			ifPresent: [ :cls | cls beep ]]! !


!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 6/7/2013 23:23'!
browseClassVarRefs 
	"Put up a menu offering all class variable names; if the user chooses one, open up a message-list browser on all methods that refer to the selected class variable"

	| lines labelStream allVars index owningClasses |
	lines _ OrderedCollection new.
	allVars _ OrderedCollection new.
	owningClasses _ OrderedCollection new.
	labelStream _ WriteStream on: (String new: 200).
	self withAllSuperclasses reverseDo: [ :class | | vars |
		vars _ class classVarNames asArray sort.
		vars do: [ :var |
			labelStream nextPutAll: var; newLine.
			allVars add: var.
			owningClasses add: class].
		vars isEmpty ifFalse: [ lines add: allVars size ]].
	labelStream contents isEmpty ifTrue: [^Smalltalk beep]. "handle nil superclass better"
	labelStream skip: -1 "cut last CR".
	index _ (PopUpMenu labels: labelStream contents lines: lines) startUp.
	index = 0 ifTrue: [^ self].
	Smalltalk browseAllCallsOn:
		((owningClasses at: index) classPool associationAt: (allVars at: index))! !

!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 6/7/2013 23:23'!
chooseClassVarName 
	"Present the user with a list of class variable names and answer the one selected, or nil if none"

	| lines labelStream  allVars index |
	lines _ OrderedCollection new.
	allVars _ OrderedCollection new.
	labelStream _ WriteStream on: (String new: 200).
	self withAllSuperclasses reverseDo: [ :class | | vars |
		vars _ class classVarNames asArray sort.
		vars do: [ :var |
			labelStream nextPutAll: var; newLine.
			allVars add: var].
		vars isEmpty ifFalse: [lines add: allVars size]].
	labelStream contents isEmpty ifTrue: [^Smalltalk beep]. "handle nil superclass better"
	labelStream skip: -1 "cut last CR".
	index _ (PopUpMenu labels: labelStream contents lines: lines) startUp.
	index = 0 ifTrue: [^ nil].
	^ allVars at: index! !


!ChangeSet class methodsFor: 'services' stamp: 'jmv 6/8/2013 15:35'!
fileIn: fullName
	"File in the entire contents of the file specified by the name provided"

	| fileName stream |
	fullName ifNil: [^ Smalltalk beep].
	fileName _ fullName.
	(fullName endsWith: '.gz')
		ifTrue: [
			(self confirm: ('{1}
appears to be a compressed file.
Do you want to uncompress it?' format:{fullName}))
				ifFalse: [ ^self ]
				ifTrue: [
					fileName _ GZipReadStream saveContents: fullName.
					stream _ FileStream readOnlyFileNamed: fileName ]]
		ifFalse: [
			stream _ FileStream readOnlyFileNamed: fileName ].
	stream fileIn! !


!CodeProvider methodsFor: 'commands' stamp: 'jmv 6/7/2013 23:23'!
removeClass
	"Remove the selected class from the system, at interactive user request.  Make certain the user really wants to do this, since it is not reversible.  Answer true if removal actually happened."

	| message  className classToRemove result |
	classToRemove _ self selectedClassOrMetaClass ifNil: [Smalltalk beep. ^ false].
	classToRemove _ classToRemove theNonMetaClass.
	className _ classToRemove name.
	message _ 'Are you certain that you
want to REMOVE the class ', className, '
from the system?'.
	(result _ self confirm: message)
		ifTrue: 
			[classToRemove subclasses size > 0
				ifTrue: [(self confirm: 'class has subclasses: ' , message)
					ifFalse: [^ false]].
			classToRemove removeFromSystem.
			self changed: #classList.
			true].
	^ result! !

!CodeProvider methodsFor: 'message list menu' stamp: 'jmv 6/7/2013 23:23'!
revertToPreviousVersion
	"Revert to the previous version of the current method"
	| aClass aSelector  changeRecords |
	aClass _ self selectedClassOrMetaClass.
	aClass ifNil: [^ self changed: #flash].
	aSelector _ self selectedMessageName.
	changeRecords _ aClass changeRecordsAt: aSelector.
	(changeRecords == nil or: [changeRecords size <= 1]) ifTrue: [self changed: #flash.  ^ Smalltalk beep].
	changeRecords second fileIn.
	self acceptedContentsChanged! !


!Browser methodsFor: 'class list' stamp: 'jmv 6/7/2013 23:22'!
recent
	"Let the user select from a list of recently visited classes.  11/96 stp.
	 12/96 di:  use class name, not classes themselves.
	 : dont fall into debugger in empty case"

	| className class recentList |
	recentList _ RecentClasses select: [:n | Smalltalk includesKey: n].
	recentList size = 0 ifTrue: [^ Smalltalk beep].
	className := (SelectionMenu selections: recentList) startUp.
	className
		ifNil: [^ self].
	class := Smalltalk at: className.
	self selectCategoryForClass: class.
	self classListIndex: (self classList indexOf: class name)! !


!ChangeList class methodsFor: 'public access' stamp: 'jmv 6/7/2013 23:22'!
browseRecentLogOnPath: fullName 
	"figure out where the last snapshot or quit was, then browse the recent  entries."

	fullName
		ifNotNil:
			[self browseRecentLogOn: (FileStream readOnlyFileNamed: fullName)]
		ifNil:
			[Smalltalk beep]
	! !


!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 6/7/2013 23:22'!
rename
	"Store a new name string into the selected ChangeSet.  reject duplicate name; allow user to back out"

	| newName |
	newName _ FillInTheBlankMorph request: 'New name for this change set'
						initialAnswer: myChangeSet name.
	(newName = myChangeSet name or: [newName size = 0]) ifTrue:
			[^ Smalltalk beep].

	(self class changeSetNamed: newName) ifNotNil:
			[^ Utilities inform: 'Sorry that name is already used'].

	myChangeSet name: newName.
	self update.
	self changed: #mainButtonName.
	self changed: #relabel.! !


!DataStream class methodsFor: 'as yet unclassified' stamp: 'jmv 6/8/2013 15:22'!
initialize
	"TypeMap maps Smalltalk classes to type ID numbers which identify the data stream primitive formats.  nextPut: writes these IDs to the data stream.  NOTE: Changing these type ID numbers will invalidate all extant data stream files.  Adding new ones is OK.  
	Classes named here have special formats in the file.  If such a class has a subclass, it will use type 9 and write correctly.  It will just be slow.  (Later write the class name in the special format, then subclasses can use the type also.)
	 See nextPut:, next, typeIDFor:, & ReferenceStream>>isAReferenceType:"
	"DataStream initialize"

	| refTypes t |
	refTypes _ OrderedCollection new.
	t _ TypeMap _ Dictionary new: 80. "sparse for fast hashing"

	t at: UndefinedObject put: 1.   refTypes add: 0.
	t at: True put: 2.   refTypes add: 0.
	t at: False put: 3.   refTypes add: 0.
	t at: SmallInteger put: 4.	 refTypes add: 0.
	t at: String put: 5.   refTypes add: 1.
	t at: Symbol put: 6.   refTypes add: 1.
	t at: ByteArray put: 7.   refTypes add: 1.
	t at: Array put: 8.   refTypes add: 1.
	"(type ID 9 is for arbitrary instances of any class, cf. typeIDFor:)"
		refTypes add: 1.
	"(type ID 10 is for references, cf. ReferenceStream>>tryToPutReference:)"
		refTypes add: 0.
	t at: Bitmap put: 11.   refTypes add: 1.
	t at: Metaclass put: 12.   refTypes add: 0.
	"Type ID 13 is used for HyperSqueak User classes that must be reconstructed."
		refTypes add: 1.
	t at: Float put: 14.  refTypes add: 1.
	t at: Rectangle put: 15.  refTypes add: 1.	"Allow compact Rects."
	"type ID 16 is an instance with short header.  See beginInstance:size:"
		refTypes add: 1.
	t at: String put: 17.   refTypes add: 1.	"new String format, 1 or 4 bytes of length"
	t at: WordArray put: 18.  refTypes add: 1.	"bitmap-like"
	"t at: WordArrayForSegment put: 19.  refTypes add: 1."		"bitmap-like"
	Smalltalk
		at: #SoundBuffer
		ifPresent: [ :cls | 
			t at: cls put: 20.  refTypes add: 1.	"And all other word arrays, both 
				16-bit and 32-bit.  See methods in ArrayedCollection.  Overridden in SoundBuffer."
				].
	t at: CompiledMethod put: 21.  refTypes add: 1.	"special creation method"
	"t at:  put: 22.  refTypes add: 0."
	ReferenceStream refTypes: refTypes.		"save it"

	"For all classes that are like WordArrays, store them the way ColorArray is stored.  As bits, and able to change endianness."
	Smalltalk do: [:cls |
		cls isInMemory ifTrue: [
			cls isBehavior ifTrue: [
				cls isPointers not & cls isVariable & cls isWords ifTrue: [
					(t includesKey: cls) ifFalse: [t at: cls put: 20]]]]].! !


!Debugger methodsFor: 'context stack menu' stamp: 'jmv 6/7/2013 23:23'!
peelToFirst
	"Peel the stack back to the second occurance of the currently selected message.  Very useful for an infinite recursion.  Gets back to the second call so you can see one complete recursion cycle, and how it was called at the beginning.  Also frees a lot of space!!"

	| ctxt |
	contextStackIndex = 0 ifTrue: [^ Smalltalk beep].
	"self okToChange ifFalse: [^ self]."
	ctxt := interruptedProcess popTo: self selectedContext findSecondToOldestSimilarSender.
	self resetContext: ctxt.
! !

!Debugger methodsFor: 'context stack menu' stamp: 'jmv 6/7/2013 23:23'!
returnValue: expression
	"Force a return of a given value to the previous context!!"

	| previous selectedContext value |
	contextStackIndex = 0 ifTrue: [^Smalltalk beep].
	selectedContext _ self selectedContext.
	value _ Compiler new 
				evaluate: expression
				in: selectedContext
				to: selectedContext receiver.
	previous _ selectedContext sender.
	self resetContext: previous.
	interruptedProcess popTo: previous value: value! !


!InputSensor methodsFor: 'user interrupts' stamp: 'jmv 6/8/2013 15:46'!
userInterruptWatcher
	"Wait for user interrupts and open a notifier on the active process when one occurs."

	[ true ] whileTrue: [
		InterruptSemaphore wait.
		Display deferUpdates: false.
		Smalltalk at: #SoundPlayer ifPresent: [ :cls | cls shutDown ].
		Smalltalk handleUserInterrupt]! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 6/7/2013 23:23'!
processUnknownEvent: aMorphicEvent localPosition: localEventPosition
	"An event of an unknown type was sent to the receiver. What shall we do?!!"

	Smalltalk beep. 
	aMorphicEvent printString displayAt: 0@0.
	aMorphicEvent wasHandled: true! !


!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 6/8/2013 15:35'!
startDispatchFrom: aHand
	"double dispatch the event dispatch"
	"An event of an unknown type was sent. What shall we do?!!"

	Smalltalk beep. 
	self printString displayAt: 0@0.
	self wasHandled: true! !


!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 6/8/2013 15:35'!
acceptDroppingMorph: aMorph event: evt 
	"The supplied morph, known to be acceptable to the receiver, is now to be assimilated; the precipitating event is supplied"

	self isWorldMorph 
		ifTrue: [	
			"Add the given morph to this world and start stepping it if it wants to be."
			self addMorphFront: aMorph.
			(aMorph morphFullBoundsInWorld intersects: self viewBox) 
				ifFalse: [
					Smalltalk beep.
					aMorph morphPosition: extent // 2]]
		ifFalse: [super acceptDroppingMorph: aMorph event: evt].
	aMorph submorphsDo: [ :m | (m is: #HaloMorph) ifTrue: [ m delete ]].
	self world startSteppingSubmorphsOf: aMorph! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 6/8/2013 15:36'!
findWindow: evt 
	"Present a menu names of windows and naked morphs, and activate the one that gets chosen.  Collapsed windows appear below line, expand if chosen; naked morphs appear below second line; if any of them has been given an explicit name, that is what's shown, else the class-name of the morph shows; if a naked morph is chosen, bring it to front and have it don a halo."

	| menu expanded collapsed nakedMorphs |
	menu := MenuMorph new.
	expanded := SystemWindow windowsIn: self
				satisfying: [:w | w isCollapsed not].
	collapsed := SystemWindow windowsIn: self
				satisfying: [:w | w isCollapsed].
	nakedMorphs := self submorphsSatisfying: [:m | (m is: #SystemWindow) not].
	expanded isEmpty & (collapsed isEmpty & nakedMorphs isEmpty) 
		ifTrue: [^Smalltalk beep].
	(expanded 
		asArray sort: [:w1 :w2 | w1 label caseInsensitiveLessOrEqual: w2 label]) 
			do: 
				[:w | 
				menu 
					add: w label
					target: w
					action: #activateAndForceLabelToShow.
				w canDiscardEdits ifFalse: [menu lastItem color: Color red]].
	expanded isEmpty | (collapsed isEmpty & nakedMorphs isEmpty) 
		ifFalse: [menu addLine].
	(collapsed 
		asArray sort: [:w1 :w2 | w1 label caseInsensitiveLessOrEqual: w2 label]) 
			do: 
				[:w | 
				menu 
					add: w label
					target: w
					action: #collapseOrExpand.
				w canDiscardEdits ifFalse: [menu lastItem color: Color red]].
	nakedMorphs isEmpty ifFalse: [menu addLine].
	(nakedMorphs asArray sort: 
			[:w1 :w2 | 
			w1 nameForFindWindowFeature 
				caseInsensitiveLessOrEqual: w2 nameForFindWindowFeature]) 
		do: 
			[:w | 
			menu 
				add: w nameForFindWindowFeature
				target: w
				action: #comeToFrontAndAddHalo].
	menu addTitle: 'find window'.
	menu popUpInWorld: self! !


!Preferences class methodsFor: 'personalization' stamp: 'jmv 6/7/2013 23:24'!
disableProgrammerFacilities
	"Warning: do not call this lightly!!  It disables all access to menus, debuggers, halos.  There is no guaranteed return from this, which is to say, you cannot necessarily reenable these things once they are disabled -- you can only use whatever the UI of the current project affords, and you cannot even snapshot -- you can only quit. 

     You can completely reverse the work of this method by calling the dual Preferences method enableProgrammerFacilities, provided you have left yourself leeway to bring about a call to that method.

	To set up a system that will come up in such a state, you have to request the snapshot in the same breath as you disable the programmer facilities.  To do this, put the following line into the 'do' menu and then evaluate it from that 'do' menu:

         Preferences disableProgrammerFacilities.

You will be prompted for a new image name under which to save the resulting image."

	Smalltalk beep.
	(self confirm: 'CAUTION!!!!
This is a drastic step!!
Do you really want to do this?')
		ifFalse: [
			Smalltalk beep.  ^ self inform: 'whew!!'].

	self disable: #cmdDotEnabled.       "No user-interrupt-into-debugger"

	"self enable: #noviceMode."           "No control-menu"
	self disable: #warnIfNoSourcesFile.
	self disable: #warnIfNoChangesFile.
	Smalltalk saveAs! !


!ProcessBrowser class methodsFor: 'process control' stamp: 'jmv 6/8/2013 15:43'!
rulesFor: aProcess
	"Answer two flags: allow-stop, and allow-debug"

	"Don't mess with the process running the ProcessBrowser.
	If we support several active UIs, we'd detect the UI process running us"
	self flag: #jmvVer2.
	aProcess == Processor activeProcess
		ifTrue: [^{false. false}].

	Smalltalk at: #SoundPlayer ifPresent: [ :cls |
		cls playerProcess = aProcess 
			ifTrue: [ ^{false. false}]].

	^ [aProcess caseOf: {
		[] -> [{false. false}].
		[Smalltalk lowSpaceWatcherProcess] -> [{false. false}].
		[WeakArray runningFinalizationProcess] -> [{false. false}].
		[Processor activeProcess] -> [{false. true}].
		[Processor backgroundProcess] -> [{false. false}].
		[Sensor interruptWatcherProcess] -> [{false. false}].
		[Sensor eventTicklerProcess] -> [{false. false}].
		[CPUWatcher currentWatcherProcess] -> [{false. false}].
		[Delay timerProcess] -> [{false. false}]}
		otherwise: [ {true. true}]]
		ifError: [ :err :rcvr | {true. true}]! !


!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 6/7/2013 23:23'!
lowSpaceWatcher
	"Wait until the low space semaphore is signalled, then take appropriate actions."

	self garbageCollectMost <= self lowSpaceThreshold ifTrue: [
		self garbageCollect <= self lowSpaceThreshold ifTrue: [
			"free space must be above threshold before starting low space watcher"
			^ Smalltalk beep]].

	LowSpaceSemaphore _ Semaphore new.
	self primLowSpaceSemaphore: LowSpaceSemaphore.
	self primSignalAtBytesLeft: self lowSpaceThreshold.  "enable low space interrupts"

	LowSpaceSemaphore wait.  "wait for a low space condition..."

	self primSignalAtBytesLeft: 0.  "disable low space interrupts"
	self primLowSpaceSemaphore: nil.
	LowSpaceProcess _ nil.
	"Note: user now unprotected until the low space watcher is re-installed"

	ProjectX currentInterruptNameX: 'Space is low'! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 6/7/2013 23:23'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	ClassicTheme beCurrent.
	PasteUpMorph allInstancesDo: [ :w |
		w backgroundImageData: nil.
		w  submorphsDo: [ :a | a delete ]].
	Preferences useNoIcons.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	FormCanvas clearFormsCache.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
"	ColorPickerMorph class removeSelector: #buildEyedropperIcon."
	CursorWithAlpha class removeSelector: #buildBiggerNormal.
	Theme removeSelector: #miscellaneousIcons.
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	StrikeFont removeMostFonts.
	StrikeFont saveSpace.
	Smalltalk garbageCollect.

	"????
	Smalltalk organization removeCategoriesMatching: 'Signal Processing*'.
	SystemOrganization removeSystemCategory: 'LinearAlgebra'.
	Smalltalk organization removeCategoriesMatching: 'Sound-*'
	"

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(ZipConstants GZipConstants ZipFileConstants SpaceTally).
	unused := Smalltalk unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		n := Smalltalk removeAllUnSentMessages.
		unused := Smalltalk unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSorter zapAllChangeSets.
	Smalltalk garbageCollect.


	Smalltalk organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	Smalltalk garbageCollect.
	oldDicts _ MethodDictionary allInstances.
	newDicts _ Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts _ newDicts _ nil.

   "Sanity checks"
"   Undeclared
   Smalltalk cleanOutUndeclared
   Smalltalk browseUndeclaredReferences
   Smalltalk obsoleteClasses
   Smalltalk obsoleteBehaviors 
   Smalltalk browseObsoleteMethodReferences
   SmalltalkImage current fixObsoleteReferences
   Smalltalk browseAllUnimplementedCalls"! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 6/8/2013 15:17'!
send: startUpOrShutDown toClassesNamedIn: startUpOrShutDownList with: argument
	"Send the message #startUp: or #shutDown: to each class named in the list.
	The argument indicates if the system is about to quit (for #shutDown:) or if
	the image is resuming (for #startUp:).
	If any name cannot be found, then remove it from the list."

	| removals class |
	removals _ OrderedCollection new.
	startUpOrShutDownList do:
		[:name |
		class _ self at: name ifAbsent: nil.
		class
			ifNil: [removals add: name]
			ifNotNil: [
				class isInMemory ifTrue: [
					class perform: startUpOrShutDown with: argument]]].

	"Remove any obsolete entries, but after the iteration"
	"Well, not. Better just ignore them. Maybe it is stuff, like SoundPlayer, that was moved to optional packages, and can be loaded again anytime."
	"startUpOrShutDownList removeAll: removals"! !


!SystemWindow methodsFor: 'menu' stamp: 'jmv 6/7/2013 23:23'!
setWindowColor: incomingColor
	| existingColor aColor |
	incomingColor ifNil: [^ self].  "it happens"
	aColor _ incomingColor asNontranslucentColor.
	aColor = Color black ifTrue: [^ self].
	existingColor _ self widgetsColor.
	existingColor ifNil: [^ Smalltalk beep].
	self widgetsColor: aColor.
	self redrawNeeded! !


!Utilities class methodsFor: 'recent method submissions' stamp: 'jmv 6/7/2013 23:24'!
revertLastMethodSubmission
	| changeRecords lastSubmission theClass theSelector |
	"If the most recent method submission was a method change, revert
	that change, and if it was a submission of a brand-new method, 
	remove that method."

	RecentSubmissions isEmptyOrNil ifTrue: [^ Smalltalk beep].
	lastSubmission _ RecentSubmissions last.
	theClass _ lastSubmission actualClass ifNil: [^ Smalltalk beep].
	theSelector _ lastSubmission methodSymbol.
	changeRecords _ theClass changeRecordsAt: theSelector.
	changeRecords isEmptyOrNil ifTrue: [^ Smalltalk beep].
	changeRecords size = 1
		ifTrue: [
			"method has no prior version, so reverting in this case means removing"
			theClass removeSelector: theSelector]
		ifFalse: [
			changeRecords second fileIn].

"Utilities revertLastMethodSubmission"! !

DataStream initialize!
