'From Cuis 4.1 of 12 December 2012 [latest update: #1700] on 26 May 2013 at 8:09:06.942031 pm'!

!ChangeListWindow methodsFor: 'menu commands' stamp: 'jmv 5/26/2013 16:07'!
browseCurrentVersionsOfSelections
	"Opens a message-list browser on the current in-memory versions of all methods that are currently seleted"
	| aList |

	aList _ model currentVersionsOfSelections.

	aList size = 0 ifTrue: [^ self inform: 'no selected methods have in-memory counterparts'].
	MessageSetWindow
		openMessageList: aList 
		label: 'Current versions of selected methods in ', model file localName! !


!ChangeRecord methodsFor: 'initialization' stamp: 'jmv 5/26/2013 16:07'!
fileIn
	"File the receiver in.  If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it."
	| s |
	type == #method ifTrue: [
		self changeClass ifNotNil: [ :methodClass |
			methodClass
				compile: self text
				classified: category
				withStamp: stamp
				notifying: nil.
			self methodSelector ifNotNil: [ :aSelector |
				Utilities
					noteMethodSubmission: aSelector
					forClass: methodClass ]]].
	type == #doIt ifTrue: [
		((s _ self string) beginsWith: '----') ifFalse: [ Compiler evaluate: s ]].
	type == #classDefinition ifTrue: [ Compiler evaluate: self string ].
	type == #classComment ifTrue: [ | cls |
		(cls _ Smalltalk at: class asSymbol)
			comment: self text
			stamp: stamp.
		Utilities
			noteMethodSubmission: #Comment
			forClass: cls ]! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 5/26/2013 16:08'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and either of the date & time or chosen to have a  
	unique numeric tag, depending on the preference  
	'changeSetVersionNumbers'"
	| slips nameToUse file |
	self checkForConversionMethods.
	nameToUse := Preferences changeSetVersionNumbers
				ifTrue: [self defaultChangeSetDirectory nextNameFor: self name coda: '-', Utilities authorInitials extension: 'cs.st']
				ifFalse: [(self name , FileDirectory dot , Utilities dateTimeSuffix , FileDirectory dot , 'cs.st') asFileName].
		file := self defaultChangeSetDirectory newFileNamed: nameToUse.
		[
			file timeStamp.
			self fileOutPreambleOn: file.
			self fileOutOn: file.
			self fileOutPostscriptOn: file]
				ensure: [file close].
	
	self hasUnsavedChanges: false.
	Preferences checkForSlips
		ifFalse: [^ self].
	slips := self checkForSlips.
	(slips size > 0
			and: [(PopUpMenu withCaption: 'Methods in this fileOut have halts
or references to the Transcript
or other ''slips'' in them.
Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')
					= 2])
		ifTrue: [Smalltalk browseMessageList: slips name: 'Possible slips in ' , name]! !


!CodeFileBrowserWindow class methodsFor: 'services' stamp: 'jmv 5/26/2013 15:49'!
browseStream: aStream named: aString

	| codeFile organizer browser |
	organizer _ SystemOrganizer defaultList: Array new.
	codeFile _ (CodeFile new fullName: aString; buildFrom: aStream).
	aStream close.
	organizer 
		classifyAll: codeFile classes keys 
		under: codeFile name.
	(browser _ CodeFileBrowser new)
		systemOrganizer: organizer;
		codeFile: codeFile.
	CodeFileBrowserWindow open: browser label: 'Code File Browser'! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 5/26/2013 16:10'!
save
	| nameToUse file |
	fullFileName ifNil: [
		fullFileName _
			ChangeSet defaultChangeSetDirectory fullNameFor: (self packageName, FileDirectory dot, 'pck.st')].
	nameToUse _ fullFileName.
"	nameToUse _ Preferences changeSetVersionNumbers
		ifTrue: [
			ChangeSet defaultChangeSetDirectory
				nextNameFor: self packageName coda: '-', Utilities authorInitials
				extension: 'pck.st' ]
		ifFalse: [ (self packageName , FileDirectory dot , Utilities dateTimeSuffix , FileDirectory dot , 'pck.st') asFileName ]."
		file _ ChangeSet defaultChangeSetDirectory forceNewFileNamed: nameToUse.
		[
			file timeStamp.
			self writeOnStream: file ]
				ensure: [ file close ].
	self hasUnsavedChanges: false.
	ChangeSorter removeChangeSet: (ChangeSorter existingOrNewChangeSetForPackage: self)! !


!CodePackageFile class methodsFor: 'services' stamp: 'jmv 5/26/2013 15:49'!
installMonticelloPackageStream: aStream
	
	| stream fullName pkName |
	fullName _ aStream name.
	pkName _ CodePackageFile monticelloPackageNameFrom: fullName.
	stream _ aStream asUnZippedStream: 'snapshot/source.st'.
	stream ascii.
	CodePackageFile
		installFileStream: stream
		packageName: pkName
		fullName: fullName! !

!CodePackageFile class methodsFor: 'services' stamp: 'jmv 5/26/2013 15:49'!
installPackageStream: aStream

	| fullName pkName existing |
	fullName _ aStream name.
	pkName _ CodePackageFile packageNameFrom: fullName.
	existing _ CodePackage named: pkName createIfAbsent: false registerIfNew: false.
	(existing isNil
		or: [ existing hasUnsavedChanges not
			or: [ self confirm: 'If you install this package, there are unsaved changes that will be lost.', String newLineString, 'Continue?' ]]) ifTrue: [
		CodePackageFile
			installFileStream: aStream
			packageName: pkName
			fullName: fullName ]! !


!CodeProvider methodsFor: 'message list menu' stamp: 'jmv 5/26/2013 16:10'!
fileOutMessage
	"Put a description of the selected message on a file"

	self selectedMessageName ifNotNil: [
		self selectedClassOrMetaClass fileOutMethod: self selectedMessageName]! !


!Browser methodsFor: 'class functions' stamp: 'jmv 5/26/2013 15:40'!
copyClass
	| originalClass originalName copysName  newDefinition newMetaDefinition newClass |
	selectedClassName ifNil: [^ self].
	originalClass := self selectedClass.
	originalName := originalClass name.
	copysName _ self request: 'Please type new class name' initialAnswer: originalName.
	copysName = '' ifTrue: [^ self].  " Cancel returns '' "
	copysName _ copysName asSymbol.
	copysName = originalName ifTrue: [^ self].
	(Smalltalk includesKey: copysName)
		ifTrue: [^ self error: copysName , ' already exists'].
	newDefinition := originalClass definition
		copyReplaceAll: originalName printString
		with: copysName printString.
	newClass _ Compiler evaluate: newDefinition logged: true.
	newMetaDefinition := originalClass class definition
		copyReplaceAll: originalClass class name
		with: newClass class name.
	Compiler evaluate: newMetaDefinition logged: true.
	newClass copyAllCategoriesFrom: originalClass.
	newClass class copyAllCategoriesFrom: originalClass class.
	originalClass hasComment ifTrue: [
		newClass comment: originalClass comment ].			
	self classListIndex: 0.
	self changed: #classList! !

!Browser methodsFor: 'class functions' stamp: 'jmv 5/26/2013 16:03'!
fileOutClass
	"Print a description of the selected class onto a file whose name is the 
	category name followed by .st."

	selectedClassName ifNotNil: [ self selectedClass fileOut ]! !

!Browser methodsFor: 'message category functions' stamp: 'jmv 5/26/2013 16:04'!
fileOutMessageCategories
	"Print a description of the selected message category of the selected class 
	onto an external file."

	selectedMessageCategory ifNotNil: [
		self selectedClassOrMetaClass fileOutCategory:
			self selectedMessageCategoryName ]! !


!ChangeList methodsFor: 'menu actions' stamp: 'jmv 5/26/2013 15:44'!
removeDoIts
	"Remove doits from the receiver, other than initializes. 1/26/96 sw"

	| newChangeList newList |
	newChangeList _ OrderedCollection new.
	newList _ OrderedCollection new.

	changeList with: list do: [ :chRec :str |
		(chRec changeType ~~ #doIt or: [str endsWith: 'initialize'])
			ifTrue: [
				newChangeList add: chRec.
				newList add: str]].
	newChangeList size < changeList size
		ifTrue: [
			changeList _ newChangeList.
			list _ newList.
			listIndex _ 0.
			self clearSelections ].
	self changed: #list.

	! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 5/26/2013 15:44'!
removeEmptyClassComments

	| newChangeList newList |
	newChangeList _ OrderedCollection new.
	newList _ OrderedCollection new.
	changeList with: list do: [ :chRec :strNstamp | | keep |
		keep _ true.
		(chRec changeType == #classComment and: [
				Smalltalk includesKey: chRec changeClassName]) ifTrue: [
			keep _ chRec text notEmpty
		].
		keep ifTrue:[
			newChangeList add: chRec.
			newList add: strNstamp]].
	newChangeList size < changeList size ifTrue: [
		changeList _ newChangeList.
		list _ newList.
		listIndex _ 0.
		self clearSelections ].
	self changed: #list! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 5/26/2013 15:45'!
removeOlderMethodVersions
	"Remove older versions of entries from the receiver."

	| newChangeList newList found |
	newChangeList _ OrderedCollection new.
	newList _ OrderedCollection new.
	found _ OrderedCollection new.
	changeList reverseWith: list do: [ :chRec :strNstamp | | str |
		str _ strNstamp copyUpTo: $;.
		(found includes: str) ifFalse: [
			found add: str.
			newChangeList add: chRec.
			newList add: strNstamp]].
	newChangeList size < changeList size ifTrue: [
		changeList _ newChangeList reversed.
		list _ newList reversed.
		listIndex _ 0.
		self clearSelections ].
	self changed: #list! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 5/26/2013 15:47'!
removeUpToDate
	"Remove all up to date version of entries from the receiver"

	| newChangeList newList |
	newChangeList _ OrderedCollection new.
	newList _ OrderedCollection new.
	changeList with: list do: [ :chRec :strNstamp | | keep cls name |
	keep _ chRec isClassDeletion not or: [ chRec changeClass notNil ]. "If a class deletion, and class already gone, don't keep it"
	keep ifTrue: [
		(cls _ chRec changeClass) ifNotNil: [ | sel str |
			str _ chRec string.
			sel _ chRec methodSelector.
			keep _ (cls sourceCodeAt: sel ifAbsent:['']) asString ~= str ]].
	(chRec changeType == #classComment and: [
			name _ chRec changeClassName.
			Smalltalk includesKey: name]) ifTrue: [
		cls _ Smalltalk at: name.
		keep _ cls organization classComment ~= chRec text ].
	(chRec changeType == #classDefinition and: [
			name _ chRec changeClassName.
			Smalltalk includesKey: name]) ifTrue: [
		cls _ Smalltalk at: name.
		chRec isMetaClassChange ifTrue: [ cls _ cls class ].
		keep _ cls definition ~= chRec text ].
	keep ifTrue: [
		newChangeList add: chRec.
		newList add: strNstamp]].
	newChangeList size < changeList size ifTrue: [
		changeList _ newChangeList.
		list _ newList.
		listIndex _ 0.
		self clearSelections ].
	self changed: #list! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 5/26/2013 16:04'!
selectAllForAbsentClasses
	"Selects all method definitions for which there is no counterpart method in the current image"

	| change |
	1 to: changeList size do: [ :i | 
		change _ changeList at: i.
		listSelections at: i put:
			((#(method classComment classDefinition) includes: change changeType)
				and: [ change changeClass isNil ])].
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 5/26/2013 15:47'!
selectEquivalentMethods
	"Selects all method definitions for which there is already an equivalent method in the current image, 
	(meaning that the difference is cosmetic and not in behavior)"
	1 to: changeList size do: [ :i | 
		| change class |
		change _ changeList at: i.
		listSelections at: i put:
			((change changeType == #method and: [
				(class _ change changeClass) notNil]) and: [
					(class includesSelector: change methodSelector) and: [
						| cmWithNode |
						cmWithNode _ [class basicCompile: change string notifying: nil trailer: class defaultMethodTrailer ifFail: nil] 
							on: SyntaxErrorNotification do: [ :ex | ex return ].
						(cmWithNode notNil and: [
							| current inChange |
							current _ (class compiledMethodAt: change methodSelector) copyWithTrailerBytes: #(0).
							inChange _ cmWithNode method copyWithTrailerBytes: #(0).
							current = inChange or: [
								| currentCmWithNode |
								currentCmWithNode _ [class basicCompile: (class decompilerClass new decompile: change methodSelector in: class) decompileString
										notifying: nil trailer: class defaultMethodTrailer ifFail: nil] on: SyntaxErrorNotification do: [ :ex | ex return ].
								(currentCmWithNode notNil and: [
									current _ currentCmWithNode method copyWithTrailerBytes: #(0).
									current = inChange])
							]
						])
					]]
			)].
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 5/26/2013 16:05'!
selectMethodsOlderThanCurrent
	"Selects all method definitions for which there is some newer counterpart in the current image.
	This is, select methods that installing would be go back in time"

	| cm doSelect change class |
	1 to: changeList size do: [ :i | 
		change _ changeList at: i.
		doSelect _ false.
		((change changeType == #method and: [ change isMethodDeletion not ] ) and: [
			((class _ change changeClass) notNil)]) ifTrue: [
				cm _ class compiledMethodAt: change methodSelector ifAbsent: nil.
				cm ifNotNil: [
					doSelect _ change dateAndTime < cm dateAndTime ]].
		listSelections at: i put: doSelect ].
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 5/26/2013 16:05'!
selectNewMethods
	"Selects all method definitions for which there is no counterpart method in the current image"

	| change |
	1 to: changeList size do: [ :i | 
		change _ changeList at: i.
		listSelections at: i put: (self isNewMethod: change)].
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 5/26/2013 16:05'!
selectRemovalsOfSent
	"Selects all method removal for sent methods"

	1 to: changeList size do: [ :i | | change |
		change _ changeList at: i.
		listSelections at: i put:
			(change changeType = #doIt and: [
				change string includesSubString: 'removeSelector: #' ] and: [
					Smalltalk isThereAReferenceTo: (change string copyAfterLast: $#) asSymbol ]) ].
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 5/26/2013 16:05'!
selectUnchangedMethods
	"Selects all method definitions for which there is already a method in the current image, whose source is exactly the same.  9/18/96 sw"

	| change class |
	1 to: changeList size do: [ :i | 
		change _ changeList at: i.
		listSelections at: i put:
			((change changeType == #method and:
				[(class _ change changeClass) notNil]) and:
					[(class includesSelector: change methodSelector) and:
						[change string = (class sourceCodeAt: change methodSelector) asString ]])].
	self changed: #allSelections.
	self changed: #annotation! !


!ChangeList class methodsFor: 'public access' stamp: 'jmv 5/26/2013 16:06'!
browseContents: aStream
	"Opens a changeList on a fileStream"
	| changeList charCount |
	aStream readOnly.
	charCount _ aStream size.
	charCount > 1000000 ifTrue: [
		(self confirm: 'The file ', aStream name , '
is really long (' , charCount printString , ' characters).
Would you prefer to view only the last million characters?')
			ifTrue: [ charCount _ 1000000 ]].
	changeList _ self new
			scanFile: aStream from: aStream size-charCount to: aStream size.
	aStream close.
	ChangeListWindow open: changeList label: aStream localName! !

!ChangeList class methodsFor: 'public access' stamp: 'jmv 5/26/2013 16:06'!
browsePackageContents: aStream
	"Opens a changeList on a fileStream"
	| changeList fullName pkName packageFile |
	fullName _ aStream name.
	pkName _ CodePackageFile packageNameFrom: fullName.
	aStream readOnly.
	changeList _ self new scanFile: aStream from: 0 to: aStream size.
	aStream reset.
	packageFile _ CodePackageFile
		buildFileStream: aStream
		packageName: pkName
		fullName: fullName.
	aStream close.
	"Add deletions of methods and classes that are in the PackageInfo (i.e., active in the system)
	but are no longer in the CodePackageFile being viewed."
	packageFile methodsToRemove do: [ :methodReference |
		changeList
			addItem: (MethodDeletionChangeRecord new methodReference: methodReference)
			text: 'method no longer in package: ', methodReference asStringOrText ].
	packageFile classesToRemove do: [ :clsName |
		changeList
			addItem: (ClassDeletionChangeRecord new clsName: clsName)
			text: 'class no longer in package: ', clsName ].
	changeList clearSelections.
	ChangeListWindow open: changeList label: aStream localName! !

!ChangeList class methodsFor: 'public access' stamp: 'jmv 5/26/2013 16:07'!
browseRecent: charCount on: origChangesFile 
	"Opens a changeList on the end of the specified changes log file"
	
	| changeList end changesFile |
	changesFile _ origChangesFile readOnlyCopy.
	end _ changesFile size.
	changeList _ self new
		scanFile: changesFile
		from: (0 max: end - charCount)
		to: end.
	changesFile close.
	ChangeListWindow open: changeList label: 'Recent changes'! !

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 5/26/2013 16:06'!
browseMCZContents: aStream
	"Browse the selected file."
	| unzipped changeList fullName packageFile pkName |
	
	"For Monticello packages we do as we do with our own .pck.st files: Instead of just browsing
	contents, also include what is no longer part of the package (and should therefore be removed on install)
	See #browsePackageContents:
	However, this was never tested to run!!"
	self flag: #jmvVer.

	fullName _ aStream name.
	pkName _ CodePackageFile monticelloPackageNameFrom: fullName.
	unzipped _ aStream asUnZippedStream: 'snapshot/source.st'.
	unzipped ascii.
	changeList _ self new scanFile: unzipped from: 0 to: unzipped size.
	aStream reset.
	packageFile _ CodePackageFile
		buildFileStream: unzipped
		packageName: pkName
		fullName: fullName.
	"Add deletions of methods and classes that are in the PackageInfo (i.e., active in the system)
	but are no longer in the PackageFile being viewed."
	packageFile methodsToRemove do: [ :methodReference |
		changeList
			addItem: (MethodDeletionChangeRecord new methodReference: methodReference)
			text: 'method no longer in package: ', methodReference asStringOrText ].
	packageFile classesToRemove do: [ :clsName |
		changeList
			addItem: (ClassDeletionChangeRecord new clsName: clsName)
			text: 'class no longer in package: ', clsName ].
	changeList clearSelections.
	ChangeListWindow open: changeList label: aStream name! !


!CodeFileBrowser methodsFor: 'removing' stamp: 'jmv 5/26/2013 15:48'!
removeUnmodifiedCategories
	| theClass |
	theClass _ self selectedClass.
	theClass ifNil: [^self].
	theClass removeUnmodifiedMethods: theClass selectors.
	theClass metaClass removeUnmodifiedMethods: theClass metaClass selectors.
	self messageCategoryListIndex: 0.
	self changed: #messageCategoryList! !

!CodeFileBrowser methodsFor: 'removing' stamp: 'jmv 5/26/2013 15:48'!
removeUnmodifiedClasses
	codeFile classes copy do: [ :theClass |
		theClass removeAllUnmodified.
		theClass hasChanges ifFalse: [ codeFile removeClass: theClass ]].
	self classListIndex: 0.
	self changed: #classList! !

!CodeFileBrowser methodsFor: 'removing' stamp: 'jmv 5/26/2013 15:49'!
removeUnmodifiedMethods
	| theClass cat |
	theClass := self selectedClassOrMetaClass.
	theClass ifNil: [ ^self].
	cat := self selectedMessageCategoryName.
	cat ifNil: [ ^self].
	theClass removeUnmodifiedMethods: (theClass organization listAtCategoryNamed: cat).
	self messageListIndex: 0.
	self changed: #messageList.! !

!CodeFileBrowser methodsFor: 'diffs' stamp: 'jmv 5/26/2013 15:48'!
methodDiffFor: aString class: aPseudoClass selector: selector meta: meta 
	"Answer the diff between the current copy of the given class/selector/meta for the string provided"

	| theClass source |
	theClass _ Smalltalk
		at: aPseudoClass name
		ifAbsent: [^ aString copy].
	meta
		ifTrue: [theClass _ theClass class].
	(theClass includesSelector: selector)
		ifFalse: [^ aString copy].
	source _ theClass sourceCodeAt: selector.
	^ DifferenceFinder
		displayPatchFrom: source to: aString
		tryWords: self shouldDiffWords
		prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [theClass])! !

!CodeFileBrowser methodsFor: 'diffs' stamp: 'jmv 5/26/2013 15:48'!
modifiedClassDefinition
	| pClass rClass old new |
	pClass := self selectedClassOrMetaClass.
	pClass hasDefinition ifFalse: [ ^pClass definition].
	rClass := Smalltalk at: self selectedClass name asSymbol ifAbsent: nil.
	rClass ifNil: [ ^pClass definition].
	self metaClassIndicated ifTrue:[ rClass := rClass class].
	old := rClass definition.
	new := pClass definition.
	^ DifferenceFinder displayPatchFrom: old to: new tryWords: true! !

!CodeFileBrowser methodsFor: 'fileIn/fileOut' stamp: 'jmv 5/26/2013 16:08'!
fileIn
	self selectedCodeFile fileIn! !

!CodeFileBrowser methodsFor: 'fileIn/fileOut' stamp: 'jmv 5/26/2013 16:09'!
fileInClass

	self selectedClass fileIn! !

!CodeFileBrowser methodsFor: 'fileIn/fileOut' stamp: 'jmv 5/26/2013 16:09'!
fileInMessage
	
	self selectedMessageName ifNil: [^self].
	self selectedClassOrMetaClass fileInMethod: self selectedMessageName! !

!CodeFileBrowser methodsFor: 'fileIn/fileOut' stamp: 'jmv 5/26/2013 16:09'!
fileInMessageCategories

	self selectedClassOrMetaClass fileInCategory: self selectedMessageCategoryName! !

!CodeFileBrowser methodsFor: 'fileIn/fileOut' stamp: 'jmv 5/26/2013 16:09'!
fileOut

	self selectedCodeFile fileOut! !

!CodeFileBrowser methodsFor: 'fileIn/fileOut' stamp: 'jmv 5/26/2013 16:09'!
fileOutClass

	self selectedClass fileOut! !

!CodeFileBrowser methodsFor: 'fileIn/fileOut' stamp: 'jmv 5/26/2013 16:09'!
fileOutMessage

	self selectedMessageName ifNil: [^self].
	self selectedClassOrMetaClass fileOutMethod: self selectedMessageName! !

!CodeFileBrowser methodsFor: 'fileIn/fileOut' stamp: 'jmv 5/26/2013 16:10'!
fileOutMessageCategories

	self selectedClassOrMetaClass fileOutCategory: self selectedMessageCategoryName! !


!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 5/26/2013 15:50'!
statsForDirectoryTree: rootedPathName
	"Return the size statistics for the entire directory tree starting at the given root. The result is a three element array of the form: (<number of folders><number of files><total bytes in all files>). This method also serves as an example of how recursively enumerate a directory tree."
	"wod 6/16/1998: add Cursor wait, and use 'self pathNameDelimiter asString' rather than hardwired ':' "
	"FileDirectory default statsForDirectoryTree: '\smalltalk'"

	| dirs files bytes todo p entries |
	dirs _ files _ bytes _ 0.
	todo _ OrderedCollection with: rootedPathName.
	[todo isEmpty] whileFalse: [
		p _ todo removeFirst.
		entries _ self directoryContentsFor: p.
		entries do: [:entry |
			(entry at: 4)
				ifTrue: [
					todo addLast: (p, self pathNameDelimiter asString, (entry at: 1)).
					dirs _ dirs + 1]
				ifFalse: [
					files _ files + 1.
					bytes _ bytes + (entry at: 5)]]].
	^ Array with: dirs with: files with: bytes! !


!FileList methodsFor: 'file menu action' stamp: 'jmv 5/26/2013 15:50'!
addNew: aString byEvaluating: aBlock
	"A parameterization of earlier versions of #addNewDirectory and
	#addNewFile.  Fixes the bug in each that pushing the cancel button
	in the FillInTheBlank dialog gave a walkback."

	| response newName index |
	(response _ FillInTheBlankMorph
						request: ('New {1} Name?' format: {aString})
						initialAnswer: ('{1}Name' format: {aString}))
		isEmpty ifTrue: [^ self].
	newName _ response asFileName.
	aBlock value: newName.
	self updateFileList.
	index _(1 to:  list size) detect: [ :i |
		(list at: i) includesSubString: newName ] ifNone: [ 0 ].
	self fileListIndex: index! !

!FileList methodsFor: 'file menu action' stamp: 'jmv 5/26/2013 16:11'!
get
	"Get contents of file again, it may have changed. Do this by making the cancel string be the contents, and doing a cancel."

	brevityState == #briefHex
		ifTrue: [brevityState _ #needToGetFullHex]
		ifFalse: [brevityState _ #needToGetFull].
	self acceptedContentsChanged! !

!FileList methodsFor: 'file menu action' stamp: 'jmv 5/26/2013 16:11'!
getHex
	"Get contents of file again, and display in Hex. Do this by making the cancel string be the contents, and doing a cancel."

	brevityState _ #needToGetBriefHex.
	self acceptedContentsChanged! !

!FileList methodsFor: 'private' stamp: 'jmv 5/26/2013 16:11'!
put: aText
	"Private - put the supplied text onto the file"

	| ff type |
	brevityState == #fullFile ifTrue: [
		ff _ directory newFileNamed: self fullName.
		ff nextPutAll: aText asString; close.
		fileName = ff localName 
			ifTrue: [ acceptedContentsCache _ aText asString]
			ifFalse: [self updateFileList].		"user renamed the file"
		^ true  "accepted"].

	listIndex = 0 ifTrue: [
		self inform: 'No fileName is selected'.
		^ false  "failed"].
	type _ 'These'.
	brevityState = #briefFile ifTrue: [type _ 'Abbreviated'].
	brevityState = #briefHex ifTrue: [type _ 'Abbreviated'].
	brevityState = #fullHex ifTrue: [type _ 'Hexadecimal'].
	brevityState = #FileList ifTrue: [type _ 'Directory'].
	self inform: ('{1} contents cannot
meaningfully be saved at present.' format:{type}).
	^ false  "failed"
! !

!FileList methodsFor: 'private' stamp: 'jmv 5/26/2013 20:02'!
updateFileList
	"Update my files list with file names in the current directory  
	that match the pattern.
	The pattern string may have embedded newlines or semicolons; these separate different patterns."
	| patterns |
	patterns _ OrderedCollection new.
	(pattern findTokens: (String with: Character crCharacter with: Character lfCharacter with: $;))
		do: [ :each |
			(each includes: $*) | (each includes: $#)
				ifTrue: [ patterns add: each]
				ifFalse: [
					each isEmpty
						ifTrue: [ patterns add: '*']
						ifFalse: [ patterns add: '*' , each , '*']]].

	list _ self listForPatterns: patterns.
	listIndex _ 0.
	fileName _ nil.
	acceptedContentsCache _ ''.
	self changed: #fileList.
	self changed: #updateButtonRow! !


!FileStream methodsFor: 'editing' stamp: 'jmv 5/26/2013 15:51'!
viewGZipContents
	"View the contents of a gzipped file"

	| stringContents |
	self binary.
	stringContents := self contentsOfEntireFile.
	stringContents := (GZipReadStream on: stringContents) upToEnd.
	stringContents := stringContents asString withCuisLineEndings.

	Workspace new
		contents: stringContents;
		openLabel: 'Decompressed contents of: ', self localName! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 5/26/2013 20:07'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	| itemMorph |
	itemMorph _ self itemFromPoint: localEventPosition.
	itemMorph ifNil: [^self].
	"No change if model is locked"
	(self ownerThatIsA: SystemWindow) ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	((autoDeselect isNil or: [autoDeselect]) and: [itemMorph == selectedMorph]) 
		ifTrue: [self setSelectedMorph: nil]
		ifFalse: [self setSelectedMorph: itemMorph]! !


!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 5/26/2013 16:13'!
formFromStream: aBinaryStream
	"Answer a ColorForm stored on the given stream.  closes the stream"
	| reader readerClass form  |

	readerClass _ self withAllSubclasses
		detect: [:subclass | subclass understandsImageFormat: aBinaryStream]
		ifNone: [
			aBinaryStream close.
			^self error: 'image format not recognized'].
	reader _ readerClass new on: aBinaryStream reset.
	form _ reader nextImage.
	reader close.
	^ form! !

!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 5/26/2013 16:13'!
putForm: aForm onFileNamed: fileName
	"Store the given form on a file of the given name."

	| writer |
	writer _ self on: (FileStream newFileNamed: fileName) binary.
	writer nextPutImage: aForm.
	writer close.
! !

!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 5/26/2013 16:13'!
putForm: aForm onStream: aWriteStream
	"Store the given form on a file of the given name."

	| writer |
	writer _ self on: aWriteStream.
	writer nextPutImage: aForm.
	writer close! !


!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 5/26/2013 16:31'!
enterClickableRegion: aMorphicEvent localPosition: localEventPosition
! !


!JPEGReadWriter2 class methodsFor: 'image reading/writing' stamp: 'jmv 5/26/2013 16:13'!
putForm: aForm quality: quality progressiveJPEG: progressiveFlag onFileNamed: fileName
	"Store the given Form as a JPEG file of the given name, overwriting any existing file of that name. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG."

	| writer |
	FileDirectory deleteFilePath: fileName.
	writer _ self on: (FileStream newFileNamed: fileName) binary.
	writer nextPutImage: aForm quality: quality progressiveJPEG: progressiveFlag.
	writer close.
! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 5/26/2013 16:14'!
invokeWithEvent: evt
	"Perform the action associated with the given menu item."

	| selArgCount w |
	self isEnabled ifFalse: [^ self].
	owner ifNotNil: [
		self flag: #workAround. "The tile system invokes menus straightforwardly so the menu might not be in the world."
		(w _ self world) ifNotNil:[
			owner deleteIfPopUp: evt.
			"Repair damage before invoking the action for better feedback"
			w displayWorldSafely]].
	selector ifNil: [ ^self ].
	(selArgCount _ selector numArgs) = 0
		ifTrue: [
			target perform: selector]
		ifFalse: [
			selArgCount = arguments size
				ifTrue: [target perform: selector withArguments: arguments]
				ifFalse: [target perform: selector withArguments: (arguments copyWith: evt)]]! !


!MessageNames methodsFor: 'search' stamp: 'jmv 5/26/2013 15:52'!
computeSelectorListFromSearchString
	"Compute selector list from search string"
	| sorted |
	sorted _ SortedCollection sortBlock: [ :x :y |
		x asLowercase <= y asLowercase ].
	(searchString findBetweenSubStrs: ';') do: [ :selPat | | raw n m |
		selPat size > 0 ifTrue: [
			(selPat first = $" and: [
				selPat last = $" and: [ selPat size > 2 ]])
				ifTrue: [
					Symbol
						hasInterned:
							(selPat
								copyFrom: 2
								to: selPat size - 1)
						ifTrue: [ :sym |
							sorted add: sym ]]
				ifFalse: [
					raw _ ((n _ selPat occurrencesOf: $*) > 0 | ((m _ selPat occurrencesOf: $#) > 0) and: [ selPat size > (n + m) ])
						ifTrue: [ Symbol selectorsMatching: selPat ]
						ifFalse: [ Symbol selectorsContaining: selPat ].
					sorted addAll: raw ]]].
	selectorList _ sorted asArray  select: [ :sym |
		(Smalltalk isThereAnImplementorOf: sym) or: [ Smalltalk isThereAReferenceTo: sym ]].
	^ selectorList.! !


!Paragraph methodsFor: 'editing' stamp: 'jmv 5/26/2013 16:31'!
clickAt: clickPoint
	"Give sensitive text a chance to fire.  Display flash: (100@100 extent: 100@100)."
	| startBlock action target range boxes box t |
	action _ false.
	startBlock _ self characterBlockAtPoint: clickPoint.
	t _ model actualContents.
	(t attributesAt: startBlock stringIndex) do: [ :att | 
		att mayActOnClick ifTrue:
				[(target _ model) ifNil: [ target _ editor morph].
				range _ t rangeOf: att startingAt: startBlock stringIndex.
				boxes _ self selectionRectsFrom: (self characterBlockForIndex: range first) 
							to: (self characterBlockForIndex: range last+1).
				box _ boxes detect: [ :each | each containsPoint: clickPoint] ifNone: nil.
				box ifNotNil: [
					box _ editor morph morphBoundsInWorld.
					editor morph allOwnersDo: [ :m | box _ box intersect: (m morphBoundsInWorld) ].
					Utilities
						awaitMouseUpIn: box
						repeating: nil
						ifSucceed: [(att actOnClickFor: target in: self at: clickPoint editor: editor) ifTrue: [action _ true]].
				]]].
	^ action! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 5/26/2013 20:06'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"The mouse came up within the list; take appropriate action"

	| row |
	row _ self rowAtLocation: localEventPosition.
	(self ownerThatIsA: SystemWindow) ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	(autoDeselect == false and: [row = 0]) ifTrue: [^ self].  "work-around the no-mans-land bug"
	"No change if model is locked"
	((autoDeselect == nil or: [autoDeselect]) and: [ row == self selectionIndex ])
		ifTrue: [
			aMouseButtonEvent mouseButton1Changed ifTrue: [
				self changeModelSelection: 0 ]]
		ifFalse: [ self changeModelSelection: row ]! !


!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 5/26/2013 16:15'!
startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean 
	"Display the menu, with caption if supplied. Wait for the mouse button to go down, then track the selection as long as the button is pressed. When the button is released,
	Answer the index of the current selection, or zero if the mouse is not released over  any menu item. Location specifies the desired topLeft of the menu body rectangle. The final argument indicates whether the menu should seize the keyboard focus in order to allow the user to navigate it via the keyboard."

	| maxHeight |
	maxHeight := Display height * 3 // 4.
	self frameHeight > maxHeight 
		ifTrue: [
			^self 
				startUpSegmented: maxHeight
				withCaption: captionOrNil
				at: location
				allowKeyboard: aBoolean].
	^(MVCMenuMorph from: self title: captionOrNil) 
		invokeAt: location
		allowKeyboard: aBoolean! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 5/26/2013 16:17'!
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
	self afterSelectionInsertAndSelect: reply! !


!StandardFileMenu methodsFor: 'menu building' stamp: 'jmv 5/26/2013 15:53'!
makeFileMenuFor: aDirectory
"Initialize an instance of me to operate on aDirectory"

	| theMenu |
	pattern ifNil: [ self pattern: '*'].
	self 
		labels: 	(self menuLabelsString: aDirectory)
		lines: 	(self menuLinesArray: aDirectory).
	theMenu _ self selections: (self menuSelectionsArray: aDirectory).
	^theMenu! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 15:54'!
allCallsOn: aLiteral   "Smalltalk browseAllCallsOn: #open:label:."
	"Answer a Collection of all the methods that call on aLiteral."
	| aCollection special aList byte |

	#(23 48 'fred' (new open:label:)) size.
"Example above should find #open:label:, though it is deeply embedded here."

	aCollection _ OrderedCollection new.
	special _ self hasSpecialSelector: aLiteral ifTrueSetByte: [:b | byte _ b ].
	self allBehaviorsDo: [:class |
		aList _ class whichSelectorsReferTo: aLiteral special: special byte: byte.
		aList do: [ :sel |
					
			"For special selectors, look for the literal in the source code.
			Otherwise, for example, searching for senders of #== will include senders of #ifNil:"
			(byte isNil or: [
				((class sourceCodeAt: sel)
					findString: aLiteral) > 0]) ifTrue: [

				aCollection add: (
					MethodReference new
						setStandardClass: class 
						methodSymbol: sel
				)
			]
		]
	].
	^ aCollection! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 15:55'!
allCallsOn: firstLiteral and: secondLiteral
	"Answer a SortedCollection of all the methods that call on both aLiteral 
	and secondLiteral."

	| aCollection secondArray firstSpecial secondSpecial firstByte secondByte |
	aCollection _ SortedCollection new.
	firstSpecial _ self hasSpecialSelector: firstLiteral ifTrueSetByte: [:b | firstByte _ b].
	secondSpecial _ self hasSpecialSelector: secondLiteral ifTrueSetByte: [:b | secondByte _ b].
	self allBehaviorsDo: [ :class |
		secondArray _ class 
			whichSelectorsReferTo: secondLiteral
			special: secondSpecial
			byte: secondByte.
		((class whichSelectorsReferTo: firstLiteral special: firstSpecial byte: firstByte) select: [ :aSel |
			(secondArray includes: aSel)]) do: [ :sel | 
				aCollection add: (
					MethodReference new
						setStandardClass: class 
						methodSymbol: sel )]].
	^aCollection! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:18'!
allGlobalRefsWithout: classesAndMessagesPair
	"Answer a set of symbols that may be refs to Global names. 
	This method computes its result in the absence of specified classes and messages.
	Does not include references from a class to itself"

	| globalRefs absentClasses absentSelectors |
	globalRefs _ IdentitySet new: CompiledMethod instanceCount.
	absentClasses _ classesAndMessagesPair first.
	absentSelectors _ classesAndMessagesPair second.
	self classNames do: [ :cName |
		((absentClasses includes: cName)
					ifTrue: [#()]
					ifFalse: [{(self at: cName). (self at: cName) class}]) do: [ :cl | 
			(absentSelectors isEmpty
					ifTrue: [cl selectors]
					ifFalse: [cl selectors copyWithoutAll: absentSelectors]) do: [ :sel |
				"Include all capitalized symbols for good measure"
				self allSymbolsIn: (cl compiledMethodAt: sel) literals do: [ :m | 
					((m isMemberOf: Symbol) and: [ m size > 0] and: [m first isUppercase]) ifTrue: [
						m = cl name ifFalse: [
							globalRefs add: m]].
					(m isVariableBinding) ifTrue:
						[m key ifNotNil: [
							m key = cl name ifFalse: [
								globalRefs add: m key]]]]]]].
	^ globalRefs! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:19'!
allImplementedMessagesWithout: classesAndMessagesPair
	"Answer a Set of all the messages that are implemented in the system, computed in the absence of the supplied classes and messages.  Note this reports messages that are in the absent selectors set."
	| messages absentClasses |
	messages _ IdentitySet new: CompiledMethod instanceCount.
	absentClasses _ classesAndMessagesPair first.
	self classNames do: [ :cName |
		((absentClasses includes: cName)
			ifTrue: [#()]
			ifFalse: [{(self at: cName). (self at: cName) class}]) do: [ :cl |
				messages addAll: cl selectors]].
	^ messages! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 15:56'!
allImplementorsOf: aSelector  
	"Answer a SortedCollection of all the methods that implement the message 
	aSelector."

	| aCollection |

	aCollection _ SortedCollection new.
	self allBehaviorsDo: [ :class |
		(class includesSelector: aSelector) ifTrue: [
			aCollection add: (
				MethodReference new
					setStandardClass: class 
					methodSymbol: aSelector )]].
	^aCollection! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 15:57'!
allImplementorsOf: aSelector  localTo: aClass
	"Answer a sorted Collection of all the methods that implement the message 
	aSelector in, above, or below the given class."

	| aSet cls |
	aSet _ Set new.
	cls _ aClass theNonMetaClass.
	cls withAllSuperAndSubclassesDoGently: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [aSet add: class name, ' ', aSelector]].
	cls class withAllSuperAndSubclassesDoGently: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [aSet add: class name, ' ', aSelector]].
	^aSet asArray sort! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 15:57'!
allMethodsInCategory: category 
	| aCollection |
	aCollection _ SortedCollection new.
	self allBehaviorsDo: [ :x |
		(x organization listAtCategoryNamed: category) do: [ :sel |
			aCollection add: x name , ' ' , sel]].
	^aCollection! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 15:58'!
allMethodsWithString: aString
	"Answer a sorted Collection of all the methods that contain, in a string literal, aString as a substring.  2/1/96 sw.  The search is case-sensitive, and does not dive into complex literals, confining itself to string constants.
	5/2/96 sw: fixed so that duplicate occurrences of aString in the same method don't result in duplicated entries in the browser"
	| aStringSize list |
	aStringSize _ aString size.
	list _ Set new.
	self allBehaviorsDo: [ :class |
		class selectorsDo: [ :sel |
			(class compiledMethodAt: sel) literalsDo: [ :aLiteral |
				((aLiteral isMemberOf: String) and: [ aLiteral size >= aStringSize ]) ifTrue: [
					(aLiteral
						findString: aString
						startingAt: 1) > 0 ifTrue: [ list add: class name , ' ' , sel ]]]]].
	^ list asArray sort! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:20'!
allPrimitiveMessages
	"Answer an OrderedCollection of all the methods that are implemented by 
	primitives."

	| aColl method | 
	aColl _ OrderedCollection new: 200.
	self allBehaviorsDo: [ :class |
		class selectorsDo: [ :sel | 
			method _ class compiledMethodAt: sel.
			method primitive ~= 0 ifTrue: [
				aColl addLast: class name , ' ' , sel 
					, ' ' , method primitive printString]]].
	^aColl! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:21'!
allPrimitiveMethodsInCategories: aList
	"Answer an OrderedCollection of all the methods that are implemented by primitives in the given categories.  1/26/96 sw"

	| aColl method | 
	aColl _ OrderedCollection new: 200.
	self allBehaviorsDo: [ :aClass |
		(aList includes: (SystemOrganization categoryOfElement: aClass theNonMetaClass name asString) asString)
			ifTrue: [
				aClass selectorsDo: [ :sel | 
					method _ aClass compiledMethodAt: sel.
					method primitive ~= 0
						ifTrue: [
							aColl addLast: aClass name , ' ' , sel 
								, ' ' , method primitive printString]]]].
	^ aColl

"Smalltalk allPrimitiveMethodsInCategories: #('Collections-Streams' 'Files-Streams' 'Files-Abstract' 'Files-Macintosh')"! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:21'!
allSelect: aBlock
	"Answer a SortedCollection of each method that, when used as the block 
	argument to aBlock, gives a true result."
	| aCollection |
	aCollection _ SortedCollection new.
	self allBehaviorsDo: [ :class |
		class selectorsDo: [ :sel |
			(aBlock value: (class compiledMethodAt: sel)) ifTrue: [
				aCollection add: (
					MethodReference new
						setStandardClass: class 
						methodSymbol: sel)
			]]].
	^ aCollection! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:24'!
allSentMessagesWithout: classesAndMessagesPair
	"Answer the set of selectors which are sent somewhere in the system, 
	computed in the absence of the supplied classes and messages."

	| sent absentClasses absentSelectors |
	sent _ IdentitySet new: CompiledMethod instanceCount.
	absentClasses _ classesAndMessagesPair first.
	absentSelectors _ classesAndMessagesPair second.
	self classNames do: [ :cName | 
		((absentClasses includes: cName)
				ifTrue: [#()]
				ifFalse: [{(self at: cName). (self at: cName) class}]) do: [ :cl |
			 (absentSelectors isEmpty
					ifTrue: [cl selectors]
					ifFalse: [cl selectors copyWithoutAll: absentSelectors]) do: [ :sel |
				 "Include all sels, but not if sent by self"
				self allSymbolsIn: (cl compiledMethodAt: sel) literals do: [ :m |
					m == sel ifFalse: [sent add: m] ]]]].
		"The following may be sent without being in any literal frame"
		1 to: self specialSelectorSize do: [ :index | 
			sent add: (self specialSelectorAt: index)].
	Smalltalk presumedSentMessages do: [ :sel | sent add: sel].
	^ sent! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:25'!
allUnimplementedCalls 
	"Answer an Array of each message that is sent by an expression in a 
	method but is not implemented by any object in the system."

	| aStream secondStream all  |
	all _ self allImplementedMessages.
	aStream _ WriteStream on: (Array new: 50).
	self allBehaviorsDo: [ :cl |
		 cl selectorsDo: [ :sel |
			 secondStream _ WriteStream on: (String new: 5).
			(cl compiledMethodAt: sel) messages do: [ :m |
				(all includes: m) ifFalse: [secondStream nextPutAll: m; space]].
			secondStream position = 0 ifFalse: [
				aStream nextPut: cl name , ' ' , sel , ' calls: ', secondStream contents ]]].
	^aStream contents! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:00'!
isThereAReferenceTo: aLiteral
	"Answer a Collection of all the methods that call on aLiteral."
	"
	Smalltalk isThereAReferenceTo: #open:label:
	"
	| special byte |

	#(23 48 'fred' (new open:label:)) size.
"Example above should find #open:label:, though it is deeply embedded here."

	special _ self hasSpecialSelector: aLiteral ifTrueSetByte: [:b | byte _ b ].
	self allBehaviorsDo: [:class |
		(class whichSelectorsReferTo: aLiteral special: special byte: byte) do: [ :sel |
			^true ]].
	^ false! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:00'!
numberOfSendersOf: aSymbol
	"Answer the count of all the methods that call on aLiteral.
	[ (Smalltalk numberOfSendersOf: #open:label:) ] timeToRun
	"
	| count specialFlag specialByte |

	count _ 0.
	specialFlag _ self hasSpecialSelector: aSymbol ifTrueSetByte: [ :b | specialByte _ b ].
	self allBehaviorsDo: [ :class |
		class selectorsAndMethodsDo: [ :sel :method |
			((method hasLiteral: aSymbol) or: [specialFlag and: [method scanFor: specialByte]])
				ifTrue: [ count _ count + 1 ]]].
	^ count! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:26'!
unimplemented
	"Answer an Array of each message that is sent by an expression in a method but is not implemented by any object in the system."

	| all unimplemented entry |
	all _ IdentitySet new: Symbol instanceCount * 2.
	self allBehaviorsDo: [ :cl | cl selectorsDo: [ :aSelector | all add: aSelector]].

	unimplemented _ IdentityDictionary new.
	self allBehaviorsDo: [:cl |
		 cl selectorsDo: [:sel |
			(cl compiledMethodAt: sel) messages do: [ :m |
				(all includes: m) ifFalse: [
					entry _ unimplemented at: m ifAbsent: [Array new].
					entry _ entry copyWith: (cl name, '>', sel).
					unimplemented at: m put: entry]]]].

	"remove some clutter from the result:"
	#(doPrimitive: primitiveFail success:) do: [ :sel |
		unimplemented removeKey: sel ifAbsent: nil].

	^ unimplemented! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 5/26/2013 15:58'!
browseAllAccessesTo: instVarName from: aClass
	"Create and schedule a Message Set browser for all the receiver's methods 
	or any methods of a subclass/superclass that refer to the instance variable name."

	"self new browseAllAccessesTo: 'contents' from: Collection."
	
	| coll |
	coll _ OrderedCollection new.
	aClass withAllSubAndSuperclassesDo: [:class | 
		(class whichSelectorsAccess: instVarName) do: [:sel |
			coll add: (
				MethodReference new
					setStandardClass: class 
					methodSymbol: sel)]].
	^ self 
		browseMessageList: coll 
		name: 'Accesses to ' , instVarName 
		autoSelect: instVarName! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 5/26/2013 16:32'!
browseAllImplementorsOfList: selectorList title: aTitle
	"Create and schedule a message browser on each method that implements the message whose selector is in the argument selectorList. For example,  Smalltalk browseAllImplementorsOf: #(at:put: size)."

	| implementorLists flattenedList |

	implementorLists _ selectorList collect: [:each | self allImplementorsOf: each].
	flattenedList _ Array streamContents: [ :stream |
		implementorLists do: [ :each | stream nextPutAll: each]].
	flattenedList sort.
	^ self browseMessageList: flattenedList name: aTitle! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 5/26/2013 15:58'!
browseAllStoresInto: instVarName from: aClass
	"Create and schedule a Message Set browser for all the receiver's methods 
	or any methods of a subclass/superclass that refer to the instance variable name."
	
	"self new browseAllStoresInto: 'contents' from: Collection."

	| coll |
	coll _ OrderedCollection new.
	aClass withAllSubAndSuperclassesDo: [:class | 
		(class whichSelectorsStoreInto: instVarName) do: [:sel |
			coll add: (
				MethodReference new
					setStandardClass: class 
					methodSymbol: sel)]].
	^ self
		browseMessageList: coll 
		name: 'Stores into ' , instVarName 
		autoSelect: instVarName! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 5/26/2013 15:59'!
browseClassCommentsWithString: aString
	"Smalltalk browseClassCommentsWithString: 'my instances' "
	"Launch a message list browser on all class comments containing aString as a substring."
	| caseSensitive suffix list |
	suffix _ (caseSensitive _ Sensor shiftPressed)
		ifTrue: [ ' (case-sensitive)' ]
		ifFalse: [ ' (use shift for case-sensitive)' ].
	list _ Set new.
	Smalltalk allClassesDo: [ :class |
		(class organization classComment asString
			findString: aString
			startingAt: 1
			caseSensitive: caseSensitive) > 0 ifTrue: [
			list add:
				(MethodReference new
					setStandardClass: class
					methodSymbol: #Comment) ]].
	^ self
		browseMessageList: list asArray sort
		name: 'Class comments containing ', aString printString, suffix
		autoSelect: aString.! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 5/26/2013 15:59'!
browseClassesWithNamesContaining: aString caseSensitive: caseSensitive 
	"Smalltalk browseClassesWithNamesContaining: 'eMorph' caseSensitive: true "
	"Launch a class-list list browser on all classes whose names containg aString as a substring."

	| suffix aList |
	suffix _ caseSensitive
				ifTrue: [' (case-sensitive)']
				ifFalse: [' (use shift for case-sensitive)'].
	aList _ OrderedCollection new.
	Smalltalk allClassesDo: [ :class |
		(class name includesSubstring: aString caseSensitive: caseSensitive)
			ifTrue: [aList add: class name]].
	aList size > 0
		ifTrue: [HierarchyBrowserWindow forClassesNamed: aList asSet asSortedArray title: 'Classes whose names contain ' , aString , suffix]! !


!TestRunner methodsFor: 'processing' stamp: 'jmv 5/26/2013 16:26'!
runOneTest
	| testSuite |
	self runWindow.
	selectedSuite isZero ifTrue: [ ^ self displayPassFail: 'No Test Suite Selected' ].
	testSuite _  TestSuite new name: 'TestRunner Suite'.
	self addTestsFor: (tests at: selectedSuite) toSuite: testSuite.
	self runSuite: testSuite! !

!TestRunner methodsFor: 'processing' stamp: 'jmv 5/26/2013 16:26'!
runProfiledTests
	| suite |
	suite _ TestSuite new name: 'TestRunner Suite'.
	self selectedTests do: [ :ea | self addTestsFor: ea toSuite: suite ].
	self runSuiteProfiled: suite! !

!TestRunner methodsFor: 'processing' stamp: 'jmv 5/26/2013 16:27'!
runTests
	| suite |
	suite _ TestSuite new name: 'TestRunner Suite'.
	self selectedTests do: [ :ea | self addTestsFor: ea toSuite: suite ].
	self runSuite: suite! !


!VersionsBrowserWindow class methodsFor: 'services' stamp: 'jmv 5/26/2013 16:27'!
browseCommentOf: class

	| changeList |
	changeList _ ClassCommentVersionsBrowser new scanVersionsOf: class.
	changeList ifNil: [^ self inform: 'No versions available'].
	VersionsBrowserWindow
		open: changeList
		label: 'Recent versions of ', class name, '''s comments'! !

!VersionsBrowserWindow class methodsFor: 'services' stamp: 'jmv 5/26/2013 16:27'!
browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector lostMethodPointer: sourcePointer 
	| changeList browser |
	changeList _ (browser _ VersionsBrowser new)
		scanVersionsOf: method class: class meta: meta
		category: msgCategory selector: selector.
	changeList ifNil: [ self inform: 'No versions available'. ^nil ].

	VersionsBrowserWindow open: changeList label: 'Recent versions of ', selector.

	^browser! !

