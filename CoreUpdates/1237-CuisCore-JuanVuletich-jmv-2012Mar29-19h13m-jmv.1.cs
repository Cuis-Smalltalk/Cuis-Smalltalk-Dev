'From Cuis 4.0Alpha of 29 March 2012 [latest update: #1225] on 29 March 2012 at 7:15:02 pm'!

!CodePackage class methodsFor: 'packages access' stamp: 'jmv 3/29/2012 16:05'!
                   deregister: aCodePackage
	"
	CodePackage register: (CodePackage newNamed: 'Tests-Files')
	CodePackage deregister: (CodePackage newNamed: 'Tests-Files')
	"
	InstalledPackages removeKey: aCodePackage packageName.
	self triggerEvent: #installedPackagesChanged! !


!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/29/2012 16:04'!
                   selection

	^selection! !


!CodePackageListWindow methodsFor: 'commands' stamp: 'jmv 3/29/2012 16:01'!
                        createPackage

	| pkName newCodePackage |
	pkName_ FillInTheBlank request: 'Name for new package?'.
	newCodePackage _ CodePackage newNamed: pkName.
	CodePackage register: newCodePackage! !


!SinglePackageBrowser methodsFor: 'lists' stamp: 'jmv 3/29/2012 19:02'!
                         messageList
	"Answer an Array of the message selectors of the currently selected message category, provided that the messageCategoryListIndex is in proper range.  Otherwise, answer an empty Array  If messageCategoryListIndex is found to be larger than the number of categories (it happens!!) (??), it is reset to zero."

	| answer |
	answer _ selectedMessageCategory
		ifNil: [ #() ]
		ifNotNil: [
			(self classOrMetaClassOrganizer listAtCategoryNamed: selectedMessageCategory) ifNil: [
				selectedMessageCategory _ nil.
				#() ]].
	selectedMessage ifNil: [
		answer size = 0 ifFalse: [
			(package includesSystemCategory: selectedSystemCategory) ifFalse: [
				selectedMessage _ answer first.
				self editSelection: #editMessage ]]].
	^answer! !

!SinglePackageBrowser methodsFor: 'lists' stamp: 'jmv 3/29/2012 19:03'!
        rawMessageCategoryList
	"Answer the selected category of messages."


	^ (package includesSystemCategory: selectedSystemCategory)
		ifTrue: [
			super rawMessageCategoryList reject: [ :cat | package isForeignClassExtension: cat ]]
		ifFalse: [
			super rawMessageCategoryList select: [ :cat | package isYourClassExtension: cat ]]! !

!SinglePackageBrowser methodsFor: 'lists' stamp: 'jmv 3/29/2012 19:03'!
          systemCategoryList

	^package systemCategories,
		(package systemCategoriesWithExtensionMethods collect: [ :cat |
			'*', cat ])! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 3/29/2012 16:10'!
          okayToDiscardUnsavedCodeSaving: wouldSave
	"Answer true unless the user cancels quitting because of some warning given.
	Smalltalk okayToDiscardUnsavedCodeSaving: true
	Smalltalk okayToDiscardUnsavedCodeSaving: false
	"

	| baseCs baseCSdirty dirtyPackages |
	baseCs _ ChangeSet changeSetForBaseSystem.
	baseCSdirty _ baseCs isEmpty not.
	baseCSdirty _ ChangeSorter allChangeSets anySatisfy: [ :any | any isForBaseSystem and: [ any isEmpty not ]].
	dirtyPackages _ CodePackage installedPackages anySatisfy: [ :pck | pck hasUnsavedChanges ].

	baseCSdirty & dirtyPackages ifTrue: [
		wouldSave ifTrue: [
			^self confirm: 'There are both unsaved Packages', String newLineString,
				'      (would need to be saved on next run), ', String newLineString,
				'and unsaved Changes to Cuis core', String newLineString,
				'      (they would be lost as a separate ChangeSet).', String newLineString,
				'Continue?' ]
		ifFalse: [
			^self confirm: 'There are both unsaved Packages', String newLineString,
				'and unsaved Changes to Cuis core.', String newLineString,
				'If you continue, they will all be lost.', String newLineString,
				'Continue?' ]].

	baseCSdirty ifTrue: [
		^self confirm: 'The current ChangeSet for Cuis core:', String newLineString,
			baseCs name, String newLineString, 
			'(or some other) has unsaved stuff. If you continue, it will be lost.', String newLineString,
			'Continue?' ].

	dirtyPackages ifTrue: [
		wouldSave ifTrue: [
			^self confirm: 'There are unsaved Packages.', String newLineString,
				'If you continue, they will need to be saved on next run.', String newLineString,
				'Continue?' ]
		ifFalse: [
			^self confirm: 'There are unsaved Packages.', String newLineString,
				'If you continue, they will all be lost.', String newLineString,
				'Continue?' ]].

	^true! !

