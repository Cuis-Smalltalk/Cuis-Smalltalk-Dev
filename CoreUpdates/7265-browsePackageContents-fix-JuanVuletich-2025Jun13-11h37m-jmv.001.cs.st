'From Cuis7.5 [latest update: #7263] on 13 June 2025 at 11:46:57 am'!

!ChangeList class methodsFor: 'public access' stamp: 'jmv 6/13/2025 11:45:20'!
browsePackageContents: aFileEntry
	"Opens a changeList on a fileStream"
	| changeList packageFile |
	packageFile := CodePackageFile onFileEntry: aFileEntry.
	changeList := aFileEntry readStreamDo: [ :stream |
		self new scanFile: stream from: 0 to: stream size ].
	"Add deletions of methods and classes that are in the CodePackage (i.e., active in the system)
	but are no longer in the CodePackageFile being viewed."
	packageFile methodsToRemove do: [ :methodReference |
		changeList
			addItem: (MethodDeletionChangeRecord new
				methodReference: methodReference;
				doItOnlyIfInBaseSystem: false)
			text: 'method no longer in package: ', methodReference stringVersion ].
	packageFile classesToRemove do: [ :clsName |
		changeList
			addItem: (ClassDeletionChangeRecord new
				className: clsName;
				doItOnlyIfInBaseSystem: false)
			text: 'class no longer in package: ', clsName ].
	changeList clearSelections.
	ChangeListWindow open: changeList label: aFileEntry name! !

