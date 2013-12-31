'From Cuis 4.0 of 21 April 2012 [latest update: #1269] on 30 April 2012 at 9:30:39 pm'!

!CodePackageFile class methodsFor: 'services' stamp: 'jmv 4/30/2012 21:29'!
monticelloPackageNameFrom: fullName
	| localName |
	localName _ FileDirectory localNameFor: fullName.
	^(localName prefixAndSuffix: $-)
		ifNotNil: [ :ary | ary first ]
		ifNil: [ localName sansPeriodSuffix ].! !


!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 4/30/2012 21:29'!
browseMCZContents: aStream
	"Browse the selected file."
	| unzipped changeList fullName packageFile pkName |
	
	"For Monticello packages we do as we do with our own .pck files: Instead of just browsing
	contents, also include what is no longer part of the package (and should therefore be removed on install)
	See #browsePackageContents:
	However, this was never tested to run!!"
	self flag: #jmvVer.

	fullName _ aStream name.
	pkName _ CodePackageFile monticelloPackageNameFrom: fullName.
	unzipped _ aStream asUnZippedStream: 'snapshot/source.st'.
	unzipped ascii.
	Cursor read showWhile: [
		changeList _ self new scanFile: unzipped from: 0 to: unzipped size.
		aStream reset.
		packageFile _ CodePackageFile
			buildFileStream: unzipped
			packageName: pkName
			fullName: fullName ].
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


!CodeFileBrowserWindow class methodsFor: 'services' stamp: 'jmv 4/30/2012 21:29'!
installMonticelloPackageStream: aStream
	
	| stream fullName pkName |
	fullName _ aStream name.
	pkName _ CodePackageFile monticelloPackageNameFrom: fullName.
	stream _ aStream asUnZippedStream: 'snapshot/source.st'.
	stream ascii.
	Cursor wait showWhile: [
		CodePackageFile
			installFileStream: stream
			packageName: pkName
			fullName: fullName ]! !


!CodePackageFile class methodsFor: 'services' stamp: 'jmv 4/30/2012 21:30'!
packageNameFrom: fullName

	^(FileDirectory localNameFor: fullName) sansPeriodSuffix! !

