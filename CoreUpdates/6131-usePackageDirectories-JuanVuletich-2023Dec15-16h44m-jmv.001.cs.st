'From Cuis6.0 [latest update: #6122] on 15 December 2023 at 4:46:57 pm'!

!CodePackage methodsFor: 'saving' stamp: 'jmv 12/15/2023 15:03:47'!
save
	"If we can't save, find a new destination directory."
	fullFileName ifNotNil: [
		fullFileName asFileEntry parent exists ifFalse: [
			fullFileName := nil ]].

	"If we were never saved, or never saved since image was moved, or target directory disappeared, then save to image directory."
	fullFileName ifNil: [
		fullFileName :=
			(DirectoryEntry newPackagesDirectory // self packageFileName) pathName ].

	fullFileName asFileEntry forceWriteStreamDo: [ :stream |
		stream timeStamp.
		self writeOnStream: stream ].

	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self)! !


!FeatureRequirement methodsFor: 'private' stamp: 'jmv 12/15/2023 16:16:44'!
placesToLookForPackagesDo: aBlock

	| myDir |

	"Look inside my own folder"
	pathName ifNotNil: [
		myDir := pathName asFileEntry parent.
		aBlock value: myDir ].

	"Look in codePackageFile folder"
	codePackageFile ifNotNil: [
		myDir := codePackageFile fullName asFileEntry parent.
		aBlock value: myDir ].

	"Packages that come included with Cuis"
	aBlock value: DirectoryEntry packagesDirectory.
	DirectoryEntry packagesDirectory allRegularDirectoriesDo: aBlock.

	"Packages created by user"
	aBlock value: DirectoryEntry newPackagesDirectory.

	"Packages in other folders or repos in the project directory"
	aBlock value: DirectoryEntry projectBaseDirectory.
	DirectoryEntry projectBaseDirectory allRegularDirectoriesDo: aBlock.! !


!PackageInstaller methodsFor: 'actions' stamp: 'jmv 12/15/2023 16:17:42'!
placesToLookForPackagesDo: aBlock

	"Packages that come included with Cuis"
	aBlock value: DirectoryEntry packagesDirectory.
	DirectoryEntry packagesDirectory allRegularDirectoriesDo: aBlock.

	"Packages in other folders or repos in the project directory"
	aBlock value: DirectoryEntry projectBaseDirectory.
	DirectoryEntry projectBaseDirectory allRegularDirectoriesDo: aBlock.! !

