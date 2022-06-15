'From Cuis 6.0 [latest update: #5171] on 15 June 2022 at 11:30:17 am'!

!FeatureRequirement methodsFor: 'accessing' stamp: 'jmv 6/15/2022 11:30:10'!
codePackageFile
	codePackageFile isNil ifTrue: [
		codePackageFile _ CodePackageFile onFileEntry: pathName asFileEntry.
		"?"
		pathName _ nil ].
	^codePackageFile! !

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 6/15/2022 11:28:26'!
install
	"Preconditions have been satisfied.  Install the required package."

	| existing |
	existing _ CodePackage named: self codePackageFile packageName createIfAbsent: false registerIfNew: false.
	(existing isNil
		or: [ existing hasUnsavedChanges not
			or: [ self confirm: 'If you install this package, there are unsaved changes that will be lost.', String newLineString, 'Continue?' ]]) ifTrue: [
		self codePackageFile install.
		codePackageFile _ nil.
	]! !

