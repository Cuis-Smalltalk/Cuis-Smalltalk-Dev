'From Cuis7.7 [latest update: #7954] on 20 May 2026 at 2:37:24 pm'!

!CodePackage class methodsFor: 'packages access' stamp: 'jmv 20/May/2026 14:36:47'!
maybeCreatePackageForSystemCategory: newSystemCategory
	"If newSystemCategory doesn't belong in an existing system category or package,
	create a new package for it."

	| inferredPackageName |
	ChangeSet notInstallOrTestRun ifTrue: [
		inferredPackageName := newSystemCategory copyUpTo: $-.
		(SystemOrganization hasAnyCategoriesSuchThat: [ :eachCat |
			(inferredPackageName = eachCat) or: [
				inferredPackageName, '-*' match: eachCat ]]) ifFalse: [
					"{CodePackage. #named:. inferredPackageName. #createIfAbsent:. true. #registerIfNew:. true} print."
					CodePackage named: inferredPackageName createIfAbsent: true registerIfNew: true.
					]].! !

!CodePackage class methodsFor: 'packages access' stamp: 'jmv 20/May/2026 14:34:45'!
named: aString createIfAbsent: neverAnswerNil registerIfNew: doRegisterIfJustCreated
	"Answer the instance with name aString.
	If there's no package named aString, answer nil or a new instance as requested.
	If a new instance is created, register it or not as requested.
	CodePackage named: 'Morphic' createIfAbsent: false registerIfNew: false
	CodePackage named: 'Morphic' createIfAbsent: true registerIfNew: false
	CodePackage named: 'XXXXXTest' createIfAbsent: true registerIfNew: true
	"

	| newInstance |
	^self installedPackages
		at: aString
		ifAbsent: [
			neverAnswerNil ifTrue: [ 
				newInstance := self new packageName: aString.
				doRegisterIfJustCreated ifTrue: [ self register: newInstance ].
				newInstance ]].! !

