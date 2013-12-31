'From Cuis 4.0 of 21 April 2012 [latest update: #1467] on 10 October 2012 at 10:42:03 am'!

!CodePackageListWindow methodsFor: 'commands' stamp: 'jmv 10/10/2012 10:41'!
createPackage

	| pkName |
	pkName _ FillInTheBlankMorph request: 'Name for new package?'.
	pkName ifNotEmpty: [
		CodePackage
			named: pkName
			createIfAbsent: true
			registerIfNew: true ]! !

