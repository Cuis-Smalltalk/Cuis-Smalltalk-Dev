'From Cuis 4.0 of 3 April 2012 [latest update: #1257] on 17 April 2012 at 10:32:20 pm'!

!CodePackage methodsFor: 'accessing' stamp: 'jmv 4/17/2012 22:31'!
description: aString

	description = aString
		ifFalse: [
			description _ aString.
			self hasUnsavedChanges: true ]! !


!CodePackageFile methodsFor: 'change record types' stamp: 'jmv 4/17/2012 22:30'!
possibleDescription: chgRec
	| tokens prefix token |
	description isEmpty ifTrue:[
		tokens _ Smalltalk actualScannerClass new scanTokens: chgRec string.
		(tokens size = 1 and: [ (token _ tokens first) class == String ]) ifTrue: [
			prefix _ 'Description '.
			(token beginsWith: prefix) ifTrue: [
				description _ token copyFrom: prefix size + 1 to: token size.
				^self ]]].
	doIts add: chgRec.! !

