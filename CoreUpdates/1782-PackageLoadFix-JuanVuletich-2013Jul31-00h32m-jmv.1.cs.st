'From Cuis 4.2 of 25 July 2013 [latest update: #1781] on 31 July 2013 at 12:32:23.511047 am'!

!CodePackage methodsFor: 'naming' stamp: 'jmv 7/31/2013 00:30'!
packageName: aString
	packageName _ aString.
	description _ 'Please enter a description for this package'.
	featureSpec _ FeatureSpec new.
	featureSpec provides: (Feature name: packageName version: 1 revision: 0).
	self hasUnsavedChanges: self includesAnyCode.
	"But reset revision if it was incremented because of marking it dirty!!"
	featureSpec provides name: packageName version: 1 revision: 0! !

