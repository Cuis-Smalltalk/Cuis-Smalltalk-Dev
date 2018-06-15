'From Cuis 5.0 of 7 November 2016 [latest update: #3345] on 5 June 2018 at 11:03:34 am'!

!CodePackage methodsFor: 'naming' stamp: 'jmv 6/5/2018 11:02:21'!
packageName: aString
	packageName _ aString.
	description _ 'Please enter a description for this package'.
	featureSpec _ FeatureSpec new.
	featureSpec provides: (Feature name: packageName version: 1 revision: 0).
	hasUnsavedChanges _ self includesAnyCode.
	"But reset revision if it was incremented because of marking it dirty!!"
	featureSpec provides name: packageName version: 1 revision: 0! !

