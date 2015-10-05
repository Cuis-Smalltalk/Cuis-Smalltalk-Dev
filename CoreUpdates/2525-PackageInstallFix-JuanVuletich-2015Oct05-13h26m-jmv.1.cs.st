'From Cuis 4.2 of 25 July 2013 [latest update: #2524] on 5 October 2015 at 1:26:47.737664 pm'!

!CodePackageFile class methodsFor: 'services' stamp: 'jmv 10/5/2015 13:26'!
installPackageStream: aStream

	| fullName pkName |
	fullName _ aStream pathName.
	pkName _ CodePackageFile packageNameFrom: fullName.
	(FeatureRequirement name: pkName)
		pathName: fullName;
		satisfyRequirementsAndInstall "Install with all requirements"! !

