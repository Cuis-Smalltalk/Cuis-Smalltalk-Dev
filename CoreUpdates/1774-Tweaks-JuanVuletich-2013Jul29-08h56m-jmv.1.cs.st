'From Cuis 4.2 of 25 July 2013 [latest update: #1772] on 29 July 2013 at 9:09:56.105015 am'!

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 7/29/2013 09:08'!
install
	"See if all the transitive closure of requirements can be met and answer the load order if so"

	CodePackageFile basicInstallPackageStream: ( FileStream readOnlyFileNamed: pathName).

	"No need to have a platform and machine specific path around anymore. It was just for installation. Clear it."
	pathName _ nil! !


!CodePackageFile class methodsFor: 'services' stamp: 'jmv 7/29/2013 09:07'!
installPackageStream: aStream

	| fullName pkName |
	fullName _ aStream name.
	pkName _ CodePackageFile packageNameFrom: fullName.
	(FeatureRequirement name: pkName)
		pathName: fullName;
		require "Install with all requirements"! !


!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 7/29/2013 09:06'!
findPackageFileAsReqOf: mainFeatureOrNil
	"Look in known places for packages providing required feature.
	Answer wether search was successful."
	| packageNameString |
	pathName ifNotNil: [
		(FileDirectory default fileExists: pathName) ifTrue: [ ^ true ]].
	packageNameString _ self packageName.
	(mainFeatureOrNil ifNil: [ self ]) placesToLookForPackagesDo: [ :fileDirectory |
		(fileDirectory fileExists: packageNameString) ifTrue: [
			pathName _ fileDirectory fullNameFor: packageNameString.
			^ true ]].
	^ false! !

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 7/29/2013 09:03'!
require
	"See if all the transitive closure of requirements can be met and answer the load order if so"

	| packagesToLoad |
	
	"Preflight before load"
	packagesToLoad _ self requireUnlessIn: OrderedCollection new asReqOf: nil main: nil.
	packagesToLoad do: [ :requirement | requirement install ]! !


!FileDirectory methodsFor: 'searching' stamp: 'jmv 7/29/2013 09:04'!
filesContaining: searchString caseSensitive: aBoolean
	| aList |
	"Search the contents of all files in the receiver and its subdirectories for the search string.  Return a list of paths found.  Make the search case sensitive if aBoolean is true."

	aList _ OrderedCollection new.
	self withAllFilesDo: [ :stream |
			(stream contentsOfEntireFile includesSubstring: searchString caseSensitive: aBoolean)
				ifTrue:	[ aList add: stream name ]]
		andDirectoriesDo: [ :d | d pathName ].
	^ aList

"FileDirectory default filesContaining: 'includesSubstring:'  caseSensitive: true"! !

!methodRemoval: FeatureRequirement #pathName!
FeatureRequirement removeSelector: #pathName!
