'From Cuis 4.1 of 12 December 2012 [latest update: #1756] on 16 July 2013 at 6:05:51.02 pm'!

!CodePackageFile class methodsFor: 'services' stamp: 'jmv 7/16/2013 18:03'!
basicInstallPackageStream: aStream

	| fullName pkName existing |
	fullName _ aStream name.
	pkName _ CodePackageFile packageNameFrom: fullName.
	existing _ CodePackage named: pkName createIfAbsent: false registerIfNew: false.
	(existing isNil
		or: [ existing hasUnsavedChanges not
			or: [ self confirm: 'If you install this package, there are unsaved changes that will be lost.', String newLineString, 'Continue?' ]]) ifTrue: [
		CodePackageFile
			installFileStream: aStream
			packageName: pkName
			fullName: fullName ]! !


!CodePackageFile class methodsFor: 'services' stamp: 'jmv 7/16/2013 18:04'!
installPackageStream: aStream

	| fullName pkName |
	fullName _ aStream name.
	pkName _ CodePackageFile packageNameFrom: fullName.
	(Feature name: pkName) require! !


!Feature methodsFor: 'requires' stamp: 'jmv 7/16/2013 18:03'!
require
	"See if all the transitive closure of requirements can be met and answer the load order if so"

	| packagesToLoad |
	
	"Preflight before load"
	packagesToLoad := self requireUnlessIn: OrderedCollection new.
	
	packagesToLoad do: [ :provideLoadSpec |
		CodePackageFile basicInstallPackageStream: ( FileStream readOnlyFileNamed: (provideLoadSpec pathName) ).
	]! !

