'From Cuis 5.0 of 7 November 2016 [latest update: #3387] on 1 August 2018 at 5:51:16 pm'!

!CodePackageFile class methodsFor: 'file list services' stamp: 'jmv 8/1/2018 08:49:06'!
serviceInstallPackage
	"Answer the service of installing a CodePackageFile (a chunk format file for a CodePackage)"

	^ (SimpleServiceEntry
		provider: self 
		label: 'install package'
		selector: #installPackage:
		description: 'install the package, replacing all existing code in the package (if any); create a new change set just for the installation'
		buttonLabel: 'install package')
			sortOrder: 100;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

!methodRemoval: CodePackageFile class #installPackageStream:!
CodePackageFile class removeSelector: #installPackageStream:!
