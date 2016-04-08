'From Cuis 4.2 of 25 July 2013 [latest update: #2723] on 8 April 2016 at 12:55:32.574614 pm'!

!CodeFileBrowser methodsFor: 'infoView' stamp: 'jmv 4/8/2016 12:53'!
extraInfo
	^ (self
		methodDiffFor: (self selectedClassOrMetaClass sourceCodeAt: self selectedMessageName)
		class: self selectedClass
		selector: self selectedMessageName
		meta: self metaClassIndicated) hasAnyAttribute
			ifTrue: [' - modified']
			ifFalse: [' - identical']! !

!methodRemoval: Text #unembellished!
Text removeSelector: #unembellished!
