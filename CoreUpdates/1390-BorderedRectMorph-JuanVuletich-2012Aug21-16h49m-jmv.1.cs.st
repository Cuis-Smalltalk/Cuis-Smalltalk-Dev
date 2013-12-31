'From Cuis 4.0 of 21 April 2012 [latest update: #1386] on 21 August 2012 at 4:51:25 pm'!
!classDefinition: #PluggableMorph category: #'Morphic-Views for Models'!
BorderedRectMorph subclass: #PluggableMorph
	instanceVariableNames: 'model '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!

!CodePackageListWindow methodsFor: 'commands' stamp: 'jmv 8/21/2012 16:51'!
createPackage

	| pkName |
	pkName _ FillInTheBlank request: 'Name for new package?'.
	CodePackage
		named: pkName
		createIfAbsent: true
		registerIfNew: true! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 8/21/2012 16:50'!
browseFullProtocol
	"Create and schedule a new protocol browser on the currently selected class or meta."

	| aPBrowser label |
	model selectedClassOrMetaClass ifNotNil: [ :classOrMetaclass |
		aPBrowser _ ProtocolBrowser new on: classOrMetaclass.
		label _ 'Entire protocol of: ', classOrMetaclass name.
		MessageSetWindow open: aPBrowser label: label ]! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 8/21/2012 16:50'!
browseProtocol
	"Create and schedule a new protocol browser on the currently selected class or meta."
	| aPBrowser label |
	model selectedClassOrMetaClass ifNotNil: [ :classOrMetaclass |
		aPBrowser _ ProtocolBrowser new onSubProtocolOf: classOrMetaclass.
		label _'Sub-protocol of: ', classOrMetaclass name.
		MessageSetWindow open: aPBrowser label: label ]! !

!classDefinition: #PluggableMorph category: #'Morphic-Views for Models'!
BorderedRectMorph subclass: #PluggableMorph
	instanceVariableNames: 'model'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
