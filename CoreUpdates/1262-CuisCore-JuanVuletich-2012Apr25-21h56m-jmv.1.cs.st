'From Cuis 4.0 of 21 April 2012 [latest update: #1261] on 25 April 2012 at 9:58:57 pm'!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 4/25/2012 10:26'!
changeSetForBaseSystem

	| csName numberToUse |
	Installing

		ifNil: [
			numberToUse _ self currentBaseCSNumber.
			ChangeSorter allChangeSets
				detect: [ :any | any name initialIntegerOrNil = numberToUse ]
				ifFound: [ :existing | ^existing ]
				ifNone: [
					csName _ (self baseSystemNameFor: numberToUse),
						(String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ]).
					^ChangeSorter existingOrNewChangeSetNamed: csName forBaseSystem: true ]]

		ifNotNil: [
			csName _ 'Affects-BaseSystem--Install-', Installing.
			^ChangeSorter existingOrNewChangeSetNamed: csName forBaseSystem: false
			"Changes are for the base system, but are not user's own changes..." ]! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 4/25/2012 10:25'!
changeSetForPackage: aCodePackage

	| csName |
	aCodePackage ifNil: [
		^self changeSetForBaseSystem ].
	csName _ Installing
		ifNil: [ 'UnsavedChangesTo-', aCodePackage name ]
		ifNotNil: [
			Installing = aCodePackage packageName
				ifTrue: [ 'Install-', Installing ]
				ifFalse: [ 'Affects-', aCodePackage name, '--Install-', Installing ]].
	^ChangeSorter existingOrNewChangeSetNamed: csName forBaseSystem: false! !


!CodeFileBrowserWindow class methodsFor: 'services' stamp: 'jmv 4/25/2012 09:37'!
installPackageStream: aStream

	| fullName pkName existing |
	fullName _ aStream name.
	pkName _ CodePackageFile packageNameFrom: fullName.
	existing _ CodePackage named: pkName createIfAbsent: false registerIfNew: false.
	(existing isNil
		or: [ existing hasUnsavedChanges not
			or: [ self confirm: 'If you install this package, there are unsaved changes that will be lost.', String newLineString, 'Continue?' ]]) ifTrue: [
		Cursor wait showWhile: [
			CodePackageFile
				installFileStream: aStream
				packageName: pkName
				fullName: fullName ]]! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 4/25/2012 09:36'!
install: aFileStream
	"Installs the package. Replace all existing code in the PackageInfo, removing any code that is not included in us."
	| localName newCodePackage |

	"Give reasonable warnings if there is stuff that can't be properly cleaned. Give the user the chance to abort."
	'=============' print.
	classesToRemove notEmpty ifTrue: [
		('classesToRemove: ', classesToRemove printString) print.
		'=============' print ].
	methodsToRemove notEmpty ifTrue: [
		'methodsToRemove: ' print.
		methodsToRemove do: [ :methodReference | methodReference print ].
		'=============' print ].
	
	"Tirar warning si hay que borrar cosas que no se puede, si hay referencias, etc. Quizas si vamos a borrar el ultimo implementor de un mensaje enviado?"

	"Crear, instalar y devolver una instancia de PackageInfo"
	newCodePackage_ CodePackage
		named: packageName
		createIfAbsent: true
		registerIfNew: true.
	newCodePackage
		fullFileName: fullName;
		sourceSystem: sourceSystem;
		description: description.

	"Esto crea el change set y carga ahi. OJO. En ese CS, hay que borrar todo lo que hay que borrar"
	"These were created in #fromFileStream: ... reuse?"
	localName _ FileDirectory localNameFor: fullName.
	ChangeSet installing: newCodePackage packageName do: [
		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ]].
	newCodePackage hasUnsavedChanges: false.
	Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
	
	"Tirar undeclared al transcript. warning si quedaron undeclared
	Es feo que tire an transcript undeclareds que despues no lo son..."
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ].

	"Descartar la instancia de CodePackageFile"
	^newCodePackage! !

!methodRemoval: CodePackage class #named:!
CodePackage class removeSelector: #named:!
!methodRemoval: CodePackage class #newNamed:!
CodePackage class removeSelector: #newNamed:!

!CodePackage class reorganize!
('packages access' deregister: installedPackages named:createIfAbsent:registerIfNew: register:)
('class initialization' initialize)
('searching' packageOfClass:ifNone: packageOfMethod:ifNone: packageOfMethodCategory:ofClass:ifNone: packageOfSystemCategory:ifNone:)
!

!methodRemoval: CodePackage #register!
CodePackage removeSelector: #register!

!CodePackage reorganize!
('comparing' = hash)
('enumerating' actualMethodsDo: allOverriddenMethodsDo: methodsInCategory:ofClass:do: overriddenMethodsDo: overriddenMethodsInClass:do: overrideCategoriesForClass:do: sortedMethods)
('modifying' addCoreMethod: addExtensionMethod: addMethod: baseCategoryOfMethod: externalBehaviors removeMethod:)
('listing' allOverriddenMethods allOverridenMethods classes classesAndMetaClasses coreMethods extensionClassNamesIn: extensionClasses extensionMethods foreignClasses foreignSystemCategories methods overriddenMethods overriddenMethodsInClass: overrideMethods selectors systemCategories systemCategoriesWithExtensionMethods)
('testing' category:matches: changeRecordForOverriddenMethod: coreCategoriesForClass: coreMethodsForClass: extensionCategoriesForClass: extensionMethodsForClass: extensionMethodsFromClasses: foreignExtensionCategoriesForClass: foreignExtensionMethodsForClass: includesAnyCode includesChangeRecord: includesClass: includesClassNamed: includesMethod:ofClass: includesMethodCategory:ofClass: includesMethodCategory:ofClassNamed: includesMethodReference: includesSystemCategory: isForeignClassExtension: isOverrideCategory: isOverrideMethod: isOverrideOfYourMethod: isYourClassExtension: methodsInCategory:ofClass: outsideClasses overrideCategoriesForClass: referenceForMethod:ofClass:)
('naming' methodCategoryPrefix name packageName packageName: systemCategoryPrefix)
('dependencies' externalCallers externalClasses externalRefsSelect:thenCollect: externalSubclasses externalUsers)
('source code management' linesOfCode)
('printing' asStringOrText printOn:)
('saving' save writeClassCommentsOn: writeClassDefinitionsOn: writeInitializersOn: writeMethodsOn: writeOnStream: writeSystemCategoriesOn:)
('accessing' description description: fullFileName fullFileName: hasUnsavedChanges hasUnsavedChanges: sourceSystem sourceSystem:)
!

!methodRemoval: ChangeSorter class #existingOrNewChangeSetNamed:!
ChangeSorter class removeSelector: #existingOrNewChangeSetNamed:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

| name |
ChangeSet allInstancesDo: [ :each |
	each hasUnsavedChanges: each isEmpty not.
	name _ each name.
	each isForBaseSystem: ((name at: 1) isDigit
		and: [ (name at: 1) isDigit
			and: [ (name at: 1) isDigit
				and: [ (name at: 1) isDigit
					and: [ name is: '-CuisCore-' substingAt: 5 ]]]])]!

