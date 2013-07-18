'From Cuis 4.1 of 12 December 2012 [latest update: #1760] on 17 July 2013 at 9:55:40.969337 am'!
'Description A small package whose purpose is to install the rest of the packages that comprise the StyledTextEditor project.'!
!provides: 'StyledTextInstaller' 1 0!
!requires: 'StyledText' 1 nil!
!requires: 'StyledTextNotebook' 1 nil!
!requires: 'StyledTextWiki' 1 nil!
!classDefinition: #StyledTextInstaller category: #StyledTextInstaller!
Object subclass: #StyledTextInstaller
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledTextInstaller'!
!classDefinition: 'StyledTextInstaller class' category: #StyledTextInstaller!
StyledTextInstaller class
	instanceVariableNames: ''!


!StyledTextInstaller commentStamp: '<historical>' prior: 0!
StyledTextInstaller new install!

!StyledTextInstaller methodsFor: 'private' stamp: 'bp 12/4/2011 09:14'!
createStyledTextEditorDocumentation
	"self new createStyledTextEditorDocumentation"
	| model styleSet |
	model _ StyledTextModel new.
	SystemWindow
		editFancierStyledText: model
		label: 'Styled Text Editor Documentation'.
	styleSet _ model styleSet.
	styleSet
		createDocumentationParagraphStyleSet;
		createDocumentationCharacterStyleSet! !

!StyledTextInstaller methodsFor: 'private' stamp: 'bp 4/5/2012 23:13'!
documentsDirectory
	^self repositoryDirectory directoryNamed: 'Documents'! !

!StyledTextInstaller methodsFor: 'features' stamp: 'bp 4/21/2012 14:58'!
featuresModelNames
	^#('STE - New Features' 'STE - Done Features' 'Cuis Features')! !

!StyledTextInstaller methodsFor: 'private' stamp: 'bp 4/22/2012 13:23'!
open: name
	| fileName model |
	fileName _ self documentsDirectory fullNameFor: name.
	model _ StyledTextModel fromFileNamed: fileName.
	^SystemWindow editFancierStyledText: model label: name! !

!StyledTextInstaller methodsFor: 'public' stamp: 'bp 12/4/2011 10:10'!
openExamples
	"self new openExamples"
	self
		openStyledTextEditorDocumentation;
		openMacbethExample! !

!StyledTextInstaller methodsFor: 'public' stamp: 'bp 4/5/2012 23:24'!
openMacbethExample
	"self new openMacbethExample"
	| window |
	window _ self open: 'Macbeth Example'.
	window ifNotNil: [window color: Color white]! !

!StyledTextInstaller methodsFor: 'public' stamp: 'bp 4/5/2012 23:24'!
openStyledTextEditorDocumentation
	"self new openStyledTextEditorDocumentation"
	self open: 'Styled Text Editor Documentation'! !

!StyledTextInstaller methodsFor: 'public' stamp: 'bp 12/7/2011 02:06'!
openStyledTextEditorFeatures
	"self new openStyledTextEditorFeatures"
	self featuresModelNames do: [:each | self open: each]! !

!StyledTextInstaller methodsFor: 'private' stamp: 'bp 12/4/2011 10:06'!
recreateDocumentationStyleSet
	"self new recreateDocumentationStyleSet"
	| model styleSet |
	model _ self styledTextModelNamed: 'Styled Text Editor Documentation'.
	styleSet _ model styleSet.
	styleSet
		createDocumentationParagraphStyleSet;
		createDocumentationCharacterStyleSet! !

!StyledTextInstaller methodsFor: 'features' stamp: 'bp 12/7/2011 02:08'!
recreateFeaturesStyleSet
	"self new recreateFeaturesStyleSet"
	self featuresModelNames do: [:each |
		| model styleSet |
		model _ self styledTextModelNamed: each.
		styleSet _ model styleSet.
		styleSet
			createFeaturesParagraphStyleSet;
			createFeaturesCharacterStyleSet]! !

!StyledTextInstaller methodsFor: 'private' stamp: 'jmv 4/30/2012 00:03'!
repositoryDirectory
	^(FileDirectory on: (CodePackage named: 'StyledTextInstaller' createIfAbsent: false registerIfNew: false) fullFileName) containingDirectory! !

!StyledTextInstaller methodsFor: 'private' stamp: 'bp 4/20/2012 21:19'!
save: name
	| model |
	model _ self styledTextModelNamed: name.
	model saveAs: (self documentsDirectory fullNameFor: name)! !

!StyledTextInstaller methodsFor: 'private' stamp: 'bp 12/4/2011 09:40'!
saveStyledTextEditorDocumentation
	"self new saveStyledTextEditorDocumentation"
	self save: 'Styled Text Editor Documentation'! !

!StyledTextInstaller methodsFor: 'features' stamp: 'bp 12/7/2011 02:07'!
saveStyledTextEditorFeatures
	"self new saveStyledTextEditorFeatures"
	self featuresModelNames do: [:each | self save: each]! !

!StyledTextInstaller methodsFor: 'private' stamp: 'bp 12/4/2011 09:59'!
styledTextModelNamed: name
	| window |
	window _ SystemWindow allInstances detect: [:each | each label = name].
	^window model! !

!StyledTextInstaller class methodsFor: 'class initialization' stamp: 'jmv 7/17/2013 09:55'!
initialize
	"
	StyledTextInstaller initialize
	"
	STETheme beCurrent.
	self new openExamples! !
StyledTextInstaller initialize!
