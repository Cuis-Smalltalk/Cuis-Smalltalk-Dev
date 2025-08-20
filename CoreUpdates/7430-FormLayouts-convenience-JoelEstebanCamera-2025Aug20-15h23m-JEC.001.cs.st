'From Cuis7.5 [latest update: #7429] on 20 August 2025 at 3:28:57 pm'!

!FormLayoutMorph methodsFor: 'testing' stamp: 'JEC 8/20/2025 12:16:51'!
is: aSymbol
	^ aSymbol == #FormLayoutMorph or: [ super is: aSymbol ]! !


!LinearLayoutMorph methodsFor: 'testing' stamp: 'JEC 8/20/2025 12:17:00'!
is: aSymbol
	^ aSymbol == #LinearLayoutMorph or: [ super is: aSymbol ]! !


!SystemWindow methodsFor: 'layout' stamp: 'JEC 8/20/2025 12:15:57'!
beForm

	self useFormLayout! !

!SystemWindow methodsFor: 'layout' stamp: 'JEC 8/20/2025 15:19:15'!
useFormLayout

	(layoutMorph is: #FormLayoutMorph) ifFalse: [
		self useLayoutMorph:  LayoutMorph newForm ].! !

!SystemWindow methodsFor: 'layout' stamp: 'JEC 8/20/2025 12:17:55'!
useLayoutMorph: aNewLayout

	self validateLayoutMorphCanBeChanged.

	aNewLayout adoptWidgetsColor: self windowColor.
	
	layoutMorph
		ifNil: [ self addMorphFront: aNewLayout ]
		ifNotNil: [ self replaceSubmorph: layoutMorph by: aNewLayout ].
		
	layoutMorph :=  aNewLayout.
	
	"We need this so that the layout is within the interiorExtent"
	self extentChanged: self morphExtent.
	
	^ layoutMorph! !

!SystemWindow methodsFor: 'layout' stamp: 'JEC 8/20/2025 15:19:11'!
useLinearLayout

	(layoutMorph is: #LinearLayoutMorph) ifFalse: [
		self useLayoutMorph:  LayoutMorph newColumn ].! !

!SystemWindow methodsFor: 'layout' stamp: 'JEC 8/20/2025 15:16:13'!
validateLayoutMorphCanBeChanged

	(layoutMorph notNil and: [ layoutMorph hasSubmorphs ])
		ifTrue: [ self error: 'Cannot replace a LayoutMorph containing submorphs' ]! !


!SystemWindow methodsFor: 'layout' stamp: 'JEC 8/20/2025 15:26:32'!
beColumn
	
	self useLinearLayout.
	layoutMorph beColumn.! !

!SystemWindow methodsFor: 'layout' stamp: 'JEC 8/20/2025 15:26:47'!
beRow
	
	self useLinearLayout.
	layoutMorph beRow.! !

!methodRemoval: SystemWindow #replacingLayoutMorphWithSubmorphsErrorMessage stamp: 'JEC 8/20/2025 15:25:12'!
SystemWindow removeSelector: #replacingLayoutMorphWithSubmorphsErrorMessage!
!methodRemoval: SystemWindow #errorOnReplacingLayoutMorphWithSubmorphs stamp: 'JEC 8/20/2025 15:25:10'!
SystemWindow removeSelector: #errorOnReplacingLayoutMorphWithSubmorphs!
!methodRemoval: SystemWindow #createLayout:unless: stamp: 'JEC 8/20/2025 15:27:10'!
SystemWindow removeSelector: #createLayout:unless:!
