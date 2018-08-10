'From Cuis 5.0 of 7 November 2016 [latest update: #3401] on 10 August 2018 at 11:07:42 am'!

!SinglePackageBrowser methodsFor: 'lists' stamp: 'jmv 8/10/2018 11:05:40'!
defaultClassList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."
	| answer |
	answer _ selectedSystemCategory
		ifNil: [#()]
		ifNotNil: [
			(package includesSystemCategory: selectedSystemCategory)
				ifTrue: [ systemOrganizer listAtCategoryNamed: selectedSystemCategory ]
				ifFalse: [ 
					package extensionClassNamesIn: (selectedSystemCategory copyFrom: 2 to: selectedSystemCategory size) ]].
	selectedClassName ifNil: [
		answer size = 0 ifFalse: [
			selectedClassName _ answer first.
			self setClassOrganizer.
			self editSelection: #editClass ]].
	^answer! !

!methodRemoval: SinglePackageBrowser #classList!
SinglePackageBrowser removeSelector: #classList!
