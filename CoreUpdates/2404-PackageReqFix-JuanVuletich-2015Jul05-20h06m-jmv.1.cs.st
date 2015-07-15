'From Cuis 4.2 of 25 July 2013 [latest update: #2402] on 5 July 2015 at 8:16:44.037613 pm'!

!CodePackageListWindow methodsFor: 'commands' stamp: 'jmv 7/5/2015 20:16'!
addRequirement
	"Ask user for a FeatureRequirement based on loaded packages"
	
	| current currentName packageNames reqiredNames selectionNames choices selection cuisBaseName req selectedName |
	current _ model selection.
	current ifNil: [ ^self ].

	packageNames := model packages collect: [ :pak | pak packageName ].
	currentName := current packageName.
	reqiredNames := current requires collect: [ :r | r name ].
	selectionNames := packageNames select: [ :name |
		((name = currentName) or: [reqiredNames includes: name]) not ].
	cuisBaseName := Feature baseSystemFeature name.
	choices := OrderedCollection with: #CANCEL.
	(reqiredNames includes: cuisBaseName)
		ifFalse: [ choices add: cuisBaseName ].		
	choices addAll: selectionNames.
	choices size = 1 ifTrue: [
		^ PopUpMenu inform: 'All loaded packages are already required, as is Cuis base system' ].
	selection := PopUpMenu
		withCaption: 'Choose package to require' 
		chooseFrom: choices.
	selection = 1
		ifTrue: [ ^ self ]  "1 -> Cancel"
		ifFalse: [
			selectedName := choices at: selection.
			req := (selectedName = cuisBaseName)
				ifTrue: [ Feature baseSystemFeature requirementOfMe ]
				ifFalse: [ (CodePackage installedPackages at: selectedName) requirementOfMe].
			current featureSpec requires: req.
			current hasUnsavedChanges: true.
			self changed: #requirement ]! !

